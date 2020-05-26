#-------------------------------------------------------------------------------
# Program: postScientificObjects.R
# Objective: functions to post a new scientific object to the WS2
#            * postScientificObjects for a single Scientific Object
#            * postScientificObjectsTable for a table of Scientific Objects
# Authors: Hollebecq Jean-Eudes
# Creation: 24/09/2019
# Update: 08/01/2020 (J-E.Hollebecq)
#-------------------------------------------------------------------------------

##' @title postScientificObjects
##' @import stringr
##' @description send a scientific object to the web service
##' @param expDesign a dataframe containing the following columns 
##'        rdfType character, the rdfType of the scientific object ex: http://www.opensilex.org/vocabulary/oeso#Plot
##'        Geometry character, give the geometry of this scientific object. For example a plot can be : "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))"
##'        Experiment character, uri of the experiment of the scientific object
##'        parent character, an other scientific object the disignated scientific object is part of
##'        properties list, a column for the different properties avaliable. To see the different properties use \code{\link{getVocabulary}} 
##' Note: The object can also be a data.frame with the 3 named column rdfType, relation and value. See example.
##' @section Important:
##' The properties list must contain the rdfs:label block ; also known as Alias.
##' See example.
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##' connectToPHISWS(apiID="ws_private",
##'                url = "http://www.opensilex.org/openSilexAPI/rest/",
##'                username="guest@opensilex.org",
##'                password="guest")
##' expDesign = data.frame(
##'      Alias = c(
##'       "1/DZ_PG_67/ZM4394/WW/1/DIA2017-05-19",
##'       "2/DZ_PG_30/ZM4361/WW/1/DIA2017-05-19",
##'       "3/DZ_PG_49/ZM4389/WW/1/DIA2017-05-19"),
##'      Type = c("Plot", "Plot", "Plot"),
##'      Experiment = c(
##'      "http://www.opensilex.org/demo/DMO2000-1", 
##'      "http://www.opensilex.org/demo/DMO2000-1", 
##'      "http://www.opensilex.org/demo/DMO2000-1"),
##'      Geometry = c("Polygon ((3.973359402 43.61321499, 3.973389651 43.61319639, 3.973437651 43.6132376, 3.973407402 43.6132562, 3.973359402 43.61321499))", 
##'      "Polygon ((3.973389651 43.61319639, 3.9734199 43.61317779, 3.973467901 43.61321899, 3.973437651 43.6132376, 3.973389651 43.61319639))", 
##'      "Polygon ((3.9734199 43.61317779, 3.973450149 43.61315919, 3.97349815 43.61320039, 3.973467901 43.61321899, 3.9734199 43.61317779))"),
##'      Species = c("Maize", "Maize", "Maize"),
##'      Variety = c("iPG310", "iPG152", "iPG228")
##'  )
##'    }
##' @export
postScientificObjects <- function(expDesign){
  if(sum(c("Alias","rdfType", "Experiment")%in%names(expDesign)) < 3 ) stop(" Authorized columns Required => Alias, rdfType, Experiment ;  Optional => (Species, Treatement, Replication, Parent, Variety, Geometry.)")
  scientificObjectsFormatted <- list()
  Response = list()
  if("Species" %in% colnames(expDesign)){
      
    species <- as.data.frame(getSpecies()$data, stringsAsFactors=FALSE)
    if(is.null(species) || missing(species) ){
      stop("Missing species")
    }
  
    for(sp in  species$data){
      print(sp)
    }
  }
  
  species$data$label
  vocabularies = getVocabulary()$data
  
  for(numSO in 1:nrow(expDesign)){
    scientificObject <- ScientificObjectPostDTO$new(
      rdfType = expDesign[numSO,]$rdfType,
      experiment = expDesign[numSO,]$Experiment
    )
    if(!is.null(expDesign[numSO,]$Parent)){
      scientificObject$isPartOf = expDesign[numSO,]$Parent
    }
    if(!is.null(expDesign[numSO,]$Year)){
      scientificObject$year = expDesign[numSO,]$Year
    }
    if(!is.null(expDesign[numSO,]$Geometry)){
      scientificObject$geometry = expDesign[numSO,]$Geometry
    }
    PropertyList <- list()
    if(!is.null(expDesign[numSO,]$Alias)){
      aliasProperty = PropertyPostDTO$new()
      aliasProperty$rdfType =   NA
      aliasProperty$relation = paste(vocabularies$namespace[stringr::str_detect(vocabularies$namespace, pattern = "rdf-schema")], "label", sep="")
      # aliasProperty$relation = "http://www.w3.org/2000/01/rdf-schema#label"
      aliasProperty$value = expDesign[numSO,]$Alias
      PropertyList <- append( PropertyList, aliasProperty)
    }
    
    if(!is.null(expDesign[numSO,]$Species)){
      nPo = PropertyPostDTO$new(
        #rdfType = "http://www.opensilex.org/vocabulary/oeso#Species",
        rdfType = unique(paste(vocabularies$namespace[str_detect(vocabularies$namespace, pattern = "oeso")],"Species", sep = "")),
        #relation = "http://www.opensilex.org/vocabulary/oeso#hasSpecies",
        relation = unique(paste(vocabularies$namespace[str_detect(vocabularies$namespace, pattern = "oeso")],"hasSpecies", sep = "")),
        value = expDesign[numSO,]$Species
      )
      PropertyList <- append( PropertyList, nPo)
    }

    if(!is.null(expDesign[numSO,]$Variety)){
      nPo2 = PropertyPostDTO$new(
        #rdfType =  "http://www.opensilex.org/vocabulary/oeso#Variety",
        rdfType =  unique(paste(vocabularies$namespace[str_detect(vocabularies$namespace, pattern = "oeso")],"Variety", sep = "")),
        #relation = "http://www.opensilex.org/vocabulary/oeso#hasVariety",
        relation = unique(paste(vocabularies$namespace[str_detect(vocabularies$namespace, pattern = "oeso")], "hasVariety", sep = "")),
        value = expDesign[numSO,]$Variety
      )
      PropertyList <- append( PropertyList, nPo2)
    }

    if(!is.null(expDesign[numSO,]$Treatment)){
      nPo3 = PropertyPostDTO$new(
        rdfType =  NA, 
        #relation = "http://www.opensilex.org/vocabulary/oeso#hasExperimentModalities",
        relation =  unique(paste(vocabularies$namespace[str_detect(vocabularies$namespace, pattern = "oeso")], "hasExperimentModalities", sep = "")),
        value = expDesign[numSO,]$Treatment
      )
      PropertyList <- append( PropertyList, nPo3)
    }

    if(!is.null(expDesign[numSO,]$Replication)){
      nPo4 = PropertyPostDTO$new(
        rdfType =  NA, 
        #relation = "http://www.opensilex.org/vocabulary/oeso#hasReplication",
        relation = unique(paste(vocabularies$namespace[str_detect(vocabularies$namespace, pattern = "oeso")], "hasReplication", sep = "")),
        value = expDesign[numSO,]$Treatment
      )
      PropertyList <- append( PropertyList, nPo4)
    }
    scientificObject$properties = PropertyList

    postScientificObject = ScientificObjectsApi$new()
    wsResponse = postScientificObject$post_scientific_object(scientificObject)
    Response = append(Response, wsResponse$metadata)
  }
  
  
  
  return(Response)
}


##' @title postScientificObjectsTable
##'
##' @description send a scientific object to the web service
##' @param table data.frame, the table of the scientific objects to enter in the Web Service. The table must have the correcponding columns: rdfType, experiment, isPartOf, geometry, properties. In order to facilitate the parsing of data, you can use this function : (une fonction à écrire)
##' @section Important:
##' The properties list must contain the rdfs:label block ; also known as alias.
##' See example.
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##' connectToPHISWS(apiID="ws_private",
##'                url = "http://www.opensilex.org/openSilexAPI/rest/",
##'                username="guest@opensilex.org",
##'                password="guest")
##' expDesign = data.frame(
##'     Alias = c(
##'       "1/DZ_PG_67/ZM4394/WW/1/DIA2017-05-19",
##'       "2/DZ_PG_30/ZM4361/WW/1/DIA2017-05-19",
##'       "3/DZ_PG_49/ZM4389/WW/1/DIA2017-05-19"),
##'      Type = c("Plot", "Plot", "Plot"),
##'      Experiment = c(
##'      "http://www.opensilex.org/demo/DMO2000-1", 
##'      "http://www.opensilex.org/demo/DMO2000-1", 
##'      "http://www.opensilex.org/demo/DMO2000-1"),
##'      Geometry = c("Polygon ((3.973359402 43.61321499, 3.973389651 43.61319639, 3.973437651 43.6132376, 3.973407402 43.6132562, 3.973359402 43.61321499))", 
##'      "Polygon ((3.973389651 43.61319639, 3.9734199 43.61317779, 3.973467901 43.61321899, 3.973437651 43.6132376, 3.973389651 43.61319639))", 
##'      "Polygon ((3.9734199 43.61317779, 3.973450149 43.61315919, 3.97349815 43.61320039, 3.973467901 43.61321899, 3.9734199 43.61317779))"),
##'      Species = c("Maize", "Maize", "Maize"),
##'      Variety = c("iPG310", "iPG152", "iPG228")
##'  )
##' insert = data.frame(
##'   rdfType = expDesign$Type,
##'   experiment = expDesign$Experiment,
##'   geometry = expDesign$Geometry,
##'   properties = expDesign$ExperimentModalities
##' )
##'    }
##' @export
postScientificObjectsTable <- function(table){
  if(sum(c("rdfType", "experiment", "isPartOf", "properties", "geometry")%in%names(table))!=5 ) stop(" You should name the columns after the arguments rdfType, experiment, isPartOf, properties, geometry")
  
  attributes <- table[, c("rdfType", "experiment", "isPartOf", "geometry", "properties")]
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("SCIENTIFIC_OBJECTS", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}
