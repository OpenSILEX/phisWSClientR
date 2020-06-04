#-------------------------------------------------------------------------------
# Program: postScientificObjects.R
# Objective: functions to post a new scientific object to the WS2
#            * postScientificObjects for a single Scientific Object
#            * postScientificObjectsTable for a table of Scientific Objects
# Authors: Hollebecq Jean-Eudes, Arnaud Charleroy
# Creation: 24/09/2019
# Update: 04/06/2020 (A.Charleroy)
#-------------------------------------------------------------------------------

##' @title postScientificObjects
##' @import stringr
##' @import dplyr
##' @import wicket
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
##'      Species = c("Maize", "Maize", "Maize"), 
##'      Variety = c("iPG310", "iPG152", "iPG228"),
##'      Geometry = c(
##'      "POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))",
##'      "POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))",
##'      "POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))"
##'      ),
##'      stringsAsFactors = FALSE
##'  )
##'  r<- postScientificObjects(expDesign)
##'  r$success
##'  r$metadata 
##'    }
##' @export
postScientificObjects <- function(expDesign){
  if(sum(c("Alias","Type", "Experiment")%in%names(expDesign)) < 3 ) stop(" Authorized columns Required => Alias, Type, Experiment ;  Optional => (Species, Treatement, Replication, Parent, Variety, Geometry.)")
  scientificObjectsFormatted <- list()
  Response = list()
  
  
  # check experiments
  experiments <- tibble::as_tibble( getExperiments2()$data, stringsAsFactors=FALSE)
  experimentsURI <- experiments %>% dplyr::select(uri)
  
  dfExp <- dplyr::setdiff(tibble::tibble(uri = expDesign$Experiment),experimentsURI)
  if(nrow(dfExp) > 0 ){
    stop("Wrong experiments set : ",dfExp)
  }
  
  # check species
  
  if("Species" %in% colnames(expDesign)){
    species <- tibble::as_tibble(getSpecies()$data, stringsAsFactors=FALSE)
    speciesLabel <- species %>% dplyr::select(label)
    
    if(is.null(species) || missing(species) ){
      stop("Missing species in OpenSILEX Instance")
    } 
    logging::logdebug(species$label)
    dfSpecies <- dplyr::setdiff(tibble::tibble(label = expDesign$Species),speciesLabel)
    if(nrow(dfSpecies) > 0 ){
        stop("Wrong species set : ", dfSpecies)
    }
  } 
   
# check vocabularies
vocabularies = getVocabulary()$data

listSO <- list()
for(numSO in 1:nrow(expDesign)){ 
  scientificObject <- ScientificObjectPostDTO$new(
    rdfType = paste(vocabularies$namespace[str_detect(vocabularies$namespace, pattern = "oeso")],expDesign[numSO,]$Type, sep = "") ,
    experiment = expDesign[numSO,]$Experiment
  )
  if(!is.null(expDesign[numSO,]$Parent)){
    scientificObject$isPartOf = expDesign[numSO,]$Parent
  }
  if(!is.null(expDesign[numSO,]$Year)){
    scientificObject$year = expDesign[numSO,]$Year
  }
  if(!is.null(expDesign[numSO,]$Geometry)){
    # check geometries
    validGeo <- wicket::validate_wkt(wicket::wkt_correct(expDesign[numSO,]$Geometry))[1,]
    if(!validGeo$is_valid){
      stop("Wrong geometry set : ", expDesign[numSO,]$Geometry, " ", validGeo$comments)
    }
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
    spName = expDesign[numSO,]$Species
    sp = species %>% dplyr::filter(label == spName)
    nPo = PropertyPostDTO$new(
      #rdfType = "http://www.opensilex.org/vocabulary/oeso#Species",
      rdfType = unique(paste(vocabularies$namespace[str_detect(vocabularies$namespace, pattern = "oeso")],"Species", sep = "")),
      #relation = "http://www.opensilex.org/vocabulary/oeso#hasSpecies",
      relation = unique(paste(vocabularies$namespace[str_detect(vocabularies$namespace, pattern = "oeso")],"hasSpecies", sep = "")),
      value = sp$uri
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
  listSO <- append(scientificObject,listSO)
}

postScientificObject = ScientificObjectsApi$new()
wsResponse = postScientificObject$post_scientific_object(listSO)

return(wsResponse)
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
##' @noRd
postScientificObjectsTable <- function(table){
  if(sum(c("rdfType", "experiment", "isPartOf", "properties")%in%names(table))!=4 ) stop(" You should name the columns after the arguments rdfType, experiment, isPartOf, properties, geometry")
  
  attributes <- table[, c("rdfType", "experiment", "isPartOf", "geometry", "properties")]
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("SCIENTIFIC_OBJECTS", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}
