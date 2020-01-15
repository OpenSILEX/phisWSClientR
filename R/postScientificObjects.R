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
##'
##' @description send a scientific object to the web service
##' @param rdfType character, the rdfType of the scientific object ex: http://www.opensilex.org/vocabulary/oeso#Plot
##' @param geometry character, give the geometry of this scientific object. For example a plot can be : "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))"
##' @param experiment character, uri of the experiment of the scientific object
##' @param isPartOf character, a scientific object the scientific object is part of ???
##' @param properties list, a list for the properties. 
##' Note: The object can also be a data.frame with the 3 named column rdfType, relation and value. See example.
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
##' postScientificObjects(
##'   rdfType = "http://www.opensilex.org/vocabulary/oeso#Plot",
##'   experiment="http://www.opensilex.org/demo/DIA2017-1",
##'   properties=list(
##'     list(
##'       rdfType = "http://www.opensilex.org/vocabulary/oeso#Species",
##'       relation = "http://www.opensilex.org/vocabulary/oeso#hasSpecies",
##'       value = "http://www.phenome-fppn.fr/id/species/triticumaestivum"),
##'     list(
##'       rdfType = NA,
##'       relation ="http://www.w3.org/2000/01/rdf-schema#label",
##'       value ="objectAlias")
##' )
##' )
##'    }
##' @export
postScientificObjects <- function(rdfType, experiment, geometry = "", isPartOf = "", properties){
  attributes <- list()
  if (rdfType!="")    attributes <- c(attributes, rdfType = rdfType)       else stop("You must provide a type of scientific object")
  if (experiment!="") attributes <- c(attributes, experiment = experiment) else stop("You must provide a experiment")
  if (geometry!="")   attributes <- c(attributes, geometry = geometry)    
  if (isPartOf!="")   attributes <- c(attributes, isPartOf = isPartOf)
  if (length(properties)!=0)  attributes <- c(attributes, properties = list(properties))
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("SCIENTIFIC_OBJECTS", configWS)),
                                                     attributes = attributes, wsVersion = 2)
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
##' expDesign = data.frame(Alias = c("1/DZ_PG_67/ZM4394/WW/1/DIA2017-05-19", "2/DZ_PG_30/ZM4361/WW/1/DIA2017-05-19", "3/DZ_PG_49/ZM4389/WW/1/DIA2017-05-19"),
##'                   Type = c("Plot", "Plot", "Plot")
##'                   Experiment =c("http://www.opensilex.org/demo/DMO2000-1", "http://www.opensilex.org/demo/DMO2000-1", "http://www.opensilex.org/demo/DMO2000-1"),
##'                   Geometry = c("Polygon ((3.973359402 43.61321499, 3.973389651 43.61319639, 3.973437651 43.6132376, 3.973407402 43.6132562, 3.973359402 43.61321499))", "Polygon ((3.973389651 43.61319639, 3.9734199 43.61317779, 3.973467901 43.61321899, 3.973437651 43.6132376, 3.973389651 43.61319639)))", "Polygon ((3.9734199 43.61317779, 3.973450149 43.61315919, 3.97349815 43.61320039, 3.973467901 43.61321899, 3.9734199 43.61317779))",
##'                   Species = c("Maize", "Maize", "Maize"),
##'                   Variety = c("iPG310", "iPG152", "iPG228")
##'                   )
##' insert = data.frame(
##'   rdfType = expDesign$Type,
##'   experiment = expDesign$Experiment,
##'   geometry = expDesign$Geometry,
##'   properties = expDesign$ExperimentModalities
##' )
##' initProperties = function(table, template, line = 1){
##'   prop = eval(parse(text = template))
##'   svt = line
##'   if(line<dim(table)[1]){
##'     svt = svt + 1
##'     svtProp = initProperties(table = table, template = template, line = svt)
##'     prop = append(prop, svtProp)
##'   }
##'   return(prop)
##' }
##' template = 'list(
##'  data.frame(
##'       rdfType = c("http://www.opensilex.org/vocabulary/oeso#Species", "http://www.opensilex.org/vocabulary/oeso#Variety", NA),
##'       relation = c("http://www.opensilex.org/vocabulary/oeso#hasSpecies", "http://www.opensilex.org/vocabulary/oeso#hasVariety", "http://www.w3.org/2000/01/rdf-schema#label"),
##'       value = c(table$Species[line],table$Variety[line], table$Alias[line])
##'  )
##' )'
##' 
##' parseProperties = initProperties(table = p1, template = template, line = 1)
##' 
##' insert$properties = parseProperties
##' insert
##' postScientificObjectsTable(insert)
# apply(X = insert, MARGIN = 1, FUN = function(X){
#   postScientificObjects(experiment = X["experiment"], rdfType = X["rdfType"], geometry = X["geometry"], properties = X["properties"])
# }
# )
##'    }
##' @export
postScientificObjectsTable <- function(table){
  if(sum(c("rdfType", "experiment", "isPartOf", "properties", "geometry")%in%names(table))!=5 ) stop(" You should name the columns after the arguments rdfType, experiment, isPartOf, properties, geometry")
  
  attributes <- table[, c("rdfType", "experiment", "isPartOf", "geometry", "properties")]
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("SCIENTIFIC_OBJECTS", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}
