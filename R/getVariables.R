#-------------------------------------------------------------------------------
# Program: getVariables.R
# Objective: functions to get the variables service from WS1 or WS2
#             * getVariablesByCategory: for WS1
#             * getVariables2: for WS2
#             * getVariablesByExperiment: for WS2
# Authors: A. Charleroy, I.Sanchez, J.E.Hollebecq, E.Chourrout
# Creation: 24/01/2019
# Last Update: 02/07/2019 (A. Charleroy)
#-------------------------------------------------------------------------------

##' @title getVariablesByCategory
##'
##' @description Retrieves the variable by categories (environment or setpoint...)
##' @param category Name of the category to search
##' @param imageryProvider character, provider of the images
##' @param experimentURI URI of the experiment
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)

##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  connectToPHISWS(apiID="ws_public","guestphis@supagro.inra.fr","guestphis")
##'  vars <- getVariablesByCategory(category="imagery",
##'           experimentURI = "http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##'  vars$data
##' }
##' @export
getVariablesByCategory<-function(category ="",
                                 experimentURI ="",
                                 imageryProvider="",
                                 page=NULL,
                                 pageSize=NULL){
  
  
  attributes = list( page=page, pageSize = pageSize)
  if (category  == ""){
    stop("no category selected")
  } else {
    if (experimentURI != ""){
      attributes <- c(attributes, experimentURI = experimentURI)
    }
    if (imageryProvider != ""){
      attributes <- c(attributes, imageryProvider = imageryProvider)
    }
    variableResponse <- opensilexWSClientR::getResponseFromWS(resource=paste0(get("VARIABLES",configWS),"/category/",category),
                                          attributes = attributes, wsVersion = 1)
    return(variableResponse)
  }
}

#----------------------------------------------------------------------------
##' @title getVariablesDetails
##'
##' @description Retrieves the variable descriptions, trait, method and unit covered by the variable
##' @param uri character, search by the uri of a variable (optional)
##' @param label character, search by label (optional)
##' @param trait character, search by trait uri (optional)
##' @param method character, search by method uri (optional)
##' @param unit character, search variables by unit uri (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize character, number of elements by page (pagination Plant Breeding API)
##' @return a WSResponse object. In the 'data' part of the returned object, a data.frame is
##'    given, containing:
##' \describe{
##' \item{trait informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{method informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{unit informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{and uri, label and comment}{for each variable}
##' }
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  connectToPHISWS(apiID="ws_private","guest@opensilex.fr","guest", url = "http://www.opensilex.org/openSilexAPI/rest/")
##'  vars <- getVariablesDetails(uri = "http://www.opensilex.org/demo/id/variables/v001")
##'  vars <- getVariablesDetails(label = "Leaf-Area_LAI-Computation_LAI")
##'  vars$data
##' }
##' @export
getVariablesDetails <- function(
                          uri = "",
                          label = "",
                          trait = "",
                          method = "",
                          unit = "",
                          pageSize = NULL,
                          page = NULL){
  
 
  
  attributes <- list(pageSize=pageSize,
                     page = page)
  if (uri!="")    attributes <- c(attributes, uri = uri)
  if (label!="")  attributes <- c(attributes, lavel = label)
  if (trait!="")  attributes <- c(attributes, trait = trait)
  if (method!="") attributes <- c(attributes, method = method)
  if (unit!="")   attributes <- c(attributes, unit = unit)
  
  variableResponse <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("VARIABLES_DETAILS", configWS)),
                                         attributes = attributes, wsVersion = 2)
  
  # Convert the JSON data.frame in real R data.frame
  tmp<-variableResponse$data
  variableResponse$data<-cbind.data.frame(as.data.frame(tmp$trait),
                                          as.data.frame(tmp$method),
                                          as.data.frame(tmp$unit),
                                          tmp[,4:6])
  colnames(variableResponse$data)<-c(paste("trait",colnames(tmp$trait),sep="."),
                                     paste("method",colnames(tmp$method),sep="."),
                                     paste("unit",colnames(tmp$unit),sep="."),
                                     colnames(tmp)[4:6])
  
  return(variableResponse)
}


#----------------------------------------------------------------------------
##' @title getVariables2
##'
##' @description Retrieves the variable descriptions, trait, method and unit covered by the variable
##' @param uri character, search by the uri of a variable (optional)
##' @param label character, search by label (optional)
##' @param trait character, search by trait uri (optional)
##' @param method character, search by method uri (optional)
##' @param unit character, search variables by unit uri (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize character, number of elements by page (pagination Plant Breeding API)
##' @return a WSResponse object. In the 'data' part of the returned object, a data.frame is
##'    given, containing:
##' \describe{
##' \item{trait informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{method informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{unit informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{and uri, label and comment}{for each variable}
##' }
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  connectToPHISWS(apiID="ws_private","guest@opensilex.org","guest", url = "http://www.opensilex.org/openSilexAPI/rest/")
##'  vars <- getVariables2(uri = "http://www.opensilex.org/demo/id/variables/v001")
##'  vars <- getVariables2(label = "Leaf-Area_LAI-Computation_LAI")
##'  vars$data
##' }
##' @export
getVariables2 <- function(
                          uri = "",
                          label = "",
                          trait = "",
                          method = "",
                          unit = "",
                          pageSize = NULL,
                          page = NULL){
  
 
  
  attributes <- list(pageSize=pageSize,
                     page = page)
  if (uri!="")    attributes <- c(attributes, uri = uri)
  if (label!="")  attributes <- c(attributes, lavel = label)
  if (trait!="")  attributes <- c(attributes, trait = trait)
  if (method!="") attributes <- c(attributes, method = method)
  if (unit!="")   attributes <- c(attributes, unit = unit)
  
  variableResponse <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("VARIABLES", configWS)),
                                         attributes = attributes, wsVersion = 2)
  
  # Convert the JSON data.frame in real R data.frame
  tmp<-variableResponse$data
  variableResponse$data<-cbind.data.frame(as.data.frame(tmp$trait),
                         as.data.frame(tmp$method),
                         as.data.frame(tmp$unit),
                         tmp[,4:6])
  colnames(variableResponse$data)<-c(paste("trait",colnames(tmp$trait),sep="."),
                    paste("method",colnames(tmp$method),sep="."),
                    paste("unit",colnames(tmp$unit),sep="."),
                    colnames(tmp)[4:6])
  
  return(variableResponse)
}

#----------------------------------------------------------------------------
##' @title getVariablesByExperiment
##'
##' @description Retrieves the variable descriptions, trait, method and unit covered by the variable
##'       for a given experiment URI
##' @param uri character, search by the uri of an experiment
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize character, number of elements by page (pagination Plant Breeding API)
##' @return a WSResponse object. In the 'data' part of the returned object, a data.frame is
##'    given, containing:
##' \describe{
##' \item{trait informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{method informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{unit informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{and uri, label and comment}{for each variable}
##' }
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @importFrom dplyr select starts_with
##' @importFrom tidyr gather
##' @examples
##' \donttest{
##'  connectToPHISWS(apiID="ws_private","guest@opensilex.org/","guest", url = "http://www.opensilex.org/openSilexAPI/rest/")
##'  varExp<- getVariablesByExperiment(uri = "http://www.opensilex.org/demo/DIA2017-1")
##'  varExp$data
##' }
##' @export
getVariablesByExperiment <- function(
                          uri = "",
                          pageSize = NULL,
                          page = NULL
                          ){
  
 
  
  attributes <- list(pageSize=pageSize,
                     page = page)
  # Retrieve the information of the given experiment URI
  tmpExp<-getExperiments2(uri = uri)
  
  # Some datamanagement to retrieve the names of the variables
  # in THIS experiment
  tmp<-select(tmpExp$data,starts_with("variables"))
  tmp<-tmp[[1]]
  tmp<-gather(tmp,"Name","label",1:ncol(tmp))
  # We need only the uri of the variables: a vector
  inputVar<-tmp[,1]
  
  # Request on VARIABLES service to retrieve all the information
  # of the variables of ALL experiments
  tmpCountVar<-getVariables2()$totalCount
  tmpVar<-getVariables2(pageSize = tmpCountVar)$data

  # Filtering variables on THIS experiment
  tmpFilterData<-tmpVar[tmpVar[,"uri"] %in% inputVar,]
  
  # create the formatted output response
  if (nrow(tmpFilterData)!= 0){
    varResponse<-list(1,nrow(tmpFilterData),1,200,
                      "Query executed and data recovered",NULL,
                      tmpFilterData)
    names(varResponse)<-c("currentPage","totalCount","totalPages","codeHttp",
                          "codeHttpMessage","codeStatusMessage","data")
    
  } else {
    varResponse<-list(currentPage=NULL,totalCount=NULL,
                      totalPages=NULL,codeHttp=200,
                      codeHttpMessage="No variable available for this experiment",
                      codeStatusMessage=NULL,
                      data=NULL)
  }
  
  return(varResponse)
}
