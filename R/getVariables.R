#-------------------------------------------------------------------------------
# Program: getVariables.R
# Objective: functions to get the variables service from WS1 or WS2
#             * getVariablesByCategory: for WS1
#             * getVariables2: for WS2
#             * getVariablesByExperiment: for WS2
# Authors: A. Charleroy, I.Sanchez, J.E.Hollebecq, E.Chourrout
# Creation: 24/01/2019
#-------------------------------------------------------------------------------

##' @title getVariablesByCategory
##'
##' @description Retrieves the variable by categories (environment or setpoint...)
##' @param category character, Name of the category to search
##' @param imageryProvider character, provider of the images
##' @param experimentURI character, URI of the experiment
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)

##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  connectToPHISWS(apiID="ws_1_public", 
##'                  username = "guestphis@supagro.inra.fr",
##'                  password = "guestphis")
##'  vars<-getVariablesByCategory(category="imagery",
##'           experimentURI = "http://www.phenome-fppn.fr/m3p/ARCH2017-03-30")
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
    Response <- opensilexWSClientR::getResponseFromWS(resource=paste0(get("VARIABLES",configWS),"/category/",category),
                                          attributes = attributes, wsVersion = 1)
    return(Response)
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
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @return a WSResponse object. In the 'data' part of the returned object, a data.frame is
##'    given, containing:
##' \describe{
##' \item{trait informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{method informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{unit informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{and uri, label and comment}{for each variable}
##' }
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
##'  vars <- getVariablesDetails(uri = "http://www.opensilex.org/demo/id/variables/v001")
##'  vars <- getVariablesDetails(label = "Leaf-Area_LAI-Computation_LAI")
##'  vars$data
##' }
##' @export
getVariablesDetails <- function(uri = "",
                          label = "",
                          trait = "",
                          method = "",
                          unit = "",
                          pageSize = NULL,
                          page = NULL){

  attributes <- list(pageSize=pageSize,
                     page = page)
  if (uri!="")    attributes <- c(attributes, uri = uri)
  if (label!="")  attributes <- c(attributes, label = label)
  if (trait!="")  attributes <- c(attributes, trait = trait)
  if (method!="") attributes <- c(attributes, method = method)
  if (unit!="")   attributes <- c(attributes, unit = unit)
  
  Response <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("VARIABLES_DETAILS", configWS)),
                                                    attributes = attributes, wsVersion = 2)
  
  # Convert the JSON data.frame in real R data.frame
  tmp<-Response$data

  Response$data<-cbind.data.frame(as.data.frame(tmp$trait),
                                          as.data.frame(tmp$method),
                                          as.data.frame(tmp$unit),
                                          as.data.frame(tmp$uri),
                                          as.data.frame(tmp$label))
  colnames(Response$data)<-c(paste("trait",colnames(tmp$trait),sep="."),
                             paste("method",colnames(tmp$method),sep="."),
                             paste("unit",colnames(tmp$unit),sep="."),
                             "uri",
                             "label")
  
  return(Response)
}


#----------------------------------------------------------------------------
##' @title getVariables2 (same as getVariablesDetails function)
##'
##' @description Retrieves the variable descriptions, trait, method and unit covered by the variable
##' @param uri character, search by the uri of a variable (optional)
##' @param label character, search by label (optional)
##' @param trait character, search by trait uri (optional)
##' @param method character, search by method uri (optional)
##' @param unit character, search variables by unit uri (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @return a WSResponse object. In the 'data' part of the returned object, a data.frame is
##'    given, containing:
##' \describe{
##' \item{trait informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{method informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{unit informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{and uri, label and comment}{for each variable}
##' }
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
##'  vars <- getVariables2(uri = "http://www.opensilex.org/demo/id/variables/v001")
##'  vars <- getVariables2(label = "Leaf-Area_LAI-Computation_LAI")
##'  vars$data
##' }
##' @name getVariables2-deprecated
##' @export
getVariables2 <- function(uri = "",
                          label = "",
                          trait = "",
                          method = "",
                          unit = "",
                          pageSize = NULL,
                          page = NULL){
      .Deprecated(msg = "'getVariables2' will change in the next version")
  # temporary code
  return(getVariablesDetails(uri = uri,
                      label = label,
                      trait = trait,
                      method = method,
                      unit = unit,
                      pageSize = pageSize,
                      page = page))
  # temporary comments
  # attributes <- list(pageSize=pageSize,page = page)
  # if (uri!="")    attributes <- c(attributes, uri = uri)
  # if (label!="")  attributes <- c(attributes, lavel = label)
  # if (trait!="")  attributes <- c(attributes, trait = trait)
  # if (method!="") attributes <- c(attributes, method = method)
  # if (unit!="")   attributes <- c(attributes, unit = unit)
  # 
  # response <- opensilexWSClientR:::getResponseFromWS(resource = paste0(get("VARIABLES", configWS)),
  #                                        attributes = attributes, wsVersion = 2)
  # 
  # # Convert the JSON data.frame in real R data.frame
  # tmp<-response$data
  # print(tmp[,1:4])
  # response$data<-cbind.data.frame(as.data.frame(tmp$trait),
  #                                 as.data.frame(tmp$method),
  #                                 as.data.frame(tmp$unit),
  #                                 tmp[,4:6])
  # colnames(response$data)<-c(paste("trait",colnames(tmp$trait),sep="."),
  #                            paste("method",colnames(tmp$method),sep="."),
  #                            paste("unit",colnames(tmp$unit),sep="."),
  #                            colnames(tmp)[4:6])
  # 
  # return(response)
}

#----------------------------------------------------------------------------
##' @title getVariablesByExperiment
##'
##' @description Retrieves the variable descriptions, trait, method and unit covered by the variable
##'       for a given experiment URI
##' @param uri character, search by the uri of an experiment
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @return a WSResponse object. In the 'data' part of the returned object, a data.frame is
##'    given, containing:
##' \describe{
##' \item{trait informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{method informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{unit informations:}{uri, label, comment, ontologiesReferences, properties}
##' \item{and uri, label and comment}{for each variable}
##' }
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @importFrom dplyr select starts_with
##' @importFrom tidyr gather
##' @examples
##' \donttest{
##' connectToPHISWS(apiID="ws_private",
##'                url = "http://www.opensilex.org/openSilexAPI/rest/",
##'                username="guest@opensilex.org",
##'                password="guest")
##' varExp<- getVariablesByExperiment(uri="http://www.opensilex.org/demo/DIA2017-1")
##' varExp$data
##' }
##' @export
getVariablesByExperiment <- function(uri = "",pageSize = NULL,page = NULL){

  attributes <- list(pageSize=pageSize,page = page)
  # Retrieve the information of the given experiment URI
  tmpExp<-getExperiments2(uri = uri)
  
  # Some datamanagement to retrieve the names of the variables
  # in THIS experiment (IS: 06/01/2020)
  inputVar<-names(tmpExp$data$variables)
    
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
