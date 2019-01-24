#-------------------------------------------------------------------------------
# Program: getResponseFromWS.R
# Objective: functions to facilitate requests on a OpenSILEX web service
#             * getResponseFromWS: for WS1
#             * getResponseFromWS2: for WS2
# Authors: A. Charleroy, I.Sanchez, J.E.Hollebecq, E.Chourrout
# Creation: 24/01/2019
# Update: 24/01/2019 by I.Sanchez
#-------------------------------------------------------------------------------

##' @title getResponseFromWS retrieves the data of a service from the WS1
##'
##' @description Create an URL to call the WS and retrun a formatted response of WSResponse class.
##' @param responseObject object HTTP httr
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @keywords internal
getResponseFromWS<-function(resource,paramPath = NULL,attributes,type="application/json",verbose=FALSE){
  webserviceBaseUrl <- get("BASE_PATH",configWS)
  urlParams <- ""
  # creation de l'url
  for (attribut in names(attributes)) {
    if (urlParams != ""){
      urlParams <- paste0(urlParams,"&")
    }
    # chaines de caractere
    if (is.character(attributes[[attribut]])){
      urlParams <- paste0(urlParams,attribut,"=",utils::URLencode(attributes[[attribut]],reserved = TRUE))
    # nombres
    } else if (is.numeric(attributes[[attribut]])){
      urlParams <- paste0(urlParams,attribut,"=",format(attributes[[attribut]], scientific=FALSE))
    # autres
    } else {
      urlParams <- paste0(urlParams,attribut,"=",attributes[[attribut]])
    }
  }
  if (is.null(paramPath)){
    finalurl <- paste0(webserviceBaseUrl, resource , "?", urlParams)
  } else {
    finalurl <- paste0(webserviceBaseUrl, resource ,"/",paramPath, "?", urlParams)
  }
  
  ptm <- proc.time()
  r <- httr::GET(finalurl)
  if (verbose) {
    print("Request Time : " )
    print(proc.time() - ptm)
    print(r)
  }
  
  if(r$status_code >= 500){
    print("WebService internal error")
  }
  if(r$status_code == 401){
    print("User not authorized")
  }
  if(r$status_code >= 400 && r$status_code != 401 &&  r$status_code < 500){
    print("Bad user request")
  }
  if(r$status_code >= 200 && r$status_code < 300){
    print("Query executed and data recovered")
  }
  
  return(getDataAndShowStatus(r))
}

#----------------------------------------------------------------------#
##' @title getResponseFromWS2 retrieves the data of a service from the WS2
##'
##' @description Create an URL to call the WS and retrun a formatted response of WSResponse class.
##' @param resource character, the name of the service to call
##' @param paramPath character, the extension of the service to call, default to NULL
##' @param attributes character, the list of attributes to give to the GET request
##' @param type character, the type of the output, default to application/json
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @keywords internal
getResponseFromWS2 <- function(resource, paramPath = NULL, attributes, type = "application/json", verbose = FALSE){
  webserviceBaseUrl <- get("BASE_PATH", configWS)
  urlParams <- ""
  # url concatenation
  for (attribut in names(attributes)) {
    if (urlParams != ""){
      urlParams <- paste0(urlParams, "&")
    }
    #     character arguments
    if (is.character(attributes[[attribut]])){
      urlParams <- paste0(urlParams, attribut, "=", utils::URLencode(attributes[[attribut]], reserved = TRUE))
      #   numeric arguments
    } else if (is.numeric(attributes[[attribut]])){
      urlParams <- paste0(urlParams, attribut, "=", format(attributes[[attribut]], scientific = FALSE))
      #   other arguments
    } else {
      urlParams <- paste0(urlParams, attribut, "=", attributes[[attribut]])
    }
  }
  if (is.null(paramPath)){
    finalurl <- paste0(webserviceBaseUrl, resource , "?", urlParams)
  } else {
    finalurl <- paste0(webserviceBaseUrl, resource , "/", paramPath, "?", urlParams)
  }
  
  ptm <- proc.time()
  r <- httr::GET(finalurl, config = httr::add_headers(Authorization=paste("Bearer ",attributes$Authorization, sep = "")))
  if (verbose) {
    print("API request : ")
    print(finalurl)
    print("API request : ")
    print(finalurl)
    print("Request Time : " )
    print(proc.time() - ptm)
    print(r)
  }
  
  if(r$status_code >= 500){
    print("WebService internal error")
  }
  if(r$status_code == 401){
    print("User not authorized")
  }
  if(r$status_code >= 400 && r$status_code != 401 &&  r$status_code < 500){
    print("Bad user request")
  }
  if(r$status_code >= 200 && r$status_code < 300){
    print("Query executed and data recovered")
  }
  
  return(getDataAndShowStatus(r))
}
