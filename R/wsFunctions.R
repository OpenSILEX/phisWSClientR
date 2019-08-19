#-------------------------------------------------------------------------------
# Program: wsFunctions.R
# Objective: functions to facilitate requests on web service Phenomeapi
# Author: A. Charleroy
# Creation: 19/03/2018
# Update: 04/04/2019 by I.Sanchez
#-------------------------------------------------------------------------------


##' @title connectToOpenSILEXWS
##' @param apiID character, a character name of an API ("ws_public" or "ws_private")
##' @param url character, if apiID is private add the url of the chosen API, containing the IP,
##'            the full url with the protocol. e.g. 'http://www.opensilex.org/openSilexAPI/rest/'
##' @param username login of the user to create the token
##' @param password password of the user to create the token
##' @description load name space and connexion parameters of the webservice.
##' Execute only once at the beginning of the requests.
##' In the case of a WebService change of address or a renaming of services, please edit this list.
##' and execute the function.
##' WS1 - connectToOpenSILEXWS(apiID="ws_public","guestphis@supagro.inra.fr","guestphis")
##' WS2 - connectToOpenSILEXWS(apiID="ws_private",username="guest@opensilex.org",password="guest", url = "http://www.opensilex.org/openSilexAPI/rest/")
##' @export
connectToOpenSILEXWS<-function(apiID, username, password, url = ""){
  
  if (username == "") {
    stop("Please, give an username")
  }
  if (password == "") {
    stop("Please, give an user password")
  }  
  
  # configWS is an environment with specific variables to opensilex web service
  # if apiID is private, we use the url given by the user
  if (apiID == "ws_private") {
    if(url == ""){
      stop("Please, give an OpenSILEX WS full URL")
    } else{
      assign("BASE_PATH", url, configWS)
    }
  }
  
  # if apiID is public then we use the public configWS given by the package
  if (apiID == "ws_public") {
    assign("BASE_PATH",get("PUBLIC_PATH",configWS),configWS)
  }
  
  # get token
  tokenData = phisWSClientR::getToken(username,password)
  
  if(!is.null(tokenData) && length(tokenData) > 0) {
    setLoginUserInformations(username, password, tokenData)
  }else{
    stop("Not able to connect to the specified OpenSILEX WS")
  }
} 
 

##' @title getTokenResponseWS
##'
##' @description Create a token to call the webservice for authentication and
##' returns a formatted response of WSResponse class.
##' @param resource character, an resource from the web service api
##' @param attributes a list containing a login and a password

##' @details This function is OK for the first version of the web service
##'  (a GET call with a visible request)
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @return responseObject an object HTTP httr
##' @keywords internal
getTokenResponseWS<-function(resource,paramPath=NULL,attributes,type = "application/json"){
  webserviceBaseUrl <- get("BASE_PATH",configWS)
  urlParams <- ""
  # create the URL
  for (attribut in names(attributes)) {
    if(urlParams != ""){
      urlParams <- paste0(urlParams,"&")
    }
    if(is.character(attributes[[attribut]])){
      urlParams <- paste0(urlParams,attribut,"=",utils::URLencode(attributes[[attribut]],  reserved = TRUE))
    } else {
      urlParams <- paste0(urlParams,attribut,"=",attributes[[attribut]])
    }
  }
  if(is.null(paramPath)){
    finalurl <- paste0(webserviceBaseUrl, resource , "?", urlParams)
  } else {
    finalurl <- paste0(webserviceBaseUrl, resource ,"/",paramPath, "?", urlParams)
  }
  
  ptm <- proc.time()
  r <- httr::GET(finalurl)
  
  # debug
  if( names(logging::getLogger()$level) == "DEBUG"){
    logging::logdebug("Request Time : " )
    print(proc.time() - ptm)
    print(r)
  } 
  return(r)
}


##' @title getTokenResponseWS2
##'
##' @description Create a token to call the webservice for authentication and
##' returns a formatted response of WSResponse class.
##' @param resource character, an resource from the web service api
##' @param attributes a list containing a login and a password

##' @details This function is OK for the second version of the web service
##'  (a POST call with an invisible request using a correct JSON list in a body)
##' @seealso https://brapi.docs.apiary.io/#introduction/structure-of-the-response-object
##' @return responseObject an object HTTP httr
##' @importFrom openssl md5
##' @keywords internal
getTokenResponseWS2<-function(resource,attributes,type = "application/json"){
  # create the URL
  finalurl <- paste0(get("BASE_PATH",configWS),resource)

  # Create the body JSON list with the attributes
  # take care that httr::POST function doesn't allow to md5 object
  # I had to convert the md5 object into a string one with the toString() function
  finalbody<-list(grant_type="password",
                  username= attributes[[1]],
                  password=toString(md5(attributes[[2]])))

  # call
  ptm <- proc.time()
  r <- httr::POST(url=finalurl,body = finalbody,encode="json")
  
  # debug
  if( names(logging::getLogger()$level) == "DEBUG"){
    logging::logdebug("Request Time : " )
    print(proc.time() - ptm)
    print(r)
  } 
  
  return(r)
}

# ##' @title postResponseFromWS
# ##'
# ##' @description Create an URL to call the WS and return a formatted response of WSResponse class.
# ##' @param resource character, the name of the webservice resource
# ##' @param paramPath character, path URL encoded parameter
# ##' @param attributes query parameters
# ##' @param encode character, type of encodage
# ##' @param requestBody body data which will be send
# ##' @return WSResponse WSResponse class instance
# ##' @keywords internal
# postResponseFromWS<-function(resource, paramPath = NULL, attributes,  encode ="json", requestBody){
#   #configWS<-connectToOpenSILEXWS()
#   webserviceBaseUrl <- configWS[["BASE_PATH"]]
#   urlParams = ""
#   # create the l'url
#   for (attribut in names(attributes)) {
#     if (urlParams != ""){
#       urlParams = paste0(urlParams,"&")
#     }
#     #     chaines de caractere
#     if (is.character(attributes[[attribut]])){
#       urlParams = paste0(urlParams,attribut,"=",utils::URLencode(attributes[[attribut]],reserved = TRUE))
#       #       nombres
#     } else if (is.numeric(attributes[[attribut]])){
#       urlParams = paste0(urlParams,attribut,"=",format(attributes[[attribut]], scientific=FALSE))
#       # autres
#     } else {
#       urlParams = paste0(urlParams,attribut,"=",attributes[[attribut]])
#     }
#   }
#   if (is.null(paramPath)){
#     finalurl = paste0(webserviceBaseUrl, resource , "?", urlParams)
#   } else {
#     finalurl = paste0(webserviceBaseUrl, resource ,"/",paramPath, "?", urlParams)
#   }
#
#   ptm <- proc.time()
#   r <- httr::POST(finalurl, body = jsonlite::toJSON(requestBody,auto_unbox = TRUE))
#   if (verbose) {
#     print("Request Time : " )
#     print(proc.time() - ptm)
#     print(r)
#   }
#
#   if(r$status_code >= 500){
#     print("WebService internal error")
#   }
#   if(r$status_code == 401){
#     print("User not authorized")
#   }
#   if(r$status_code >= 400 && r$status_code != 401 &&  r$status_code < 500){
#     print("Bad user request")
#   }
#   if(r$status_code >= 200 && r$status_code < 300){
#     print("Query executed and data recovered")
#   }
#   return(getDataAndShowStatus(r))
# }

##' @title getDataAndShowStatus
##'
##' @description Recupere les status et les informations presentes dans les entetes de reponse HTTP
##'  ainsi que dans la partie metadata de la reponse
##' @param responseObject objet de reponse HTTP httr
##' @keywords internal
getDataAndShowStatus<-function(responseObject){
  status = NULL
  json = jsonlite::fromJSON(httr::content(responseObject, as = "text", encoding = "UTF-8"))
  if (responseObject$status_code >= 400){
    if (!is.null(json$metadata$status) && length(json$metadata$status) > 0){
      status = json$metadata$status
      logging::loginfo("Additional Request information :")
      logging::loginfo(status)
    }
    if(responseObject$status_code >= 500){
      msg = "WebService internal error"
    }
    if(responseObject$status_code == 401){
      msg = "User not authorized"
    }
    if(responseObject$status_code >= 400 && responseObject$status_code != 401 &&  responseObject$status_code < 500){
      msg = "Bad user request"
    }
    response <- list(
      currentPage = NULL,
      totalCount = NULL,
      totalPages = NULL,
      codeHttp = responseObject$status_code,
      codeHttpMessage = msg,
      codeStatusMessage = status,
      data = NULL
    )
  } else {
    if (!is.null(json$metadata$status) && length(json$metadata$status) > 0){
      logging::loginfo("Additional Request information :")
      logging::loginfo(json$metadata$status)
      status = json$metadata$status
    }
    if (responseObject$status_code >= 200 && responseObject$status_code < 300){
      msg = "Query executed and data recovered"
    }
    response <- list(
      currentPage = json$metadata$pagination$currentPage,
      totalCount = json$metadata$pagination$totalCount,
      totalPages = json$metadata$pagination$totalPages,
      codeHttp = responseObject$status_code,
      codeHttpMessage = msg,
      codeStatusMessage = status,
      data = json$result$data
    )
  }
  class(response) <- append(class(response),"WSResponse")
  return(response)
}

##' @title ObjectType
##' @param obj an object
##' @description Returns the type of object received by R Development function
##' @return string
##' @importFrom utils str
##' @keywords internal
ObjectType<-function(obj){
  return(utils::capture.output(str(obj)))
}

##'@title setDebugMode
##'@description Allows to retreive a particular level debugging messages
##'@seealso https://docs.python.org/3/library/logging.html#levels
##'@param level character, Default value values "INFO", Allowed values "CRITICAL"," ERROR", "WARNING", "INFO", "DEBUG", "NOTSET"
##'@export
setDebugMode<-function(level= "INFO"){
  if(level == "CRITICAL") logging::setLevel(50)
  if(level == "ERROR") logging::setLevel(40)
  if(level == "WARNING") logging::setLevel(30)
  if(level == "INFO") logging::setLevel(20)
  if(level == "DEBUG") logging::setLevel(10)
  if(level == "NOTSET") logging::setLevel(0)
}

##' @title setLoginUserInformations
##' @param tokenData S3 class, saves informations extract from WS getToken response
##' @description Save information in config environment
##' @keywords internal
setLoginUserInformations<-function(username,password,tokenData){
  # save user parameters in config environment
  assign("TOKEN_VALUE", tokenData$data, configWS)
  assign("USERNAME", username, configWS)
  assign("PASSWORD", password, configWS)
  assign("WS_VERSION", tokenData$webserviceVersion, configWS)
  assign("TOKEN_VALID_TIME",tokenData$expiresIn,configWS)
  assign("TOKEN_VALID",TRUE,configWS)
  assign("USER_VALID",TRUE,configWS)

  later::later(function(){assign("TOKEN_VALID",FALSE,configWS)},tokenData$expiresIn)
  assign("RECONNECT_ON_DISCONNECTION",tokenData$expiresIn,configWS)
  
  #debug
  logging::logdebug(paste("BASE_PATH",get("BASE_PATH", configWS)))
  logging::logdebug(paste("USERNAME",get("USERNAME",configWS)))
  logging::logdebug(paste("TOKEN_VALUE",get("TOKEN_VALUE",configWS)))
  logging::logdebug(paste("WS_VERSION",get("WS_VERSION",configWS)))
  logging::logdebug(paste("TOKEN_VALID_TIME",get("TOKEN_VALID_TIME",configWS)))
}

##' @title getConfigInformations
##' @description show useful informations from config environment
##' @return a dataframe with informations from config environment
##' @export
getUserInformations<-function(){
  if(is.null(get("TOKEN_VALUE",configWS))) stop("Connect first using connectionToOpenSILEXWS() function")
     
  print(paste("BASE_PATH ",get("BASE_PATH", configWS)))
  print(paste("USERNAME",get("USERNAME",configWS)))
  print(paste("TOKEN_VALUE",get("TOKEN_VALUE",configWS)))
  
  df <- data.frame("BASE_PATH" = get("BASE_PATH", configWS),
                   "USERNAME" = get("USERNAME",configWS),
                   "TOKEN_VALUE" = get("TOKEN_VALUE",configWS),
                   "TOKEN_VALID_TIME" = get("TOKEN_VALID_TIME",configWS),
                   "WS_VERSION" = get("WS_VERSION",configWS))
  str(df)
  return(df)
}