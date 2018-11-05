#-------------------------------------------------------------------------------
# Program: wsFunctions.R
# Objective: functions to facilitate requests on web service Phenomeapi
# Author: A. Charleroy
# Creation: 19/03/2018
# Update: 03/07/2018 by I.Sanchez
#-------------------------------------------------------------------------------


##' @title initializeClientConnection
##' @param apiID character, a character name of an API ("ws_public" or "ws_private")
##' @param url character, if apiID is private add the url of the chosen API, containing the IP,
##'            the port and the path of the WS
##'
##' @description load name space and connexion parameters of the webservice.
##' Execute only once at the beginning of the requests.
##' In the case of a WebService change of address or a renaming of services, please edit this list.
##' and execute the function.
##' @export
initializeClientConnection<-function(apiID,url = ""){
  # if apiID is public then we use the public configWS given by the package
  # else if apiID is private, we use the url procided by the user
  if (apiID == "ws_private") {
    if(url != ""){
      # configWS is an environment with specific variables to phenomeapi web service
      assign("BASE_PATH",paste0("http://",url),configWS)
    } else {
      print("Please, you have to give an URL and port address")
    }
  } else if (apiID == "ws_public") {
      assign("BASE_PATH",get("PUBLIC_PATH",configWS),configWS)
  }
}

##' @title getTokenResponseWS
##'
##' @description Create a token to call the webservice for authentication and
##' returns a formatted response of WSResponse class.
##' @param resource character, an resource from the web service api
##' @param attributes a list containing a login and a password
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @details This function is OK for the first version of the web service
##'  (a GET call with a visible request)
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @return responseObject an object HTTP httr
##' @keywords internal
getTokenResponseWS<-function(resource,paramPath=NULL,attributes,type = "application/json",verbose=FALSE){
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
  if (verbose) {
    print("Request Time : " )
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
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @details This function is OK for the second version of the web service
##'  (a POST call with an invisible request using a correct JSON list in a body)
##' @seealso https://brapi.docs.apiary.io/#introduction/structure-of-the-response-object
##' @return responseObject an object HTTP httr
##' @importFrom openssl md5
##' @keywords internal
getTokenResponseWS2<-function(resource,attributes,type = "application/json",verbose=FALSE){
  # create the URL
  #finalurl <- paste0(get("BASE_PATH",configWS),"brapi/v1/token")
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
  if (verbose) {
    print("Request Time : " )
    print(proc.time() - ptm)
    print(r)
  }

  # if (r$status_code >= 500){
  #   print("WebService internal error")
  # }
  # if (r$status_code == 401){
  #   print("User not authorized")
  # }
  # if (r$status_code >= 400 && r$status_code != 401 &&  r$status_code < 500){
  #   print("Bad user request")
  # }
  # if (r$status_code >= 200 && r$status_code < 300){
  #   print("Query executed and data recovered")
  # }
  return(r)
}



##' @title getResponseFromWS
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
    #     chaines de caractere
    if (is.character(attributes[[attribut]])){
      urlParams <- paste0(urlParams,attribut,"=",utils::URLencode(attributes[[attribut]],reserved = TRUE))
      #       nombres
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

# ##' @title postResponseFromWS
# ##'
# ##' @description Create an URL to call the WS and return a formatted response of WSResponse class.
# ##' @param resource character, the name of the webservice resource
# ##' @param paramPath character, path URL encoded parameter
# ##' @param attributes query parameters
# ##' @param encode character, type of encodage
# ##' @param requestBody body data which will be send
# ##' @param verbose logical FALSE by default, if TRUE display information about the progress
# ##' @return WSResponse WSResponse class instance
# ##' @keywords internal
# postResponseFromWS<-function(resource, paramPath = NULL, attributes,  encode ="json", requestBody, verbose=FALSE){
#   #configWS<-initializeClientConnection()
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
      print("Additional Request information :")
      print(json$metadata$status)
      status = json$metadata$status
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
      print("Additional Request information :")
      print(json$metadata$status)
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
