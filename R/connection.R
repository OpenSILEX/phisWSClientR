#-------------------------------------------------------------------------------
# Program: connection.R
# Objective: functions to facilitate connection on OpenSILEX web service
# Author: A. Charleroy
# Creation: 04/09/2019
# Update: 06/09/2019 (I.Sanchez)
#-------------------------------------------------------------------------------


##' @title connectToPHISWS
##' @param apiID character, a character name of an API ("ws_public" or "ws_private")
##' @param url character, if apiID is private add the url of the chosen API, containing the IP,
##'            the full url with the protocol. e.g. 'http://www.opensilex.org/openSilexAPI/rest/'
##' @param username login of the user to create the token
##' @param password password of the user to create the token
##' @param reconnection to force the client reconnection
##' @import opensilexWSClientR
##' @description load name space and connexion parameters of the webservice.
##' Execute only once at the beginning of the requests.
##' In the case of a WebService change of address or a renaming of services, please edit this list.
##' and execute the function.
##' Demonstration instances : 
##' \describe{
##' \item{WS1}{connectToPHISWS(apiID="ws_1_public","guestphis@supagro.inra.fr","guestphis")}
##' \item{WS2}{connectToPHISWS(apiID="ws_2_public","guest@opensilex.org","guest")}
##' \item{WS2}{connectToPHISWS(apiID="ws_private",username="guest@opensilex.org",password="guest", 
##' url = "http://www.opensilex.org/openSilexAPI/rest/")}
##' }
##' @export
connectToPHISWS<-function(apiID, username = NULL, password = NULL, url = NULL, reconnection = TRUE){
  
  # if apiID is public then we use the public configWS given by the package
  if (apiID == "ws_1_public") {
    username = get("WS_1_PUBLIC_USERNAME",configWS)
    password = get("WS_1_PUBLIC_PASSWORD",configWS)
    url = get("WS_1_PUBLIC_PATH",configWS)
  }
  
  if (apiID == "ws_2_public") {
    username = get("WS_2_PUBLIC_USERNAME",configWS)
    password = get("WS_2_PUBLIC_PASSWORD",configWS)
    url = get("WS_2_PUBLIC_PATH",configWS)
  }
  
  # configWS is an environment with specific variables to opensilex web service
  # if apiID is private, we use the url given by the user
  
  if (apiID == "ws_private") {
    if (is.null(username)) {
      stop("Please, give an username")
    }
    if (is.null(password)) {
      stop("Please, give an user password")
    }  
    if(is.null(url)){
      stop("Please, give an OpenSILEX WS full URL")
    }
  }
  opensilexWSClientR::connectToOpenSILEXWS(username = username,
                                           password = password,
                                           url = url)

} 