#-------------------------------------------------------------------------------
# Program: postSensors.R
# Objective: functions to post a new sensor to the WS2
#            * postSensors
# Authors: Hollebecq Jean-Eudes
# Creation: 24/09/2019 
# Update: 15/11/2019 (JE.H)
#-------------------------------------------------------------------------------

##' @title postSensors
##'
##' @description send a sensor to the web service
##' @param rdfType character, the rdfType of the sensor ex: http://www.opensilex.org/vocabulary/oeso#Spectrometer
##' @param label character, give the label of this sensor
##' @param brand character, the brand of the sensor
##' @param model character, the model of the sensor
##' @param serialNumber character, the serial number of the sensor
##' @param inServiceDate character, date which the sensor has been in service. Format "AAAA-MM-DD"
##' @param dateOfPurchase character, date which the sensor has been purchased. Format "AAAA-MM-DD"
##' @param dateOfLastCalibration character, date which the sensor has been last calibrated. Format "AAAA-MM-DD"
##' @param personInCharge the mail of the person in charge of this sensor
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
##' postSensors(rdfType = "http://www.opensilex.org/vocabulary/oeso#Spectrometer",label = "Sensor_label",
##'             brand = "Sensor_brand",model = "Sensor_model",serialNumber = "",
##'             inServiceDate = "2017-06-15",dateOfPurchase = "2017-06-15",
##'             dateOfLastCalibration = "2017-06-15",personInCharge = "admin@opensilex.org"
##' )
##'    }
##' @export
postSensors <- function(rdfType, label, brand, model, serialNumber, inServiceDate, dateOfPurchase, dateOfLastCalibration, personInCharge){
  attributes <- list()
  if (rdfType!="")        attributes <- c(attributes, rdfType = rdfType)   else stop("You must provide a type of sensor")
  if (label!="")          attributes <- c(attributes, label = label)       else stop("You must provide a label")
  if (brand!="")          attributes <- c(attributes, brand = brand)       else stop("You must provide a brand")
  if (model!="")          attributes <- c(attributes, model = model)       #else stop("You must provide a model")
  if (serialNumber!="")   attributes <- c(attributes, serialNumber = serialNumber) 
  if (inServiceDate!="")  attributes <- c(attributes, inServiceDate = inServiceDate)   else stop("You must provide a date - and a correct format")
  if (dateOfPurchase!="") attributes <- c(attributes, dateOfPurchase = dateOfPurchase) else stop("You must provide a date - and correct format")
  if (dateOfLastCalibration!="") attributes <- c(attributes, dateOfLastCalibration = dateOfLastCalibration) else stop("You must provide a date - and correct format")
  if (personInCharge!="") attributes <- c(attributes, personInCharge = personInCharge) else stop("You must provide a person e-mail in charge of this sensor")
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("SENSORS", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}
