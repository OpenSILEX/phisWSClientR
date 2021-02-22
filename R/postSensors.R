#-------------------------------------------------------------------------------
# Program: postSensors.R
# Objective: functions to post a new sensor to the WS2
#            * postSensors
# Authors: Hollebecq Jean-Eudes
# Creation: 24/09/2019 
# Update: 15/11/2019 (JE.H)
#-------------------------------------------------------------------------------

##' @title postSensor
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
##' postSensor(rdfType = "http://www.opensilex.org/vocabulary/oeso#Spectrometer",
##'             label = "Sensor_label",
##'             brand = "Sensor_brand",model = "Sensor_model",serialNumber = "",
##'             inServiceDate = "2017-06-15",dateOfPurchase = "2017-06-15",
##'             dateOfLastCalibration = "2017-06-15",personInCharge = "admin@opensilex.org"
##' )
##'    }
##' @export
postSensor <- function(rdfType, label, brand, model, serialNumber, inServiceDate, dateOfPurchase, dateOfLastCalibration, personInCharge){
  attributes <- list()
  if (rdfType!="")        attributes <- c(attributes, rdfType = rdfType)   else stop("You must provide a type of sensor")
  if (label!="")          attributes <- c(attributes, label = label)       else stop("You must provide a label")
  if (brand!="")          attributes <- c(attributes, brand = brand)       else stop("You must provide a brand")
  if (!is.null(model) && model!="")          attributes <- c(attributes, model = model)       #else stop("You must provide a model")
  if (!is.null(serialNumber) && serialNumber!="")   attributes <- c(attributes, serialNumber = serialNumber) 
  if (inServiceDate!="")  attributes <- c(attributes, inServiceDate = inServiceDate)   else stop("You must provide a date - and a correct format")
  if (!is.null(dateOfPurchase) && dateOfPurchase!="") attributes <- c(attributes, dateOfPurchase = dateOfPurchase) 
  if (!is.null(dateOfLastCalibration) && dateOfLastCalibration!="") attributes <- c(attributes, dateOfLastCalibration = dateOfLastCalibration) else stop("You must provide a date - and correct format")
  if (personInCharge!="") attributes <- c(attributes, personInCharge = personInCharge) else stop("You must provide a person e-mail in charge of this sensor")
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("SENSORS", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}

##' @title postSensors
##' @import stringr
##' @import dplyr
##' @import wicket
##' @import tibble
##' @description send a sensor list to the web service
##' @param sensorList a dataframe containing the following columns
##' rdfType character, the rdfType of the sensor ex: http://www.opensilex.org/vocabulary/oeso#Spectrometer
##' label character, give the label of this sensor
##' brand character, the brand of the sensor
##' model character, the model of the sensor
##' serialNumber character, the serial number of the sensor
##' inServiceDate character, date which the sensor has been in service. Format "AAAA-MM-DD"
##' dateOfPurchase character, date which the sensor has been purchased. Format "AAAA-MM-DD"
##' dateOfLastCalibration character, date which the sensor has been last calibrated. Format "AAAA-MM-DD"
##' personInCharge the mail of the person in charge of this sensor
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
##' sensorList = data.frame(
##'   rdfType = c(
##'     "http://www.opensilex.org/vocabulary/oeso#Spectrometer",
##'     "http://www.opensilex.org/vocabulary/oeso#Spectrometer",
##'     "http://www.opensilex.org/vocabulary/oeso#Spectrometer"),
##'   label = c("Sensor_label", "Sensor_label2", "Sensor_label3"),
##'   brand = c("Sensor_brand", "Sensor_brand2", "Sensor_brand3"),
##'   inServiceDate = c("2017-06-15", "2017-06-15", "2017-06-15"), 
##'   dateOfPurchase = c("2017-06-15", "2017-06-15", "2017-06-15"),
##'   personInCharge = c(
##'     "admin@opensilex.org",
##'     "admin@opensilex.org",
##'     "admin@opensilex.org",
##'   ),
##'   stringsAsFactors = FALSE
##' ) 
##' r<- postSensors(sensorList)
##' r$success
##' r$metadata 
##'}
##' @export
postSensors <- function(sensorList){
  if(sum(c("rdfType","label", "brand","personInCharge","inServiceDate")%in%names(sensorList)) < 5 ) stop(" Authorized columns Required => rdfType, label, brand, personInCharge, inServiceDate ;  Optional => (dateOfPurchase)")

  listSensors <- list()
  for(numSO in 1:nrow(sensorList)){
    sensor <- phisWSClientRTools::SensorPostDTO$new(
      rdfType =  sensorList[numSO,]$rdfType,
      label=  sensorList[numSO,]$label,
      brand=  sensorList[numSO,]$brand,
      inServiceDate=  sensorList[numSO,]$inServiceDate,
      personInCharge=  sensorList[numSO,]$personInCharge,
    )
    if(!is.null(sensorList[numSO,]$dateOfPurchase)){
      sensor$dateOfPurchase <- sensorList[numSO,]$dateOfPurchase
    }
    if(!is.null(sensorList[numSO,]$serialNumber)){
      sensor$serialNumber <- sensorList[numSO,]$serialNumber
    }
    if(!is.null(sensorList[numSO,]$model)){
      sensor$model <- sensorList[numSO,]$model
    }
    if(!is.null(sensorList[numSO,]$dateOfLastCalibration)){
      sensor$dateOfLastCalibration <- sensorList[numSO,]$dateOfLastCalibration
    }
    
    listSensors <- append(sensor,listSensors)
  }
  sensorsApi = phisWSClientRTools::SensorsApi$new()
  wsResponse = sensorsApi$post5(listSensors)

  return(wsResponse)

}