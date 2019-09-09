## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  library(phisWSClientR)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  # If you want to access to the public web service 
  connectToPHISWS(apiID="ws_1_public")

## ----echo=TRUE,eval=FALSE,message=FALSE, warning=FALSE-------------------
#    # If you want to access to a private web service, you have to insert the address of the WS and the port
#    connectToPHISWS(apiID="ws_private","guestphis@supagro.inra.fr","guestphis",url = "147.99.7.5:8080/phenomeapi/resources/")

## ----echo=TRUE,eval=FALSE,message=FALSE, warning=FALSE-------------------
#    getUserInformations()

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getProjects()$data
  
  getExperiments(projectName = "PHIS_Publi")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  myExp<-getExperiments(projectName = "XYZ")$data$experimentURI
  print(myExp)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  vars <- getVariablesByCategory(category = "environment",experimentURI = myExp)

  # Only the first rows of the data.frame containing the 'environment' variables...
  head(vars$data)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  # first rows of 'imagery' variables
  head(getVariablesByCategory(category ="imagery",experimentURI=myExp)$data)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  # first rows of 'watering' variables
  head(getVariablesByCategory(category ="watering",experimentURI=myExp)$data)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  # first we count the number of observation in the dataset
  tpCount<-getImagesAnalysis(experimentURI=myExp,
                             variablesName = list("plantHeight"),
                             labelView ="side0")$totalCount
  # we retrieve this number with $totalcount
  # In this example, we set the pageSize to 10 for speed purpose but in real example, please use tpCount
  requetManip<-getImagesAnalysis( experimentURI=myExp,
                             variablesName = list("plantHeight"),
                             labelView ="side0",
                             pageSize=10)$data
  head(requetManip)

  # getPlants function allows the user to retrieve all the information for all the plants of an experiment
  # In this example, we set the pageSize to 10 for speed purpose but in real example, please use tpCount
  idManip<-getPlants( experimentURI=myExp,
                     pageSize=10)$data
  head(idManip)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  myFacility<-"http://www.phenome-fppn.fr/m3p/es2"
  tpCount<-getEnvironment(
                     experimentURI = myExp,
                     facility=myFacility,
                     variables = "air humidity_weather station_percentage")$totalCount
  #--- Air humidity
  # we retrieve this number with $totalcount
  # In this example, we set the pageSize to 10 for speed purpose but in real example, please use tpCount
  myMeteoHumidity<-getEnvironment(experimentURI=myExp,
                     facility=myFacility,
                     variables="air humidity_weather station_percentage",
                     pageSize=10)$data

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  tpCount<-getWatering(experimentURI=myExp,
                       variablesName = list("weightAfter"))$totalCount
  # we retrieve this number with $totalcount
  # In this example, we set the pageSize to 10 for speed purpose but in real example, please use tpCount
  requetIrrig<-getWatering(experimentURI=myExp,
                           variablesName = list("weightAfter"),
                           pageSize=10)$data

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  setLogLevel("DEBUG")
  tpCount<-getWatering(experimentURI=myExp,
                       variablesName = list("weightAfter"))$totalCount
  setLogLevel("INFO")

## ----session,echo=FALSE,message=FALSE, warning=FALSE---------------------
  sessionInfo()

