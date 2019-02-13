## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  library(phisWSClientR)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  # If you want to access to a private web service, you have to insert the address of the WS and the port
  initializeClientConnection(apiID="ws_private",url = "138.102.159.36:8080/uesAPI/rest/")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  aToken<-getToken("guest@phis.fr","guest")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getProjects2(aToken$data, uri="http://www.phenome-fppn.fr/ues/PHENOME-FPPN")

