% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ws1Calls.R
\name{getImagesAnalysis}
\alias{getImagesAnalysis}
\title{retrieves the images analysis data from the web service}
\usage{
getImagesAnalysis(experimentURI = "", variablesName = list(),
  labelView = "", provider = "", date = "", page = NULL,
  pageSize = NULL)
}
\arguments{
\item{experimentURI}{URI of the experiment}

\item{variablesName}{list, variable names of images analysis (ex : "objectSumArea")}

\item{labelView}{character, label view of an image}

\item{provider}{character, provider of data}

\item{date}{character, data for one day (format: YYYY-MM-DD)}

\item{page}{displayed page (pagination Plant Breeding API)}

\item{pageSize}{number of elements by page (pagination Plant Breeding API)}
}
\value{
WSResponse object
}
\description{
Retrieves data from image analysis
}
\details{
You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
service
}
\examples{
\donttest{
 connectToPHISWS(apiID="ws_1_public", 
                 username = "guestphis@supagro.inra.fr",
                 password = "guestphis")
myExp<-"http://www.phenome-fppn.fr/m3p/ARCH2017-03-30"
getVariablesByCategory(category ="imagery",experimentURI=myExp)$data$name
myImages<-getImagesAnalysis(experimentURI = myExp,
           variablesName = list("objectSumArea"),pageSize = 100000)
str(myImages$data)
}
}
\seealso{
http://docs.brapi.apiary.io/#introduction/url-structure

You have to install the opensilexWSClientR before running any 
         request on PHIS web service.
}
