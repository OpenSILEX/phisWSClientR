% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getEnvironmentData.R
\name{getEnvironment}
\alias{getEnvironment}
\title{retrieves the environmental mesures of an experiment from the web service}
\usage{
getEnvironment(variableCategory = "", startDate = "", endDate = "",
  variables = "", facility = "", experimentURI = "", page = NULL,
  pageSize = NULL)
}
\arguments{
\item{variableCategory}{character, a category of variables}

\item{startDate}{data > startDate (Format: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS )}

\item{endDate}{data < startDate (Format: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS )}

\item{variables}{list of variables for the request (Ex : "wind speed_weather station_meter per second")}

\item{facility}{place of the experiment (Ex : "http://www.phenome-fppn.fr/m3p/ec3")}

\item{experimentURI}{URI of the experiment}

\item{page}{displayed page (pagination Plant Breeding API)}

\item{pageSize}{number of elements by page (pagination Plant Breeding API)}
}
\value{
WSResponse object
}
\description{
Retrieves environmental mesures of an experiment or by dates
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
 test<-getEnvironment(experimentURI="http://www.phenome-fppn.fr/m3p/ARCH2017-11-23")
 test$data
 
 getEnvironment( experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2017-11-23",
                 startDate="2017-06-28",
                 endDate="2017-06-28")
                 
 getEnvironment(experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2017-11-23",
    facility="http://www.phenome-fppn.fr/m3p/es2",
    variables="net irradiance_calculated variable_watt.m2")
}
}
\seealso{
http://docs.brapi.apiary.io/#introduction/url-structure

You have to install the opensilexWSClientR before running any 
         request on PHIS web service.
}
