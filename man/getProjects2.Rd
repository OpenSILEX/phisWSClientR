% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getProjects.R
\name{getProjects2}
\alias{getProjects2}
\title{getProjects2}
\usage{
getProjects2(page = NULL, pageSize = NULL, uri = "", name = "",
  acronyme = "", subprojectType = "", financialSupport = "",
  financialName = "", dateStart = "", dateEnd = "", keywords = "",
  parentProject = "", website = "")
}
\arguments{
\item{page}{numeric, displayed page (pagination Plant Breeding API) (optional)}

\item{pageSize}{numeric, number of elements by page (pagination Plant Breeding API) (optional)}

\item{uri}{character, search by the URI of a project (optional)}

\item{name}{character, search by the name of a project (optional)}

\item{acronyme}{character, search by the acronyme of a project (optional)}

\item{subprojectType}{character, search by the subproject type of a project (optional)}

\item{financialSupport}{character, search by the financial support of a project (optional)}

\item{financialName}{character, search by the financial name of a project (optional)}

\item{dateStart}{character, the date of the beginning of a project (optional)}

\item{dateEnd}{character, the date of the end of a project (optional)}

\item{keywords}{character, search by keywords of a project (optional)}

\item{parentProject}{character, search by parent project (optional)}

\item{website}{character, search by website of a project (optional)}
}
\value{
WSResponse object
}
\description{
Retrieves the list of projects in the WS2 with the selected filters
}
\details{
You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
service
}
\examples{
\donttest{
connectToPHISWS(apiID="ws_private",
               url = "http://www.opensilex.org/openSilexAPI/rest/",
               username="guest@opensilex.org",
               password="guest")
 projects <- getProjects2(uri="http://www.opensilex.org/demo/PHENOME-FPPN")
 projects$data
}
}
\seealso{
http://docs.brapi.apiary.io/#introduction/url-structure

You have to install the opensilexWSClientR before running any 
         request on PHIS web service.
}
