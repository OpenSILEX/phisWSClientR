% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getTraits.R
\name{getTraits}
\alias{getTraits}
\title{getTraits}
\usage{
getTraits(uri = "", label = "", page = NULL, pageSize = NULL)
}
\arguments{
\item{uri}{character, search by the uri of a trait (optional)}

\item{label}{character, search by the label of a trait (optional)}

\item{page}{numeric, displayed page (pagination Plant Breeding API)}

\item{pageSize}{numeric, number of elements by page (pagination Plant Breeding API)}
}
\value{
WSResponse object
}
\description{
retrieves the trait based on search criterion
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
traits <- getTraits(uri = "http://www.opensilex.org/demo/id/traits/t010")
traits$data
}
}
\seealso{
http://docs.brapi.apiary.io/#introduction/url-structure

You have to install the opensilexWSClientR before running any 
         request on PHIS web service.
}
