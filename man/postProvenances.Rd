% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postProvenances.R
\name{postProvenances}
\alias{postProvenances}
\title{postProvenance}
\usage{
postProvenances(provs)
}
\arguments{
\item{provs}{list of ProvenanceDTO object comment character, 
give a comment for this provenance label character, 
give the label of this provenance 
some metadata concerning the provenance. 
The format of the metadata is a list with every item being a different metadata. 
Exemple: ObjectDTO$new(list(meta1 = "15", meta2 = "owner1")}
}
\value{
WSResponse object
}
\description{
send a provenance to the web service
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
    prov = ProvenanceDTO$new(
      label = "insertionProvenance_label",
      comment = "comment my provenance",
      experiments = list("http://opensilex.org/experiment/maug"),
      metadata = ObjectDTO$new(list(meta1 = "15", meta2 = "owner1"))
    )
    response <- postProvenances(list(prov))
    response$success
    response$metadata
   }
}
\seealso{
http://docs.brapi.apiary.io/#introduction/url-structure
}
