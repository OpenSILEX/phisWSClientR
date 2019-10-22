# library(phisWSClientR)
# library(jsonlite)
# library(httr)
# 
# connectToPHISWS(apiID = "ws_private", username = "guest@opensilex.org", password = "guest", url = "http://138.102.159.37:8080/openSilexTestAPI/rest/")
# 
# # getVariables2(label = "NDVI")
# # essaiData = getData(variableUri = "http://www.phenome-fppn.fr/test/id/variables/v027", pageSize = 715)
# essaiProv = getProvenances(label = "trois")
# essaiProv$data
# 
# attributes = list()
# label = "essai_insert_provcinq"
# comment = "comic"
# metadata = list(meta1 = "heinhein", meta2 = "oeuf")
# metadata = "{
#     'meta1': 'hein',
#     'meta2': 'oeuf'
#   }"
# 
# attributes = c(attributes, label = label)
# attributes = c(attributes, comment = comment)
# attributes = c(attributes, metadata = list(metadata))
# 
# postProvenances(label = label, comment = comment, metadata = metadata)
# 
# r <- httr::POST("http://138.102.159.37:8080/openSilexTestAPI/rest/provenances", config = httr::add_headers(Authorization=paste("Bearer ","06fb6b3297436aee2f1b3d554a0c5156", sep = "")), body = list(attributes), encode = "json", verbose())
# r$status_code
# 
# jsonRes = jsonlite::fromJSON(
#   httr::content(
#     r,
#     as = "text",
#     encoding = "UTF-8")
# )
# 
# 
# list(
# rdfType = "http://www.opensilex.org/vocabulary/oeso#Plot",
# alias = "105/DI-75/WW",
# geometry = "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))",
# experiment = "http://www.phenome-fppn.fr/diaphen/DIA2017-1",
# year = "2017",
# properties = list( rdfType = "http://xmlns.com/foaf/0.1/Agent",
#         relation = "http://www.opensilex.org/vocabulary/2018#hasContact",
#         value = "http://www.opensilex.org/demo/id/agent/marie_dupond")
# )