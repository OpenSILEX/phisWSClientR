# library(phisWSClientR)
# library(jsonlite)
# library(httr)
# library(roxygen2)
# 
# connectToPHISWS(apiID = "ws_private", username = "guest@opensilex.org", password = "guest", url = "http://138.102.159.37:8080/openSilexTestAPI/rest/")
# setLogLevel(level = "INFO")
# getUserInformations()
# # getVariables2(label = "NDVI")
# # essaiData = getData(variableUri = "http://www.phenome-fppn.fr/test/id/variables/v027", pageSize = 715)
# essaiProv = getProvenances(label = "trois")
# essaiVec = getVectors()
# essaiVec$totalCount
# essaiVec$data
# 
# attributes = list()
# label = "essai_insert_pro"
# comment = "comI"
# metadata = list(meta1 = "metaA", meta2 = "metaB")
# # metadata = "{
# #     'meta1': 'hein',
# #     'meta2': 'oeuf'
# #   }"
# 
# attributes = c(attributes, label = label)
# attributes = c(attributes, comment = comment)
# attributes = c(attributes, metadata = list(metadata))
# 
# #ppospro = postProvenances(label = label, comment = comment, metadata = metadata)
# posvec = postVectors(label = label, comment = comment, metadata = metadata)
# 
# r <- httr::POST("http://138.102.159.37:8080/openSilexTestAPI/rest/vectors/post_6", config = httr::add_headers(Authorization=paste("Bearer ","cc0b5662516e1a6567fdd7e129812f16", sep = "")), body = list(attributes), encode = "json", verbose())
# r$status_code
# 
# jsonRes = jsonlite::fromJSON(
#   httr::content(
#     r,
# 
#     as = "text",
#     encoding = "UTF-8")
# )
# jsonRes$metadata
# 
# ########
# 
# connectToPHISWS(apiID="ws_private",
#                url = "http://www.opensilex.org/openSilexAPI/rest/",
#                username="guest@opensilex.org",
#                password="guest")
#    postVectors(
#    rdfType = "http://www.opensilex.org/vocabulary/oeso#UAV",
#    label = "aligot",
#    brand = "Fait maison",
#    model = "avec du  cantal",
#    serialNumber = "",
#    inServiceDate = "2017-06-15",
#    dateOfPurchase = "2017-06-15",
#    dateOfLastCalibration = "2017-06-15",
#    personInCharge = "admin@opensilex.org"
#    )
