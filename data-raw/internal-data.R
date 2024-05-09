# This script builds all information stored within galaxias/R/sysdata.rda
# storing of such code in /data-raw is recommended in 'R Packages' by
# Hadley Wickham, section 8.3 'Internal data'
# https://r-pkgs.org/data.html

devtools::load_all()
# library(xml2) # convert example XML to list
library(usethis) # adding content to sysdata.rda

# # eml validation 
# download.file(
#   url = "http://rs.gbif.org/schema/eml-gbif-profile/1.1/eml-gbif-profile.xsd",
#   destfile = "./data-raw/eml-gbif-profile.xsd")
# metadata_validator_xsd <- read_xml("http://rs.gbif.org/schema/eml-gbif-profile/1.1/eml-gbif-profile.xsd")
# # NOTE: using `read_xml()` on local file causes `xml_validate()` to break
# # Hence we cache the file here for safety reasons, but load it with `read_xml`
# # from the url to ensure it actually works.

# eml.xml template 
# https://support.ala.org.au/support/solutions/articles/6000261427-sharing-a-dataset-with-the-ala

library(readr)
dwc_terms <- read_csv("./data-raw/dwc_terms.csv")$column_name
country_codes <- read_csv("./data-raw/wikipedia_country_codes.csv")

# add to r/sysdata.rda
use_data(
  dwc_terms,
  country_codes,
  internal = TRUE,
  overwrite = TRUE)
