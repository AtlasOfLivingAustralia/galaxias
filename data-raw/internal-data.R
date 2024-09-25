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
library(dplyr)

## Processing for DwC terms - rerun only when updates needed
dwc_terms <- read_csv("https://raw.githubusercontent.com/tdwg/rs.tdwg.org/master/terms/terms.csv")
dwc_terms <- dwc_terms |>
  mutate(url = glue("{term_isDefinedBy}{term_localName}"),
         description = sub(" NA$", "", glue("{rdfs_comment} {dcterms_description}")),
         class = sub("^http://rs.tdwg.org/dwc/terms/|http://purl.org/dc/terms/", "", tdwgutility_organizedInClass)) |>
  filter(is.na(term_deprecated),
         grepl("Property$", rdf_type)) |>
  rename(term = term_localName) |>
  select(class, term, url, description, examples) |>
  arrange(class, term)
dwc_terms$class[is.na(dwc_terms$class)] <- "NoParentClass"
write_csv(dwc_terms, "./data-raw/dwc_terms.csv")


# ## Below are attempts to build a replaced_by column
# ## requires a whole graph; park for later
# # make 'replaced_by' column
# dwc_terms$replaced_by <- NA
# 
# v1 <- dwc_terms |>
#   filter(!is.na(replaces_term)) |>
#   select(url, replaces_term) |>
#   rename(replaces = replaces_term)
# 
# v2 <- dwc_terms |>
#   filter(!is.na(replaces1_term)) |>
#   select(url, replaces1_term) |>
#   rename(replaces = replaces1_term)
# 
# # note: replaces2_term is empty
# replaces_df <- bind_rows(v1, v2) |>
#   arrange(url)
# 
# length(unique(replaces_df$replaces))
# 
# replaces_duplicates <- replaces_df |>
#   group_by(replaces) |>
#   summarize(count = n()) |>
#   arrange(desc(count)) |>
#   filter(count > 1) |>
#   pull(replaces)
# 
# replaces_df |>
#   filter(replaces %in% replaces_duplicates) |>
#   arrange(replaces) |>
#   left_join(select(dwc_terms, url, term_deprecated))
# 
# dwc_terms |>
#   filter(url ==)
# 
# for(i in seq_len(nrow(replaces_df))){
#   
# }


# add to package
dwc_terms <- read_csv("./data-raw/dwc_terms.csv")
country_codes <- read_csv("./data-raw/wikipedia_country_codes.csv")

# add to r/sysdata.rda
use_data(
  dwc_terms,
  country_codes,
  internal = TRUE,
  overwrite = TRUE)
