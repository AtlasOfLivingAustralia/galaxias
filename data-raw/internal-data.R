# This script builds all information stored within galaxias/R/sysdata.rda
# storing of such code in /data-raw is recommended in 'R Packages' by
# Hadley Wickham, section 8.3 'Internal data'
# https://r-pkgs.org/data.html

devtools::load_all()
library(xml2) # convert example XML to list
library(usethis) # adding content to sysdata.rda

# eml validation 
download.file(
  url = "http://rs.gbif.org/schema/eml-gbif-profile/1.1/eml-gbif-profile.xsd",
  destfile = "./data-raw/eml-gbif-profile.xsd")
metadata_validator_xsd <- read_xml("http://rs.gbif.org/schema/eml-gbif-profile/1.1/eml-gbif-profile.xsd")
# NOTE: using `read_xml()` on local file causes `xml_validate()` to break
# Hence we cache the file here for safety reasons, but load it with `read_xml`
# from the url to ensure it actually works.

# eml.xml template 
# This is the set structure based on provided example found on the 
# ALA data sharing website 
# https://support.ala.org.au/support/solutions/articles/6000261427-sharing-a-dataset-with-the-ala
# Example found in inst/example_xml/eml_completed.xml
# Blank eml.xml found in inst/example_xml/eml_blank.xml
# metadata_template <- read_xml("inst/example_xml/eml_blank.xml") # fails to load, for some reason
# eml_template_list <- eml_template |> as_list() 

# Darwin Core terms
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(galah)
library(purrr)
library(forcats)

# download and clean terms from TDWG
df <- read_csv("https://raw.githubusercontent.com/tdwg/rs.tdwg.org/master/terms/terms.csv")
terms <- df |>
  filter(is.na(term_deprecated),
         grepl("Property$", rdf_type)) |>
  rename(
    term = term_localName,
    description = rdfs_comment,   
    example = examples) |>
  mutate(
    parent_class = str_extract(tdwgutility_organizedInClass, "[:alpha:]+$")) |>
  replace_na(list(parent_class = "No parent class")) |>
  select(parent_class, term, label, description, example)
  
# get term counts in ALA, add as `count` field
available_terms <- show_all_fields() |>
  pull(id)
available_check <- terms$term %in% available_terms
count_results <- map(
  .x = terms$term[available_check],
  .f = \(a){
    Sys.sleep(1) # rate limit to one per second
    galah_call() |>
      filter(!is.na(!!!a)) |>
      count() |>
      collect() |>
      pull(count)
  },
  .progress = TRUE
) |>
  unlist()
count_df <- tibble(term = terms$term[available_check], 
                   count = count_results)
terms <- left_join(terms, count_df)

# create a 'final' version that reorders based on `count`, 
# first via `parent_class` then `term`
dwc_terms_df <- terms |>
  mutate(count = replace_na(count, 0)) |>
  mutate(parent_class = fct_reorder(parent_class, 
                                    count, 
                                    .fun = sum,
                                    .desc = TRUE),
         term = fct_reorder(term, 
                            count, 
                            .fun = sum,
                            .desc = TRUE)) |>
  arrange(parent_class, term)

## For checking order:
# parent_class_order <- terms2 |>
#   group_by(parent_class) |>
#   summarize(count = sum(count)) |>
#   arrange(desc(count))

# add to r/sysdata.rda
use_data(
  # metadata_template,
  metadata_validator_xsd,
  dwc_terms_df,
  internal = TRUE,
  overwrite = TRUE)
