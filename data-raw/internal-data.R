# This script builds all information stored within galaxias/R/sysdata.rda
# storing of such code in /data-raw is recommended in 'R Packages' by
# Hadley Wickham, section 8.3 'Internal data'
# https://r-pkgs.org/data.html

devtools::load_all()
library(readr) # import csvs straight to tibble
library(tibble) # generate tibbles
library(dplyr) # data manipulation
library(xml2) # convert example XML to list
library(usethis) # adding content to sysdata.rda


# Default:
# Cached dwc terms
dwc_terms_archived <- read_csv("./data-raw/dwc_terms.csv")

# eml.xml template 
# This is the set structure based on provided example found on the 
# ALA data sharing website 
# https://support.ala.org.au/support/solutions/articles/6000261427-sharing-a-dataset-with-the-ala
# Example found in inst/example_xml/eml_completed.xml
# Blank eml.xml found in inst/example_xml/eml_blank.xml
eml_template <- read_xml("inst/example_xml/eml_blank.xml")
eml_template_list <- eml_template |> as_list() 

# add to r/sysdata.rda
use_data(
  dwc_terms_archived,
  eml_template_list,
  internal = TRUE,
  overwrite = TRUE)
