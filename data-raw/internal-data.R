# This script builds all information stored within correa/R/sysdata.rda
# storing of such code in /data-raw is recommended in 'R Packages' by
# Hadley Wickham, section 8.3 'Internal data'
# https://r-pkgs.org/data.html

devtools::load_all()
library(readr) # import csvs straight to tibble
library(tibble) # generate tibbles
library(dplyr) # data manipulation
library(usethis) # adding content to sysdata.rda


# Default:
# Cached dwc terms
dwc_terms_archived <- read_csv("./data-raw/dwc_terms.csv")

# add to r/sysdata.rda
use_data(
  dwc_terms_archived,
  internal = TRUE,
  overwrite = TRUE)
