# This script contains an example workflow for building a functional
# Biodiversity Data Package. It should be stored - along with any raw data 
# in csv format - in the `/data-raw` folder of your package.
#
# Note that the use of the `data-raw` folder is optional - you are free to 
# bypass this stage and simply put data in `data` if you'd prefer.
#
# You should feel free to add, remove, or modify any of the code below to meet
# your specific requirements and use cases.


# Step 1: Load required packages
# NOTE: packages called here should be added to SUGGESTS in the DESCRIPTION file
# (the default packages listed below have already been added!)
library(dplyr)
library(galaxias)
library(readr)
library(tibble)
library(usethis)


# Step 2: Import
# When adding your own data, use something like:
# `df <- readr::read_csv("occurrences_raw.csv")`
# for now, we'll use an example dataset:
df <- tibble(
  latitude = c(-35.310, -35.273),
  longitude = c(149.125, 149.133),
  date = c("14-01-2023", "15-01-2023"),
  time = c("10:23", "11:25"),
  species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
  n = c(2, 3))


# Step 2: Make this dataset compatible with Darwin Core fields
# Mostly this consists of changing column names; but some fields may need to 
# be formatted differently as well.
# 
# You can find more information on the Darwin Core standard ...
# 
# Below we use a piped syntax based on `dplyr` verbs. Note that it is critical 
# to assign the resulting object a valid Darwin Core name; in this case
# `occurrences` (`events` or `media` are other valid examples). This is 
# important for correct building of the Darwin Core Archive.
occurrences <- df |>
  
  # rename columns to Darwin Core names
  rename(decimalLatitude = latitude,
         decimalLongitude = longitude,
         scientificName = species,
         individualCount = n) |>
  
  # Amend date column to standard format
  mutate(date = dmy(date)) |>
  
  # Add previously missing information supported or required by Darwin Core
  mutate(occurrenceID = seq_len(nrow(df)),
         eventDate = glue("{date}T{time}:00Z"),
         taxonRank = "species",
         basisOfRecord = "humanObservation",
         recordedBy = "Martin Westgate",
  ) |>
  
  # Place columns in a sensible order
  select(occurrenceID, eventDate, decimalLatitude, decimalLongitude,
         scientificName, taxonRank, individualCount, recordedBy,
         basisOfRecord)


# Step 4: Export
# once complete, processed data should be saved to `/data`, using a file name
# that describes the data it contains.
#
# This saves the data in a compressed format with a `.rda` suffix.
usethis::use_data(occurrences)


# Step 5: Document your dataset (optional)
# This is a standard step for an R package, but not for a Darwin Core Archive.
# It consists of writing a roxygen2 file outlining its' contents.
# see https://r-pkgs.org/data.html#sec-documenting-data for details.
#
# In the case of the above example, we would create a file named `occurrences.R`
# in the `R` folder, and write our documentation there. We would then build the 
# documentation using `devtools::document()`.


# Step 6: Update your 'schema'
# This function looks up what data is present in the `data` folder,
# and uses it to update `vignettes/schema.md`.
# This is required if you want your package to be compatible with the Darwin 
# Core Standard.
galaxias::update_schema()