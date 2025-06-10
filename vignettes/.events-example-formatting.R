# this is a script to convert the dataset used in `events-example.Rmd` to csvs
# it is sourced from https://doi.org/10.5061/dryad.75s51
# I've assumed it's stored as 'Frogwatch_dataset.xlsx'

library(readxl)
library(janitor)
library(readr)
library(here)
library(dplyr)
library(purrr)

# species
species <- here::here("vignettes", "Frogwatch_dataset.xlsx") |>
  readxl::read_xlsx(sheet = "species list") |> 
  janitor::clean_names()

write_csv(species, 
          here::here("vignettes", "events_species.csv"))



# sites 
sites <- here::here("vignettes", "Frogwatch_dataset.xlsx") |>
  readxl::read_xlsx(sheet = "sites") |> 
  janitor::clean_names()

# get a subset of sites that include at least one observation of each species
# how many rows available per species
species_counts <- sites |>
  select(any_of(species$abbreviation)) |>
  colSums()

# make a tibble from rarest > commonest
species_observations <- tibble(
  species = names(species_counts),
  count = species_counts) |>
  arrange(count)

# choose the first row that hasn't already been been selected
n_obs <- nrow(species_observations)
rows <- rep(0, n_obs)
for(i in seq_len(n_obs)){
  available_rows <- which(sites[, species_observations$species[i]] > 0)
  unselected_rows <- which(!(available_rows %in% rows))
  first_available_row <- available_rows[unselected_rows][1]
  rows[i] <- first_available_row
}

# use to filter
sites_final <- sites[sort(rows), ]
write_csv(sites_final, 
          here::here("vignettes", "events_sites.csv"))



# observations
obs <- here::here("vignettes", "Frogwatch_dataset.xlsx") |>
  readxl::read_xlsx(sheet = "observations") |>
  janitor::clean_names()

# first filter to only include the sites we chose above
obs_small <- obs |>
  filter(site_code %in% sites_final$site_code)

# save out
write_csv(obs_small, 
          here::here("vignettes", "events_observations.csv"))
