
library(tibble)
library(lubridate)
library(hms)

df <- tibble(
  latitude = c(-35.310, "-35.273"), # deliberate error for demonstration purposes
  longitude = c(149.125, 149.133),
  date = c("14-01-2023", "15-01-2023"),
  date2 = c("14-01-2023 10:23:00", "14-01-2023 10:24:01"),
  time = c("10:23", "11:25"),
  time2 = c("10:23:00", "11:25:00"),
  month = c("Jan", "BLF"),
  month2 = c("January", "February"),
  day = c(100, 101),
  day2 = c("100", "101"),
  species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
  n = c(2, 3),
  crs = c("WGS84", "WGS8d"),
  country1 = c("AU", "DE"),
  country_real = c("Naustralia", "Denmark"),
  continent = c("bork", "Europe")
  )

df2 <- tibble(
  basisOfRecord = c("humanObservation", "preservedSpecimen")
  )

df |>
  check_dwc()

df |>
  select(boink)

df |>
  use_locality(countryCode = country1,
               country = country_real)

df |>
  use_occurrences()

df |>
  use_occurrences(occurrenceID = use_id_random(),
                  basisOfRecord = "humanObservation") |>
  use_coordinates(decimalLongitude = longitude) |>
  check_dwc()


df2 |>
  check_dwc()


df |>
  use_datetime(eventDate = lubridate::dmy(date))

df |>
  # mutate(id = uuid::UUIDgenerate(use.time = NA, n = 2)) |>
  use_occurrences(occurrenceID = use_id_random(),
                  basisOfRecord = "hum")

uuid::UUIDvalidate(uuid::UUIDgenerate(use.time = NA, n = 2))

df |>
  use_coordinates(decimalLatitude = as.numeric(latitude),
                  decimalLongitude = longitude) |>
  use_datetime(eventDate = lubridate::dmy(date)) |>
  use_scientific_name(scientificName = species) |>
  use_occurrences(occurrenceID = use_id_random())
  check_dwc()
  


new_datadf |>
  mutate(
    date = date |> dmy()
    # time = time |> as_hms()
    ) |>
  use_datetime(eventDate = date)

df$time |> as.POSIXct(format = "%H:%M") |> format("%H:%M")

df$time |> as.POSIXct(format = "%H:%M%S")

df$time |> as.POSIXct(format = "%H:%M:%S") |> format("%H:%M")

df$month2 |> match(month) |> month()

df$month |> mutate(month = month(month))

df |>
  use_datetime(day = day2)


df |>
  # mutate(date = date) |>
  use_datetime(time = time)

use_datetime(df,
             coordinates="decimalLongitude")


library(galah)
library(sf)

galah_config(email = "dax.kellie@csiro.au")

galah_call() |>
  filter(year == 2010) |>
  identify("alcedinidae") |>
  group_by(specificEpithet) |>
  atlas_counts()

occs <- galah_call() |>
  identify("perameles") |>
  filter(year == 2003) |>
  # select(eventDate, eventTime) |>
  atlas_occurrences()

occs_clean <- occs |>
  tidyr::drop_na(decimalLatitude, decimalLongitude) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)


occs_clean |>
  use_coordinates_sf(coords = geometry)

occs_clean |>
  check_dwc()

occs


df_correct <- tibble(
  latitude = c(-35.310, -35.273),
  longitude = c(149.125, 149.133),
  date = c("14-01-2023", "15-01-2023"),
  species = c("Callocephalon fimbriatum", "Callocephalon fimbriatum"),
  crs = c("WGS84", "WGS84"),
  uncertainty = c(0.001, 0.001)
)

df_correct |>
  use_taxonomy(specificEpithet = species)

df_correct |>
  use_coordinates(decimalLatitude = latitude,
                  decimalLongitude = longitude,
                  geodeticDatum = crs,
                  coordinateUncertaintyInMeters = uncertainty
                  ) |>
  use_datetime(eventDate = lubridate::dmy(date)) |>
  use_scientific_name(scientificName = species) |>
  use_occurrences(basisOfRecord = "humanObservation",
                  occurrenceID = use_id_random()) |>
  check_dwc()




