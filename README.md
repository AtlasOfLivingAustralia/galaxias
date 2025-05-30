
<!-- README.md is generated from README.Rmd. Please edit that file -->

# galaxias <img src="man/figures/logo.png" align="right" style="margin: 0px 10px 0px 10px;" alt="" width="120"/><br>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/galaxias)](https://CRAN.R-project.org/package=galaxias)
<!-- badges: end -->

## Overview

`galaxias` is an R package that helps users describe, bundle and share
biodiversity information using the [‘Darwin Core’](https://dwc.tdwg.org)
data standard. `galaxias` provides tools in R to build a [Darwin Core
Archive](), a zip file containing standardised data and metadata
accepted by global data infrastructures. The package mirrors
functionality in [devtools](https://devtools.r-lib.org/),
[usethis](https://usethis.r-lib.org/) and
[dplyr](https://dplyr.tidyverse.org/) to manage data, files and folders.
`galaxias` was created by the [Science & Decision Support
Team](https://labs.ala.org.au) at the [Atlas of Living
Australia](https://www.ala.org.au) (ALA).

The package is named for a genus of freshwater fish that is found only
in the Southern Hemisphere, and predominantly in Australia and New
Zealand. The logo shows a [Spotted
Galaxias](https://bie.ala.org.au/species/https://biodiversity.org.au/afd/taxa/e4d85845-3e34-4112-90a9-f954176721ec)
(*Galaxias truttaceus*) drawn by [Ian
Brennan](http://www.iangbrennan.org).

If you have any comments, questions or suggestions, please [contact
us](mailto:support@ala.org.au).

## Installation

This package is under active development, and is not yet available on
CRAN. You can install the latest development version from GitHub with:

``` r
install.packages("remotes")
remotes::install_github("atlasoflivingaustralia/galaxias")
```

Load the package:

``` r
library(galaxias)
```

## Features

`galaxias` contains tools to:

- Standardise and save data with `use_data()`.
- Convert and save metadata statements written in Rmarkdown or Quarto as
  EML files with `use_metadata()`.
- Build Darwin Core Archives for sharing or publication using
  `build_archive()`.
- Check files for consistency with the Darwin Core Standard, either
  locally using `check_directory()`, or via API using `check_archive()`.

`galaxias` is part of a group of packages that help users publish data
using the Darwin Core standard. These packages are loaded with
`galaxias`. The other packages are:

- [`corella`](https://corella.ala.org.au) for converting tibbles to the
  required column names
- [`delma`](https://delma.ala.org.au) for converting markdown files to
  `xml`

## Usage

Here we have a small example dataset of species observations.

``` r
library(tibble)

df <- tibble(
  scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
  latitude = c(-35.310, -35.273), 
  longitude = c(149.125, 149.133),
  eventDate = lubridate::dmy(c("14-01-2023", "15-01-2023")),
  status = c("present", "present")
)

df
#> # A tibble: 2 × 5
#>   scientificName           latitude longitude eventDate  status 
#>   <chr>                       <dbl>     <dbl> <date>     <chr>  
#> 1 Callocephalon fimbriatum    -35.3      149. 2023-01-14 present
#> 2 Eolophus roseicapilla       -35.3      149. 2023-01-15 present
```

We can standardise data according to Darwin Core Standard.

``` r
df_dwc <- df |>
   set_occurrences(occurrenceID = random_id(),
                   basisOfRecord = "humanObservation",
                   occurrenceStatus = status) |>
   set_coordinates(decimalLatitude = latitude,
                   decimalLongitude = longitude)

df_dwc
#> # A tibble: 2 × 7
#>   scientificName          eventDate  basisOfRecord occurrenceID occurrenceStatus
#>   <chr>                   <date>     <chr>         <chr>        <chr>           
#> 1 Callocephalon fimbriat… 2023-01-14 humanObserva… 1881b588-36… present         
#> 2 Eolophus roseicapilla   2023-01-15 humanObserva… 1881b592-36… present         
#> # ℹ 2 more variables: decimalLatitude <dbl>, decimalLongitude <dbl>
```

Once standardised, we can specify that we wish to use these standardised
data in a Darwin Core Archive.`use_data()` saves the `df_dwc`  
with a valid csv file name and location.

``` r
use_data(df_dwc)
```

Create a template metadata statement for our data.

``` r
use_metadata_template("metadata.Rmd")
```

After editing, we can specify that we wish to use this metadata in a
Darwin Core Archive. `use_metadata()` converts our metadata to
[EML](https://eml.ecoinformatics.org/) format and saves it with a valid
xml file name and location.

``` r
use_metadata("metadata.Rmd")
```

Build a Darwin Core Archive and save it to the working directory.

``` r
build_archive()
```

Validate whether the constructed archive passes Darwin Core Standard
criteria.

``` r
check_archive()
```

See the [Quick Start
Guide](https://galaxias.ala.org.au/articles/quick_start_guide.html) for
an in-depth explanation of building Darwin Core Archives.

## Citing galaxias

To generate a citation for the package version you are using, you can
run:

``` r
citation(package = "galaxias")
```

The current recommended citation is:

> Westgate MJ, Balasubramaniam S & Kellie D (2025) galaxias: Describe,
> Package, and Share Biodiversity Data. R Package version 0.1.0.

## Contributors

Developers who have contributed to `galaxias` are as follows (in
alphabetical order by surname):

Amanda Buyan ([@acbuyan](https://github.com/acbuyan)), Fonti Kar
([@fontikar](https://github.com/fontikar)), Peggy Newman
([@peggynewman](https://github.com/peggynewman)) & Andrew Schwenke
([@andrew-1234](https://github.com/andrew-1234))
