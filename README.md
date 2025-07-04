
<!-- README.md is generated from README.Rmd. Please edit that file -->

# galaxias <img src="man/figures/logo.png" align="right" style="margin: 0px 10px 0px 10px;" alt="" width="120"/><br>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/galaxias)](https://CRAN.R-project.org/package=galaxias)
[![Codecov test
coverage](https://codecov.io/gh/AtlasOfLivingAustralia/galaxias/graph/badge.svg)](https://app.codecov.io/gh/AtlasOfLivingAustralia/galaxias)
<!-- badges: end -->

## Overview

`galaxias` is an R package that helps users describe, bundle, and share
biodiversity information using the [‘Darwin Core’](https://dwc.tdwg.org)
data standard. `galaxias` provides tools in R to build a [Darwin Core
Archive](https://ipt.gbif.org/manual/en/ipt/latest/dwca-guide#what-is-darwin-core-archive-dwc-a),
a zip file containing standardised data and metadata accepted by global
data infrastructures. The package mirrors functionality in
[devtools](https://devtools.r-lib.org/),
[usethis](https://usethis.r-lib.org/), and
[dplyr](https://dplyr.tidyverse.org/) to manage data, files, and
folders. `galaxias` was created by the [Science & Decision Support
Team](https://labs.ala.org.au) at the [Atlas of Living
Australia](https://www.ala.org.au) (ALA).

The package is named for a genus of freshwater fish that is found only
in the Southern Hemisphere, and predominantly in Australia and Aotearoa
New Zealand. The logo shows a Spotted Galaxias (*Galaxias truttaceus*)
drawn by [Ian Brennan](http://www.iangbrennan.org).

If you have any comments, questions, or suggestions, please [contact
us](mailto:support@ala.org.au).

## Installation

You can install the latest version from GitHub with:

``` r
install.packages("remotes")
remotes::install_github("atlasoflivingaustralia/galaxias")
```

Once on CRAN, you can use:

``` r
install.packages("galaxias")
```

To load the package, call:

``` r
library(galaxias)
```

## Features

`galaxias` contains tools to:

- Standardise `tibbles` containing biodiversity observations to match
  the Darwin Core Standard.
- Convert metadata statements written in R Markdown or Quarto to EML
  files.
- Store all your publication-ready files in a single directory, and zip
  that directory for publication.
- Check files for consistency with the Darwin Core Standard, either
  locally using or via API.

`galaxias` draws on functionality from two underlying packages that
address different challenges of the data publication workflow:
[`corella`](https://corella.ala.org.au), which converts tibbles to use
standard column names; and [`delma`](https://delma.ala.org.au) which
converts markdown files to `EML` format.

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

We can standardise data according to Darwin Core Standard using `set_`
functions.

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
#> 1 Callocephalon fimbriat… 2023-01-14 humanObserva… e16986de-57… present         
#> 2 Eolophus roseicapilla   2023-01-15 humanObserva… e16986f2-57… present         
#> # ℹ 2 more variables: decimalLatitude <dbl>, decimalLongitude <dbl>
```

We can then specify that we wish to use these standardised data in a
Darwin Core Archive with `use_data()`. This saves `df_dwc` with a valid
file name and extension, and in a standardised location (a new directory
called `/data-publish`).

``` r
use_data(df_dwc)
```

Before publishing your data, it is also necessary to create a metadata
statement that describes who owns the data, what the data shows, and
what licence it is released under. `galaxias` enables you to write your
metadata statement in R Markdown or Quarto format, and seamlessly
convert it to [EML](https://eml.ecoinformatics.org/) for publication.

``` r
# 1. Create a boilerplate file
use_metadata_template("metadata.Rmd")

# 2. Edit in your preferred IDE

# 3. Load into /data-publish as an EML file
use_metadata("metadata.Rmd")
```

The final step in your data publication workflow is to zip your
directory into a single file. This file is placed in your parent
directory.

``` r
build_archive(file = "my_biodiversity_data.zip")
```

You can share your data via any mechanism you wish, but `galaxias`
provides the `submit_archive()` function to open a submission window for
the Atlas of Living Australia.

Please see the [Quick Start
Guide](https://galaxias.ala.org.au/R/articles/quick_start_guide.html)
for a more in-depth explanation of building Darwin Core Archives.

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
