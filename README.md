
<!-- README.md is generated from README.Rmd. Please edit that file -->

# galaxias <img src="man/figures/logo.png" align="right" style="margin: 0px 10px 0px 10px;" alt="" width="120"/><br>

## Overview

`galaxias` is an R package that helps users standardize, document and
share biodiversity information using the [‘Darwin
Core’](https://dwc.tdwg.org) data standard. It supports users to set up
repositories in a standardised way; provides tools and examples for how
to use the Darwin Core standard; and allows remote validation and
publication of said data via API. `galaxias` was built, and is
maintained, by the [Science & Decision Support
Team](https://labs.ala.org.au) at the [Atlas of Living
Australia](https://www.ala.org.au) (ALA).

The package is named for a genus of freshwater fish that is found only
in the Southern Hemisphere, and predominantly in Australia and New
Zealand.

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

## Citing galaxias

To generate a citation for the package version you are using, you can
run:

``` r
citation(package = "galaxias")
```

The current recommended citation is:

> Westgate MJ, Balasubramaniam S & Kellie D (2024) galaxias:
> Standardise, Document and Share Biodiversity Data. R Package version
> 0.1.0.9999.
