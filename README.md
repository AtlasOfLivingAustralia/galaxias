
<!-- README.md is generated from README.Rmd. Please edit that file -->
<img src="man/figures/logo.png" align="left" style="margin: 20px 10px 0px 0px;" alt="" width="120"/><br>
<h2>
Create Darwin-Core Archives in R
</h2>

This package is intended to support data submission to biodiversity data
infrastructures such as the [ALA](https://www.ala.org.au) and
[GBIF](https://gbif.org), by providing the following features:

- auto-detect field headings and data types (`{janitor}`, `{lubridate}`
  etc) IN PROGRESS
- provide metadata in `.md` format, tools for converting to and from
  `xml` IN PROGRESS
- (optionally) manually re-assign column headings for unrecognised
  fields
- ask sensible questions to fill unavailable columns (e.g. spatial
  resolution, unique record identifier)
- generate (and optionally submit) a Darwin Core Archive
- optionally output revised objects in standard formats (`.csv`, `.md`)
- run the above using functions in R or via `{shiny}`

This package is currently under active development.

``` r
# Install the development version of this package:
library(remotes)
install_github("atlasoflivingaustralia/galaxias")

# Load package
library(galaxias)
```

# What is Darwin Core?

The ‘Darwin Core’ format is the data format used by GBIF and it’s node
member organisations. It stores observations of plants and animals
(‘occurrences’) in a standardised manner to facilitate sharing and
re-use. It is maintained by the Biodiversity Information Standards group
(<https://www.tdwg.org>).

A Darwin Core archive contains three files:

- `data.csv`: A dataset containing one occurrence per row, using
  standardised column names
- `eml.xml`: A metadata file containing information about the provider,
  and how the data were collected
- `meta.xml`: A file describing mapping each column name in `data.csv`
  to a corresponding name from the Darwin Core standard

These components are placed inside a zip file, which is then known as a
Darwin Core ‘Archive’ (DwC-A).

# What does `galaxias` do?

This package is designed to help people who have made field-based
biological observations to submit those observations to the ALA. It does
this by providing:

- Simple tools for checking column names, and converting them to Darwin
  Core standards
- Boilerplate text for what metadata is required with your submission,
  and converting it to the required format
- Functions for zipping these derived files into a DwC-A

Our intention is to make the process of submitting data to ALA as quick
and seamless as possible, without requiring the user to master lots of
complex functions, and without requiring an in-depth knowledge of the
data structures used by ALA and GBIF.

## Example workflow

First we import data:

``` r
library(readr)
x <- read_csv("a_file.csv")
```

We can then check this data using `galaxias`:

``` r
library(galaxias)
y <- detect_dwc_columns(x)
# Q: should we add reporting loop in here?
```

Once you are happy with how your data is formatted, you need to start on
metadata:

``` r
# Fonti to outline steps here
```

Finally, we run …, which builds the `meta.xml` file and packages the
archive:

``` r
# This hasn't been written yet
```
