# Quick start guide

`galaxias` is an R package that helps users bundle their data into a
standardised format optimised for storing, documenting, and sharing
biodiversity data. This standardised format is called a [**Darwin Core
Archive**](https://ipt.gbif.org/manual/en/ipt/latest/dwca-guide#what-is-darwin-core-archive-dwc-a)—a
zip file containing data and metadata that conform to the [Darwin Core
Standard](https://dwc.tdwg.org/), the accepted data standard of the
[Global Biodiversity Information Facility (GBIF)](https://gbif.org) and
its partner nodes (e.g. the Atlas of Living Australia).

Sharing Darwin Core Archives with data infrastructures allows data to be
reconstructed and aggregated accurately. Let’s see how to prepare a
Darwin Core Archive using `galaxias`.

## Getting started

Here we have an existing R project containing data collected over the
course of a research project. Our project uses a fairly standard folder
structure.

    ├── README.md                        : Description of the repository
    ├── my-project-name.Rproj            : RStudio project file
    ├── data                             : Folder to store cleaned data
    |  └── my_data.csv
    ├── data-raw                         : Folder to store original/source data
    |  └── my_raw_data.csv
    ├── plots                            : Folder containing plots/dataviz
    └── scripts                          : Folder with analytic coding scripts

Let’s see how galaxias can help us to package our data as a Darwin Core
Archive.

## Use standardised data in an archive

Data that we wish to share are in the `data` folder. They might look
something like this:

``` r
my_data
#> # A tibble: 2 × 6
#>   latitude longitude date       time  species                  location_id
#>      <dbl>     <dbl> <chr>      <chr> <chr>                    <chr>      
#> 1    -35.3      149. 14-01-2023 10:23 Callocephalon fimbriatum ARD001     
#> 2    -35.3      149. 15-01-2023 11:25 Eolophus roseicapilla    ARD001
```

First, we’ll need to standardise our data to conform to the Darwin Core
Standard.
[`suggest_workflow()`](https://rdrr.io/pkg/corella/man/suggest_workflow.html)
can help by summarising our dataset and suggesting the steps we should
take.

``` r
my_data |> suggest_workflow()
#> 
#> ── Matching Darwin Core terms ──────────────────────────────────────────────────
#> Matched 0 of 6 column names to DwC terms:
#> ✔ Matched:
#> ✖ Unmatched: date, latitude, location_id, longitude, species, time
#> 
#> ── Minimum required Darwin Core terms ──────────────────────────────────────────
#> 
#>   Type                      Matched term(s)  Missing term(s)                                                                
#> ✖ Identifier (at least one) -                occurrenceID, catalogNumber, recordNumber                                       
#> ✖ Record type               -                basisOfRecord                                                                   
#> ✖ Scientific name           -                scientificName                                                                  
#> ✖ Location                  -                decimalLatitude, decimalLongitude, geodeticDatum, coordinateUncertaintyInMeters 
#> ✖ Date/Time                 -                eventDate
#> 
#> ── Suggested workflow ──────────────────────────────────────────────────────────
#> 
#> To make your data Darwin Core compliant, use the following workflow:
#> 
#> df |>
#>   set_occurrences() |>
#>   set_datetime() |>
#>   set_coordinates() |>
#>   set_scientific_name()
#> 
#> ── Additional functions
#> ℹ See all `set_` functions at
#>   http://corella.ala.org.au/reference/index.html#add-rename-or-edit-columns-to-match-darwin-core-terms
```

Following the advice of
[`suggest_workflow()`](https://rdrr.io/pkg/corella/man/suggest_workflow.html),
we can use the `set_` functions to standardise `my_data`. `set_`
functions work a lot like
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html):
they modify existing columns or create new columns. The suffix of each
`set_` function gives an indication of the type of data it accepts
(e.g. [`set_coordinates()`](https://rdrr.io/pkg/corella/man/set_coordinates.html),
`set_scientific_name`), and function arguments are valid Darwin Core
terms to use as column names. Each `set_` function also checks to make
sure that each column contains valid data according to Darwin Core
Standard.

``` r
library(lubridate)

my_data_dwc <- my_data |>
  # basic requirements of Darwin Core
  set_occurrences(occurrenceID = sequential_id(),
                  basisOfRecord = "humanObservation") |> 
  # place and time
  set_coordinates(decimalLatitude = latitude, 
                  decimalLongitude = longitude) |>
  set_locality(country = "Australia", 
               locality = "Canberra") |>
  set_datetime(eventDate = lubridate::dmy(date),
               eventTime = lubridate::hm(time)) |>
  # taxonomy
  set_scientific_name(scientificName = species, 
                      taxonRank = "species") |>
  set_taxonomy(kingdom = "Animalia",
               family = "Cacatuidae") 

my_data_dwc 
#> # A tibble: 2 × 13
#>   location_id basisOfRecord    occurrenceID decimalLatitude decimalLongitude
#>   <chr>       <chr>            <chr>                  <dbl>            <dbl>
#> 1 ARD001      humanObservation 01                     -35.3             149.
#> 2 ARD001      humanObservation 02                     -35.3             149.
#> # ℹ 8 more variables: country <chr>, locality <chr>, eventDate <date>,
#> #   eventTime <Period>, scientificName <chr>, taxonRank <chr>, family <chr>,
#> #   kingdom <chr>
```

You may have noticed that we added some additional columns that were not
included in the advice of
[`suggest_workflow()`](https://rdrr.io/pkg/corella/man/suggest_workflow.html)
(`country`, `locality`, `taxonRank`, `kingdom`, `family`). We encourage
users to specify additional information where possible to avoid
ambiguity once their data are shared.

To use our standardised data in a Darwin Core Archive, we can select
columns that use valid Darwin Core terms as column names. Invalid
columns won’t be accepted when we try to build our Darwin Core Archive.
Our data is an occurrence-based dataset (each row contains information
at the observation level, as opposed to site/survey level), so we’ll
select columns that match names in
[`occurrence_terms()`](https://rdrr.io/pkg/corella/man/accepted_terms.html).

``` r
library(dplyr)

my_data_dwc_occ <- my_data_dwc |>
  select(any_of(occurrence_terms()))

my_data_dwc_occ
```

    ## # A tibble: 2 × 12
    ##   basisOfRecord    occurrenceID eventDate  eventTime  country   locality
    ##   <chr>            <chr>        <date>     <Period>   <chr>     <chr>   
    ## 1 humanObservation 01           2023-01-14 10H 23M 0S Australia Canberra
    ## 2 humanObservation 02           2023-01-15 11H 25M 0S Australia Canberra
    ## # ℹ 6 more variables: decimalLatitude <dbl>, decimalLongitude <dbl>,
    ## #   scientificName <chr>, kingdom <chr>, family <chr>, taxonRank <chr>

Now we can specify that we wish to use `my_data_dwc_occ` in our Darwin
Core Archive with
[`use_data()`](https://galaxias.ala.org.au/R/reference/use_data.md),
which saves this dataset in the `data_publish` folder with the correct
file name `occurrences.csv`.

``` r
use_data(my_data_dwc_occ) 
```

If we look again at our file structure, we now find our data has been
added to our new folder:

    ├── README.md
    ├── my-project-name.Rproj
    ├── data
    |  └── my_data.csv
    ├── data-publish                    : New folder to store data for publication
    |  └── occurrences.csv              : Data formatted as per Darwin Core Standard
    ├── data-raw
    |  └── my_raw_data.csv
    ├── plots
    └── scripts

## Add metadata

A critical part of a Darwin Core archive is a metadata statement: this
tells users who owns the data, what the data were collected for, and
what uses they can be put to (i.e. a data licence). To get an example
statement, call
[`use_metadata_template()`](https://delma.ala.org.au/R/reference/use_metadata_template.html).

``` r
use_metadata_template()
```

By default, this creates an R Markdown template named `metadata.Rmd` in
your working directory. We can edit this template to include information
about our dataset, and specify that we wish to use it in our Darwin Core
Archive with
[`use_metadata()`](https://galaxias.ala.org.au/R/reference/use_metadata.md).

``` r
use_metadata("metadata.Rmd")
```

This converts our metadata statement to Ecological Meta Language
(`EML`), the accepted format of metadata for Darwin Core Archives, and
saves it as `eml.xml` in the `data-publish` folder.

## Build an archive

At the end of the above process, we should have a folder named
`data-publish` that contains at least two files:

- One or more `.csv` files containing data (e.g. `occurrences.csv`,
  `events.csv`, `multimedia.csv`)
- An `eml.xml` file containing your metadata

We can now run
[`build_archive()`](https://galaxias.ala.org.au/R/reference/build_archive.md)
to build our Darwin Core Archive!

``` r
build_archive()
```

Running
[`build_archive()`](https://galaxias.ala.org.au/R/reference/build_archive.md)
first checks whether we have a ‘schema’ document (`meta.xml`) in our
`data-publish` folder. This is a machine-readable `xml` document that
describes the content of the archive’s data files and their structure.
The schema document is a required file in a Darwin Core Archive. If it
is missing,
[`build_archive()`](https://galaxias.ala.org.au/R/reference/build_archive.md)
will build one. We can also build a schema document ourselves using
[`use_schema()`](https://galaxias.ala.org.au/R/reference/use_schema.md).

At the end of this process, you should have a Darwin Core Archive zip
file (`dwc-archive.zip`) in your parent directory. You should also have
a `data-publish` folder in your working directory containing
standardised data files (e.g. `occurrences.csv`), a metadata statement
in EML format (`eml.xml`), and a schema document (`meta.xml`).

## Check archive

There are two ways to check whether the contents of your Darwin Core
Archive meet the Darwin Core Standard.

The first is to run local tests on the files inside a local folder that
will be used to build a Darwin Core Archive.
[`check_directory()`](https://galaxias.ala.org.au/R/reference/check_directory.md)
allows us to check csv files and xml files in the folder against Darwin
Core Standard criteria, using the same checking functionality that is
built into the `set_` functions. This function is especially beneficial
if you have standardized your data to Darwin Core headers using
functions outside of `galaxias`/`corella`, such as
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
for example.

``` r
check_directory()
```

The second is to check whether a complete Darwin Core Archive meets an
institution’s Darwin Core criteria via an API. For example, we can test
an archive against GBIF’s API tests.

``` r
# Check against GBIF API
check_archive("dwc-archive.zip",
              email = "your-email",
              username = "your-username",
              password = "your-password")
```

## Publish/share your archive

The final step is to share your completed Darwin Core Archive with a
data infrastructure like the Atlas of Living Australia. To share with
the ALA, you can launch our data submission process in your browser by
calling:

``` r
submit_archive()
```

This function will provide you with the option to open a GitHub issue
where you can attach your archive. We will run the galaxias test suite
on your dataset and respond as soon as we can.

If you’d prefer not to use GitHub, you can send your file and a brief
description to <support@ala.org.au>.
