
<!-- README.md is generated from README.Rmd. Please edit that file -->
<img src="man/figures/logo.png" align="left" style="margin: 20px 10px 0px 0px;" alt="" width="120"/><br>
<h2>
Create Darwin-Core Archives in R
</h2>

`galaxias` is an R package designed to simplify the process of
converting biodiversity data into [Darwin Core](https://dwc.tdwg.org)
archives (DwC-A), facilitating data submission to infrastructures such
as the [Atlas of Living Australia (ALA)](https://www.ala.org.au) and the
[Global Biodiversity Information Facility (GBIF)](https://gbif.org). The
key features include:

- Check and conform data columns and values to meet Darwin Core
  standards
- Automatic metafile creation
- User friendly workflow for creating a resource metadata document
  (`eml.xml`) using simple markdown
- Final export of a compressed (`ZIP`) DwC-A from the above components

## Installation

This package is under active development, and is not yet available on
CRAN. You can install the latest development version from GitHub with:

``` r
# devtools::install_github("atlasoflivingaustralia/galaxias")
devtools::load_all()
#> ℹ Loading galaxias
```

Load the package:

``` r
# library(galaxias)
```

## Basic usage

A ‘Darwin Core Archive’ is a zip file containing three files:

- A table of biodiversity observations
- A metadata statement about what the data is, and who collected it
- A file that maps your column headings to those of the Darwin Core
  standard

`galaxias` helps you generate, store, modify, and validate these files.
To get started, simply start a pipe using the `dwca()` function:

``` r
archive <- dwca()
archive
#> An object of class `dwca` containing: 
#> data: Not supplied
#> metadata: Not supplied
#> column_mappings: Not supplied
```

You can then amend this object using `add_` functions to supply the
required information:

``` r
archive <- archive |>
  add_data(file = "your_data_here.csv") |>
  add_metadata(file = "your_metadata_here.Rmd") |>
  add_column_mappings()
```

These functions work by interacting with files in your working
directory. While the data `csv` is in a simple format, both the metadata
and column mapping functions use `xml` internally. Rather than work
natively with `xml`, however, `galaxias` contains functions to convert
`xml` to Markdown formats, particularly `rmarkdown` (`.Rmd`) and Quarto
(`.qmd`).

``` r
read_xml()
write_xml()
```

In practice, supplying valid metadata is tricky, so best practice is to
download an example from ALA and modify it to your needs:

``` r
get_metadata(id = "df368", 
             file = "example.Rmd")
```

Column mapping is trickier still; it is generally best to build your
objects and let `add_column_mappings()` do the heavy lifting; if a file
or object is not supplied to `add_column_mappings()`, it will attempt to
build one. This differs from `add_data()` and `add_metadata()`, which
will not attempt to guess what data you want to provide.

## You can then use the following functions to complete your archive:

## What is Darwin Core?

Darwin Core (DwC) is a standardised framework used for compiling
biodiversity data, and is maintained by the Biodiversity Information
Standards group (<https://www.tdwg.org>). As a data standard, DwC
facilitates the sharing and use of open-access biodiversity data, and is
used by GBIF and it’s node member organisations.

In practice, the DwC standards and terms are used to create a single,
self-contained dataset, known as a Darwin Core Archive (DwC-A). The
format of this archive is a `ZIP` file containing text files of data and
metadata, and represents the fundamental unit required to share DwC
compliant biodiversity data.

A DwC-A can contain a range of different files, but there are three main
files that are required:

- A data core: A core dataset, such as an Occurrence or Event core
- `eml.xml`: A resource metadata file describing the dataset, in the
  Ecological Metadata Language (EML) format
- `meta.xml`: A metadata file describing the structure and relationships
  of files within the archive

## What does `galaxias` do? And who is it for?

The collection of field-based biological observations is widespread
across a range of scientific disiplines. The importance and value in
sharing this data for re-use is well understood by the research
community. However, various constraints such as limited time, or
unfamiliarity with the standards and data structures required by online
services such as the ALA and GBIF, can hinder user contributions.

The aim of `galaxias` is therefore to facilitate the sharing of data, by
streamlining the process of creating a DwC-A from field-based biological
observations, for submission to online data infrastructures such as the
ALA. As an R package, `galaxias` can integrate seamlessly with existing
workflows, making the process of submitting data to online services as
fast and seamless as possible.

We provide tools to assist with the creation and compliance of the three
main components required by a DwC-A:

- Core data file:
  - Generate a summary report of the data’s compatibility with Darwin
    Core required fields and formats
  - Import or create a lookup table (dictionary) for mapping column
    headings with Darwin Core terms.
  - Tools to assist with conforming data to the DwC standards (fill
    missing fields, convert to required formats, etc.)
- Automatic descriptor metafile (`meta.xml`) creation
- Resource metadata document (`eml.xml`):
  - Create and edit the required resource metadata file using simple
    markdown and our provided template, with tools for converting to and
    from `xml`
- Final export of a compressed (`ZIP`) DwC-A from the above components

A function to allow direct upload of DwC-As to the ALA via API is also
in progress.

## Example workflow

Starting with our core data file, we will import observation data into
R:

``` r
occurrence_data <- read_csv(
  system.file("data/westerband_2022_wdate.csv", package = "galaxias")
)
```

### Dataset validation

If you have an existing lookup table, which matches column names from
your data set to valid DwC terms, you can import it with `import_map()`.
Otherwise, we can use the `map_fields()` function to interactively
create a lookup table. This functions offers suggestions for mappings
which can help speed up the process.

``` r
occurrence_data_mapped <- map_fields(occurrence_data)
```

We can check if our dataset contains all required and suggested DwC
fields using `check_fields()`. This check is also interactive, offering
suggested fixes where supported (e.g. unique identifiers, date formats
(TBD)). A report is shown in the console, identifying needed fixes. By
default, both presence of required and recommended fields (for
Occurrence data) are checked. Specify \`check_fields(data, all_fields =
FALSE) to check only the required fields.

``` r
data_clean <- read_csv(
  system.file("data/occurrence_exemplar.csv", package = "galaxias")
)
```

``` r
check_unique_identifiers(data_clean)
check_unique_identifiers(occurrence_data)

check_fields(data_clean)
check_fields(occurrence_data)

check_percent_match(data_clean)
check_percent_match(occurrence_data)
```

With valid terms, we can run some validation steps on the data.

``` r
validate(data_clean)
generate_report(data_clean)
```

## EML

``` r
edit_template()
build_eml("./user_template.md")
```

## Meta

``` r
build_metafile(data_clean)
```

## Final archive

``` r
archive(
  data = data_clean,
  meta = NULL,
  eml = eml_template_updated,
  folder = "./archive"
)
```

## In progress

- The current aim is a MVP, no warranty provided way to create a DwC
  archive
- The packge will have a main function entry point
- There are two main user facing processes:
  - Check (validate)
  - Modify (basic modifications to data where possible)
  - Build (zip)
- There are three products required for a DwC archive:
  - data.csv
  - meta.xml
  - eml.xml

### Check

### Build
