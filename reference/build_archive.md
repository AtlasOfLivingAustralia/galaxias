# Build a Darwin Core Archive from a folder

A Darwin Core archive is a zip file containing a combination of data and
metadata. `build_archive()` constructs this zip file in the parent
directory. The function assumes that all necessary files have been
pre-constructed, and can be found inside the `"data-publish"` directory
with no additional or redundant information. Structurally,
`build_archive()` is similar to `devtools::build()`, in the sense that
it takes a repository and wraps it for publication.

## Usage

``` r
build_archive(file = "dwc-archive.zip", overwrite = FALSE, quiet = FALSE)
```

## Arguments

- file:

  The name of the file to be built in the parent directory. Should end
  in `.zip`.

- overwrite:

  (logical) Should existing files be overwritten? Defaults to `FALSE`.

- quiet:

  (logical) Whether to suppress messages about what is happening.
  Default is set to `FALSE`; i.e. messages are shown.

## Value

Doesn't return anything; called for the side-effect of building a
'Darwin Core Archive' (i.e. a zip file).

## Details

This function looks for three types of objects in the `data-publish`
directory:

- Data

  One or more csv files named `occurrences.csv`, `events.csv` and/or
  `multimedia.csv`. These csv files contain data standardised using
  Darwin Core Standard (see
  [`corella::corella-package()`](https://rdrr.io/pkg/corella/man/corella-package.html)
  for details). A `data.frame`/`tibble` can be added to the correct
  folder using
  [`use_data()`](https://galaxias.ala.org.au/R/reference/use_data.md).

- Metadata

  A metadata statement in `EML` format with the file name `eml.xml`.
  Completed metadata statements written markdown as `.Rmd` or `qmd`
  files can be converted and saved to the correct folder using
  [`use_metadata()`](https://galaxias.ala.org.au/R/reference/use_metadata.md).
  Create a new template with
  [`use_metadata_template()`](https://delma.ala.org.au/R/reference/use_metadata_template.html).

- Schema

  A 'schema' document in xml format with the file name `meta.xml`.
  `build_archive()` will detect whether this file is present and build a
  schema file if missing. This file can also be constructed separately
  using
  [`use_schema()`](https://galaxias.ala.org.au/R/reference/use_schema.md).

## See also

[`use_data()`](https://galaxias.ala.org.au/R/reference/use_data.md),
[`use_metadata()`](https://galaxias.ala.org.au/R/reference/use_metadata.md),
[`use_schema()`](https://galaxias.ala.org.au/R/reference/use_schema.md)
