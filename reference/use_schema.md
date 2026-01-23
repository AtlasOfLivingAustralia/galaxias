# Create a `schema` for a Darwin Core Archive

A schema is an xml document that maps the files and field names in a
DwCA. This map makes it easier to reconstruct one or more related
datasets so that information is matched correctly. It works by detecting
column names on csv files in a specified directory; these should all be
Darwin Core terms for this function to produce reliable results. This
function assumes that the publishing directory is named
`"data-publish"`. This function is primarily internal and is called by
[`build_archive()`](https://galaxias.ala.org.au/R/reference/build_archive.md),
but is exported for clarity and debugging purposes.

## Usage

``` r
use_schema(overwrite = FALSE, quiet = FALSE)
```

## Arguments

- overwrite:

  By default, `use_schema()` will not overwrite existing files. If you
  really want to do so, set this to `TRUE`.

- quiet:

  (logical) Should progress messages be suppressed? Default is set to
  `FALSE`; i.e. messages are shown.

## Value

Does not return an object to the workspace; called for the side effect
of building a schema file in the publication directory.

## Details

To be compliant with the Darwin Core Standard, the schema file **must**
be called `meta.xml`, and this function enforces that.

## See also

[`build_archive()`](https://galaxias.ala.org.au/R/reference/build_archive.md)
which calls this function.

## Examples

``` r
#> ✔ Setting active project to "/tmp/RtmpDGDYQW".

# First build some data to add to our archive
df <- tibble::tibble(
  occurrenceID = c("a1", "a2"),
  species = c("Eolophus roseicapilla", "Galaxias truttaceus"))
  
use_data_occurrences(df, quiet = TRUE)
#> ✔ Creating data-publish/.

# Now we can build a schema document to describe that dataset
use_schema(quiet = TRUE)

# Check that specified files have been created
list.files("data-publish") 
#> [1] "meta.xml"        "occurrences.csv"

# The publish directory now contains:
#  - "occurrences.csv" which contains data
#  - "meta.xml" which is the schema document

#> ✔ Setting active project to
#>   "/home/runner/work/galaxias/galaxias/docs/reference".
```
