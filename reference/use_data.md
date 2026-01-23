# Use standardised data in a Darwin Core Archive

Once data conform to Darwin Core Standard, `use_data()` makes it easy to
save data in the correct place for building a Darwin Core Archive with
[`build_archive()`](https://galaxias.ala.org.au/R/reference/build_archive.md).

`use_data()` is an all-in-one function for accepted data types
"occurrence", "event" and "multimedia". `use_data()` attempts to detect
and save the correct data type based on the provided
`tibble`/`data.frame`. Alternatively, users can call the underlying
functions `use_data_occurrences()` or `use_data_events()` to specify
data type manually.

## Usage

``` r
use_data(..., overwrite = FALSE, quiet = FALSE)

use_data_occurrences(df, overwrite = FALSE, quiet = FALSE)

use_data_events(df, overwrite = FALSE, quiet = FALSE)
```

## Arguments

- ...:

  Unquoted name of `tibble`/`data.frame` to save.

- overwrite:

  By default, `use_data_events()` will not overwrite existing files. If
  you really want to do so, set this to `TRUE`.

- quiet:

  Whether to message about what is happening. Default is set to `FALSE`.

- df:

  A `tibble`/`data.frame` to save.

## Value

Does not return anything to the workspace; called for the side-effect of
saving a `.csv` file to `/data-publish`.

## Details

This function saves data in the `data-publish` folder. It will create
that folder if it is not already present.

Data type is determined by detecting type-specific column names in
supplied data.

- Event: (`eventID`, `parentEventID`, `eventType`)

- Multimedia: not yet supported

## See also

[`use_metadata()`](https://galaxias.ala.org.au/R/reference/use_metadata.md)
to save metadata to `/data-publish`.

## Examples

``` r
#> ✔ Setting active project to "/tmp/RtmpDGDYQW".

# Build an example dataset
df <- tibble::tibble(
  occurrenceID = c("a1", "a2"),
  species = c("Eolophus roseicapilla", "Galaxias truttaceus"))

# The default function *always* asks about data type
if(interactive()){
  use_data(df)
}

# To manually specify the type of data - and avoid questions in your 
# console - use the underlying functions instead
use_data_occurrences(df, quiet = TRUE)
#> ✔ Creating data-publish/.

# Check that file has been created
list.files("data-publish")
#> [1] "occurrences.csv"

# returns "occurrences.csv" as expected

#> ✔ Setting active project to
#>   "/home/runner/work/galaxias/galaxias/docs/reference".
```
