# Check whether contents of directory comply with the Darwin Core Standard

Checks that files in the `data-publish` directory meet Darwin Core
Standard. `check_directory()` runs
[`corella::check_dataset()`](https://rdrr.io/pkg/corella/man/check_dataset.html)
on `occurrences.csv` and `events.csv` files, and
[`delma::check_metadata()`](https://delma.ala.org.au/R/reference/check_metadata.html)
on the `eml.xml` file, if they are present. These `check_` functions run
tests to determine whether data and metadata pass Darwin Core Standard
criteria.

## Usage

``` r
check_directory()
```

## Value

Doesn't return anything; called for the side-effect of generating a
report in the console.

## See also

[`check_archive()`](https://galaxias.ala.org.au/R/reference/check_archive.md)
checks a Darwin Core Archive via a GBIF API, rather than locally.
