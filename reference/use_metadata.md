# Use a metadata statement in a Darwin Core Archive

A metadata statement lists the owner of the dataset, how it was
collected, and how it can be used (i.e. its' licence). This function
reads and converts metadata saved in markdown (.md), Rmarkdown (.Rmd) or
Quarto (.qmd) to xml, and saves it in the `data-publish` directory.

This function is a convenience wrapper function of
[`delma::read_md()`](https://delma.ala.org.au/R/reference/read_md.html)
and
[`delma::write_eml()`](https://delma.ala.org.au/R/reference/write_eml.html).

## Usage

``` r
use_metadata(file = NULL, overwrite = FALSE, quiet = FALSE)
```

## Arguments

- file:

  A metadata file in Rmarkdown (`.Rmd`) or Quarto markdown (`.qmd`)
  format.

- overwrite:

  By default, `use_metadata()` will not overwrite existing files. If you
  really want to do so, set this to `TRUE`.

- quiet:

  Whether to message about what is happening. Default is set to `FALSE`.

## Value

Does not return an object to the workspace; called for the side effect
of building a file in the `data-publish` directory.

## Details

To be compliant with the Darwin Core Standard, the schema file **must**
be called `eml.xml`, and this function enforces that.

## See also

[`use_metadata_template()`](https://delma.ala.org.au/R/reference/use_metadata_template.html)
to create a metadata statement template;
[`use_data()`](https://galaxias.ala.org.au/R/reference/use_data.md) to
save data to `/data-publish`.

## Examples

``` r
#> ✔ Setting active project to "/tmp/RtmpDGDYQW".

# Get a boilerplate metadata statement
use_metadata_template(file = "my_metadata.Rmd", quiet = TRUE)

# Once editing is complete, call `use_metadata()` to convert to an EML file
use_metadata("my_metadata.Rmd", quiet = TRUE)
#> ✔ Creating data-publish/.

# Check that file has been created
list.files("data-publish")
#> [1] "eml.xml"

# returns "eml.xml" as expected

#> ✔ Setting active project to
#>   "/home/runner/work/galaxias/galaxias/docs/reference".
```
