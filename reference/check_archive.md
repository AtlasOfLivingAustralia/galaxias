# Check whether an archive meets the Darwin Core Standard via API

Check whether a specified Darwin Core Archive is ready for sharing and
publication, according to the Darwin Core Standard. `check_archive()`
tests an archive - defaulting to `"dwc-archive.zip"` in the users'
parent directory - using an online validation service. Currently only
supports validation using GBIF.

## Usage

``` r
check_archive(
  file = "dwc-archive.zip",
  username = NULL,
  email = NULL,
  password = NULL,
  wait = TRUE,
  quiet = FALSE
)

get_report(
  obj,
  username = NULL,
  password = NULL,
  n = 5,
  wait = TRUE,
  quiet = FALSE
)

view_report(x, n = 5)

# S3 method for class 'gbif_validator'
print(x, ...)
```

## Arguments

- file:

  The name of the file in the parent directory to pass to the validator
  API, ideally created using
  [`build_archive()`](https://galaxias.ala.org.au/R/reference/build_archive.md).

- username:

  Your GBIF username.

- email:

  The email address used to register with `gbif.org`.

- password:

  Your GBIF password.

- wait:

  (logical) Whether to wait for a completed report from the API before
  exiting (`TRUE`, the default), or try the API once and return the
  result regardless (`FALSE`).

- quiet:

  (logical) Whether to suppress messages about what is happening.
  Default is set to `FALSE`; i.e. messages are shown.

- obj:

  Either an object of class `character` containing a key that uniquely
  identifies your query; or an object of class `gbif_validator`.
  returned by `check_archive()` or `get_report()`

- n:

  Maximum number of entries to print per file. Defaults to 5.

- x:

  An object of class `gbif_validator`.

- ...:

  Additional arguments, currently ignored.

## Value

Both `check_archive()` and `get_report()` return an object of class
`gbif_validator` to the workspace. `view_report()` and
`print.gbif_validator()` don't return anything, and are called for the
side-effect of printing useful information to the console.

## Details

Internally, `check_archive()` both `POST`s the specified archive to the
GBIF validator API and then calls `get_report()` to retrieve (`GET`) the
result. `get_report()` is exported to allow the user to download results
at a later time should they wish; this is more efficient than repeatedly
generating queries with `check_archive()` if the underlying data are
unchanged. A third option is simply to assign the outcome of
`check_archive()` or `get_report()` to an object, then call
`view_report()` to format the result nicely. This approach doesn't
require any further API calls and is considerably faster.

Note that information returned by these functions is provided verbatim
from the institution API, not from galaxias.

## See also

[`check_directory()`](https://galaxias.ala.org.au/R/reference/check_directory.md)
which runs checks on a directory (but **not** an archive) locally,
rather than via API.
