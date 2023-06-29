# galaxia

This package is intended to support data submission to biodiversity data
infrastructures such as the [ALA](https://www.ala.org.au) and 
[GBIF](https://gbif.org), by providing the following features:

 - import and auto-detect field headings and data types (`{readr}`, `{janitor}`, `{lubridate}`)
 - provide metadata in `.md` format, tools for converting to and from `xml`
 - (optionally) manually re-assign column headings for unrecognised fields
 - ask sensible questions to fill unavailable columns (e.g. spatial resolution, unique record identifier)
 - generate (and optionally submit) a Darwin Core Archive
 - optionally output revised objects in standard formats (`.csv`, `.md`)
 - run the above using functions in R or via `{shiny}`
 
This package is currently under active development