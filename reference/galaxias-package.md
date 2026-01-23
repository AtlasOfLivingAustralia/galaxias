# Build repositories to share biodiversity data

`galaxias` helps users describe, package and share biodiversity
information using the 'Darwin Core' data standard, which is the format
used and accepted by the Global Biodiversity Information Facility (GBIF)
and its' partner nodes. `galaxias` is functionally similar to
`devtools`, but with a focus on building Darwin Core Archives rather
than R packages.

The package is named for a genus of freshwater fish.

## References

If you have any questions, comments or suggestions, please email
<support@ala.org.au>.

**Prepare information for Darwin Core**

- [`use_metadata_template()`](https://delma.ala.org.au/R/reference/use_metadata_template.html)
  Add a blank metadata statement template to the working directory

- [`suggest_workflow()`](https://rdrr.io/pkg/corella/man/suggest_workflow.html)
  Advice to standardise data using the Darwin Core Standard

**Add information to the `data-publish` directory**

- [`use_data()`](https://galaxias.ala.org.au/R/reference/use_data.md)
  Save standardised data for use in a Darwin Core Archive

- [`use_metadata()`](https://galaxias.ala.org.au/R/reference/use_metadata.md)
  Convert a metadata file from markdown to EML (`eml.xml`) and save for
  use in a Darwin Core Archive

- [`use_schema()`](https://galaxias.ala.org.au/R/reference/use_schema.md)
  Build a schema file (`meta.xml`) for a given directory and save for
  use in a Darwin Core Archive

**Build an archive**

- [`check_directory()`](https://galaxias.ala.org.au/R/reference/check_directory.md)
  Check files in your local Darwin Core directory

- [`build_archive()`](https://galaxias.ala.org.au/R/reference/build_archive.md)
  Convert a directory to a Darwin Core Archive

- [`check_archive()`](https://galaxias.ala.org.au/R/reference/check_archive.md)
  Check whether archive passes Darwin Core criteria via the GBIF API

- [`submit_archive()`](https://galaxias.ala.org.au/R/reference/submit_archive.md)
  Open a browser to submit your data to the ALA

## See also

Useful links:

- <https://galaxias.ala.org.au/R/>

- Report bugs at
  <https://github.com/AtlasOfLivingAustralia/galaxias/issues>

## Author

**Maintainer**: Martin Westgate <martin.westgate@csiro.au>

Authors:

- Shandiya Balasubramaniam <shandiya.balasubramaniam@csiro.au>

- Dax Kellie <dax.kellie@csiro.au>
