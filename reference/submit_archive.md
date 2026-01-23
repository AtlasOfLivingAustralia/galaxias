# Submit a Darwin Core Archive to the ALA

The preferred method for submitting a dataset for publication via the
ALA is to raise an issue on our ['Data Publication' GitHub
Repository](https://github.com/AtlasOfLivingAustralia/data-publication),
and attached your archive zip file (constructed using
[`build_archive()`](https://galaxias.ala.org.au/R/reference/build_archive.md))
to that issue. If your dataset is especially large (\>100MB), you will
need to post it in a publicly accessible location (such as a GitHub
release) and post the link instead. This function simply opens a new
issue in the users' default browser to enable dataset submission.

## Usage

``` r
submit_archive(quiet = FALSE)
```

## Arguments

- quiet:

  Whether to suppress messages about what is happening. Default is set
  to `FALSE`; i.e. messages are shown.

## Value

Does not return anything to the workspace; called for the side-effect of
opening a submission form in the users' default browser.

## Details

The process for accepting data for publication at ALA is not automated;
this function will initiate an evaluation process, and will not result
in your data being instantly visible on the ALA. Nor does submission
guarantee acceptance, as ALA reserves the right to refuse to publish
data that reveals the locations of threatened or at-risk species.

This mechanism is **entirely public**; your data will be visible to
others from the point you put it on this webpage. If your data contains
sensitive information, contact <support@ala.org.au> to arrange a
different delivery mechanism.

## Examples

``` r
if(interactive()){
  submit_archive()
}
```
