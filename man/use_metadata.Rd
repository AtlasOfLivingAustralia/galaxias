% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_metadata.R
\name{use_metadata}
\alias{use_metadata}
\title{Use a metadata statement in a Darwin Core Archive}
\usage{
use_metadata(file = NULL, overwrite = FALSE, quiet = FALSE)
}
\arguments{
\item{file}{A metadata file in Rmarkdown (\code{.Rmd}) or Quarto markdown (\code{.qmd})
format.}

\item{overwrite}{By default, \code{use_metadata()} will not
overwrite existing files. If you really want to do so, set this to \code{TRUE}.}

\item{quiet}{Whether to message about what is happening. Default is set to
\code{FALSE}.}
}
\value{
Does not return an object to the workspace; called for the side
effect of building a file in the \code{data-publish} directory.
}
\description{
A metadata statement lists the owner of the dataset, how it was collected,
and how it can be used (i.e. its' licence). This function reads and
converts metadata saved in markdown (.md), Rmarkdown (.Rmd) or Quarto (.qmd)
to xml, and saves it in the publishing directory. This directory is
set using \code{\link[=galaxias_config]{galaxias_config()}} and defaults to \code{"data-publish"}. folder.

This function is a convenience wrapper function of \code{\link[delma:read_md]{delma::read_md()}} and
\code{\link[delma:write_eml]{delma::write_eml()}}.
}
\details{
To be compliant with the Darwin Core Standard, the schema file \strong{must} be
called \code{eml.xml}, and this function enforces that.
}
\examples{
\dontshow{
.old_wd <- setwd(tempdir())
}
use_metadata_template(quiet = TRUE)
use_metadata()
\dontshow{
setwd(.old_wd)
}
}
\seealso{
\code{\link[=use_metadata_statement]{use_metadata_statement()}} to create a metadata statement template.
}
