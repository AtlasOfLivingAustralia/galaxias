% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_archive.R
\name{check_archive}
\alias{check_archive}
\title{Check an archive against Darwin Core standards}
\usage{
check_archive(x = "data")
}
\arguments{
\item{x}{(string) A directory containing the files to be published, or
optionally a \code{.zip} file built from the same (i.e. with \code{build_archive()}).
Defaults to the \code{data} folder within the current working directory.}
}
\value{
Invisibly returns a tibble to the workspace containing check
results; but primarily called for the side-effect of generating a report in
the console.
}
\description{
This is a wrapper to two other packages; schema and EML files (i.e. xml) are
checked with the \code{delma} package; csv files are checked with the \code{corella}
package.
}
\seealso{
\code{validate_archive()} which runs checks via API, rather than locally.
}
