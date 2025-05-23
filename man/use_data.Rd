% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_data.R
\name{use_data}
\alias{use_data}
\alias{use_data_occurrences}
\alias{use_data_events}
\title{Use standardised data in a Darwin Core Archive}
\usage{
use_data(..., overwrite = FALSE, quiet = FALSE)

use_data_occurrences(df, overwrite = FALSE, quiet = FALSE)

use_data_events(df, overwrite = FALSE, quiet = FALSE)
}
\arguments{
\item{...}{Unquoted name of \code{tibble}/\code{data.frame} to save.}

\item{overwrite}{By default, \code{use_data_events()} will not
overwrite existing files. If you really want to do so, set this to \code{TRUE}.}

\item{quiet}{Whether to message about what is happening. Default is set to
\code{FALSE}.}

\item{df}{A \code{tibble}/\code{data.frame} to save.}
}
\value{
Invisibly returns the location of the saved csv file.
}
\description{
Once data conform to Darwin Core Standard, \code{use_data()} makes it
easy to save data in the correct place for building a Darwin Core Archive
with \code{\link[=build_archive]{build_archive()}}.

\code{use_data()} is an all-in-one function for accepted data types "occurrence",
"event" and "multimedia". \code{use_data()} attempts to detect and save the
correct data type based on the provided \code{tibble}/\code{data.frame}.
Alternatively, users can call the underlying functions
\code{\link[=use_data_occurrences]{use_data_occurrences()}}, \code{\link[=use_data_events]{use_data_events()}} and \code{\link[=use_data_multimedia]{use_data_multimedia()}} to
specify data type manually.
}
\details{
By default, this function saves data in the \code{data-publish} folder. To change
this default, see \code{\link[=galaxias_config]{galaxias_config()}}.

Data type is determined by detecting type-specific column names in
supplied data.
\itemize{
\item Event: (\code{eventID}, \code{parentEventID}, \code{eventType})
\item Multimedia: not yet supported
}
}
\seealso{
\code{\link[=use_metadata]{use_metadata()}}
}
