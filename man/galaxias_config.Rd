% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/galaxias_config.R, R/print_config.R
\name{galaxias_config}
\alias{galaxias_config}
\alias{print.galaxias_config}
\title{Provide configuration information to \code{galaxias}}
\usage{
galaxias_config(directory = NULL, gbif = NULL)

\method{print}{galaxias_config}(x, ...)
}
\arguments{
\item{directory}{A string giving the name of the directory to be used for
storing working files. Defaults to \code{data-publish}.}

\item{gbif}{A list containing the entries \code{username}, \code{email} and \code{password}.}

\item{x}{An object of class \code{galaxias_config}, created with
\code{galaxias_config()}.}

\item{\dots}{Additional arguments, currently ignored.}
}
\description{
To validate (or in future, to publish) your dataset, you need to provide
credentials to the relevant web service. This function allows you to store
that information for access by \code{galaxias} API functions. This function
also enables you to change the directory where working documents are stored,
which defaults to \code{data-publish}.
}
\details{
Note that unlike \code{galah}, you cannot set a 'default' provider in \code{galaxias};
the organisation is always an argument to the function in question. Also
unlike \code{galah}, \code{galaxias_config()} enables you to store configuration
details for multiple organisations at once.
}
