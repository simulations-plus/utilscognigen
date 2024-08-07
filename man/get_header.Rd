% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/headers.R
\name{get_header}
\alias{get_header}
\title{Extract a header from R code}
\usage{
get_header(x, min_hash = 30L, max_first_line = 1L)
}
\arguments{
\item{x}{a scanned R or Rmd file or a file path.}

\item{min_hash}{\code{numeric}; The minimum number of "#" that define the
beginning of the header. There is no maximum.}

\item{max_first_line}{\code{numeric}; The maximum number of non-blank lines
permitted before the beginning of the header in an R script. For Rmd files,
this is determined by considering the number of lines in the YAML.}
}
\value{
a \code{character} vector of lines of the header or \code{FALSE} with
  a reason attribute if no acceptable header is found.
}
\description{
Extract a header from R code
}
\keyword{internal}
