% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_standards.R
\name{check_standards}
\alias{check_standards}
\title{Check an R file against CPP standards}
\usage{
check_standards(
  path = NULL,
  standards = c("check_rout", "check_header", "check_session_info")
)
}
\arguments{
\item{path}{a file path to an R or R Markdown file. Defaults to the path of
the source editor context.}

\item{standards}{either a \code{character} vector of the standards provided
and described with this function, or a list that can include the provided
standard names as well as custom functions. Custom functions should accept
arguments \code{x} (a scanned R file) and \code{path} (a file path). Custom
functions should return a named list consisting of a \code{logical} element
named "pass" and a \code{character} element named "reason" that describes
the reason the standard is not satisfied.}
}
\value{
\code{logical} indicating whether the file satisfied all standards.
}
\description{
Performs each of the checks described in Details. All checks must pass for
\code{check_standards} to return \code{TRUE}.
}
\details{
\describe{
  \item{\code{\link{check_rout}}}{Determines if the R script should have an
  associated Rout file, then checks the content of the Rout.}
  \item{\code{\link{check_header}}}{Checks that the R script has an
  appropriate header.}
  \item{\code{\link{check_session_info}}}{Checks that
  \code{\link[utils]{sessionInfo}} or
  \code{\link[devtools]{session_info}} is called in the R script, preferably
  as the final call.}
}
}
\examples{
\dontrun{
check_standards("script.R")
}
}
