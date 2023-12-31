% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render_roxygen.R
\name{.render_roxygen}
\alias{.render_roxygen}
\title{Render R documentation for a single file}
\usage{
.render_roxygen(file, outdir = tempdir())
}
\arguments{
\item{file}{file path to an R file documented with \pkg{roxygen2}}

\item{outdir}{existing directory to output files to}
}
\value{
path to a rendered html file containing hyperlinks to all
help topics
}
\description{
Credit for {roxygen2} parsing: Konrad Rudolph - https://stackoverflow.com/a/57990228/14525241
}
\keyword{internal}
