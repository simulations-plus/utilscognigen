% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/record_io.R
\name{recorded_io}
\alias{recorded_io}
\alias{record_input}
\alias{record_output}
\alias{clear_recorded_io}
\alias{get_recorded_io}
\title{Record and report input and output files used and created in R scripts}
\usage{
record_input(call, quiet = FALSE, invisible = FALSE)

record_output(call, quiet = FALSE)

recorded_io()

clear_recorded_io()

get_recorded_io(path = NULL)
}
\arguments{
\item{call}{a function call to read a data file or create a new file;
alternatively, \code{call} can be one or more file paths.}

\item{quiet}{a logical value indicating whether messages should be printed
(\code{FALSE}; default) or not (\code{TRUE}).}

\item{invisible}{a logical value indicating whether the function should
return a value (\code{FALSE}; default) or not (\code{TRUE}). This is mostly
useful with \code{source} calls}

\item{path}{a file path to an R, Rmd, qmd, or log file. Defaults to the path of
the source editor context.}
}
\value{
\code{record_input()} returns the output of the evaluated \code{call}.

\code{record_output()} invisibly returns \code{NULL} after evaluating
\code{call} which is expected to create one or more files as a side-effect of
the call.

\code{recorded_io()} and \code{clear_recorded_io()} both invisibly return
\code{NULL}.

\code{get_recorded_io()} returns a list with two character vector elements:
input_files and output_files.
}
\description{
Record and report input and output files used and created in R scripts
}
\details{
\subsection{Recording input and output files}{

Calling either \code{record_input()} or \code{record_output()} will cause
information about file paths to be stored in a dedicated environment. If
\code{call} is a function call, both functions expect attempts to detect file
names and paths based upon known arguments of common functions that read or
creates files; if the argument of the \code{call} are not recognized, both
\code{record_input()} and \code{record_output()} will assume that the 1st
argument \code{call} provided the path of the file to be recorded.

By default, a message will be printed by both \code{record_input()} and
\code{record_output()} to report the path of the files that were stored.
}

\subsection{Reporting input and output files}{

Ultimately, the collected information can be summarized by calling
\code{recorded_io()} typically at the end of an R script. In an .Rmd file,
\code{recorded_io()} would need to be called inside a chunk set with the
\code{message = FALSE} option so the information could be printed to console
or the .Rout file when .Rmd file is rendered using
\code{\link[utilscognigen]{render}}.

In some cases (eg, in an interactive R session), it could be useful to call
\code{clear_recorded_io()} to erase all the recorded input and output file
paths.

Retrieve collected input and output files from the log of an executed R or
Rmd file with \code{get_recorded_io()}.
}
}
\examples{

\dontrun{
df <- record_input(
  read.csv(
    file = '/path/to/existing/file.csv',
    header = TRUE
  )
)
df <- record_input(
  haven::read_sas('/path/to/existing/file.sas7bdat')
)
record_input('../../some/adhoc/file.RDS')

# Information will not be recorded if the call fails
df <- record_input(
  read.csv(
    file = '/path/to/invalid/file.csv',
    header = TRUE
  )
)

record_output(
  png('Rplots_\%03d.png')
)
plot(1) ; plot(2)
dev.off()

recorded_io()
}

}
