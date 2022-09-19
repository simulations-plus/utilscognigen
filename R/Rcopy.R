#' Copy and open R and Rmd files with QMS approved headers.
#'
#' This function is somewhat similar to the Cognigen shell command \code{Rcopy},
#' but it is not called.
#'
#' @param from path of original R program or Rmd file. Defaults to the path of
#'   the source editor context.
#'   
#' @param to path or directory of new file. If \code{to} is a directory,
#'   \code{from} is copied to that directory. Defaults to the basename of the
#'   \code{from} file.
#'   
#' @param version either \code{NULL} for the current R version, or a
#'   \code{character} in the form \code{"N.n.n"} or \code{"Nnn"}.
#' 
#' @param copyright_holder either \code{NULL} for the default Cognigen copyright
#'   statement, a single \code{character} defining the copyright holders and
#'   accompanying text to follow copyright mark and year, a \code{character}
#'   vector for multiple separate copyright statements, or \code{FALSE} for no
#'   copyright.
#'   
#' @param open \code{logical} indicating whether to open files in RStudio.
#' 
#' @param save \code{logical} indicating whether to save files prior to copy.
#' 
#' @return invisibly returns \code{NULL}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Copy an R program from some directory to the working directory with the same file name
#' Rcopy("../../d1pk/R/file.R")
#'
#' # Copy an R program from some directory to the working directory with a new name
#' Rcopy("../../d1pk/R/file.R", "new-file.R")
#'
#' # Copy the active R program to the working directory with the same file name
#' Rcopy()
#'
#' # Copy the active R program to the working directory with a new name
#' Rcopy(to = "new-file.R")
#'
#' # R Markdown works too
#' Rcopy("markdown-file.Rmd", "new-markdown-file.Rmd")
#'
#' }
#'
#' @seealso \code{\link{Rcopy_shared_code}} for simpler copying from the
#'   shared-code repository; \code{\link{Redit}} for creating R programs
Rcopy <- function(from = NULL, 
                  to = NULL, 
                  version = NULL, 
                  copyright_holder = NULL,
                  open = rstudioapi::isAvailable(), 
                  save = FALSE) {

  assertthat::assert_that(
    length(from) == 1 || is.null(from),
    length(to) == 1 || is.null(to),
    msg = "`from` and `to` must have length 1"
    )

  if (is.null(from)){
    # Rcopy is called from interactive session
    from <- get_source_file()
    # save current document from interactive session
    if (is.logical(save) && save[1] == TRUE) rstudioapi::documentSave()
  } 

  # to is the basename of from by default
  to <- if (is.null(to))
    basename(from)
  # if to is a directory, Rcopy from to that directory
  else if (dir.exists(to))
    file.path(to, basename(from))
  else
    to

  file_ext_from <- tolower(tools::file_ext(from))
  file_ext_to <- tolower(tools::file_ext(to))

  # Update extension to `to` if it was blank and normalize
  to <- ifelse(file_ext_to == "", paste0(to, ".", tools::file_ext(from)), to)
  to <- normalizePath(file.path(normalizePath(dirname(to), mustWork = FALSE), basename(to)), mustWork = FALSE)
  file_ext_to <- tolower(tools::file_ext(to))

  assertthat::assert_that(
    file_ext_from %in% c("r", "rmd"),
    msg = "`from` must be an R or Rmd file"
  )

  assertthat::assert_that(
    file_ext_to %in% c("r", "rmd"),
    !file.exists(to),
    msg = "`to` must be a non-existing R or Rmd file"
  )

  assertthat::assert_that(
    file_ext_from == file_ext_to,
    msg = "`from` and `to` must be the same file type"
  )

  from_original <- from

  assertthat::assert_that(
    file.exists(from),
    msg = "`from` must be an existing R or Rmd file"
  )
  
  old_header <- get_header(from)
  
  new_header <- if(isFALSE(old_header)) {
    build_new_header(path = to,
                     version = version,
                     copyright_holder = copyright_holder)
  } else {
    build_new_header(path = to,
                     version = version,
                     copyright_holder = copyright_holder,
                     purpose = FALSE,
                     input_files = FALSE,
                     output_files = FALSE)
  }

  # how files are copied depends on their type
  if(file_ext_to == "rmd") {

    if(isFALSE(old_header)) {

      # file_copy is expected to be used on Linux at Cognigen. since some files
      # that are copied are read-only, do not preserve mode and ownership
      file_copy(from, to, args = c("--preserve=timestamps", "--no-preserve=mode,ownership"))

      suppressMessages(Redit(to, open = FALSE))

      cli::cli_warn("{.arg from} did not have a valid header. It was copied with a new header to {.arg to}.")

    } else {
      
      lines <- readLines(from)
      header_start <- min(which(lines == old_header[[1]]))

      # add the new header to the beginning of the header
      new_lines <- c(
        lines[1:(header_start - 1)],
        new_header,
        lines[header_start:length(lines)]
      )
      
      writeLines(new_lines, to)

    }

    if(file.exists(to)) {
      cli::cli_alert_success("Created {.file {to}}")
    } else {
      cli::cli_abort("Failed to create {.file {to}}")
    }

  } else if(file_ext_to == "r") {

    writeLines(new_header, to)

    if(file.exists(to)) {
      cli::cli_alert_success("Created {.file {to}}")
    } else {
      cli::cli_abort("Failed to create {.file {to}}")
    }
    
    if(isFALSE(old_header)) {
      cli::cli_warn("{.arg from} did not have a valid header. It was copied with a new header to {.arg to}.")
    }

    # append `from` into `to`
    appended <- file.append(file1 = to, file2 = from)

    # Messages use `from_original` for clear representation
    if(appended) {
      cli::cli_alert_success("Appended {.file {to}} with {.file {from_original}}")
    } else {
      cli::cli_abort("Failed to append {.file {to}} with {.file {from_original}}")
    }

  }

  if(open && rstudioapi::isAvailable()) {
    file_open(to)
  }

  return(invisible(NULL))

}
