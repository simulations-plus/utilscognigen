#' Copy and open R and Rmd files with QMS approved headers.
#'
#' This function is somewhat similar to the Cognigen system command
#' \code{Rcopy}, but it is not called.
#'
#' @param from path or GitLab web URL of original R program or Rmd file.
#'   Defaults to the path of the source editor context.
#' @param to path or directory of new file. If `to` is a directory, `from` is
#'   copied to that directory. Defaults to the basename of the `from` file.
#' @param version either \code{NULL} for the current R version, or a
#'   \code{character} in the form \code{"N.n.n"} or \code{"Nnn"}.
#' @param open \code{logical} indicating whether to open files in RStudio
#'
#' @return invisibly returns \code{NULL}
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
#' # Copy an R program from GitLab to an "R/includes" directory
#' Rcopy(
#'   "https://gitlab.cognigencorp.com/r/shared-code/-/blob/master/functions/sstat.R",
#'   "../R/includes"
#' )
#' }
#'
#' @seealso \code{?interactivecog::Rcopy_shared_code} for simpler copying from
#'   the shared-code repository; \code{\link{Redit}} for creating R programs
Rcopy <- function(from = NULL, to = NULL, version = NULL, open = rstudioapi::isAvailable()) {

  assertthat::assert_that(
    length(from) == 1 || is.null(from),
    length(to) == 1 || is.null(to),
    msg = "`from` and `to` must have length 1"
    )

  from <- if(is.null(from)) get_source_file() else from

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

  # Special case where `from` is a GitLab url.
  # Download from GitLab to tempdir
  # Once downloaded, set `from` to the file in tempdir

  from_is_gitlab_url <-
    grepl("gitlab\\.", from, ignore.case = TRUE) &&
    grepl("\\.com", from, ignore.case = TRUE) &&
    !file.exists(from)

  if(from_is_gitlab_url) {

    from_temp <- file.path(tempdir(), basename(from))

    # Download to tempdir; download_gitlab returns a logical
    download_gitlab_result <- download_gitlab(url = from,
                                              destfile = from_temp)
    assertthat::assert_that(
      download_gitlab_result,
      msg = "`from` could not be downloaded from GitLab"
    )

    from <- from_temp

  }

  assertthat::assert_that(
    file.exists(from),
    msg = "`from` must be an existing R or Rmd file"
  )

  new_header <- build_new_header(path = to,
                                 version = version,
                                 purpose = FALSE,
                                 input_files = FALSE,
                                 output_files = FALSE)

  # how files are copied depends on their type
  if(file_ext_to == "rmd") {

    old_header <- get_header(from)

    if(isFALSE(old_header)) {

      file.copy(from, to)

      suppressMessages(Redit(to))

      warning(
        "`from` did not have a valid header. It was copied with a new header to `to`.",
        call. = FALSE
      )

    } else {

      lines <- readLines(from)
      header_chunk_start <- min(grep("^```\\{r header[,\\s\\}]", lines))

      # add the new header to the beginning of the header chunk
      new_lines <- c(
        lines[1:header_chunk_start],
        new_header,
        lines[(header_chunk_start + 1):length(lines)]
      )

      writeLines(new_lines, to)

    }

    if(file.exists(to)) {
      message("Created '", to, "'")
    } else {
      stop("Failed to create '", to, "'")
    }

  } else if(file_ext_to == "r") {

    writeLines(new_header, to)

    if(file.exists(to)) {
      message("Created '", to, "'")
    } else {
      stop("Failed to create '", to, "'")
    }

    # append `from` into `to`
    appended <- file.append(file1 = to, file2 = from)

    # Messages use `from_original` for clear representation
    if(appended) {
      message("Appended '", to, "' with '", from_original, "'")
    } else {
      stop("Failed to append '", to, "' with '", from_original, "'")
    }

  }

  if(open && rstudioapi::isAvailable()) {
    file_open(to)
  }

  return(invisible(NULL))

}