#' Create, update, and open R programs with QMS approved headers.
#'
#' @md
#'
#' @param ... file paths of R or Rmd files. Files without extensions are set to
#'   \code{".R"}. Defaults to the path of the source editor context.
#'
#' @param version either \code{NULL} for the current R version, or a
#'   \code{character} in the form \code{"N.n.n"} or \code{"Nnn"}. Ignored with a
#'   warning for files that already have a header.
#'
#' @param purpose,input_files,output_files purpose, input files, and output
#'   files of R program(s) given as \code{character} vectors. The purpose will
#'   be wrapped to fit in the header. When \code{NULL} (the default), a blank
#'   section is included. When \code{FALSE}, no section is included. Ignored
#'   with a warning for files that already have a header.
#'
#' @param backup \code{logical} indicating whether to create backup files
#'
#' @param open \code{logical} indicating whether to open files in RStudio
#'
#' @return invisibly returns \code{NULL}
#'
#' @details
#' Cognigen's R program header documents key elements of programs including:
#' * Name: The full path to the program on Cognigen's file system.
#' * History: The timestamp and programmer of key updates to the program.
#' * Copyright: Cognigen's standard Copyright language.
#' * Purpose: What the program is designed to do.
#' * Input Files: Files read by the program.
#' * Output Files: Files written by the program.
#'
#' `Redit` serves a few distinct purposes:
#' * Create new R and Rmd files with standard headers
#' * Add headers to existing R and Rmd files without a header
#' * Add new timestamps to headers in existing R and Rmd files
#'
#' R programs do not have to be `Redit`ed every time they are updated, and
#' `Redit` should not be used for files that are simply being opened.
#'
#' If a standard header cannot be identified in an existing file, a new header
#' will be added if possible. In some cases, an error will occur with a
#' suggestion to update the program: For R files, update the first line so it is
#' an R command; For Rmd files, delete or rename any chunks named "header".
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create two new R programs
#' Redit("file1.R", "file2.R")
#'
#' # Update these programs and create a backup
#' Redit("file1.R", "file2.R", backup = TRUE)
#'
#' # Update the history of the open R program if it has a valid header
#' # If there is no valid header, either a header will be added or the error
#' # will try to help resolve the issue
#' Redit()
#'
#' # R Markdown works too
#' Redit("new-markdown.Rmd")
#'
#' # Header items can be set programmatically, which is particularly useful when
#' # several programs are required
#' Redit(
#'   "new-file.R",
#'   purpose = "experiment with headers",
#'   input_files = c("../data/file1.csv", "..data/file1.xpt"),
#'   output_files = FALSE
#' )
#' }
#'
#' @seealso \code{\link{Rcopy}} for copying R programs, \code{\link{get_header_content}}
#' for getting content from headers
Redit <- function(...,
                  version = NULL,
                  purpose = NULL,
                  input_files = NULL,
                  output_files = NULL,
                  backup = FALSE,
                  open = rstudioapi::isAvailable()) {

  paths <- unlist(list(...))
  paths <- if(length(paths) == 0) get_source_file() else paths

  invisible(lapply(paths, function(path) {
    .Redit(
      path = path,
      version = version,
      purpose = purpose,
      input_files = input_files,
      output_files = output_files,
      backup = backup,
      open = open
    )
  }))

}

#' Create, update, and open an R program.
#'
#' @inheritParams Redit
#'
#' @return invisibly returns \code{NULL}
#'
#' @keywords internal
.Redit <- function(path = NULL,
                   version = NULL,
                   purpose = NULL,
                   input_files = NULL,
                   output_files = NULL,
                   backup = FALSE,
                   open = rstudioapi::isAvailable()) {

  assertthat::assert_that(
    length(path) == 1
  )

  call_file_refresh <- is.null(path) && open && rstudioapi::isAvailable()

  if(is.null(path)) {
    stop(
      "An R or Rmd file must be open or a path must be specified to use `.Redit()`.",
      call. = FALSE
    )
  }

  path <- normalizePath(file.path(normalizePath(dirname(path), mustWork = FALSE), basename(path)), mustWork = FALSE)

  file_ext <- tools::file_ext(path)

  if(!(tolower(file_ext) %in% c("r", "rmd", ""))) {
    try(stop("'", file_ext, "' is not a valid file extension", call. = FALSE))
  } else {

    # Update files without extension to .R
    path <- ifelse(file_ext == "", paste0(path, ".R"), path)

    file_existed <- file.exists(path)

    tried <- try(make_header(path = path,
                             version = version,
                             purpose = purpose,
                             input_files = input_files,
                             output_files = output_files,
                             backup = backup))

    if(!inherits(tried, "try-error")) {
      message(ifelse(file_existed, "Updated '", "Created '"), path, "'")

      if(call_file_refresh) {
        file_refresh()
      } else if(open && rstudioapi::isAvailable()) {
        file_open(path)
      }
    }

  }

  return(invisible(NULL))

}


# Helpers -----------------------------------------------------------------

.days_of_week <- paste0(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), collapse = "|")

.split_length <- 79

.clean_copyright <- paste0(
  strwrap(
    paste0(
      "Copyright ",
      format(Sys.Date(), format = "%Y"),
      ", Cognigen Corporation. The contents of this program are confidential and ",
      "cannot be used - in any form - for anything outside the drug and specific ",
      "project for which the file is provided."),
    width = .split_length,
    prefix = "# "),
  collapse = "\n")

.section_break <- paste0(rep("#", .split_length), collapse = "")

#' Create a file with a header or append a modification history entry to an
#' existing file with a header
#'
#' @inheritParams Redit
#'
#' @return invisibly returns \code{NULL}
#'
#' @keywords internal
make_header <- function(path = NULL,
                        version = NULL,
                        purpose = NULL,
                        input_files = NULL,
                        output_files = NULL,
                        backup = FALSE) {

  path <- if(is.null(path)) get_source_file() else path
  if(is.null(path)) {
    stop(
      "An R or Rmd file must be open or a path must be specified to use `make_header()`.",
      call. = FALSE
    )
  }

  file_ext <- tolower(tools::file_ext(path))

  if(file.exists(path)) {

    if(backup) {
      file_copy(path, paste0(path, ".backup"))
    }

    old_header <- get_header(path)

    # Potential warnings upon exit
    on.exit(expr = {
      if(!is.null(version) && !isFALSE(old_header))
        warning("`version` is ignored since header in '", path, "' already exists.")
      if(!is.null(purpose) && !isFALSE(old_header))
        warning("`purpose` is ignored since header in '", path, "' already exists.")
      if(!is.null(input_files) && !isFALSE(old_header))
        warning("`input_files` is ignored since header in '", path, "' already exists.")
      if(!is.null(output_files) && !isFALSE(old_header))
        warning("`output_files` is ignored since header in '", path, "' already exists.")
    },
    add = TRUE)

    # old_header returns FALSE if there is no header
    # For files with a valid header, just add a new timestamp/user
    if(!isFALSE(old_header)) {

      # Insert new date_user_line above current top date/user line

      old_date_user_line_i <- grep(paste0("# (", .days_of_week, ")"), old_header)

      if(length(old_date_user_line_i) == 0) {
        stop(
          "Header is missing creation/modification history in existing file: '", path, "'",
          "\n", .try_this_on_header_errors,
          call. = FALSE
        )
      }

      # Replace multiple spaces with a single space in date user lines
      # This is required to match the behavior of scan
      xfun::gsub_file(path, pattern = paste0("(", format(Sys.Date(), format = "%Y"), ")", "\\s+"), replacement = "\\1 ")

      old_date_user_line <- old_header[old_date_user_line_i[1]]
      old_date_user_line_replacement <- paste0(make_date_user_line(), "\n", old_date_user_line)
      xfun::gsub_file(path, pattern = old_date_user_line, replacement = old_date_user_line_replacement, fixed = TRUE)

      # Early return
      return(invisible(NULL))

    } else if(isFALSE(old_header)) {

      # Rmd special cases
      if(file_ext == "rmd") {
        code <- attr(old_header, "code")
        code <- if(is.null(code)) "" else code

        # Issues that can only be resolved by the user
        if(code == "multiple_chunks_named_header") {
          stop(attr(old_header, "reason"), call. = FALSE)
        } else if(code == "no_end_of_header") {
          stop(attr(old_header, "reason"), call. = FALSE)
        }

        # We can add a header chunk or update an empty header chunk

        header <- build_new_header(path = path,
                                   version = version,
                                   purpose = purpose,
                                   input_files = input_files,
                                   output_files = output_files)

        if(code == "no_chunk_named_header") {

          lines <- readLines(path)
          yamls <- grep("^---", lines)
          if(length(yamls) < 2) {
            stop("No YAML found in Rmd file and there is no header chunk.")
          }
          yaml_end <- yamls[[2]]

          new_header_chunk <- paste(
            '```{r header, include=FALSE}',
            header,
            '```',
            sep = "\n"
          )

          # append the new header chunk after the end of the YAML
          new_lines <- c(
            lines[1:yaml_end],
            "",
            new_header_chunk,
            "",
            lines[(yaml_end + 1):length(lines)]
          )

          writeLines(new_lines, path)

          # Early return
          return(invisible(NULL))

        } else if (code == "empty_header_chunk") {

          lines <- readLines(path)
          header_chunk_start <- min(grep("^```\\{r header[,\\s\\}]", lines))
          chunk_ends <- grep("^```$", trimws(lines))
          header_chunk_end <- min(chunk_ends[chunk_ends > header_chunk_start])

          new_header_chunk <- paste(
            # intentionally does not include the named header line, since the
            # existing file could include intentional settings
            header,
            '```',
            sep = "\n"
          )

          # replace the empty chunk with the new header
          new_lines <- c(
            lines[1:header_chunk_start],
            new_header_chunk,
            "",
            lines[(header_chunk_end + 1):length(lines)]
          )

          writeLines(new_lines, path)

          # Early return
          return(invisible(NULL))

        } else if(code == "no_header_in_header_chunk") {

          lines <- readLines(path)
          header_chunk_start <- min(grep("^```\\{r header[,\\s\\}]", lines))

          # add the header to the beginning of the empty chunk
          new_lines <- c(
            lines[1:header_chunk_start],
            header,
            "",
            lines[(header_chunk_start + 1):length(lines)]
          )

          writeLines(new_lines, path)

          # Early return
          return(invisible(NULL))

        }

        stop(
          attr(old_header, "reason"),
          call. = FALSE
        )

      } else if(file_ext == "r") {

        # Copy `path` as `tmp`
        tmp <- file.path(tempdir(), basename(path))
        file_copy(from = path, to = tmp)

        # Remove `path`
        file.remove(path)

        # Recreate `path`
        suppressMessages(Redit(path))

        # Append `tmp` into `path`
        file.append(file1 = path, file2 = tmp)

        message("Added header to '", path, "'")

        # Early return
        return(invisible(NULL))

      }

    }

  }


  # File does not already exist

  header <- build_new_header(path = path,
                             version = version,
                             purpose = purpose,
                             input_files = input_files,
                             output_files = output_files)

  # Write the header to R or Rmd
  if(file_ext == "r") {
    writeLines(header, path)
  } else if(file_ext == "rmd") {
    # TODO what should a new Rmd look like? I know what I like
    rmd_content <- paste(
      '---',
      'title: ""',
      paste0('author: "', get_user_full_name(), '"'),
      'date: "`r Sys.Date()`"',
      'output:',
      '  html_document:',
      '    theme: spacelab',
      '    toc: true',
      '---',
      '',
      '```{r header, include=FALSE}',
      header,
      '```',
      sep = "\n"
    )

    writeLines(rmd_content, path)
  }

  return(invisible(NULL))

}


build_new_header <- function(path = NULL,
                             version = NULL,
                             purpose = NULL,
                             input_files = NULL,
                             output_files = NULL) {

  path <- if(is.null(path)) get_source_file() else path
  path <- normalizePath(file.path(normalizePath(dirname(path), mustWork = FALSE), basename(path)), mustWork = FALSE)

  blank <- "# \n"
  name_line <- paste0("# Name: ", path)
  version <- clean_version(version)
  r_version_line <- paste0("# Written for use with ", gsub("\\s\\(.*", "", version))

  header <- paste0(
    .section_break, "\n",
    name_line, "\n",
    blank,
    make_date_user_line(), "\n",
    blank,
    r_version_line, "\n",
    blank,
    .clean_copyright, "\n",
    .section_break
  )

  # Add purpose, input_files, and output_files as long as they are not all FALSE
  if(!all(isFALSE(purpose), isFALSE(input_files), isFALSE(output_files))) {

    purpose <- make_purpose(purpose)
    input_files <- make_header_files(input_files, type = "input")
    output_files <- make_header_files(output_files, type = "output")

    header <- paste0(
      header, "\n",
      blank,
      purpose,
      ifelse(is.null(purpose), "", "\n"),
      input_files, ifelse(is.null(input_files), "", "\n"),
      output_files, ifelse(is.null(output_files), "", "\n"),
      .section_break
    )

  }

  return(header)

}


#' Clean a supported R version to its \code{R.version.string} representation
#'
#' @inheritParams Redit
#'
#' @return \code{character} in the form of \code{R.version.string}
#' @keywords internal
clean_version <- function(version) {
  if(is.null(version)) {
    return(R.version.string)
  } else if(version == R.version.string) {
    return(version)
  }

  clean <- gsub("\\.|\\s", "", version)
  if(!grepl("^\\d{3}$", clean)) {
    stop("`version` should be `NULL` or an R version number in the form 'N.n.n' or 'Nnn'.")
  }

  clean <- switch(clean,
                  "343" = "R version 3.4.3 (2017-11-30)",
                  "351" = "R version 3.5.1 (2018-07-02)",
                  "361" = "R version 3.6.1 (2019-07-05)")

  if(is.null(clean)) {
    stop("R version ", version, " is not supported")
  }

  return(clean)

}



#' Get the full name of a \code{user}
#'
#' @param user \code{character} user. Defaults to the system environment
#'   variable \code{USER}
#'
#' @return \code{character} full name of the \code{user}
#' @keywords internal
get_user_full_name <- function(user = Sys.getenv("USER")) {
  assertthat::assert_that(
    .Platform$OS.type == "unix",
    user != "",
    !is.null(user)
    )

  user_account <- suppressWarnings(system2("getent",
                                           c("passwd", user),
                                           stdout = TRUE))

  if(length(user_account) == 0) {
    stop("user not found: '", user, "'")
  }

  # Fifth field is full name
  full_name <- unlist(lapply(strsplit(user_account, ":"), `[[`, 5))
  return(full_name)

}


#' Make the header line containing the date and user
#'
#' @return \code{character}
#' @keywords internal
make_date_user_line <- function() {
  return(paste0(
    "# ",
    format(Sys.time(), format = "%a %b %d %H:%M:%S %Z %Y", tz = "America/New_York"), " ",
    get_user_full_name(),
    " (", Sys.getenv("USER"), ")"
  ))
}


#' Make the header lines containing the purpose
#'
#' @inheritParams Redit
#' @param split_length \code{integer} representing how many characters to split
#'   at
#'
#' @return \code{character}
#' @keywords internal
make_purpose <- function(purpose = NULL, split_length = .split_length) {

  if(is.null(purpose)) {
    return("# PURPOSE: \n# \n# \n# ")
  } else if(isFALSE(purpose)) {
    return(NULL)
  }

  # Otherwise, content of purpose is provided
  purpose <- paste0(purpose, collapse = "\n")
  purpose <- unlist(strsplit(purpose, "\n"))
  purpose <- strwrap(purpose, width = split_length, prefix = "# ")
  purpose <- paste0(purpose, collapse = "\n")
  purpose <- paste0("# PURPOSE: \n", purpose, "\n# ")

  return(purpose)

}


#' Make the header lines containing the input or output files
#'
#' @param files input or output files
#' @param type either \code{"input"} or \code{"output"}
#'
#' @return \code{character}
#' @keywords internal
make_header_files <- function(files, type = c("input", "output")) {

  assertthat::assert_that(
    type %in% c("input", "output")
  )

  section_title <- ifelse(type == "input", "INPUT FILES", "OUTPUT FILES")

  if(is.null(files)) {
    return(paste0("# ", section_title, ": \n# \n# \n# "))
  } else if(isFALSE(files)) {
    return(NULL)
  }

  # Otherwise, content of header files is provided
  files <- paste0("# ", files, collapse = "\n")
  files <- paste0("# ", section_title, ": \n", files, "\n# ")
  return(files)

}
