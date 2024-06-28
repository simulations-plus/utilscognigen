#' Create, update, and open R programs with QMS approved headers
#'
#' @md
#'
#' @param ... file paths of R, Rmd, or qmd files. Files without extensions are
#'   set to \code{".R"}. Defaults to the path of the source editor context.
#'
#' @param version either \code{NULL} for the current R version, or a
#'   \code{character} in the form \code{"N.n.n"} or \code{"Nnn"}. Ignored with a
#'   warning for files that already have a header.
#' 
#' @param copyright_holder either \code{NULL} for the default Cognigen copyright
#'   statement, a single \code{character} defining the copyright holders and
#'   accompanying text to follow copyright mark and year, a \code{character}
#'   vector for multiple separate copyright statements, or \code{FALSE} for no
#'   copyright.
#'
#' @param purpose,input_files,output_files purpose, input files, and output
#'   files of R program(s) given as \code{character} vectors. The purpose will
#'   be wrapped to fit in the header. When \code{NULL} (the default), a blank
#'   section is included. When \code{FALSE}, no section is included. Ignored
#'   with a warning for files that already have a header.
#'
#' @param backup \code{logical} indicating whether to create backup files.
#'
#' @param open \code{logical} indicating whether to open files in RStudio.
#'
#' @return invisibly returns \code{NULL}.
#'
#' @details
#' Cognigen's R program header documents key elements of programs including:
#' * Name: The full path to the program on Cognigen's file system.
#' * Timestamp: The timestamp and programmer of key updates to the program.
#' * Copyright: Cognigen's standard Copyright language.
#' * Purpose: What the program is designed to do.
#' * Input Files: Files read by the program.
#' * Output Files: Files written by the program.
#'
#' `Redit` serves a few distinct purposes:
#' * Create new R, Rmd, and qmd files with standard headers.
#' * Add headers to existing files without a header.
#' * Add new timestamps and program owner name to headers in existing files.
#'
#' `Redit` does not need to be used every time a program is updated and `Redit`
#' should not be used for files that are simply being opened.
#'
#' If a standard header cannot be identified in an existing file, a new header
#' will be added if possible. In some cases, an error will occur with a
#' suggestion to update the program: For R files, update the first line so it is
#' an R command; For Rmd files, temporarily remove all comments from the YAML.
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
#' # Update the timestamp of the open R program if it has a valid header.
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
#' for getting content from headers.
Redit <- function(...,
                  version = NULL,
                  copyright_holder = NULL,
                  purpose = NULL,
                  input_files = NULL,
                  output_files = NULL,
                  backup = FALSE,
                  open = rstudioapi::isAvailable()) {

  paths <- unlist(list(...))
  if (length(paths) == 0){
    # Redit is called from interactive session
    paths <- get_source_file()
    # save current document from interactive session
    rstudioapi::documentSave()
  }
  
  # Fail if any paths have spaces (because callr::rcmd with fail, and it is bad practice)
  paths_with_spaces <- grep("\\s", paths, value = TRUE)
  if(length(paths_with_spaces) > 0) {
    cli::cli_abort(
      "Detected files with spaces. Files should not contain spaces: {.path {paths_with_spaces}}"
    )
  }
  
  invisible(lapply(paths, function(path) {
    .Redit(
      path = path,
      version = version,
      copyright_holder = copyright_holder,
      purpose = purpose,
      input_files = input_files,
      output_files = output_files,
      backup = backup,
      open = open
    )
  }))

}

#' Create, update, and open an R program
#'
#' @inheritParams Redit
#'
#' @return invisibly returns \code{NULL}.
#'
#' @keywords internal
.Redit <- function(path = NULL,
                   version = NULL,
                   copyright_holder = NULL,
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
    cli::cli_abort("An R, Rmd, or qmd file must be open or a path must be specified to use {.fn .Redit}.")
  }

  path <- normalizePath(file.path(normalizePath(dirname(path), mustWork = FALSE), basename(path)), mustWork = FALSE)

  file_ext <- tools::file_ext(path)

  if(!(tolower(file_ext) %in% c("r", "rmd", "qmd", ""))) {
    try(cli::cli_abort("'{file_ext}' is not a valid file extension"))
  } else {

    # Update files without extension to .R
    path <- ifelse(file_ext == "", paste0(path, ".R"), path)

    file_existed <- file.exists(path)

    tried <- try(make_header(path = path,
                             version = version,
                             copyright_holder = copyright_holder,
                             purpose = purpose,
                             input_files = input_files,
                             output_files = output_files,
                             backup = backup))

    if(!inherits(tried, "try-error")) {
      updated_created <- ifelse(file_existed, "Updated", "Created")
      cli::cli_alert_success("{updated_created} {.file {path}}")

      if(call_file_refresh) {
        file_refresh()
      } else if(open && rstudioapi::isAvailable()) {
        file_open(path)
      }
    }

  }

  invisible(NULL)

}


# Helpers -----------------------------------------------------------------

.days_of_week <- paste0(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), collapse = "|")

.split_length <- 79

.section_break <- paste0(rep("#", .split_length), collapse = "")

.cognigen_copyright_message <- "Cognigen Corporation. The contents of this program are confidential and cannot be used - in any form - for anything outside the drug and specific project for which the file is provided."

#' Create a file with a header or append a timestamp entry to an existing file
#' with a header
#'
#' @inheritParams Redit
#'
#' @return invisibly returns \code{NULL}.
#'
#' @keywords internal
make_header <- function(path = NULL,
                        version = NULL,
                        copyright_holder = NULL,
                        purpose = NULL,
                        input_files = NULL,
                        output_files = NULL,
                        backup = FALSE) {

  path <- if(is.null(path)) get_source_file() else path
  if(is.null(path)) {
    cli::cli_abort("An R, Rmd, or qmd file must be open or a path must be specified to use {.fn make_header}.")
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
        cli::cli_warn("{.arg version} is ignored since header in {.file {path}} already exists.")
      if(!is.null(purpose) && !isFALSE(old_header))
        cli::cli_warn("{.arg purpose} is ignored since header in {.file {path}} already exists.")
      if(!is.null(input_files) && !isFALSE(old_header))
        cli::cli_warn("{.arg input_files} is ignored since header in {.file {path}} already exists.")
      if(!is.null(output_files) && !isFALSE(old_header))
        cli::cli_warn("{.arg output_files} is ignored since header in {.file {path}} already exists.")
    },
    add = TRUE)

    # old_header will be FALSE if there is no header
    # For files with a valid header, just add a new timestamp/user
    if(!isFALSE(old_header)) {

      # Insert new date_user_line above current top date/user line

      old_date_user_line_i <- grep(paste0("# (", .days_of_week, ")"), old_header)

      if(length(old_date_user_line_i) == 0) {
        cli::cli_abort(
          c(
            "Header is missing creation/modification timestamp in existing file: {.file {path}}",
            i = "{.try_this_on_header_errors[file_ext]}"
          )
        )
      }

      # is expected to only update the most recent timestamp
      old_date_user_line <- old_header[old_date_user_line_i[1]]
      old_date_user_line_replacement <- paste0(make_date_user_line(), "\n", old_date_user_line)
      gsub_file_first(path, pattern = old_date_user_line, replacement = old_date_user_line_replacement, fixed = TRUE)

      # Early return
      return(invisible(NULL))

    } else if(isFALSE(old_header)) {
      
      reason <- attr(old_header, "reason")
      reason <- if(is.null(reason)) "" else reason

      # Rmd special cases
      if(file_ext %in% c("rmd", "qmd")) {

        header <- build_new_header(path = path,
                                   version = version,
                                   copyright_holder = copyright_holder,
                                   purpose = purpose,
                                   input_files = input_files,
                                   output_files = output_files)
        
        # insert header at bottom of YAML if no header is found in the YAML (or
        # at all)
        if(reason %in% c("No header found", "No header found in YAML of Rmd/qmd file")) {
          
          lines <- readLines(path, warn = FALSE)
          
          yamls <- grep("^---", lines)
          if(length(yamls) < 2) {
            cli::cli_abort("No YAML found in Rmd/qmd file.")
          }
          yaml_start <- yamls[[1]]
          yaml_end <- yamls[[2]]
          
          lines_with_header <- c(
            lines[1:(yaml_end - 1)],
            header,
            lines[yaml_end:length(lines)]
          )
          
          writeLines(lines_with_header, path)
          
          cli::cli_alert_success("Added header to {.file {path}}")
          
          # early return
          return(invisible(NULL))
        }

        cli::cli_abort(attr(old_header, "reason"))

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

        cli::cli_alert_success("Added header to {.file {path}}")

        # Early return
        return(invisible(NULL))

      }

    }

  }


  # File does not already exist

  header <- build_new_header(path = path,
                             version = version,
                             copyright_holder = copyright_holder,
                             purpose = purpose,
                             input_files = input_files,
                             output_files = output_files)

  # Write the header to the file
  if(file_ext == "r") {
    writeLines(header, path)
  } else if(file_ext == "rmd") {
    # TODO what should a new Rmd look like? I know what I like
    rmd_content <- paste(
      '---',
      paste0('title: "', tools::file_path_sans_ext(basename(path)), '"'),
      paste0('author: "', get_user_full_name(), '"'),
      'date: "`r Sys.Date()`"',
      'output:',
      '  html_document:',
      '    theme: spacelab',
      '    toc: true',
      header,
      '---',
      sep = "\n"
    )

    writeLines(rmd_content, path)
  } else if(file_ext == "qmd") {
    qmd_content <- paste(
      '---',
      paste0('title: "', tools::file_path_sans_ext(basename(path)), '"'),
      paste0('author: "', get_user_full_name(), '"'),
      'date: today',
      'format:',
      '  html:',
      '    theme: cosmo',
      '    toc: true',
      header,
      '---',
      sep = "\n"
    )
    
    writeLines(qmd_content, path)
  }

  invisible(NULL)

}


build_new_header <- function(path = NULL,
                             version = NULL,
                             copyright_holder = NULL,
                             purpose = NULL,
                             input_files = NULL,
                             output_files = NULL) {

  path <- if(is.null(path)) get_source_file() else path
  path <- ifelse(
    path == "",
    "",
    normalizePath(file.path(normalizePath(dirname(path), mustWork = FALSE), basename(path)), mustWork = FALSE)
  )

  blank <- "# \n"
  name_line <- paste0("# Name: ", path)
  version <- clean_version(version)
  r_version_line <- paste0("# Written for use with ", gsub("\\s\\(.*", "", version))
  copyright_line <- make_copyright_line(copyright_holder)
  
  # since either the r version line or the copyright line will come prior to a
  # section break, add a break after the version line if there is a copyright
  if(!is.null(copyright_line)) {
    r_version_line <- paste0(r_version_line, "\n", blank)
  }

  header <- paste0(
    .section_break, "\n",
    name_line, "\n",
    blank,
    make_date_user_line(), "\n",
    blank,
    r_version_line, "\n",
    copyright_line, "\n",
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
  
  # remove double new-lines
  header <- gsub("\\n\\n+", "\n", header)

  header

}


#' Clean a supported R version to its \code{R.version.string} representation
#'
#' @inheritParams Redit
#'
#' @return \code{character} in the form of \code{R.version.string} or \code{NULL}.
#' @keywords internal
clean_version <- function(version) {
  if(is.null(version)) {
    return(R.version.string)
  } else if(version == R.version.string) {
    return(version)
  }

  clean <- gsub("\\.|\\s", "", version)
  if(!grepl("^\\d{3}$", clean)) {
    cli::cli_abort("{.arg version} should be {.code NULL} or an R version number in the form 'N.n.n' or 'Nnn'.")
  }

  clean <- switch(clean,
                  "343" = "R version 3.4.3 (2017-11-30)",
                  "351" = "R version 3.5.1 (2018-07-02)",
                  "361" = "R version 3.6.1 (2019-07-05)",
                  "413" = "R version 4.1.3 (2022-03-10)")

  if(is.null(clean)) {
    cli::cli_abort("R version {.version {version}} is not supported")
  }

  clean

}



#' Get the full name of a \code{user}
#'
#' @param user \code{character} user. Defaults to the system environment
#'   variable \code{USER}.
#'
#' @return \code{character} full name of the \code{user} or \code{NULL}.
#' @keywords internal
get_user_full_name <- function(user = Sys.getenv("USER")) {
  
  # return "testing" if testing package
  if(isTRUE(testthat::is_testing())) {
    return(invisible("testing"))
  }
  
  assertthat::assert_that(
    .Platform$OS.type == "unix",
    user != "",
    !is.null(user)
    )

  user_account <- suppressWarnings(system2("getent",
                                           c("passwd", user),
                                           stdout = TRUE))

  if(length(user_account) == 0) {
    cli::cli_warn("user not found: '{user}'")
    return(NULL)
  }

  # Fifth field is full name
  full_name <- unlist(lapply(strsplit(user_account, ":"), `[[`, 5))
  
  full_name

}


#' Make the header line containing the date and user
#'
#' @return \code{character} or \code{NULL}
#' @keywords internal
make_date_user_line <- function() {
  
  full_name <- get_user_full_name()
  user_name <- Sys.getenv("USER")
  
  full_name_user_name <- if(full_name == user_name) {
    user_name
  } else if(full_name == "" & user_name == "") {
    ""
  } else if(is.null(full_name)) {
    user_name
  } else if(full_name == "") {
    user_name
  } else if(user_name == "") {
    full_name
  } else {
    glue::glue("{full_name} ({user_name})")
  }
  
  date_user_line <- paste0(
    "# ",
    format(Sys.time(), format = "%a %b %d %H:%M:%S %Z %Y", tz = "America/New_York"), " ",
    full_name_user_name
  )
  
  date_user_line
  
}

#' Make the header lines for a copyright statement
#'
#' @return \code{character} or \code{NULL}
#' @keywords internal
make_copyright_line <- function(copyright_holder = NULL) {
  
  copyright_with_year <- paste0("Copyright ", format(Sys.Date(), format = "%Y"))
  
  if(is.null(copyright_holder)) {
    copyright_holder <- .cognigen_copyright_message
  }
  
  if(is.character(copyright_holder)) {
    paste0(
      strwrap(
        paste(copyright_with_year, copyright_holder, collapse = "\n\n"),
        width = .split_length,
        prefix = "# "
      ),
      collapse = "\n"
    )
  } else if(isFALSE(copyright_holder)) {
    NULL
  } else {
    NULL
  }
  
}

#' Make the header lines containing the purpose
#'
#' @inheritParams Redit
#' @param split_length \code{integer} representing how many characters to split
#'   at.
#'
#' @return \code{character} or \code{NULL}
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

  purpose

}


#' Make the header lines containing the input or output files
#'
#' @param files input or output files.
#' @param type either \code{"input"} or \code{"output"}.
#'
#' @return \code{character} or \code{NULL}
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
  
  files

}

# behaves like xfun::gsub_file but only modifies the first line with a match
gsub_file_first <- function(file, ..., rw_error = TRUE) {
  if (!(file.access(file, 2) == 0 && file.access(file, 4) == 0)) {
    (if (rw_error) stop else warning)("Unable to read or write to ", file)
    if (!rw_error) return(invisible())
  }
  x1 <- tryCatch(xfun::read_utf8(file, error = TRUE), error = function(e) if (rw_error) stop(e))
  if (is.null(x1)) return(invisible())
  
  pattern <- list(...)[["pattern"]]
  first_occurrence <- grep(pattern, x = x1, fixed = TRUE)
  if (length(first_occurrence) == 0) return(invisible())
  first_occurrence <- min(first_occurrence)
  x_subbed <- gsub(x = x1[1:first_occurrence], ...)
  x2 <- c(x_subbed, x1[(first_occurrence + 1):length(x1)])
  if (!identical(x1, x2)) xfun::write_utf8(x2, file)
}