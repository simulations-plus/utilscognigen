# Various functions to interact with R script headers

.try_this_on_header_errors <- "For R files, update the first line so it is an R command.\nFor Rmd files, delete or rename any chunks named 'header'.\nThen run `Redit()` to include a clean header."


# Header content retrievers -----------------------------------------------

#' Get content from an R script header
#'
#' For `get_header_input_files` and `get_header_output files`, file paths are
#' split by new lines, commas, and semicolons.
#'
#' @param path a file path to an R script. Defaults to the path of the source
#'   editor context.
#' @name get_header_content
#'
#' @return \code{character} vector of requested header item.
NULL
#> NULL


#' @rdname get_header_content
#' @export
get_header_name <- function(path = NULL) {
  files <- unname(unlist(parse_header(path, "name")))
  return(files)
}

#' @rdname get_header_content
#' @export
get_header_history <- function(path = NULL) {
  files <- unname(unlist(parse_header(path, "history")))
  return(files)
}

#' @rdname get_header_content
#' @export
get_header_version <- function(path = NULL) {
  files <- unname(unlist(parse_header(path, "version")))
  return(files)
}

#' @rdname get_header_content
#' @export
get_header_copyright <- function(path = NULL) {
  files <- unname(unlist(parse_header(path, "copyright")))
  return(files)
}

#' @rdname get_header_content
#' @export
get_header_purpose <- function(path = NULL) {
  files <- unname(unlist(parse_header(path, "purpose")))
  return(files)
}

#' @rdname get_header_content
#' @export
get_header_input_files <- function(path = NULL) {
  files <- unname(unlist(parse_header(path, "input_files")))
  return(files)
}

#' @rdname get_header_content
#' @export
get_header_output_files <- function(path = NULL) {
  files <- unname(unlist(parse_header(path, "output_files")))
  return(files)
}


# Get header --------------------------------------------------------------

#' Extract a Cognigen header for an R script
#'
#' @param x a scanned R or Rmd file or a file path
#' @param min_hash \code{numeric}; The minimum number of "#" that define the
#'   beginning of the header. There is no maximum.
#' @param max_first_line \code{numeric}; The maximum number of non-blank lines
#'   permitted before the beginning of the header in an R script. For Rmd files,
#'   this is determined by considering the chunk named header.
#'
#' @return a \code{character} vector of lines of the header or \code{FALSE} with
#'   a reason attribute if no acceptable header is found
#'
#' @keywords internal
get_header <- function(x, min_hash = 30L, max_first_line = 1L) {

  # If x is a file path, scan it
  if(length(x) == 1 && file.exists(x)) {
    x <- structure(
      scan(x, what = character(), sep = "\n", quiet = TRUE),
      path = x
    )
  }

  path <- attr(x, "path")
  file_ext <- tolower(tools::file_ext(path))

  hashes <- paste0(rep("#", min_hash), collapse = "")
  top_of_header_regex <- paste0("^", hashes)
  potential_header_lines <- grep(top_of_header_regex, x)

  # require header chunk (named header) for Rmd files
  if(file_ext == "rmd") {

    header_chunk_start <- grep("^```\\{r header[,\\s\\}]", x)
    if(length(header_chunk_start) == 0) {
      return(structure(FALSE,
                       reason = "No chunk named header found in Rmd file",
                       code = "no_chunk_named_header"))
    } else if(length(header_chunk_start) > 1) {
      return(structure(FALSE,
                       reason = "Multiple chunks named header found in Rmd file. There should be only 1 header.",
                       code = "multiple_chunks_named_header"))
    }

    chunk_ends <- grep("^```$", trimws(x))
    if(length(chunk_ends) == 0) {
      return(structure(FALSE,
                       reason = "No end of header chunk found in Rmd file",
                       code = "no_end_of_header"))
    }

    header_chunk_end <- min(chunk_ends[chunk_ends > header_chunk_start])
    if(length(header_chunk_end) == 0) {
      return(structure(FALSE,
                       reason = "No end of header chunk found in Rmd file",
                       code = "no_end_of_header"))
    }

    if(header_chunk_end - header_chunk_start == 1) {
      return(structure(FALSE,
                       reason = "Empty header chunk",
                       code = "empty_header_chunk"))
    }

    if(length(potential_header_lines) == 0) {
      return(structure(FALSE,
                       reason = "No header found in header chunk",
                       code = "no_header_in_header_chunk"))
    }

    # Update max_first_line which is the max line where the header content can start
    max_first_line <- header_chunk_end

  }

  if(length(potential_header_lines) == 0) {
    return(structure(FALSE, reason = "No header found"))
  }

  first_line_header <- min(potential_header_lines)

  if(first_line_header > max_first_line) {
    return(structure(FALSE, reason = "Too many lines before header"))
  }

  # 2 is subtracted to account for the starting point and the min(grep())
  # returning the first non-commented line
  suppressWarnings(
    last_line_header <- (first_line_header - 2) + min(grep("^[^#]", x[first_line_header:length(x)]))
  )
  # Adjustment in case a file contains only a header
  last_line_header <- ifelse(is.infinite(last_line_header), length(x), last_line_header)
  header <- x[first_line_header:last_line_header]

  # Remove last character from header if it a "#" and remove extra white space
  header <- trimws(gsub("#$", "", header))
  header <- gsub("\\s\\s", " ", header)

  return(header)
}


# Header parsing functions ------------------------------------------------

.valid_header_sections <- c("name",
                            "history",
                            "version",
                            "copyright",
                            "purpose",
                            "input_files",
                            "output_files",
                            "is_sourced")

#' Parse R script headers
#'
#' @param path a file path to an R script. Defaults to the path of the source
#'   editor context.
#' @param sections header sections to parse
#' @param header a header from an R script
#' @param ... additional arguments passed to \code{\link{get_header}}
#'
#' @return \code{parse_header} returns a \code{list} of parsed sections. Each
#'   \code{parse_<section>} function returns the parsed section.
#' @keywords internal
parse_header <- function(path = NULL, sections = .valid_header_sections, ...) {

  path <- if(is.null(path)) get_source_file() else path

  assertthat::assert_that(
    is.character(path),
    length(path) == 1,
    file.exists(path),
    all(sections %in% .valid_header_sections)
    )

  sections <- tolower(sections)
  header <- get_header(path, ...)

  if(is.logical(header)) {
    stop(attr(header, "reason"), call. = FALSE)
  }

  # Prepare list of sections
  list_of_sections <- vector("list", length = length(sections))
  names(list_of_sections) <- sections

  # Replace each section with its parsed value
  if("name" %in% sections) list_of_sections[["name"]] <- parse_header_name(header)
  if("history" %in% sections) list_of_sections[["history"]] <- parse_header_history(header)
  if("version" %in% sections) list_of_sections[["version"]] <- parse_header_version(header)
  if("copyright" %in% sections) list_of_sections[["copyright"]] <- parse_header_copyright(header)
  if("purpose" %in% sections) list_of_sections[["purpose"]] <- parse_header_purpose(header)
  if("input_files" %in% sections) list_of_sections[["input_files"]] <- parse_header_input_files(path, header)
  if("output_files" %in% sections) list_of_sections[["output_files"]] <- parse_header_output_files(path, header)
  if("is_sourced" %in% sections) list_of_sections[["is_sourced"]] <- parse_header_is_sourced(header)

  return(list_of_sections)

}


#' @rdname parse_header
parse_header_name <- function(header) {

  has_name <- any(grepl("Name:", header, ignore.case = TRUE))

  if(has_name) {
    name_line <- min(grep("Name:", header, ignore.case = TRUE))
    name <- gsub("#+\\s?+Name:\\s?+(.*)", "\\1", header[name_line], ignore.case = TRUE, perl = TRUE)
    return(name)
  } else {
    warning(
      "Name of program not included in current header.",
      "\n", .try_this_on_header_errors,
      call. = FALSE
    )
    return(NULL)
  }

}


#' @rdname parse_header
parse_header_history <- function(header) {

  # History must start with a day of the week abbreviation
  history <- header[grepl(paste0("^#\\s?(", .days_of_week, ")"), header, ignore.case = TRUE)]
  if(length(history)) {
    history <- trimws(gsub("#", "", history))
    return(history)
  } else {
    return(NULL)
  }

}


#' @rdname parse_header
parse_header_version <- function(header) {

  has_version_line <- any(grepl("Written for use with R version ", header, ignore.case = TRUE))
  if(has_version_line) {
    header_version_line <- header[grepl("Written for use with R version ", header, ignore.case = TRUE)]
    header_version <- gsub(".*Written for use with R [vV]ersion ([0-9]\\.[0-9]\\.[0-9]).*", "\\1", header_version_line, ignore.case = TRUE)
    return(header_version)
  } else {
    return(NULL)
  }

}


#' @rdname parse_header
parse_header_copyright <- function(header) {

  copyright_section_start <- grep("Copyright.*Cognigen Corporation", header, ignore.case = TRUE)
  if(length(copyright_section_start) == 0) {
    # No copyright file section detected
    return(NULL)
  }

  copyright_section_end <- grep("^#{3}|^#\\s{3}", header, ignore.case = TRUE)
  if(length(copyright_section_end) == 0) {
    # No end of copyright section detected
    return(NULL)
  }
  copyright_section_end <- min(copyright_section_end[copyright_section_end > copyright_section_start]) - 1

  copyright_section <- header[copyright_section_start:copyright_section_end]

  copyright_section <- trimws(gsub("#", "", copyright_section))
  copyright_section <- paste0(copyright_section, collapse = " ")

  return(copyright_section)

}


#' @rdname parse_header
parse_header_purpose <- function(header) {

  purpose_section_start <- grep("PURPOSE", header, ignore.case = TRUE)
  if(length(purpose_section_start) == 0) {
    # No purpose file section detected
    return(NULL)
  } else {
    purpose_section_start <- min(purpose_section_start)
  }

  purpose_section_end <- grep("INPUT FILE|OUTPUT FILE|#NOTE|#\\s+NOTE", header, ignore.case = TRUE)
  # If there are no matches, find last header row starting with 3 consecutive comments
  if(length(purpose_section_end) == 0) {
    purpose_section_end <- grep("^#{3}", header)
  }

  if(length(purpose_section_end) == 0) {
    # No end of purpose section detected
    return(NULL)
  } else {
    purpose_section_end <- purpose_section_end[purpose_section_end > purpose_section_start]
    purpose_section_end <- min(purpose_section_end)
  }

  purpose_section_end <- min(purpose_section_end[purpose_section_end > purpose_section_start]) - 1

  purpose_section <- header[purpose_section_start:purpose_section_end]

  purpose_section <- trimws(gsub("#", "", purpose_section))
  purpose_section <- paste0(purpose_section, collapse = " ")
  purpose_section <- gsub("PURPOSE:?", "", purpose_section)
  purpose_section <- trimws(gsub("\\s+", " ", purpose_section))

  return(purpose_section)


}


#' @rdname parse_header
parse_header_input_files <- function(path, header) {

  input_section_start <- grep("INPUT FILE", header, ignore.case = TRUE)
  if(length(input_section_start) == 0) {
    # No input file section detected
    return(NULL)
  } else if(length(input_section_start) > 1) {
    warning("Multiple input file sections detected. None will be reported.", call. = FALSE)
    return(NULL)
  }

  input_section_end <- grep("OUTPUT FILE|#NOTE|#\\s+NOTE|^#{3}", header, ignore.case = TRUE)
  if(length(input_section_end) == 0) {
    # No end of input file section or end of header detected
    return(NULL)
  }

  input_section_end <- min(input_section_end[input_section_end > input_section_start]) - 1

  input_section <- header[input_section_start:input_section_end]

  files <- file_section_to_files(path = path, section = input_section)

  return(files)

}


#' @rdname parse_header
parse_header_output_files <- function(path, header) {
  output_section_start <- grep("OUTPUT FILE", header, ignore.case = TRUE)
  if(length(output_section_start) == 0) {
    # No output file section detected
    return(NULL)
  } else if(length(output_section_start) > 1) {
    warning("Multiple output file sections detected. None will be reported.", call. = FALSE)
    return(NULL)
  }

  output_section_end <- grep("#NOTE|#\\s+NOTE|^#{3}", header, ignore.case = TRUE)
  if(length(output_section_end) == 0) {
    # No end of output file section or end of header detected
    return(NULL)
  }

  output_section_end <- min(output_section_end[output_section_end > output_section_start]) - 1

  output_section <- header[output_section_start:output_section_end]

  files <- file_section_to_files(path = path, section = output_section)

  return(files)
}


#' @rdname parse_header
parse_header_is_sourced <- function(header) {
  return(any(grepl("sourced in|sourced from|sources this", header, ignore.case = TRUE)))
}


# Helpers -----------------------------------------------------------------

# Convert an input or output section to a character vector of file paths
# Working directory is set in order to normalize path, then set back
file_section_to_files <- function(path, section) {

  oldwd <- setwd(dirname(path))
  on.exit(setwd(oldwd))

  section <- gsub("#", "", section)
  section <- gsub("INPUT FILES?:?|OUTPUT FILES?:?", "", section, ignore.case = TRUE)
  section <- trimws(gsub("\\s+", " ", section))

  files <- trimws(unlist(strsplit(section, ",|;")))
  files <- files[files != ""]
  files <- normalizePath(files, mustWork = FALSE)

  return(files)

}