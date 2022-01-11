
#' Scan log files of all R and/or SAS programs within a directory
#'
#' @param path a single directory path. Defaults to the working directory.
#' @param pattern optional \code{character} string containing a regular
#'   expression. Only file names which match the regular expression will be
#'   scanned
#' @param ext a \code{character} vector of file extensions to scan the logs of
#'
#' @return \code{dir_scanlogs()} invisibly returns a named \code{list} of
#'   \code{character} vectors
#' @export
#'
#' @examples
#' \dontrun{
#' # Scan all Rout and SAS log files in the working directory
#' dir_scanlogs()
#'
#' # Scan all Rout files with "final" in the file name
#' dir_scanlogs(path = "/stage_directory/asmbdat/", pattern = "final", ext = "Rout")
#' }
#'
#' @rdname scanlogs
dir_scanlogs <- function(path = getwd(), pattern = NULL, ext = c("Rout", "log")) {

  assertthat::assert_that(
    length(path) == 1,
    dir.exists(path)
    )

  ext[ext %in% c("R", "r")] <- "Rout"
  ext[ext %in% c("sas", "SAS", "lst")] <- "log"
  ext <- unique(ext)
  if(!all(ext %in% c("Rout", "log"))) {
    stop("Only Rout and log files are supported for `dir_scanlogs()`")
  }

  log_paths <- list.files(path, pattern = paste0(".", ext, "$", collapse = "|"), full.names = TRUE)

  if(!is.null(pattern)) {
    log_paths <- log_paths[grep(pattern, basename(log_paths))]
  }

  if(length(log_paths) == 0) {
    message("'", path, "' contains no matching ", paste0(ext, collapse = " or "), " files")
    return(invisible(NULL))
  } else if(length(log_paths) == 1) {
    results <- list(scanlogs(log_paths))
  } else {
    results <- scanlogs(log_paths)
  }

  names(results) <- log_paths
  return(invisible(results))
}

#' Scan log files of R and/or SAS programs
#'
#' Scan the log files of multiple R and/or SAS programs The scan searches
#' \*.Rout files for warnings and errors; \*.log files for warnings, errors,
#' notes, and other messages using the system command \code{scanlogs}.
#'
#' @param ... \code{character} vectors containing file names or paths with
#'   extensions .R, .r, .Rout, .sas, .log, or .lst. Defaults to the path of the
#'   source editor context.
#'
#' @return \code{scanlogs()} invisibly returns a \code{character} vector of
#'   scanned results or a \code{list} of \code{character} vectors when multiple
#'   paths are provided.
#' @export
#'
#' @examples
#' \dontrun{
#' scanlogs()
#' scanlogs("/stage_directory/asmbdat/mk-something.Rout")
#' scanlogs("/stage_directory/sas/check-something.log")
#' scanlogs("/stage_directory/asmbdat/mk-something.Rout", "/stage_directory/sas/check-something.log")
#' }
#' 
scanlogs <- function(...) {
  paths <- unlist(list(...))
  if(length(paths) == 0) {
    invisible(scanlogs_single())
  } else if(length(paths) == 1) {
    invisible(scanlogs_single(paths))
  } else {
    invisible(lapply(paths, function(path) {
      res <- scanlogs_single(path)
      if(!is.null(res)) {
        cat("\n")
      }
      return(res)
    }))
  }
}


#' Scan log file of R, Rmd, or SAS programs
#'
#' Only a single path should be passed directly to \code{scanlogs_single()}.
#'
#' @param path a file name or path with extension .R, .r, .Rmd, .Rout, .r.Rout,
#'   .sas, .log, or .lst. Defaults to the path of the source editor context
#'
#' @return invisibly returns a \code{character} vector of scanned results. For
#'   R, the findings are prepended by the log path and line number. For SAS, the
#'   findings are prepended by the log path.
#'
#' @keywords internal
#'   
scanlogs_single <- function(path = NULL) {

  assertthat::assert_that(
    length(path) <= 1
    )

  path <- if(is.null(path)) get_source_file() else path
  if(is.null(path)) {
    stop("A script must be open or a path must be specified to use `scanlogs_single()`.")
  } else if(!file.exists(path)) {
    stop(path, " does not exist.")
  }
  extension <- tools::file_ext(path)
  for_method <- structure(path, class = extension)
  UseMethod("scanlogs_single", for_method)
}

scanlogs_single.sas <- function(path = NULL) {
  path <- if(is.null(path)) get_source_file() else path
  return_value <- system(paste0("scanlogs ", path), intern = TRUE)
  cat(return_value, sep = "\n")
  return(invisible(return_value))
}

scanlogs_single.log <- scanlogs_single.lst <- scanlogs_single.sas

scanlogs_single.R <- function(path = NULL) {
  path <- if(is.null(path)) get_source_file() else path
  extension <- tools::file_ext(path)
  extension_replacement <- ifelse(extension == "R", ".Rout",
                                  ifelse(extension == "r", ".r.Rout",
                                         ifelse(extension == "Rout", ".Rout", NA_character_)))
  log_name <- paste0(tools::file_path_sans_ext(path), extension_replacement)
  if(!file.exists(log_name)) {
    stop(
      log_name, " has not been created. \n",
      "Generate a log file with `rcb ", basename(path), "` from the Terminal within the script directory."
    )
  }
  scan_rout(log_name)
}

scanlogs_single.r <- scanlogs_single.Rout <- scanlogs_single.R

scanlogs_single.Rmd <- function(path = NULL) {
  path <- if(is.null(path)) get_source_file() else path
  extension <- tools::file_ext(path)
  extension_replacement <- "-render.Rout"
  log_name <- paste0(tools::file_path_sans_ext(path), extension_replacement)
  if(!file.exists(log_name)) {
    stop(
      log_name, " has not been created. \n",
      "Generate a log file with `interactivecog::render()` from the R console."
    )
  }
  scan_rout(log_name)
}


scan_rout <- function(path) {
  rout <- readLines(path)
  single_warnings <- scan_single_warnings(rout)
  multiple_warnings <- scan_warning_messages(rout)
  fifty_or_more_warnings <- grep("There were 50 or more warnings", rout)
  nn_warnings <- grep("There were \\d\\d? warnings", rout)
  errors <- scan_error_messages(rout)
  execution_halteds <- grep("Execution halted", rout)

  all_kept_lines <- unique(sort(c(single_warnings,
                                  multiple_warnings,
                                  fifty_or_more_warnings,
                                  nn_warnings, errors,
                                  execution_halteds)))

  if(length(all_kept_lines) == 0) {
    return(invisible(NULL))
  }

  return_value <- paste0(path, " line ", all_kept_lines, ": ", rout[all_kept_lines])

  cat(return_value, sep = "\n")
  return(invisible(return_value))
}

# This function returns the indices including and following single warnings
# messages, the line below, and any indented lines that consecutively follow
scan_single_warnings <- function(rout) {
  single_warning_lines <- c(grep("^Warning:", rout),
                            grep("^Warning message:", rout),
                            grep("^In addition: Warning message:", rout))
  unlist(lapply(
    X = c(single_warning_lines, single_warning_lines + 1),
    FUN = keep_indented_after_message,
    rout = rout
  ))
}


# This function returns the indices including and following "Warning messages:",
# which can be followed by up to 50 warnings
scan_warning_messages <- function(rout) {
  warning_messages_lines <- grep("^Warning messages:", rout)
  starts_with_num_or_indent <- c(grep("^\\d\\d?: ", rout), grep("^\\s{2}", rout))
  unlist(lapply(
    X = warning_messages_lines,
    FUN = function(message_line) {
      lines_to_keep <- c(message_line,
                         sort(starts_with_num_or_indent[starts_with_num_or_indent > message_line]))
      lines_jumps <- diff(lines_to_keep)

      if(all(lines_jumps == 1)) {
        return(lines_to_keep)
      } else if(length(lines_jumps) == 1) {
        return(message_line)
      }

      first_to_drop <- min(which(lines_jumps != 1))
      last_line_to_keep <- message_line + first_to_drop - 1
      lines_to_keep <- lines_to_keep[1]:last_line_to_keep
      return(lines_to_keep)
    }
  ))
}


# This function returns the indices including and following error messages, and
# any indented lines that consecutively follow
scan_error_messages <- function(rout) {
  error_messages_lines <- c(grep("^Error:", rout), grep("Error in", rout))
  unlist(lapply(
    X = error_messages_lines,
    FUN = keep_indented_after_message,
    rout = rout
  ))
}

# This function returns the indices of the message line and consecutive indented
# lines (2 spaces) of an rout
keep_indented_after_message <- function(rout, message_line, trunc = 10) {
  # Only first trunc lines are considered before truncation
  lines_to_check <- message_line:(message_line + trunc)
  lines_to_check <- lines_to_check[lines_to_check <= length(rout)]

  lines_to_keep <- c(message_line, lines_to_check[grep("^\\s{2}", rout[lines_to_check])])
  lines_jumps <- diff(lines_to_keep)

  if(all(lines_jumps == 1)) {
    return(lines_to_keep)
  } else if(length(lines_jumps) == 1) {
    return(message_line)
  }

  first_to_drop <- min(which(lines_jumps != 1))
  last_line_to_keep <- message_line + first_to_drop - 1
  last_line_to_keep <- message_line + which.max(lines_jumps) - 1
  lines_to_keep <- lines_to_keep[1]:last_line_to_keep
  return(lines_to_keep)
}