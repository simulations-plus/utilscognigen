#' Convert file paths to their Unix or Windows representations
#'
#' @details Paths are split by commas. Paths that do not exist result in
#'   \code{NA} if \code{normalize} is \code{TRUE}.
#'
#'   When converting to Unix representation, paths in Windows 'C:/' result in
#'   \code{NA} since there is no Unix equivalent.
#'
#'   When converting to Windows representation, paths not in '~', '/cognigen',
#'   '/doc', '/home', '/misc' result in \code{NA} since there is no Windows
#'   equivalent.
#'
#' @param path file paths. Defaults to the path of the source editor context.
#' @param ask \code{logical} indicating whether to interactively ask for input.
#'   This is particularly helpful for file paths including single backslashes
#'   (Windows paths).
#' @param normalize \code{logical} indicating whether to normalize the file
#'   paths. Ignored on platforms other than the desired OS. When \code{TRUE},
#'   files that do not exist result in \code{NA}.
#' @param to which OS representation to match. Either 'unix' or 'windows'.
#'
#' @return \code{character} vector with length of \code{path} containing the
#'   converted paths
#'
#' @examples
#' \dontrun{
#' # Ask for input
#' path_to_unix(ask = TRUE)
#'
#' # Get the Windows representation for the active source document
#' path_to_windows()
#'
#' # Get the theoretical Unix representation of Windows paths without checking for their existence
#' path_to_unix(c("M:/client/drug", "M:\\client2\\drug"), normalize = FALSE)
#'
#' # Get the Windows representation of Unix paths, checking for their existence (if using Windows)
#' path_to_windows(c("/doc/client/drug", "/doc/client2/drug"), normalize = TRUE)
#' }
convert_path <- function(path = NULL, ask = FALSE, normalize = TRUE, to = "unix") {

  # Ask for input. Designed to enter paths containing single backslashes
  if(ask) {
    path <- readline("Enter paths: ")
  }

  path <- if(is.null(path)) get_source_file() else path
  path <- clean_path(path)

  # Normalize early if normalize is requested on a different OS
  if(normalize && .Platform$OS.type != to) {
    path <- normalizePath(path, mustWork = FALSE)
  }

  if(to == "unix") {
    path <- make_unix_replacements(path = path, normalize = normalize)
  } else if(to == "windows") {
    path <- make_windows_replacements(path = path, normalize = normalize)
  }

  return(path)

}



#' @rdname convert_path
#' @export
path_to_unix <- function(path = NULL, ask = FALSE, normalize = TRUE) {
  return(convert_path(path, ask, normalize, to = "unix"))
}

#' @rdname convert_path
#' @export
path_to_windows <- function(path = NULL, ask = FALSE, normalize = TRUE) {
  return(convert_path(path, ask, normalize, to = "windows"))
}


# Helpers -----------------------------------------------------------------


#' Clean file paths
#'
#' Removes quote characters, replaces backslashes with forward slashes, re-lists
#' paths if multiple paths are stored in one element of the vector.
#'
#' @inheritParams convert_path
#'
#' @return \code{character} vector of cleaned file paths
#' @keywords internal
clean_path <- function(path) {

  assertthat::assert_that(is.character(path))

  path <- gsub('"', "", path)
  path <- gsub("'", "", path)
  path <- unlist(strsplit(path, ","))
  path <- gsub(",", "", path)
  path <- gsub("\\\\", "/", path)
  path <- gsub("/+", "/", path)
  path <- trimws(path)

  return(path)
}


#' Make replacements to file paths to get the Unix representation
#'
#' @inheritParams convert_path
#'
#' @return \code{character} vector the length of \code{path} containing the
#'   converted paths
#' @keywords internal
make_unix_replacements <- function(path, normalize) {

  # Replace Windows-specific path representations with their Unix equivalent
  path <- gsub("^~", paste0("/home/", Sys.info()[["user"]]), path)
  path <- gsub("^H:", paste0("/home/", Sys.info()[["user"]]), path, ignore.case = TRUE)
  path <- gsub("^I:", "/home", path, ignore.case = TRUE)
  path <- gsub("^L:", "/cognigen", path, ignore.case = TRUE)
  path <- gsub("^M:", "/doc", path, ignore.case = TRUE)

  # Catch any C: paths
  c_drive <- grepl("^C:", path, ignore.case = TRUE)
  if(any(c_drive)) {
    warning("There is no equivalent to the C drive on Unix")
    path[c_drive] <- NA_character_
  }

  # Normalize paths if requested and OS is unix
  # Warnings are thrown if the file does not exist
  if(normalize && .Platform$OS.type == "unix") {
    path <- vapply(X = path,
                   FUN = function(x) {
                     if(is.na(x)) return(NA_character_)
                     try(normalizePath(x, mustWork = TRUE), silent = TRUE)
                   },
                   FUN.VALUE = character(1),
                   USE.NAMES = FALSE)

    dne <- startsWith(path, "Error in normalizePath")

    if(any(dne)) {
      warning("NA results do not exist", call. = FALSE)
      path[dne] <- NA
    }
  }

  return(path)

}


#' Make replacements to file paths to get the Windows representation
#'
#' @inheritParams convert_path
#'
#' @return \code{character} vector the length of \code{path} containing the
#'   converted paths
#' @keywords internal
make_windows_replacements <- function(path, normalize) {

  # Replace Unix-specific path representations with their Windows equivalent
  path <- gsub("^~", "H:", path)
  path <- gsub(paste0("^/home/", Sys.info()[["user"]]), "H:", path, ignore.case = TRUE)
  path <- gsub("^/home", "I:", path, ignore.case = TRUE)
  path <- gsub("^/misc/cognigen", "L:", path, ignore.case = TRUE)
  path <- gsub("^/cognigen", "L:", path, ignore.case = TRUE)
  path <- gsub("^/doc", "M:", path, ignore.case = TRUE)
  path <- gsub("^/misc/doc", "M:", path, ignore.case = TRUE)
  path <- gsub("^/misc", "M:", path, ignore.case = TRUE)

  # Catch any directories that don't exist on Windows
  dne_paths <- grepl("^/", path)
  if(any(dne_paths)) {
    warning("There are only Windows equivalents for '~', '/cognigen', '/doc', '/home', '/misc'")
    path[dne_paths] <- NA_character_
  }

  # Normalize paths if requested and OS is Windows
  # Warnings are thrown if the file does not exist
  if(normalize && .Platform$OS.type == "windows") {
    path <- vapply(X = path,
                   FUN = function(x) {
                     if(is.na(x)) return(NA_character_)
                     try(normalizePath(x, winslash = "/", mustWork = TRUE), silent = TRUE)
                   },
                   FUN.VALUE = character(1),
                   USE.NAMES = FALSE)

    dne <- startsWith(path, "Error in normalizePath")

    if(any(dne)) {
      warning("NA results do not exist", call. = FALSE)
      path[dne] <- NA
    }
  }

  return(path)

}
