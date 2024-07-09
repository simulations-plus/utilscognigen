#' Convert file paths to their Unix or Windows representations
#'
#' @details Paths are split by commas. Paths that do not exist result in
#'   \code{NA} if \code{normalize} is \code{TRUE}.
#'
#'   When converting to Unix representation, paths in Windows 'C:/' result in
#'   \code{NA} since there is no Unix equivalent.
#'
#'   When converting to Windows representation, paths not in '~', '/cognigen',
#'   '/doc', '/home', '/miguel', or '/misc' result in \code{NA} since there is
#'   no Windows equivalent.
#'   
#'   Also see the Toggle File Path Selection RStudio Addin.
#'
#' @param path file paths. Defaults to the path of the source editor context.
#' @param ask \code{logical} indicating whether to interactively ask for input.
#'   This is particularly helpful for file paths including single backslashes
#'   (Windows paths).
#' @param normalize \code{logical} indicating whether to normalize the file
#'   paths. Ignored on platforms other than the desired OS. When \code{TRUE},
#'   files that do not exist result in \code{NA}.
#'
#' @return \code{character} vector with length of \code{path} containing the
#'   converted paths.
#'
#' @name path_to_
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
NULL
#> NULL

#' @rdname path_to_
#' @export
path_to_unix <- function(path = NULL, ask = FALSE, normalize = TRUE) {
  require_cognigen()
  return(convert_path(path, ask, normalize, to = "unix"))
}

#' @rdname path_to_
#' @export
path_to_windows <- function(path = NULL, ask = FALSE, normalize = TRUE) {
  require_cognigen()
  return(convert_path(path, ask, normalize, to = "windows"))
}

#' Toggle paths between Unix or Windows based on the first entry
#' 
#' This is intended to be used as an RStudio Addin.
#' 
#' @keywords internal
toggle_path_selection <- function() {
  
  assertthat::assert_that(
    rstudioapi::isAvailable()
  )
  
  context <- rstudioapi::getActiveDocumentContext()
  if(!utils::hasName(context, "selection")) return(invisible(NULL))
  
  selection <- context[["selection"]][[1]]
  if(!utils::hasName(selection, "text")) return(invisible(NULL))
  
  text <- selection[["text"]]
  
  if(is.null(text)) return(invisible(NULL))
  if(text == "") return(invisible(NULL))
  
  text <- unlist(strsplit(text, "\n|;|,"))
  text <- text[text != ""]
  
  which_os <- ifelse(grepl("\\:", text[[1]]), "windows", "unix")
  which_new_os <- ifelse(which_os == "windows", "unix", "windows")
  
  comments <- gsub("^(#*\\s*).*", "\\1", text)
  uncommented_text <- gsub("^#*\\s*", "", text)
  
  was_single_quoted <- grepl("^\\'", uncommented_text)
  was_double_quoted <- grepl('^\\"', uncommented_text)
  
  new_text <- suppressWarnings(convert_path(path = uncommented_text, to = which_new_os))
  new_text <- ifelse(is.na(new_text), text, new_text)
  new_text <- ifelse(was_single_quoted, paste0("'", new_text, "'"), new_text)
  new_text <- ifelse(was_double_quoted, paste0('"', new_text, '"'), new_text)
  new_text <- paste0(comments, new_text)
  new_text <- paste0(new_text, collapse = "\n")
  
  rstudioapi::insertText(text = new_text)
}


# Helpers -----------------------------------------------------------------

#' Facilitator for \code{path_to_unix} and \code{path_to_windows}
#' 
#' @inheritParams path_to_unix
#' @param to which OS representation to match. Either 'unix' or 'windows'.
#' 
#' @return \code{character} vector with length of \code{path} containing the
#'   converted paths.
#'
#' @keywords internal
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
  
  path
  
}


#' Clean file paths
#'
#' Removes quote characters, replaces backslashes with forward slashes, re-lists
#' paths if multiple paths are stored in one element of the vector.
#'
#' @inheritParams convert_path
#'
#' @return \code{character} vector of cleaned file paths.
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
#'   converted paths.
#' @keywords internal
make_unix_replacements <- function(path, normalize) {
  
  # Replace Windows-specific path representations with their Unix equivalent
  path <- gsub("^~", paste0("/home/", Sys.info()[["user"]]), path)
  path <- gsub("^H:", paste0("/home/", Sys.info()[["user"]]), path, ignore.case = TRUE)
  path <- gsub("^I:", "/home", path, ignore.case = TRUE)
  path <- gsub("^L:", "/cognigen", path, ignore.case = TRUE)
  
  # since SLP employees do not have access to /cognigen files, use that to 
  # determine whether M: is /miguel (SLP) or /cognigen (CPP)
  if(length(list.files("/cognigen")) == 0) {
    path <- gsub("^M:", "/miguel", path, ignore.case = TRUE)
  } else {
    path <- gsub("^M:", "/doc", path, ignore.case = TRUE)
  }
  
  # Catch any C: paths
  c_drive <- grepl("^C:", path, ignore.case = TRUE)
  if(any(c_drive)) {
    cli::cli_warn("There is no equivalent to the C:/ drive on Unix")
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
      cli::cli_warn("NA results do not exist")
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
#'   converted paths.
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
  # /miguel is M: at SLP
  path <- gsub("^/miguel", "M:", path, ignore.case = TRUE)
  
  # Catch any directories that don't exist on Windows
  dne_paths <- grepl("^/", path)
  if(any(dne_paths)) {
    cli::cli_warn("There are only Windows equivalents for '~', '/cognigen', '/doc', '/home', '/misc', '/miguel'")
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
      cli::cli_warn("NA results do not exist")
      path[dne] <- NA
    }
  }
  
  return(path)
  
}
