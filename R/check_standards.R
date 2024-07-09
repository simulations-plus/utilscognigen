#' Check an R file against CPP standards
#'
#' Performs each of the checks described in Details. All checks must pass for
#' \code{check_standards} to return \code{TRUE}.
#'
#' \describe{
#'   \item{\code{\link{check_rout}}}{Determines if the R script should have an
#'   associated Rout file, then checks the content of the Rout.}
#'   \item{\code{\link{check_header}}}{Checks that the R script has an
#'   appropriate header.}
#'   \item{\code{\link{check_session_info}}}{Checks that
#'   \code{\link[utils]{sessionInfo}} or
#'   \code{\link[devtools]{session_info}} is called in the R script, preferably
#'   as the final call.}
#' }
#'
#' @param path a file path to an R or R Markdown file. Defaults to the path of
#'   the source editor context.
#' 
#' @param standards either a \code{character} vector of the standards provided
#'   and described with this function, or a list that can include the provided
#'   standard names as well as custom functions. Custom functions should accept
#'   arguments \code{x} (a scanned R file) and \code{path} (a file path). Custom
#'   functions should return a named list consisting of a \code{logical} element
#'   named "pass" and a \code{character} element named "reason" that describes
#'   the reason the standard is not satisfied.
#' 
#' @return \code{logical} indicating whether the file satisfied all standards.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' check_standards("script.R")
#' }
check_standards <- function(
    path = NULL, 
    standards = c(
      "check_rout",
      "check_header",
      "check_session_info"
    )
  ) {
  
  path <- if(is.null(path)) get_source_file() else path
  path <- gsub("\\.Rout$", ".R", path)
  
  assertthat::assert_that(
    length(path) <= 1,
    file.exists(path),
    tolower(tools::file_ext(path)) %in% c("r", "rmd", "qmd")
  )
  
  # attribute path is set for cases where the scanned script gets passed to
  # another function that may depend on file type
  x <- structure(
    scan(path, what = character(), sep = "\n", quiet = TRUE),
    path = path
  )
  
  x_all_checks <- lapply(standards, function(standard) {
    
    fun <- if(is.character(standard)) {
      # only provided standards can be supplied as character
      switch(
        standard,
        check_rout = check_rout,
        check_header = check_header,
        check_session_info = check_session_info,
        cli::cli_abort(
          "{.arg standards} can include the names of standards that are included in this package or should be provided as functions."
        )
      )
    } else if(is.function(standard)) {
      
      if(!all(c("x", "path") %in% names(formals(standard)))) {
        cli::cli_abort(
          "Functions in {.arg standards} must accept arguments {.arg x} and {.arg path}."
        )
      }
      
      standard
      
    }
    
    fun(x = x, path = path)
    
  })
  
  standards_names <- vapply(
    X = as.list(substitute(standards))[-1L], 
    FUN = function(.nameish) {
      as.character(.nameish)[[1]]
    }, 
    FUN.VALUE = character(1)
  )
  
  names(x_all_checks) <- standards_names
  
  x_all_statuses <- unlist(lapply(x_all_checks, `[[`, "pass"))
  
  x_failed_checks <- x_all_checks[!x_all_statuses]
  
  x_failed_reasons <- unlist(lapply(x_failed_checks, function(.x) {
    paste0(.x[["reason"]], collapse = "\n\n")
  }))
  x_failed_names <- names(x_failed_reasons)
  
  if(length(x_failed_checks)) {
    cli::cli_alert_danger("Not all standards satisfied.")
    cli::cli_h1("Failed Standards")
    for(fail_name in x_failed_names) {
      cli::cli_h2(fail_name)
      cli::cli_verbatim(x_failed_reasons[[fail_name]])
    }
    return(invisible(FALSE))
  } else {
    cli::cli_alert_success("All standards satisfied.")
    return(invisible(TRUE))
  }
  
}


# rout functions ----------------------------------------------------------


#' Determines if an R script should have an associated Rout file, then checks
#' the content of the Rout
#'
#' For any R file, there should either be an associated Rout file or
#' documentation in the header stating that the script is sourced elsewhere.
#' This checks that one and only one of these conditions is met. If there is an
#' associated Rout file, this also checks that the R version in the header
#' matches the R version in the Rout.
#'
#' @param x a scanned R script
#' @param path a \code{character} containing a directory path
#'
#' @return a named \code{list} with "pass" and "reason" elements
#'
#' @keywords internal
#'
check_rout <- function(x, path) {
  
  list_of_sections <- parse_header(path, c("version", "is_sourced"))
  header <- get_header(x)
  
  if(is.character(header)) {
    header_says_sourced <- list_of_sections[["is_sourced"]]
    header_version <- list_of_sections[["version"]]
  } else {
    header_says_sourced <- NULL
    header_version <- NULL
  }
  
  rout_path <- switch(
    tools::file_ext(path),
    "R" = paste0(path, "out"),
    "r" = paste0(path, ".Rout"),
    "Rmd" = paste0(tools::file_path_sans_ext(path), "-render.Rout"),
    "rmd" = paste0(tools::file_path_sans_ext(path), "-render.Rout"),
    "qmd" = paste0(tools::file_path_sans_ext(path), "-render.Rout")
  )
  
  rout_exists <- file.exists(rout_path)
  
  if(rout_exists) {
    header_source_matches_rout_source <- !isTRUE(header_says_sourced)
    rout_version_line <- scan(rout_path, character(), n = 1, sep = "\n", quiet = TRUE)
    rout_version <- gsub("R version ([0-9]\\.[0-9]\\.[0-9]).*", "\\1", rout_version_line)
    header_version_matches_rout_version <- header_version == rout_version
    
    statuses <- c(
      header_source_matches_rout_source = header_source_matches_rout_source,
      header_version_matches_rout_version = header_version_matches_rout_version
    )
    
    failure_reasons <- c(
      header_source_matches_rout_source = paste0(
        "The header and Rout do not agree regarding whether this script is sourced. ",
        "The header ", ifelse(header_says_sourced, "says", "does not say"), " that this script is sourced. ",
        "The Rout ", ifelse(rout_exists, "exists", "does not exist"), "."
      ),
      header_version_matches_rout_version = paste0(
        "The header and Rout version numbers do not match. ",
        "The header states this script is written for R version ", header_version,
        ". The Rout states this script was run in R version ", rout_version, "."
      )
    )
    
  } else if(is.character(header)) {
    
    header_source_matches_rout_source <- header_says_sourced
    
    statuses <- c(
      header_source_matches_rout_source = header_source_matches_rout_source
    )
    
    failure_reasons <- c(
      header_source_matches_rout_source =  paste0(
        "The header and Rout do not agree regarding whether this script is sourced. ",
        "The header ", ifelse(header_says_sourced, "says", "does not say"), " that this script is sourced. ",
        "The Rout ", ifelse(rout_exists, "exists", "does not exist"), "."
      )
    )
    
  } else {
    
    statuses <- c(
      has_header = FALSE,
      has_rout = FALSE
    )
    
    failure_reasons <- c(
      has_header = "No header found.",
      has_rout = "No associated Rout found."
    )
    
  }
  
  failures <- statuses[!statuses]
  
  if(length(failures) > 0) {
    return(list(pass = FALSE, reason = unname(failure_reasons[names(failures)])))
  } else {
    return(list(pass = TRUE, reason = NULL))
  }
  
}



# header functions --------------------------------------------------------

#' Check if an R script has an appropriate header
#'
#' This checks that the script has a header, defines a name that matches the
#' file path, has a copyright statement, and defines the script's purpose.
#'
#' @param x a scanned R script
#' @param path a \code{character} containing a directory path
#'
#' @return a named \code{list} with "pass" and "reason" elements
#'
#' @keywords internal
#'
check_header <- function(x, path) {
  
  list_of_sections <- parse_header(path, sections = c("name", "copyright", "purpose"))
  
  name <- list_of_sections[["name"]]
  has_name <- !is.null(name)
  path_matches_name <- if(has_name) {
    path_matches_name <- normalizePath(path, mustWork = FALSE) == normalizePath(name, mustWork = FALSE)
  } else {
    path_matches_name <- FALSE
  }
  
  copyright <- list_of_sections[["copyright"]]
  has_copyright <- !is.null(copyright)
  
  purpose <- list_of_sections[["purpose"]]
  has_purpose <- !is.null(purpose)
  
  
  statuses <- c(
    has_name = has_name,
    path_matches_name = path_matches_name,
    has_copyright = has_copyright,
    has_purpose = has_purpose
  )
  
  failure_reasons <- c(
    has_name = "No name found in header.",
    path_matches_name = "Name in header does not match the file path.",
    has_copyright = "No copyright statement found in header.",
    has_purpose = "No purpose found in header."
  )
  
  failures <- statuses[!statuses]
  
  if(length(failures) > 0) {
    return(list(pass = FALSE, reason = unname(failure_reasons[names(failures)])))
  } else {
    return(list(pass = TRUE, reason = NULL))
  }
  
}


# session_info functions --------------------------------------------------


#' Check if a scanned R script calls \code{\link[utils]{sessionInfo}} or
#' \code{\link[devtools]{session_info}} as the final call of the script
#'
#' @param x a scanned R script
#' @param path a \code{character} containing a directory path
#'
#' @return a named \code{list} with "pass" and "reason" elements
#'
#' @keywords internal
#'
check_session_info <- function(x, path) {
  x <- x[!grepl("^#", x)]
  last_line <- x[length(x)]
  last_line_session_info <- grepl("session_?[iI]nfo", last_line)
  if(last_line_session_info) {
    return(list(pass = TRUE, reason = NULL))
  }
  any_line_session_info <- any(grepl("session_?[iI]nfo", x))
  if(any_line_session_info) {
    return(list(
      pass = FALSE,
      reason = "sessionInfo or session_info is called, but is not the last command."))
  } else {
    return(list(pass = FALSE, reason = "No sessionInfo or session_info is called."))
  }
}
