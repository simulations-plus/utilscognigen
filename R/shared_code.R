#' Interact with a shared code directory on the file system.
#'
#' @param from relative path of an R program in the shared code directory.
#' @inheritParams Rcopy
#'
#' @name shared_code
#' @examples
#' \dontrun{
#' # set the option that points to the shared code directory
#' options(utilscognigen.path_shared_code = "/path/to/shared-code")
#' 
#' # access the shared code directory path
#' path_shared_code()
#' 
#' # list files to see what's available
#' list_files_shared_code()
#' 
#' # or browse
#' browse_shared_code()
#'
#' # copy a program with a new name
#' Rcopy_shared_code("templates/qc/dm-striking-qc.R", "qc-my-dataset.R")
#'
#' # keep the original name
#' Rcopy_shared_code("templates/qc/dm-striking-qc.R")
#'
#' # shorthand matching of file names is also supported
#' Rcopy_shared_code("dm-striking-qc")
#'
#' # write to a different directory
#' Rcopy_shared_code("dm-striking-qc", "../asmbdat")
#'
#' }
NULL

#' @rdname shared_code
#' @export
path_shared_code <- function() {
  path_shared_code <- getOption("utilscognigen.path_shared_code")
  if(is.null(path_shared_code)) {
    cli::cli_abort(
      "The {.arg utilscognigen.path_shared_code} option is not set."
    )
  } else if(!dir.exists(path_shared_code)) {
    cli::cli_abort(
      "The {.arg utilscognigen.path_shared_code} option is set to a non-existing directory."
    )
  }
  path_shared_code
}


#' @rdname shared_code
#' @export
browse_shared_code <- function() {
  file_open(path_shared_code())
}


#' @rdname shared_code
#' @export
list_files_shared_code <- function() {
  fs::dir_tree(path_shared_code())
}

#' @rdname shared_code
#' @export
Rcopy_shared_code <- function(
    from, 
    to = NULL, 
    version = NULL, 
    copyright_holder = NULL, 
    open = rstudioapi::isAvailable()
    ) {
  
  # function to clean paths to standardize for matching
  .clean_files <- function(.files) {
    .files <- tolower(tools::file_path_sans_ext(.files))
    .files <- gsub("^/|^\\./", "", .files)
  }
  
  # function to ask user to choose between items
  .ask_user <- function(.items, .matching_what) {
    assertthat::assert_that(interactive())
    .choice <- utils::menu(.items,
                           title = paste("More than one matching", .matching_what,
                                         "found.\nPlease choose a file to copy:"))
    
    if(.choice == 0L) return(invisible(NULL))
    
    .choice
  }
  
  assertthat::assert_that(
    length(from) == 1L,
    is.character(from),
    from != "",
    msg = "`from` must be a non-blank character with length 1"
  )
  
  clean_from <- .clean_files(from)
  
  # get current state of directory (only keep R and Rmd files)
  paths <- list.files(
    path_shared_code(),
    pattern = "\\.[rR](md)?$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  paths_names <- tools::file_path_sans_ext(basename(paths))
  
  clean_paths <- .clean_files(paths)
  clean_names <- .clean_files(paths_names)
  
  clean_paths_match_lgl <- clean_from == clean_paths
  clean_names_match_lgl <- clean_from == clean_names
  
  paths_match <- paths[clean_paths_match_lgl]
  names_match <- paths[clean_names_match_lgl]
  
  # first check for exact path matches
  # then check for exact name matches
  if(length(paths_match)) {
    
    if(length(paths_match) > 1L) {
      choice <- .ask_user(paths_match, "file path")
      if(is.null(choice)) return(invisible(NULL))
      paths_match <- paths_match[[choice]]
    }
    
    final_path <- paths_match
    
  } else if(length(names_match)) {
    
    if(length(names_match) > 1L) {
      choice <- .ask_user(names_match, "file name")
      if(is.null(choice)) return(invisible(NULL))
      names_match <- names_match[[choice]]
    }
    
    final_path <- names_match
    
  } else {
    
    cli::cli_bullets(c(
      `!` = "No file found.",
      i = "Use {.code list_files_shared_code()} to display all files in the shared code directory."
    ))
    
    return(invisible(NULL))
    
  }
  
  Rcopy(
    from = final_path, 
    to = to, 
    version = version, 
    copyright_holder = copyright_holder, 
    open = open
  )
  
}
