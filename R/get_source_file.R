#' Get the path of the RStudio source editor or the R script being executed
#'
#' @description 
#' For interactive sessions: Calls
#' \code{\link[rstudioapi]{getSourceEditorContext}} to obtain the source
#' context. The source editor context refers to the active file opened in
#' RStudio.
#' 
#' For non-interactive sessions: Parses \code{\link[base]{commandArgs}} to 
#' obtain the name of the script being executed.
#'
#' @return returns the path of the source editor or the script being executed.
#' @export
get_source_file <- function() {
  
  # return NULL if testing package
  if(isTRUE(testthat::is_testing())) {
    return(invisible(NULL))
  }
  
  if(interactive()) {
    
    assertthat::assert_that(
      rstudioapi::isAvailable()
    )
    
    file <- rstudioapi::getSourceEditorContext()$path
    
    no_active_file <- "There is no active file in the RStudio source editor"
    
    if(is.null(file)) {
      cli::cli_abort(no_active_file)
    }
    
    if(file == "") {
      cli::cli_abort(no_active_file)
    }
    
  } else {
    
    args <- commandArgs()
    file <- args[grepl("\\.[rR]$", args)]
    
    # error handling
    if(length(file) == 0) {
      cli::cli_abort("No R script can be identified as the source file")
    } else if(length(file) > 1) {
      cli::cli_abort(c(
        "Multiple R scripts identified as source files:",
        file
      ))
    } else if(!file.exists(file)) {
      cli::cli_abort(c(
        "R script identified as source file does not exist:",
        file
      ))
    }
    
    # if the file name ends with, "-render.R", update to the Rmd file that 
    # the render program is intended to render.
    if(grepl("-render\\.[rR]$", file)) {
      rmd_file <- gsub("-render\\.[rR]$", ".Rmd", file)
      if(file.exists(rmd_file)) {
        file <- rmd_file
      }
    }
    
  }
  
  file
  
}
