#' Get the path of the RStudio source editor
#'
#' Calls \code{\link[rstudioapi]{getSourceEditorContext}} to obtain the source
#' context. The source editor context refers to the active file opened in
#' RStudio.
#'
#' @return returns the path of the source editor; fails if there is no file
#'   active in the source editor.
#' @export
get_source_file <- function() {
  
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
  
  return(file)
  
}
