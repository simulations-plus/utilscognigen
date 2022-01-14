#' Get the path of the RStudio source editor
#'
#' Calls \code{\link[rstudioapi]{getSourceEditorContext}} to obtain the source
#' context.
#'
#' @return returns the path of the source editor; fails if there is no file
#'   active in the source editor.
#' @export
get_source_file <- function() {

  assertthat::assert_that(
    rstudioapi::isAvailable()
  )

  file <- rstudioapi::getSourceEditorContext()$path
  if(is.null(file)) {
      cli::cli_abort("There is no active file in the RStudio source editor")
  }
  return(file)
}
