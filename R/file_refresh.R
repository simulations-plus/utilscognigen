#' Refresh the active file in the RStudio source editor without saving
#'
#' Files updated outside of RStudio do not refresh immediately, even with
#' \code{rstudioapi::navigateToFile()}. This closes then opens the active file
#' in the RStudio source editor.
#'
#' @return invisibly returns \code{NULL}
#' @keywords internal
file_refresh <- function() {
  context <- rstudioapi::getSourceEditorContext()
  id <- context$id
  path <- normalizePath(context$path)
  rstudioapi::documentClose(id, save = FALSE)
  Sys.sleep(.5)
  rstudioapi::navigateToFile(path)
  return(invisible(NULL))
}
