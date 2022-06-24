
#' Helper function for cli messages to set all names of x to some nm and
#' optionally style as inline markup
#'
#' @param x vector
#' @param nm name
#' @param inline_class an inline class. See \code{\link[cli]{inline-markup}}.
#'
#' @return named vector
#' @keywords internal
set_all_names <- function(x, nm, inline_class = NULL) {
  assertthat::assert_that(
    is.vector(x),
    length(nm) == 1,
    length(inline_class) <= 1
  )
  
  if(!is.null(inline_class)) {
    x <- paste0("{", inline_class, " ", x, "}")
  }
  
  stats::setNames(x, rep(nm, length(x)))
}
