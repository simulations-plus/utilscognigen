
#' Helper function for cli messages to set all names of x to some nm
#'
#' @param x vector
#' @param nm name
#'
#' @return named vector
#' @keywords internal
set_all_names <- function(x, nm) {
  assertthat::assert_that(
    is.vector(x),
    length(nm) == 1
  )
  stats::setNames(x, rep("x", length(x)))
}
