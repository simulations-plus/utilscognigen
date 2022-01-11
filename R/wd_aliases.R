#' Change or print working directory
#'
#' These are wrappers around \code{\link[base]{setwd}} and
#' \code{\link[base]{getwd}}.
#'
#' @inheritParams base::setwd
#'
#' @name wd_aliases
NULL
#> NULL

#' @rdname wd_aliases
#' @export
cd <- function(dir) {
  base::setwd(dir)
}

#' @rdname wd_aliases
#' @export
pwd <- function() {
  base::getwd()
}
