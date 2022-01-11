#' Change or print working directory
#'
#' These are straight wrappers around \code{\link[base]{setwd}} and
#' \code{\link[base]{getwd}}.
#' 
#' @inheritParams base::setwd
#'
#' @name wd_aliases
NULL
#> NULL

#' @rdname wd_aliases
#' @export
cd <- base::setwd

#' @rdname wd_aliases
#' @export
pwd <- base::getwd
