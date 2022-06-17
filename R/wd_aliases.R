#' Change or print working directory
#'
#' @description 
#' 
#' These are wrappers around \code{\link[base]{setwd}} and
#' \code{\link[base]{getwd}}.
#' 
#' \code{cd()}, when called without any arguments, sets the working directory
#' to the directory of the source file.
#'
#' @inheritParams base::setwd
#' @param focus \code{logical} indicating whether to show the working directory
#' in the RStudio Files pane
#'
#' @name wd_aliases
NULL
#> NULL

#' @rdname wd_aliases
#' @export
cd <- function(dir, focus = TRUE) {
  
  if(missing(dir)) {
    source_file <- rstudioapi::documentPath()
    if(is.null(source_file)) {
      cli::cli_abort(c(
        "There is no active file in the RStudio source editor.",
        i = "Provide a {.code dir} or call {.fn cd} from RStudio with an open file to change the working directory."
      ))
    }
    dir <- dirname(source_file)
  }
  
  # in case dir is actually a file, cd to the parent directory
  if(file.exists(dir) && !dir.exists(dir)) {
    dir <- dirname(dir)
  }
  
  prev_dir <- base::setwd(dir)
  
  if(rstudioapi::isAvailable() && isTRUE(focus)) {
    invisible(rstudioapi::executeCommand("goToWorkingDir"))
  }
  
  invisible(prev_dir)
  
}

#' @rdname wd_aliases
#' @export
pwd <- function() {
  base::getwd()
}
