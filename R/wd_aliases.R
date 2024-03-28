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
#' in the RStudio Files pane.
#'
#' @name wd_aliases
NULL
#> NULL

#' @rdname wd_aliases
#' @export
cd <- function(dir, focus = getOption("utilscognigen.cd_focus", TRUE)) {
  
  if(missing(dir)) {
    source_file <- tryCatch(
      get_source_file(), 
      finally = if(inherits(try(get_source_file(), silent = TRUE), "try-error")) {
        cli::cli_alert_info(
          "Provide a {.arg dir} or call {.fn cd} from RStudio with an open file to change the working directory."
        )
      }
    )
    dir <- dirname(source_file)
  }
  
  if(length(dir) > 1) {
    cli::cli_abort("Multiple {.arg dir} provided.")
  }
  
  # in case dir is actually a file, cd to the parent directory
  if(file.exists(dir) && !dir.exists(dir)) {
    dir <- dirname(dir)
  }
  
  prev_dir <- base::setwd(dir)
  
  if(rstudioapi::isAvailable() && isTRUE(focus)) {
    invisible(rstudioapi::filesPaneNavigate(dir))
    invisible(rstudioapi::executeCommand("activateFiles"))
  }
  
  cli::cli_alert_info("Working directory set: \n{.file {base::getwd()}}")
  
  invisible(prev_dir)
  
}

#' @rdname wd_aliases
#' @export
pwd <- function() {
  fs::path_real(base::getwd())
}
