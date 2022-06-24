#' Identify and open R, Rmd, SAS, tex, Rnw log files
#'
#' @param ... file paths to retrieve the log files of. Defaults to the path of
#'   the source editor context.
#' @param open \code{logical} indicating whether to open files in RStudio.
#'
#' @return file paths to log files.
#'
#' @export
logfile <- function(..., open = TRUE) {

  paths <- unlist(list(...))
  paths <- if(is.null(paths)) get_source_file() else paths
  
  paths_exist <- file.exists(paths)
  if(any(!paths_exist)) {
    cli::cli_abort(
      c(
        "File(s) do not exist: ",
        set_all_names(paths[!paths_exist], "x")
      )
    )
  }
  
  assertthat::assert_that(
    is.character(paths),
    all(file.exists(paths)),
    is.logical(open)
  )

  log_paths <- mapply(function(path, extension, open) {
    switch(
      extension,
      R = logfile_R(path, open = open),
      r = logfile_r(path, open = open),
      Rmd = logfile_Rmd(path, open = open),
      sas = logfile_sas(path, open = open),
      tex = logfile_tex(path, open = open),
      Rnw = logfile_Rnw(path, open = open)
    )
  },
  path = paths,
  extension = tools::file_ext(paths),
  open = open)
  
  log_paths <- unlist(log_paths)
  
  if(open) {
    invisible(log_paths)
  } else {
    log_paths
  }
  
}


# helpers -----------------------------------------------------------------

logfile_R <- function(path, open) {
  log_name <- paste0(tools::file_path_sans_ext(path), ".Rout")
  if(!file.exists(log_name)) {
    code <- paste0("rcb ", basename(path))
    cli::cli_warn(
      c(
        "{.file {log_name}} has not been created.",
        i = "Generate a log file with {.fn rcb} from the R console or {.code {code}} from the Terminal within the script directory."
      )
    )
    return(NA_character_)
  }
  
  if(rstudioapi::isAvailable() && open) {
    rstudioapi::navigateToFile(log_name)
  }
  
  invisible(log_name)
}

logfile_r <- function(path, open) {
  log_name <- paste0(path, ".Rout")
  if(!file.exists(log_name)) {
    code <- paste0("rcb ", basename(path))
    cli::cli_warn(
      c(
        "{.file {log_name}} has not been created.",
        i = "Generate a log file with {.fn rcb} from the R console or {.code {code}} from the Terminal within the script directory."
      )
    )
    return(NA_character_)
  }
  
  if(rstudioapi::isAvailable() && open) {
    rstudioapi::navigateToFile(log_name)
  }
  
  invisible(log_name)
}

logfile_Rmd <- function(path, open) {
  log_name <- paste0(tools::file_path_sans_ext(path), "-render.Rout")
  if(!file.exists(log_name)) {
    cli::cli_warn(
      c(
        "{.file {log_name}} has not been created.",
        i = "Generate a log file with {.fn render} from the R console."
      )
    )
    return(NA_character_)
  }
  
  if(rstudioapi::isAvailable() && open) {
    rstudioapi::navigateToFile(log_name)
  }
  
  invisible(log_name)
}

logfile_sas <- function(path, open) {
  log_name <- paste0(tools::file_path_sans_ext(path), c(".log", ".lst"))
  log_exists <- file.exists(log_name)
  if(!log_exists[1]) {
    code <- paste0("sas94 +we ", basename(path))
    cli::cli_warn(
      c(
        "{.file {log_name[1]}} has not been created.",
        i = "Generate a log file with {.code {code}} from the Terminal within the script directory."
      )
    )
    return(NA_character_)
  }
  log_name <- log_name[log_exists]
  
  if(rstudioapi::isAvailable() && open) {
    invisible(lapply(log_name, rstudioapi::navigateToFile))
  }
  
  invisible(log_name)
}

logfile_tex <- function(path, open) {
  log_name <- paste0(tools::file_path_sans_ext(path), ".log")
  if(!file.exists(log_name)) {
    cli::cli_warn(
      c(
        "{.file {log_name}} has not been created."
      )
    )
    return(NA_character_)
  }
  
  if(rstudioapi::isAvailable() && open) {
    rstudioapi::navigateToFile(log_name)
  }
  
  invisible(log_name)
}

logfile_Rnw <- logfile_tex
