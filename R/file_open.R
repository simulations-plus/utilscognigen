#' Open files according to their extensions or show a directory
#'
#' @description 
#' The files will be opened if possible. Up to one directory can be shown in the
#' RStudio Files pane. 
#' 
#' The behavior for files that cannot be displayed in the RStudio source editor
#' or a browser is determined by \code{no_copies}. When \code{FALSE} (the
#' default), a read-only copy will be opened. When \code{TRUE}, the Windows file
#' path is printed as a message.
#'
#' @param ... file paths to be opened.
#' @param no_copies \code{logical} indicating whether to avoid downloading
#'   copies of files. If \code{TRUE} and some file cannot be opened by this
#'   system, the Windows path is printed as a message.
#'
#' @return invisibly returns \code{NULL}.
#' @export
file_open <- function(..., no_copies = FALSE) {
  
  paths <- unique(unlist(list(...)))
  
  if(is.null(paths)) {
    cli::cli_alert_danger("No files provided.")
    return(invisible(NULL))
  }
  
  assertthat::assert_that(
    is.character(paths),
    is.logical(no_copies),
    testthat::is_testing() || interactive()
    )
  
  dir_paths <- paths[dir.exists(paths)]
  
  if(length(dir_paths) > 1) {
    cli::cli_warn(
      c(
        "Only one directory can be included in {.fn file_open}.",
        set_all_names(dir_paths, "!", inline_class = ".file")
      )
    )
    show_dirs <- FALSE
  } else if(length(dir_paths) == 1 && !rstudioapi::isAvailable()) {
    cli::cli_warn(
      c(
        "RStudio is not available. Cannot show directory:",
        set_all_names(dir_paths, "!", inline_class = ".file")
      )
    )
    show_dirs <- FALSE
  } else {
    show_dirs <- TRUE
  }

  vapply(X = paths,
         FUN = .file_open,
         FUN.VALUE = logical(1),
         no_copies = no_copies,
         show_dirs = show_dirs
         )
  
  return(invisible(NULL))

}

# Open a single file
.file_open <- function(path, no_copies = FALSE, show_dirs) {

  if(!file.exists(path)) {
    cli::cli_alert_danger("File not found: {.file {path}}")
    return(FALSE)
  }

  assertthat::assert_that(
    is.character(path),
    length(path) == 1,
    is.logical(no_copies),
    is.logical(show_dirs),
    length(show_dirs) == 1,
    .Platform$OS.type == "unix"
    )
  
  path <- fs::path_real(path)

  if(dir.exists(path) && show_dirs) {
    cli::cli_alert_info("Showing directory: {.file {path}}")
    rstudioapi::filesPaneNavigate(path)
    return(TRUE)
  } else if(dir.exists(path) && !show_dirs) {
    # not showing directories.
    # warning has already been output by file_open with all directories.
    return(FALSE)
  }

  file_ext <- tools::file_ext(path)

  # Function to use depends on file extension
  open_function <- switch(tolower(file_ext),
                          doc = ,
                          docx = ,
                          ppt = ,
                          pptx = ,
                          rtf = ,
                          xls = ,
                          xlsx = file.show,
                          xlsm = .xlsm_copy_open,

                          pdf = ,
                          png = ,
                          html = utils::browseURL,

                          rproj = rstudioapi::openProject,

                          rstudioapi::navigateToFile)

  if(no_copies && identical(open_function, file.show)) {
    cli::cli_alert_info(
      "Windows path: {.file {path_to_windows(path)}}"
    )
    return(TRUE)
  }

  # Check if file is binary and extension is not expected
  if(is_binary(path) && identical(open_function, rstudioapi::navigateToFile)) {
    cli::cli_alert_warning(
      "File is binary and cannot be opened: {.file {path}}"
    )
    return(FALSE)
  }

  do.call(open_function, list(path))

  return(TRUE)

}


# Helpers -----------------------------------------------------------------


# Credit to Spacedman: https://stackoverflow.com/a/16353581/14525241
is_binary <- function(path, max = 1000) {
  f <- file(path, "rb", raw = TRUE)
  b <- readBin(f, "int", max, size = 1, signed = FALSE)
  close(f)
  return(max(b) > 128)
}

# Copy xlsm file to xls in tempdir() then re-call .file_open
.xlsm_copy_open <- function(path) {

  assertthat::assert_that(
    length(path) == 1,
    tools::file_ext(path) == "xlsm"
  )

  xls_file <- file.path(
    tempdir(),
    paste0(tools::file_path_sans_ext(basename(path)), ".xls")
  )

  file_copy(path, xls_file)
  .file_open(xls_file)

}
