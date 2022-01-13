#' Open files according to their extensions
#'
#' The files will be opened if possible. The behavior for files that cannot be
#' displayed in the RStudio source editor or a browser is determined by
#' \code{no_copies}. When \code{FALSE} (the default), a read-only copy will be
#' opened. When \code{TRUE}, the Windows file path is printed as a message.
#'
#' @param ... file paths to be opened
#' @param no_copies \code{logical} indicating whether to avoid downloading
#'   copies of files. If \code{TRUE} and some file cannot be opened by this
#'   system, the Windows path is printed as a message.
#'
#' @return invisibly returns \code{NULL}
#' @export
file_open <- function(..., no_copies = FALSE) {

  paths <- unlist(list(...))

  assertthat::assert_that(
    is.character(paths),
    is.logical(no_copies)
    )

  vapply(X = paths,
         FUN = .file_open,
         FUN.VALUE = logical(1),
         no_copies = no_copies
         )

}

# Open a single file
.file_open <- function(path, no_copies = FALSE) {

  if(!file.exists(path)) {
    cli::cli_alert_danger("File not found: '{path}'")
    return(FALSE)
  }

  assertthat::assert_that(
    is.character(path),
    length(path) == 1,
    is.logical(no_copies),
    .Platform$OS.type == "unix"
    )

  if(dir.exists(path)) {
    cli::cli_alert_danger("File is a directory: '{path}'")
    return(FALSE)
  }

  path <- normalizePath(path)

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
    cli::cli_alert_danger("Cannot open an editable version. Printing Windows path: '{path_to_windows(path)}'")
    return(FALSE)
  }

  # Check if file is binary and extension is not expected
  if(is_binary(path) && identical(open_function, rstudioapi::navigateToFile)) {
    cli::cli_alert_danger("File is binary and cannot be opened: '{path}'")
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
