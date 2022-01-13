#' Render an R Markdown document and open the output
#'
#' @description An R program is generated with the name of \code{path} sans
#' extension, appended with "-render.R". This contains a header and a call to
#' \code{rmarkdown::}\code{\link[rmarkdown]{render}}. This program is then
#' executed from its directory and the output file is optionally opened.
#'
#' The generated R program is not modified if it already exists, but the program
#' is examined to confirm it calls \code{rmarkdown::render}.
#'
#' @param path file path of R Markdown document. Defaults to the path of the
#'   source editor context.
#' @param open \code{logical} indicating whether to open the output file
#'
#' @return \code{logical} indicating whether the document was successfully
#'   rendered.
#' @export
#'
#' @examples
#' \dontrun{
#' render("markdown_doc.Rmd")
#' }
render <- function(path = NULL, open = rstudioapi::isAvailable()) {

  path <- if(is.null(path)) get_source_file() else path

  assertthat::assert_that(
    file.exists(path),
    tolower(tools::file_ext(path)) == "rmd",
    msg = "`path` must be an existing Rmd file"
  )

  assertthat::assert_that(
    is.logical(open),
    msg = "`open` must be logical"
  )

  path <- normalizePath(path)

  r_path <- paste0(tools::file_path_sans_ext(path), "-render.R")

  # If the generated R program already exists, confirm it calls `rmarkdown::render`
  # Otherwise, generate R program

  if(file.exists(r_path)) {

    r_path_read <- readLines(r_path)
    has_render_call <- any(grepl("rmarkdown::render", r_path_read))
    assertthat::assert_that(
      has_render_call,
      msg = paste0("The R program to be generated already exists and does not call `rmarkdown::render`: '", r_path, "'")
    )

  } else {

    Redit(r_path,
          purpose = paste0("Render ", path),
          input_files = FALSE,
          output_files = FALSE,
          open = FALSE)

    cat(paste0('\nrmarkdown::render("', path, '")\n'),
        file = r_path,
        append = TRUE)

  }

  execution_status <- unname(rcb(r_path))

  if(!execution_status) {
    return(FALSE)
  }

  if(open && rstudioapi::isAvailable()) {
    rout_path <- paste0(r_path, "out")
    rout <- readLines(rout_path)
    output <- rout[grepl("Output created:", rout)]
    output <- output[length(output)]
    output <- trimws(gsub("Output created:", "", output))
    output <- file.path(dirname(r_path), basename(output))

    if(file.exists(output)) {
      opened <- file_open(output)
      if(isTRUE(opened)) {
        cli::cli_alert_success("Opened output file: '{output}'")
      } else {
        cli::cli_alert_danger("Could not open output file: '{output}'")
      }
    }
  }

  return(execution_status)

}
