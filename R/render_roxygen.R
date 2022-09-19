#' Render R documentation for roxygenized content
#'
#' @description
#' The main output file for each element of \code{...} contains links to each
#' help topic found in the file. This function is designed to render documentation
#' for functions documented with \pkg{roxygen2}.
#'
#' Note that the code in provided files will be executed. This function is not
#' intended to be used on entire programs used for analysis or data assembly. To
#' only render documentation for a highlighted selection, see the
#' \code{selected} argument.
#'
#' @param ... file paths to R files documented with \pkg{roxygen2}. Defaults to
#'   the path of the source editor context.
#' @param outdir existing directory to output files to
#' @param selected \code{logical} indicating whether to include selected content
#'   from the RStudio Source Editor. When \code{TRUE}, the path to the source
#'   editor context is not automatically rendered in full.
#' @param open \code{logical} indicating whether to open the output files
#'
#' @return a named \code{logical} vector. The names represent the output files
#'   while the value indicates whether the output file was successfully
#'   rendered.
#' @export
render_roxygen <- function(..., outdir = tempdir(), selected = FALSE, open = rstudioapi::isAvailable()) {
  
  files <- unlist(list(...))
  
  if(isTRUE(selected)) {
    
    assertthat::assert_that(rstudioapi::isAvailable())
    selection <- unlist(rstudioapi::getSourceEditorContext()[["selection"]])
    assertthat::assert_that("text" %in% names(selection))
    text <- unname(selection["text"])
    
    if(nchar(text) == 0) {
      cli::cli_abort(
        "No selection found. Only set {.code selected = TRUE} if content is selected in the RStudio Source Editor"
      )
    }
    
    selection_file <- file.path(outdir, "roxygen-selection.R")
    cat(text, file = selection_file)
    files <- c(files, selection_file)
    
  } else if(length(files) == 0) {
    
    files <- get_source_file()
    
  }
  
  assertthat::assert_that(
    all(file.exists(files)),
    dir.exists(outdir),
    all(tools::file_ext(files) %in% c("R", "r"))
  )
  
  html_files <- vapply(X = files,
                       FUN = function(.file, outdir) {
                         .output_file <- callr::r(func = .render_roxygen,
                                                  args = list(file = .file, outdir = outdir),
                                                  show = TRUE)
                         if(is.na(.output_file)) {
                           file.path(outdir, basename(.file))
                         } else {
                           .output_file
                         }
                       },
                       FUN.VALUE = character(1),
                       outdir = outdir)
  
  html_files_exist <- file.exists(html_files)
  names(html_files_exist) <- html_files
  
  if(open && rstudioapi::isAvailable()) {
    file_open(html_files[html_files_exist])
  }
  
  return(html_files_exist)
  
}


#' Render R documentation for a single file
#'
#' Credit for {roxygen2} parsing: Konrad Rudolph - https://stackoverflow.com/a/57990228/14525241
#'
#' @param file file path to an R file documented with \pkg{roxygen2}
#' @inheritParams render_roxygen
#'
#' @return path to a rendered html file containing hyperlinks to all
#' help topics
#'
#' @keywords internal
.render_roxygen <- function(file, outdir = tempdir()) {
  
  file_basename <- basename(tools::file_path_sans_ext(file))
  
  # this is the main output for file. documentation for each help topic will be
  # linked here
  single_html_outfile <- file.path(outdir,
                                   paste0(file_basename, ".html"))
  
  # subdirectory to contain each help topic
  outdir_sub <- file.path(outdir, file_basename)
  dir.create(outdir_sub, showWarnings = FALSE)
  
  # Use {roxygen2} to parse the file
  source_env <- try(roxygen2::env_file(file),
                    silent = TRUE)
  # Catch error in source file
  if(inherits(source_env, "try-error")) {
    warning("File produced the error below: '", file, "'\n", source_env)
    return(NA_character_)
  }
  rd_blocks <- roxygen2::parse_file(file, env = source_env)
  help_topics <- roxygen2::roclet_process(x = roxygen2::rd_roclet(),
                                          blocks = rd_blocks,
                                          env = source_env,
                                          base_path = dirname(file))
  
  # outputs an html file for each help topic in outdir_sub.
  # results in a list of lists of html hyperlink tags and breaks
  hyperlink_tags <- lapply(names(help_topics), function(htn) {
    
    ht <- help_topics[[htn]]
    htn <- tools::file_path_sans_ext(htn)
    
    outfile <- file.path(outdir_sub,
                         paste0(htn, ".html"))
    
    outfile_relative <- file.path(basename(outdir_sub), basename(outfile))
    
    rd_parsed <- tools::parse_Rd(textConnection(format(ht)))
    
    tools::Rd2HTML(Rd = rd_parsed,
                   out = outfile)
    
    list(htmltools::a(href = outfile_relative, htn),
         htmltools::br())
    
  })
  
  # unlist each list pair
  hyperlink_tags <- unlist(hyperlink_tags, recursive = FALSE)
  
  single_html <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$title(file_basename)),
    htmltools::tags$body(
      htmltools::h1("R Documentation"),
      hyperlink_tags
    ))
  
  htmltools::save_html(single_html, single_html_outfile)
  
  # return the path to the main output file
  return(single_html_outfile)
  
}
