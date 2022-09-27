#' Set and open RStudio snippets
#'
#' @description
#' \code{set_rstudio_snippets()} combines multiple snippet files, then opens
#' any modified snippet configs. The default behavior is to copy all of the 
#' default RStudio snippet types as well as all recognized snippet files found in
#' the \code{\link{path_shared_code}()} directory.
#'
#' Files created by \code{\link[usethis]{edit_rstudio_snippets}()} and
#' \code{set_rstudio_snippets()} will *mask*, not supplement, the built-in
#' default snippets. If you like the built-in snippets, make sure they are
#' included with your custom snippets.
#' 
#' @param ... named \code{character} vectors of file paths to snippet files. The
#'   names may be any of the values of \code{type} (up to one argument per
#'   \code{type}).
#' @param include_rstudio_defaults \code{logical} indicating whether to include
#'   RStudio version 2022.2.3.492.3 default snippets.
#' @param include_shared_code \code{logical} indicating whether to include
#'   snippets found in a "snippets" directory under
#'   \code{\link{path_shared_code}()}.
#' @param include_ask_first \code{logical} indicating whether to ask which 
#'   files to include from RStudio default snippets and shared code snippets.
#'
#' @return path to the users snippet file(s), invisibly.
#'
#' @name snippets
#'  
NULL

#' @rdname snippets
#' @export
set_rstudio_snippets <- function(
    ...,
    include_rstudio_defaults = TRUE,
    include_shared_code = TRUE,
    include_ask_first = FALSE
  ) {
  
  assertthat::assert_that(
    is.logical(include_rstudio_defaults),
    is.logical(include_shared_code),
    is.logical(include_ask_first)
  )
  
  valid_types <- c(
    "r", "markdown", "c_cpp", "css", "html", "java", 
    "javascript", "python", "sql", "stan", "tex", "yaml"
  )
  valid_files <- fs::path_ext_set(valid_types, "snippets")
  
  # identify custom files provided by the user
  if(missing(...)) {
    # no custom paths provided
    custom_types <- NULL
    custom_snippets <- NULL
  } else {
    
    custom_pairs_list <- list(...)
    custom_types <- names(custom_pairs_list)
    
    if(is.null(custom_types)) {
      cli::cli_abort("Elements of {.arg ...} must be named.")
    } else if(any(names(custom_types) == "")) {
      cli::cli_abort("All elements of {.arg ...} must be named.")
    }
    
    custom_types <- tolower(custom_types)
    custom_types <- match.arg(custom_types, choices = valid_types, several.ok = TRUE)
    
    custom_snippets <- stats::setNames(custom_pairs_list, nm = custom_types)
    
  }
  
  # default RStudio snippets are stored in this package
  rstudio_snippets <- if(!isTRUE(include_rstudio_defaults)) {
    NULL
  } else {
    
    valid_default_rstudio_snippets <- system.file(
      file.path("rstudio", "snippets", valid_files), 
      package = "utilscognigen"
    )
    
    if(length(valid_default_rstudio_snippets) > 0) {
      
      if(isTRUE(include_ask_first)) {
        
        assertthat::assert_that(
          interactive()
        )
        
        utils::select.list(
          choices = valid_default_rstudio_snippets,
          multiple = TRUE,
          title = "Please choose which default RStudio snippets to include:"
        )
        
      } else {
        valid_default_rstudio_snippets
      }
      
    } else {
      NULL
    }
  }
  
  # shared code directory may or may not be set (path_shared_code() is not used
  # because it results in an error). this object could be NULL or one or more
  # file paths.
  shared_code_snippets <- if(!isTRUE(include_shared_code)) {
    NULL
  } else {
    
    path_shared_code <- getOption("utilscognigen.path_shared_code")
    
    if(is.null(path_shared_code)) {
      
      cli::cli_alert_warning(
        "The {.arg utilscognigen.path_shared_code} option is not set."
      )
      NULL
      
    } else if(!dir.exists(path_shared_code)) {
      
      cli::cli_alert_warning(
        "The {.arg utilscognigen.path_shared_code} option is set to a non-existing directory."
      )
      NULL
      
    } else {
      
      # directory exists, check for snippets files
      shared_code_snippet_files <- fs::dir_ls(
        path_shared_code, 
        recurse = TRUE, 
        type = "file", 
        regexp = paste0("(", paste0(valid_types, collapse = "|"), ")", "\\.snippets"), 
        ignore.case = TRUE
      )
      
      if(length(shared_code_snippet_files) == 0) {
        cli::cli_alert_warning(
          "The {.arg utilscognigen.path_shared_code} option is set to an existing directory, but no snippets files were identified."
        )
        NULL
      } else if(isTRUE(include_ask_first)) {
        
        assertthat::assert_that(
          interactive()
        )
        
        utils::select.list(
          choices = shared_code_snippet_files,
          multiple = TRUE,
          title = "Please choose which shared code snippets to include:"
        )
        
      } else {
        shared_code_snippet_files
      }
    }
  }
  
  all_type_snippets <- lapply(valid_types, function(type) {
    
    type_custom_snippets <- if(is.null(custom_snippets)) {
      NULL
    } else {
      custom_snippets[[type]]
    }
    
    type_rstudio_snippets <- if(is.null(rstudio_snippets)) {
      NULL
    } else {
      fs::path_filter(
        rstudio_snippets,
        regexp = paste0(type, "\\.snippets$"),
        ignore.case = TRUE
      )
    }
    
    type_shared_code_snippets <- if(is.null(shared_code_snippets)) {
      NULL
    } else {
      fs::path_filter(
        shared_code_snippets,
        regexp = paste0(type, "\\.snippets$"),
        ignore.case = TRUE
      )
    }
    
    type_snippets <- unname(c(
      type_custom_snippets,
      type_rstudio_snippets,
      type_shared_code_snippets
    ))
    
    if(is.null(type_snippets)) {
      return(NULL)
    }
    
    type_snippets_exist <- file.exists(type_snippets)
    if(any(!type_snippets_exist)) {
      cli::cli_warn(
        c(
          "{type} snippet file(s) do not exist: ",
          set_all_names(type_snippets[!type_snippets_exist], "!", inline_class = ".file")
        )
      )
      type_snippets <- type_snippets[type_snippets_exist]
    }
    
    type_snippets
    
  })
  
  names(all_type_snippets) <- valid_types
  
  type_snippets <- Filter(
    function(x) {
      if(is.null(x)) {
        FALSE
      } else if(length(x) == 0) {
        FALSE
      } else {
        TRUE
      }
    },
    all_type_snippets
  )
  
  # Snippet location changed in 1.3:
  # https://blog.rstudio.com/2020/02/18/rstudio-1-3-preview-configuration/
  new_rstudio <- !rstudioapi::isAvailable() || rstudioapi::getVersion() >= "1.3.0"
  old_dir <- fs::path_home_r(".R", "snippets")
  new_dir <- usethis:::rstudio_config_path("snippets")
  user_snippet_dir <- if (new_rstudio) new_dir else old_dir
  
  usethis:::create_directory(user_snippet_dir)
  
  # all files that will be written
  write_types <- names(type_snippets)
  write_files <- fs::path_abs(
    file.path(user_snippet_dir, paste0(write_types, ".snippets"))
  )
  names(write_files) <- write_types
  
  # RStudio and usethis::edit_rstudio_snippet behavior is to copy to new
  # location if you edit, but we ignore snippets in the old location here.
  
  drop_write_files <- c()
  
  for(type in write_types) {
    snippets <- unique(fs::path_abs(type_snippets[[type]]))
    write_file <- write_files[[type]]
    
    if(!file.exists(write_file)) {
      usethis::ui_done("New snippet file at {usethis::ui_path(write_file)}")
      usethis::ui_info(c(
        "This masks the default snippets for {usethis::ui_field(type)}.",
        "Delete this file and restart RStudio to restore the default snippets."
      ))
      overwrite <- TRUE
    } else {
      overwrite <- usethis::ui_yeah(c(
        "An existing {type} snippet file was identified: {usethis::ui_path(write_file)}",
        "Overwrite?"
      ), n_yes = 1, n_no = 1, shuffle = FALSE)
    }
    
    if(isTRUE(overwrite)) {
      cat("", file = write_file)
      lapply(snippets, function(snippet) {
        cat("# copied from", snippet, "\n\n", file = write_file, append = TRUE)
        file.append(write_file, snippet)
        cat("\n", file = write_file, append = TRUE)
      })
    } else {
      drop_write_files <- c(drop_write_files, write_file)
    }
    
  }
  
  write_files <- setdiff(write_files, drop_write_files)
  
  if(interactive()) {
    file_open(write_files)
  }
  
  invisible(write_files)
  
}
