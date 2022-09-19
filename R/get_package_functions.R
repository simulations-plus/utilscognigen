#' Get the names and/or arguments of the functions included in a package.
#'
#' @param package a package name as a \code{character}
#' @param unexported \code{logical} indicating whether to include unexported
#'   functions
#' @param invisible \code{logical} indicating whether to include unexported
#'   functions that start with a \code{"."}. Ignored unless \code{unexported} is
#'   \code{TRUE}
#'
#' @return
#' \code{get_package_functions} -- returns a \code{character} vector of function
#' names.
#'
#' \code{get_package_function_arguments} -- returns named \code{character} of
#' function arguments.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_package_functions("utilscognigen")
#' get_package_function_arguments("utilscognigen")
#' }
get_package_functions <- function(package, unexported = FALSE, invisible = FALSE) {
  
  # confirm package exists
  invisible(find.package(package))
  
  functions_exported <- getNamespaceExports(package)
  
  if(unexported) {
    functions_all <- ls(getNamespace(package), all.names = invisible)
    functions_unexported <- setdiff(functions_all, functions_exported)
    functions <- data.frame(
      fun = c(functions_exported, functions_unexported),
      exported = c(rep(TRUE, length(functions_exported)),
                   rep(FALSE, length(functions_unexported))),
      stringsAsFactors = FALSE
    )
    functions$colons <- vapply(
      X = functions$exported,
      FUN = function(exported) paste0(rep(":", 3 - exported), collapse = ""),
      FUN.VALUE = character(1)
    )
    
  } else {
    functions <- data.frame(
      fun = functions_exported,
      exported = TRUE,
      colons = "::",
      stringsAsFactors = FALSE
    )
  }
  
  functions$fun <- paste0(package, functions$colons, "`", functions$fun, "`")
  
  # only keep functions
  is_function <- vapply(
    X = functions$fun,
    FUN = function(x) {
      is.function(eval(parse(text = x)))
    },
    FUN.VALUE = logical(1)
  )
  
  functions <- functions[is_function, ]
  
  sort(gsub(".*:::?`(.*)`", "\\1", functions$fun))
  
}


#' @rdname get_package_functions
#' @keywords internal
get_package_function_arguments <- function(package, unexported = FALSE, invisible = FALSE) {
  
  functions <- get_package_functions(package, unexported, invisible)
  
  package_function_arguments <- sapply(
    X = functions,
    FUN = function(x) {
      .formals <- try(
        formals(args(eval(parse(text = paste0("`", x, "`"))))),
        silent = TRUE
      )
      if(inherits(.formals, "try-error")) {
        .formals <- try(
          formals(args(eval(parse(text = paste0(package, ":::`", x, "`"))))),
          silent = TRUE
        )
      }
      arg_names <- names(.formals)
      paste0(arg_names, collapse = ", ")
    }
  )
  
  package_function_arguments
  
}
