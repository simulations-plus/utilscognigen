#' Download trees (directories) and blobs (files) from GitLab using the GitLab
#' API
#'
#' @description \code{url} is converted to its GitLab API format. For tree
#'   input, the first 100 pages are downloaded. For blob input, the blob is
#'   downloaded. Version 4 of the GitLab API is used.
#'
#' @param url a single web URL pointing to a GitLab resource
#' @param destfile either a single non-existing directory or a single file.
#'   Defaults to the basename of the \code{from} file.
#'
#' @return \code{logical} indicating whether resources were successfully
#'   downloaded. This function does report success even if some resources are
#'   not successfully downloaded, with a warning for each resource that could
#'   not be downloaded.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Download a single file
#' download_gitlab(
#'   "https://gitlab.cognigencorp.com/r/shared-code/-/blob/master/functions/sstat.R",
#'   "sstat.R"
#' )
#'
#' # Download a directory
#' download_gitlab(
#'   "https://gitlab.cognigencorp.com/r/shared-code/-/tree/master/functions",
#'   "functions"
#' )
#' }
download_gitlab <- function(url, destfile = NULL) {
  
  assertthat::assert_that(
    is.character("url"),
    length(url) == 1,
    msg = "`url` must be a single character web URL"
  )
  
  if(is.null(destfile)) {
    destfile <- basename(url)
  }

  assertthat::assert_that(
    is.character(destfile),
    length(destfile) == 1L,
    msg = "`destfile` must be a single file path"
  )

  # Expand destfile
  destfile <- normalizePath(file.path(normalizePath(dirname(destfile), mustWork = FALSE),
                                      basename(destfile)),
                            mustWork = FALSE)

  # Convert url to GitLab api url and assign attributes
  api_url <- as_gitlab_api_url(url)

  curl_result <- curl_and_parse(api_url)

  # If the requested url is a tree, create needed directories and download each blob
  # download_gitlab_tree calls download_gitlab for each blob
  # This function returns early if the url is a tree

  if(attr(api_url, which = "is_tree")) {

    assertthat::assert_that(
      tools::file_ext(destfile) == "",
      !file.exists(destfile),
      msg = "If `url` points to a tree, `destfile` must be a non-existing directory"
    )

    download_gitlab_tree(api_url = api_url,
                         destfile = destfile,
                         curl_result = curl_result)

    return(file.exists(destfile))

  }

  assertthat::assert_that(
    "file_name" %in% names(curl_result),
    msg = "Unexpected structure of curl results"
  )
  
  # Try decoding content
  content <- try(
    jsonlite::base64_dec(curl_result$content), 
    silent = TRUE
  )
  
  # Warning and return FALSE if url cannot be decoded
  if(inherits(content, "try-error")) {
    cli::cli_warn("Content of {.arg url} could not be decoded: {.url {url}}")
    return(FALSE)
  }
  
  # Try converting to raw
  raw_content <- try(
    rawToChar(content), 
    silent = TRUE
  )
  
  # If converting to raw fails, we expect the content to be binary
  # Try writing decoded content as binary
  if(inherits(raw_content, "try-error")) {
    binary_written <- try(
      writeBin(
        object = content,
        con = destfile
      ),
      silent = TRUE
    )
    
    if(inherits(binary_written, "try-error")) {
      cli::cli_warn(
        "Content of {.arg url} could not be converted to text or saved as binary: {.url {url}}"
      )
    }
    
  } else {
    cat(raw_content, file = destfile)
  }
  
  return(file.exists(destfile))

}


# Helpers -----------------------------------------------------------------

#' Helper to \code{download_gitlab} to facilitate traversing through trees
#'
#' @param api_url a single GitLab API URL pointing to a tree
#' @param destfile a single non-existing directory
#' @param curl_result \code{list} result of the parsed \code{curl} call to
#'   \code{api_url}
#'
#' @return created needed directories, calls \code{download_gitlab} for each
#'   blob / file path pair, then invisibly returns \code{NULL}
#' @keywords internal
download_gitlab_tree <- function(api_url, destfile, curl_result) {

  branch <- attr(api_url, which = "branch")
  api_url_path <- attr(api_url, which = "file")
  repo_api_url <- gsub("(/repository/).*", "\\1", api_url)

  # trees are directories
  trees <- Filter(function(resource) resource$type == "tree",
                  curl_result)
  tree_paths <- unlist(lapply(trees, `[[`, "path"))
  tree_file_paths <- gsub(paste0(api_url_path, "?"), "", tree_paths)
  tree_file_paths <- gsub("^/", "", tree_file_paths)
  tree_file_paths <- file.path(destfile, tree_file_paths)

  # blobs are files
  blobs <- Filter(function(resource) resource$type == "blob",
                  curl_result)
  blob_paths <- unlist(lapply(blobs, `[[`, "path"))
  blob_file_paths <- gsub(paste0(api_url_path, "?"), "", blob_paths)
  blob_file_paths <- gsub("^/", "", blob_file_paths)
  blob_file_paths <- file.path(destfile, blob_file_paths)
  blob_api_paths <- gsub("/", "%2F", blob_paths)
  blob_api_urls <- file.path(repo_api_url,
                             "files",
                             paste0(blob_api_paths, "\\?ref\\=", branch))

  # Create directories

  dir.create(destfile)

  if(length(tree_file_paths) > 0) {
    dirs_created <- vapply(
      X = tree_file_paths,
      FUN = dir.create,
      FUN.VALUE = logical(1)
    )

    assertthat::assert_that(
      all(dirs_created),
      msg = "Not all trees could be created"
    )
  }

  # Call download_gitlab for each blob
  mapply(
    FUN = download_gitlab,
    url = blob_api_urls,
    destfile = blob_file_paths
  )

  return(invisible(NULL))

}


#' List GitLab projects, optionally subset to groups
#'
#' @param host GitLab API host to use
#' @param groups GitLab group name(s)
#'
#' @return JSON \code{list} of GitLab projects
#' @keywords internal
list_gitlab_projects <- function(host = "gitlab.cognigencorp.com", groups = NULL) {

  url <- file.path(host, "api/v4/projects?per_page=100")

  # Append https protocol if needed
  url <- gsub("^http://", "", url)
  if(!grepl("^https://", url)) {
    url <- paste0("https://", url)
  }

  assertthat::assert_that(
    !httr::http_error(url)
  )

  projects <- curl_and_parse(url)

  # Filter to projects in provided groups
  if(!is.null(groups)) {
    projects <- Filter(
      function(proj) {
        (proj$namespace$id %in% groups) |
          (toupper(proj$namespace$name) %in% toupper(groups))
      },
      projects)
  }

  return(projects)

}


#' Convert a GitLab URL to its API format
#'
#' The recursive option is set to \code{true} for trees
#'
#' @param url URL of a GitLab resource
#'
#' @return \code{character} URL with attributes for host, group, project,
#'   branch, file, and whether it is a tree (is_tree).
#' @keywords internal
as_gitlab_api_url <- function(url) {

  # Special case where url is already a GitLab api url
  if(is_gitlab_api_url(url)) {

    curl_result <- curl_and_parse(url)

    is_tree <- !("file_name" %in% names(curl_result))

    api_url <- structure(url,
                         is_tree = is_tree)

    return(api_url)

  }

  assertthat::assert_that(
    !httr::http_error(url),
    length(url) == 1L,
    msg = "`url` must be an accessbile URL with length 1"
  )

  # Append https protocol if needed
  url <- gsub("^http://", "", url)
  if(!grepl("^https://", url)) {
    url <- paste0("https://", url)
  }

  # Remove repeated forward slashes that do not follow a colon
  url <- gsub("(^:)/+", "/", url)
  # Remove trailing forward slashes
  url <- gsub("/$", "", url)

  is_tree <- grepl("/-/tree/", url)
  is_blob <- grepl("/-/blob/", url)
  is_gitlab <- grepl("gitlab", url)

  assertthat::assert_that(
    is_tree || is_blob,
    is_gitlab,
    msg = "`url` must be a GitLab URL and must be either a blob or a tree"
  )

  host <- gsub("(.*?\\.com).*", "\\1", url)
  group <- gsub(".*?\\.com/(.*?)/.*", "\\1", url)
  project <- gsub(".*?\\.com/.*?/(.*?)/.*", "\\1", url)
  project_id <- get_gitlab_project_id(host, group, project)

  if(is_tree) {

    branch <- gsub(".*/-/tree/(.*?)/?", "\\1", url)
    branch <- gsub("/.*", "", branch)
    file <- gsub(".*/-/tree/?", "", url)
    file <- gsub(paste0(branch, "/?"), "", file)

    end_of_url <- ifelse(nchar(file) > 0,
                         paste0("tree?path=", file, "\\&recursive=true\\&per_page=100"),
                         paste0("tree?recursive=true\\&per_page=100"))

    api_url <- file.path(host,
                         "api/v4/projects",
                         project_id,
                         "repository",
                         end_of_url)


  } else {

    branch <- gsub(".*/-/blob/(.*?)/.*", "\\1", url)
    file <- gsub(".*/-/blob/.*?/", "", url)
    file <- gsub("/", "%2F", file)

    api_url <- file.path(host,
                         "api/v4/projects",
                         project_id,
                         "repository/files",
                         paste0(file, "\\?ref\\=", branch))

  }

  api_url <- structure(api_url,
                       host = host,
                       group = group,
                       project = project,
                       branch = branch,
                       file = file,
                       is_tree = is_tree)

  return(api_url)
}


#' Check if a URL is a GitLab API URL
#'
#' @param url URL of a GitLab resource
#'
#' @return \code{logical}
#' @keywords internal
is_gitlab_api_url <- function(url) {

  assertthat::assert_that(
    is.character(url),
    length(url) == 1L
  )

  # Remove repeated forward slashes so regex is reliable
  url <- gsub("(^:)/+", "/", url)

  return(
    grepl("gitlab", url) && grepl("/api/v\\d/", url)
  )

}


#' Get a GitLab project id
#'
#' @param host GitLab API host to use
#' @param group GitLab group name
#' @param project GitLab project name
#'
#' @return \code{integer} project id
#' @keywords internal
get_gitlab_project_id <- function(host, group, project) {

  all_projects <- list_gitlab_projects(host = host, groups = group)
  this_project <- Filter(
    function(proj) {
      toupper(proj$name) == toupper(project)
    },
    all_projects
  )

  assertthat::assert_that(
    length(this_project) == 1L
  )

  return(this_project[[1]]$id)

}


#' Run the \code{curl} system command and parse the JSON result
#'
#' @param url URL of a GitLab resource
#'
#' @return \code{list} of information about the resource
#' @keywords internal
curl_and_parse <- function(url) {

  assertthat::assert_that(
    is.character(url),
    length(url) == 1L,
    msg = "`url` must be a character with length 1"
  )

  assertthat::assert_that(
    Sys.which("curl") != "",
    msg = "The curl system command must be available"
  )

  curl_result_json <- system2(command = "curl",
                              args = url,
                              stdout = TRUE,
                              stderr = FALSE)

  curl_result <- jsonlite::parse_json(curl_result_json)

  # Throw warning for message in curl_result
  # a 404 error might be present if there is a bug in the api formatting
  if("message" %in% names(curl_result)) {
    cli::cli_warn("{curl_result$message}: {.url {url}}")
  }

  return(curl_result)

}
