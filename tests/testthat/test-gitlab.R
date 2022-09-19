test_that(
  "download a single file", 
  {
    accessible <- try(
      httr::http_error("https://gitlab.cognigencorp.com"),
      silent = TRUE
    )
    if(isTRUE(accessible)) {
      download_gitlab(
        "https://gitlab.cognigencorp.com/r/shared-code/-/blob/master/functions/sstat.R",
        "sstat.R"
      )
    } else {
      succeed()
    }
  }
)

test_that(
  "download a directory",
  {
    accessible <- try(
      httr::http_error("https://gitlab.cognigencorp.com"),
      silent = TRUE
    )
    if(isTRUE(accessible)) {
      download_gitlab(
        "https://gitlab.cognigencorp.com/r/shared-code/-/tree/master/functions",
        "functions"
      )
    } else {
      succeed()  
    }
  }
)
