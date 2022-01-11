test_that("download a single file", {
  download_gitlab(
    "https://gitlab.cognigencorp.com/r/shared-code/-/blob/master/functions/sstat.R",
    "sstat.R"
  )
})

test_that("download a directory", {
  download_gitlab(
    "https://gitlab.cognigencorp.com/r/shared-code/-/tree/master/functions",
    "functions"
  )
})
