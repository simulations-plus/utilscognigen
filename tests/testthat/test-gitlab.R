test_that("download a single file", {
  expect_true(
    download_gitlab(
      "https://gitlab.cognigencorp.com/r/shared-code/-/blob/master/functions/sstat.R",
      file.path(tempdir(), "sstat.R")
    )
  )
})

test_that("download a directory", {
  expect_true(
    download_gitlab(
      "https://gitlab.cognigencorp.com/r/shared-code/-/tree/master/functions",
      file.path(tempdir(), "functions")
    )
  )
})
