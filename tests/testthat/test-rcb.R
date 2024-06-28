filename <- file.path(tempdir(), "test-rcb.R")

filename_with_spaces <- file.path(tempdir(), "test rcb spaces.R")

Redit(filename)

cat(
  "warning('rcb warning')\n",
  file = filename,
  append = TRUE
)

file.copy(filename, filename_with_spaces)

test_that("rcb executes", {
  expect_true(
    object = rcb(filename, as_job = FALSE)
  )
})

test_that("scanlogs identifies warning", {
  expect_match(
    object = scanlogs(filename)[[1]],
    regexp = "rcb warning",
    all = FALSE
  )
})

cat(
  "stop('rcb error')\n",
  file = filename,
  append = TRUE
)

test_that("rcb fails", {
  expect_false(
    object = rcb(filename, as_job = FALSE)
  )
})

test_that("scanlogs identifies error", {
  expect_match(
    object = scanlogs(filename)[[1]],
    regexp = "rcb error",
    all = FALSE
  )
})

test_that("rcb errors with spaces in file name", {
  expect_error(
    object = rcb(filename_with_spaces, as_job = FALSE)
  )
})
