filename <- file.path(tempdir(), "test-rcb.R")


Redit(filename)

cat(
  "warning('rcb warning')\n",
  file = filename,
  append = TRUE
)

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
