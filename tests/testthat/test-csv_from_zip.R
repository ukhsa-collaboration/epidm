# tests/testthat/test-csv_from_zip.R
library(testthat)

# Helper to create a fake ZIP with given files
create_fake_zip <- function(files) {
  fake_zip <- tempfile(fileext = ".zip")
  zip(fake_zip, files = files)
  fake_zip
}

# Tests

test_that("csv_from_zip extracts CSV files from mocked ZIP", {
  tmp_dir <- tempdir()
  file1 <- file.path(tmp_dir, "file1.csv")
  file2 <- file.path(tmp_dir, "file2.csv")
  write.csv(data.frame(a = 1), file1, row.names = FALSE)
  write.csv(data.frame(b = 2), file2, row.names = FALSE)
  fake_zip <- create_fake_zip(c(file1, file2))

  local_mocked_bindings(
    download.file = function(url, destfile, ...) file.copy(fake_zip, destfile),
    .package = "utils"
  )

  result <- csv_from_zip("http://example.com/data.zip")
  expect_length(result, 2)
  expect_true(all(grepl("\\.csv$", result)))
})

test_that("csv_from_zip fails when ZIP has no CSV files", {
  tmp_dir <- tempdir()
  txt_file <- file.path(tmp_dir, "file1.txt")
  writeLines("hello", txt_file)
  fake_zip <- create_fake_zip(txt_file)

  local_mocked_bindings(
    download.file = function(url, destfile, ...) file.copy(fake_zip, destfile),
    .package = "utils"
  )

  expect_error(csv_from_zip("http://example.com/data.zip"),
               "No files matching pattern")
})

test_that("csv_from_zip fails on corrupted ZIP", {
  fake_zip <- tempfile(fileext = ".zip")
  writeLines("not a zip", fake_zip)

  local_mocked_bindings(
    download.file = function(url, destfile, ...) file.copy(fake_zip, destfile),
    .package = "utils"
  )

  expect_error(csv_from_zip("http://example.com/data.zip"),
               "Failed to read ZIP")
})

test_that("csv_from_zip handles download failure", {
  local_mocked_bindings(
    download.file = function(url, destfile, ...) stop("Simulated download error"),
    .package = "utils"
  )

  expect_error(csv_from_zip("http://example.com/data.zip"),
               "Download failed")
})

test_that("csv_from_zip handles case-insensitive CSV matching", {
  tmp_dir <- tempdir()
  file_upper <- file.path(tmp_dir, "DATA.CSV")
  write.csv(data.frame(x = 1), file_upper, row.names = FALSE)
  fake_zip <- create_fake_zip(file_upper)

  local_mocked_bindings(
    download.file = function(url, destfile, ...) file.copy(fake_zip, destfile),
    .package = "utils"
  )

  result <- csv_from_zip("http://example.com/data.zip")
  expect_true(any(grepl("DATA.CSV$", result)))
})

test_that("csv_from_zip returns all CSV files", {
  tmp_dir <- tempdir()
  files <- c(file.path(tmp_dir, "a.csv"), file.path(tmp_dir, "b.csv"))
  lapply(files, function(f) write.csv(data.frame(x = 1), f, row.names = FALSE))
  fake_zip <- create_fake_zip(files)

  local_mocked_bindings(
    download.file = function(url, destfile, ...) file.copy(fake_zip, destfile),
    .package = "utils"
  )

  result <- csv_from_zip("http://example.com/data.zip")
  expect_equal(length(result), 2)
  expect_setequal(basename(result), c("a.csv", "b.csv"))
})
