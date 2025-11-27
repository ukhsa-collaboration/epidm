# tests/testthat/sql_clean.R
library(testthat)

# Tests

test_that("sql_clean handles empty input", {
  expect_error(sql_clean(NULL))
  expect_error(sql_clean(character(0)))
})

# Empty file
test_that("sql_clean handles empty file gracefully", {
  tmp_empty <- tempfile(fileext = ".sql")
  file.create(tmp_empty)
  expect_error(sql_clean(tmp_empty), "File is empty")
})

# Non-existent file
test_that("sql_clean errors on non-existent file", {
  fake_path <- tempfile(fileext = ".sql")
  expect_error(sql_clean(fake_path), "File does not exist")
})

test_that("sql_clean handles non-character input", {
  expect_error(sql_clean(123))
})

test_that("sql_clean removes comments", {
  sql <- c("SELECT * -- comment", "FROM table /* block comment */")
  expect_equal(sql_clean(sql), "SELECT * FROM table")
})

test_that("sql_clean removes multiline comments", {
  sql <- c(
    "SELECT *",
    "/* This is a",
    "multiline comment */",
    "FROM table"
  )
  expect_equal(sql_clean(sql), "SELECT * FROM table")
})

# Mixed vector with empty strings and comments
test_that("sql_clean handles mixed vector with empty strings and comments", {
  sql <- c("", "-- comment", "SELECT col1", "FROM table")
  expect_equal(sql_clean(sql), "SELECT col1 FROM table")
})

test_that("sql_clean reads and cleans SQL from a .sql file", {
  tmp_sql <- tempfile(fileext = ".sql")
  writeLines(c(
    "-- header comment",
    "SELECT col1, col2",
    "FROM my_table /* inline block comment */"
  ), tmp_sql)

  result_sql <- sql_clean(tmp_sql)
  expect_equal(result_sql, "SELECT col1, col2 FROM my_table")
})

test_that("sql_clean reads and cleans SQL from a .txt file", {
  tmp_txt <- tempfile(fileext = ".txt")
  writeLines(c(
    "/* block comment */",
    "SELECT col1",
    "FROM my_table -- trailing comment"
  ), tmp_txt)

  result_txt <- sql_clean(tmp_txt)
  expect_equal(result_txt, "SELECT col1 FROM my_table")
})

test_that("sql_clean handles a single SQL query as text", {
  sql <- "SELECT * FROM my_table -- trailing comment"
  expect_equal(sql_clean(sql), "SELECT * FROM my_table")
})

test_that("sql_clean combines character vector into single query", {
  sql <- c("SELECT col1", "FROM table", "WHERE col2 = 10")
  expect_equal(sql_clean(sql), "SELECT col1 FROM table WHERE col2 = 10")
})

test_that("sql_clean normalizes whitespace", {
  sql <- "SELECT    *    FROM   table"
  expect_equal(sql_clean(sql), "SELECT * FROM table")
})
