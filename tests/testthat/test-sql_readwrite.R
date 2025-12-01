library(testthat)
library(mockery)

# Tests for sql_read

test_that("sql_read fails when 'server' is missing or invalid", {
  expect_error(sql_read(database = "db", sql = "SELECT *"), "server")
  expect_error(sql_read(server = NULL, database = "db", sql = "SELECT *"), "server")
  expect_error(sql_read(server = "", database = "db", sql = "SELECT *"), "server")
  expect_error(sql_read(server = 123, database = "db", sql = "SELECT *"), "server")
})

test_that("sql_read fails when 'database' is missing or invalid", {
  expect_error(sql_read(server = "srv", sql = "SELECT *"), "database")
  expect_error(sql_read(server = "srv", database = NULL, sql = "SELECT *"), "database")
  expect_error(sql_read(server = "srv", database = "", sql = "SELECT *"), "database")
  expect_error(sql_read(server = "srv", database = 123, sql = "SELECT *"), "database")
})

test_that("sql_read fails when 'sql' is missing or invalid", {
  expect_error(sql_read(server = "srv", database = "db"), "sql")
  expect_error(sql_read(server = "srv", database = "db", sql = NULL), "sql")
  expect_error(sql_read(server = "srv", database = "db", sql = ""), "sql")
  expect_error(sql_read(server = "srv", database = "db", sql = 123), "sql")
})

test_that("sql_read returns a mocked data frame", {
  fake_con <- new.env()

  # Stub namespaced calls inside sql_read
  stub(sql_read, "epidm::sql_connect", function(server, database) fake_con)
  stub(sql_read, "epidm::sql_clean", function(sql) sql)
  stub(sql_read, "DBI::dbIsValid", function(conn) TRUE)
  stub(sql_read, "DBI::dbDisconnect", function(conn) TRUE)
  stub(sql_read, "DBI::dbGetQuery", function(conn, statement) data.frame(id = 1:3))

  # Call the function under test
  result <- sql_read("server", "db", "SELECT * FROM table")

  # Assertions
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

# Test for sql_write

test_that("sql_read parameter validation works", {
  expect_error(sql_read("", "db", "SELECT *"), "server")
  expect_error(sql_read("srv", "", "SELECT *"), "database")
  expect_error(sql_read("srv", "db", ""), "sql")
})

test_that("sql_write parameter validation works", {
  expect_error(sql_write(NULL, "srv", "db", "tbl"), "data.frame")
  expect_error(sql_write(data.frame(), "srv", "db", "tbl"), "at least one row")
  expect_error(sql_write(data.frame(a = 1), "", "db", "tbl"), "server")
  expect_error(sql_write(data.frame(a = 1), "srv", "", "tbl"), "database")
  expect_error(sql_write(data.frame(a = 1), "srv", "db", ""), "tablename")
})


test_that("sql_read returns mocked data frame", {
  fake_con <- new.env()
  stub(sql_read, "epidm::sql_connect", function(server, database) fake_con)
  stub(sql_read, "epidm::sql_clean", function(sql) sql)
  stub(sql_read, "DBI::dbIsValid", function(conn) TRUE)
  stub(sql_read, "DBI::dbDisconnect", function(conn) TRUE)
  stub(sql_read, "DBI::dbGetQuery", function(conn, statement) data.frame(id = 1:3))

  result <- sql_read("srv", "db", "SELECT * FROM table")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})


test_that("sql_write writes data and returns row count", {
  fake_con <- new.env()

  stub(sql_write, "epidm::sql_connect", function(server, database) fake_con)
  stub(sql_write, "DBI::dbIsValid", function(conn) TRUE)
  stub(sql_write, "DBI::dbDisconnect", function(conn) TRUE)
  stub(sql_write, "DBI::dbExistsTable", function(conn, name) FALSE)  # Force create table
  stub(sql_write, "DBI::dbWriteTable", function(...) TRUE)
  stub(sql_write, "DBI::dbGetQuery", function(conn, sql) data.frame(count = 5))  # Always return 5

  result <- sql_write(data.frame(a = 1:5), "srv", "db", "tbl")
  expect_equal(result, 5)
})

test_that("sql_write outputs correct success message", {
  fake_con <- new.env()

  # Stub all DBI calls
  stub(sql_write, "epidm::sql_connect", function(server, database) fake_con)
  stub(sql_write, "DBI::dbIsValid", function(conn) TRUE)
  stub(sql_write, "DBI::dbDisconnect", function(conn) TRUE)
  stub(sql_write, "DBI::dbExistsTable", function(conn, name) FALSE)
  stub(sql_write, "DBI::dbWriteTable", function(...) TRUE)
  stub(sql_write, "DBI::dbGetQuery", function(conn, sql) data.frame(count = 5))

  # Capture and validate message
  expect_message(
    sql_write(data.frame(a = 1:5), "srv", "db", "tbl"),
    regexp = "5 records written to \\[db\\]\\.\\[dbo\\]\\.\\[tbl\\] in"
  )
})








