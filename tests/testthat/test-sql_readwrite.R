library(testthat)

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
  # Fake connection object
  fake_con <- new.env()

  # Mock all dependencies used by sql_read:
  local_mocked_bindings(
    sql_connect  = function(server, database) fake_con,                # bypass real connection
    sql_clean    = function(sql) sql,                                  # return SQL unchanged
    dbGetQuery   = function(conn, statement) data.frame(id = 1:3),     # mock query result
    dbIsValid    = function(conn) TRUE,                                # always valid
    dbDisconnect = function(conn) TRUE                                 # prevent disconnect error
  )

  # Call the function under test
  result <- sql_read("server", "db", "SELECT * FROM table")

  # Assertions
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})


# Test for sql_write

test_that("sql_write fails when 'server' is missing or invalid", {
  expect_error(sql_write(x = data.frame(a = 1), database = "db", tablename = "tbl"), "server")
  expect_error(sql_write(x = data.frame(a = 1), server = NULL, database = "db", tablename = "tbl"), "server")
  expect_error(sql_write(x = data.frame(a = 1), server = "", database = "db", tablename = "tbl"), "server")
})

test_that("sql_write fails when 'database' is missing or invalid", {
  expect_error(sql_write(x = data.frame(a = 1), server = "srv", tablename = "tbl"), "database")
  expect_error(sql_write(x = data.frame(a = 1), server = "srv", database = NULL, tablename = "tbl"), "database")
  expect_error(sql_write(x = data.frame(a = 1), server = "srv", database = "", tablename = "tbl"), "database")
})

test_that("sql_write fails when 'tablename' is missing or invalid", {
  expect_error(sql_write(x = data.frame(a = 1), server = "srv", database = "db"), "tablename")
  expect_error(sql_write(x = data.frame(a = 1), server = "srv", database = "db", tablename = NULL), "tablename")
  expect_error(sql_write(x = data.frame(a = 1), server = "srv", database = "db", tablename = ""), "tablename")
})


test_that("sql_write errors when x is NULL", {
  expect_error(sql_write(x = NULL, server = "srv", database = "db", tablename = "tbl"),
               "Parameter 'x' must be a non-null data.frame.")
})

test_that("sql_write errors when x has zero rows", {
  expect_error(sql_write(x = data.frame(), server = "srv", database = "db", tablename = "tbl"),
    "Parameter 'x' must contain at least one row.")
})

test_that("sql_write uploads data successfully with mocked DB", {
  fake_con <- new.env()
  fake_data <- data.frame(id = 1:3)

  local_mocked_bindings(
    sql_connect    = function(server, database) fake_con,
    dbIsValid      = function(conn) TRUE,
    dbExistsTable  = function(conn, name) FALSE,
    dbGetQuery     = function(conn, statement) {
      if (grepl("COUNT", statement)) return(data.frame(`COUNT(*)` = 3))
    },
    dbWriteTable   = function(conn, name, value, ...) TRUE,
    dbDisconnect   = function(conn) TRUE
  )

  expect_message(
    sql_write(fake_data, "srv", "db", "tbl"),
    "records written"
  )
})







