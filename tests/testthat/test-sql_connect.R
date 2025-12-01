
library(testthat)

# -----------------------------------------------------------
test_that("sql_connect validates inputs", {
  # These tests confirm that invalid inputs for server or database
  # trigger the correct validation errors before any ODBC logic runs
  expect_error(sql_connect(server = NULL, database = "db"), "'server' must be")
  expect_error(sql_connect(server = "", database = "db"), "'server' must be")
  expect_error(sql_connect(server = "srv", database = NULL), "'database' must be")
  expect_error(sql_connect(server = "srv", database = ""), "'database' must be")
})
# -----------------------------------------------------------
test_that("sql_connect errors when no drivers available", {
  # Mock odbcListDrivers to return an empty data frame, simulating
  # a system with no ODBC drivers installed
  local_mocked_bindings(
    odbcListDrivers = function() data.frame(name = character())
  )

  # Expect the function to stop with an error about missing drivers
  expect_error(sql_connect("srv", "db"), "No ODBC drivers found")
})
# -----------------------------------------------------------
test_that("sql_connect returns valid connection when driver works", {
  # Create a fake connection object to simulate a successful DBI connection
  fake_con <- new.env()

  # Mock all dependencies:
  # - odbcListDrivers returns two drivers (simulate a normal system)
  # - dbConnect always returns fake_con (simulate success)
  # - dbIsValid returns TRUE only for fake_con (simulate a valid connection)
  local_mocked_bindings(
    odbcListDrivers = function() data.frame(name = c("SQL Server", "ODBC Driver 17 for SQL Server")),
    dbConnect       = function(...) fake_con,
    dbIsValid       = function(con) identical(con, fake_con)
  )

  # Execute the function under test with sample server/database values
  # Thanks to the mocks above, this will not touch the real ODBC system
  con <- sql_connect("srv", "db")

  # Verify the function returns exactly the fake connection object we provided
  expect_identical(con, fake_con)
})
# -----------------------------------------------------------
test_that("sql_connect fails when all drivers fail", {
  # Mock all dependencies:
  # - odbcListDrivers returns two drivers (simulate a normal system)
  # - dbConnect always returns NULL (simulate connection failure for all drivers)
  # - dbIsValid always returns FALSE (simulate invalid connection)
  local_mocked_bindings(
    odbcListDrivers = function() data.frame(name = c("SQL Server", "ODBC Driver 17 for SQL Server")),
    dbConnect       = function(...) NULL,
    dbIsValid       = function(con) FALSE
  )

  # With all drivers failing and validation returning FALSE, the function should error
  # with your final validation message
  expect_error(sql_connect("srv", "db"), "Failed to establish a SQL connection")
})
# -----------------------------------------------------------
test_that("sql_connect handles dbConnect errors gracefully", {
  # Mock dependencies:
  # - odbcListDrivers returns one driver (simulate normal system)
  # - dbConnect throws an error (simulate connection failure)
  # - dbIsValid returns FALSE (simulate invalid connection)
  local_mocked_bindings(
    odbcListDrivers = function() data.frame(name = "SQL Server"),
    dbConnect       = function(...) stop("Connection failed"),  # Simulate error
    dbIsValid       = function(con) FALSE
  )

  # Expect the function to catch the error via tryCatch and eventually fail
  # with the final validation message after exhausting all drivers
  expect_error(sql_connect("srv", "db"), "Failed to establish a SQL connection")
})


