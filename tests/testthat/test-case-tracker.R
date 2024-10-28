library(testthat)

# Ensure each test starts with a clean tracking environment
reset_tracking_env <- function() {
  .tracking_env$tracking_list <- NULL
}

# 1. Testing `case_tracker_add()`
test_that("case_tracker_add initializes a new tracking list with cases", {
  reset_tracking_env()
  case_tracker_add(c("case1", "case2"))

  expect_true(!is.null(.tracking_env$tracking_list))
  expect_equal(nrow(.tracking_env$tracking_list), 2)
  expect_equal(.tracking_env$tracking_list$status, rep("active", 2))
  expect_equal(.tracking_env$tracking_list$case_id, c("case1", "case2"))
})

test_that("case_tracker_add handles data frames with case_id column", {
  reset_tracking_env()
  cases_df <- data.frame(case_id = c("case4", "case5"))
  case_tracker_add(cases_df)

  expect_equal(nrow(.tracking_env$tracking_list), 2)
  expect_equal(.tracking_env$tracking_list$case_id, c("case4", "case5"))
})

# 2. Testing `case_tracker_update()`
test_that("case_tracker_update updates the status of specified cases", {
  reset_tracking_env()
  case_tracker_add(c("case1", "case2", "case3"))
  case_tracker_update(c("case1", "case3"), "processed")

  expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case1"], "processed")
  expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case3"], "processed")
  expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case2"], "active") # Unchanged
})

test_that("case_tracker_update warns if no specified cases are found", {
  reset_tracking_env()
  case_tracker_add(c("case1", "case2"))
  expect_warning(
    case_tracker_update("nonexistent_case", "excluded"),
    "None of the specified cases were found"
  )
})

# 3. Testing `case_tracker_report()`
test_that("case_tracker_report returns the correct tracking list", {
  reset_tracking_env()
  case_tracker_add(c("case1", "case2"))
  report <- case_tracker_report()

  expect_equal(nrow(report), 2)
  expect_equal(report$case_id, c("case1", "case2"))
  expect_equal(report$status, c("active", "active"))
})

test_that("case_tracker_report stops with an error if tracking list is not initialized",
          {
            reset_tracking_env()
            expect_error(case_tracker_report(), "Tracking list not initialized")
          })

# 4. Testing `case_tracker_save()`
test_that("case_tracker_save writes the tracking list to a CSV file", {
  reset_tracking_env()
  case_tracker_add(c("case1", "case2"))
  temp_file <- tempfile(fileext = ".csv")

  case_tracker_save(temp_file)

  saved_data <- read.csv(temp_file, stringsAsFactors = FALSE)
  expect_equal(saved_data$case_id, c("case1", "case2"))
  expect_equal(saved_data$status, c("active", "active"))

  # Clean up
  file.remove(temp_file)
})

test_that("case_tracker_save stops with an error if tracking list is not initialized",
          {
            reset_tracking_env()
            temp_file <- tempfile(fileext = ".csv")
            expect_error(case_tracker_save(temp_file),
                         "Tracking list not initialized")
          })
