library(testthat)

# Utility function to reset the environment for each test
reset_tracking_env <- function() {
  .tracking_env$tracking_list <- NULL
  .tracking_env$tracking_count <- NULL
  .tracking_env$last_status <- "active"
}

# 1. Testing `case_tracker_add()` for case_list and case_count
test_that("case_tracker_add initializes a new tracking list with cases", {
  reset_tracking_env()
  case_tracker_add(c("case1", "case2"), type = "case_list")

  expect_true(!is.null(.tracking_env$tracking_list))
  expect_equal(nrow(.tracking_env$tracking_list), 2)
  expect_equal(.tracking_env$tracking_list$status, rep("active", 2))
  expect_equal(.tracking_env$tracking_list$case_id, c("case1", "case2"))
})

test_that("case_tracker_add initializes a new tracking count with initial count",
          {
            reset_tracking_env()
            case_tracker_add(234, type = "case_count")

            expect_true(!is.null(.tracking_env$tracking_count))
            expect_equal(nrow(.tracking_env$tracking_count), 1)
            expect_equal(.tracking_env$tracking_count$case_count, 234)
            expect_equal(.tracking_env$tracking_count$status, "initial import")
          })

# 2. Testing `case_tracker_update()` with checkpoint-based case removal tracking
test_that("case_tracker_update marks cases as dropped at specific checkpoints",
          {
            reset_tracking_env()

            # Step 1: Add initial cases
            initial_case_ids <- c("case1", "case2", "case3", "case4", "case5")
            case_tracker_add(initial_case_ids, type = "case_list")

            # Step 2: After cleaning, remove "case3"
            cleaned_case_ids <- setdiff(initial_case_ids, "case3")
            case_tracker_update(cleaned_case_ids, "after cleaning", type = "case_list")

            # Step 3: After merging, remove "case2"
            merged_case_ids <- setdiff(cleaned_case_ids, "case2")
            case_tracker_update(merged_case_ids, "after merging", type = "case_list")

            # Expectations
            expect_equal(nrow(.tracking_env$tracking_list), 5)

            # Verify correct statuses
            expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case1"], "active")
            expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case4"], "active")
            expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case5"], "active")
            expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case3"],
                         "Dropped at after cleaning")
            expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case2"],
                         "Dropped at after merging")
          })

test_that("case_tracker_update updates the case count at various pipeline stages",
          {
            reset_tracking_env()
            case_tracker_add(234, type = "case_count")
            case_tracker_update(230, "after cleaning", type = "case_count")
            case_tracker_update(215, "after merging", type = "case_count")

            expect_equal(nrow(.tracking_env$tracking_count), 3)
            expect_equal(.tracking_env$tracking_count$case_count, c(234, 230, 215))
            expect_equal(
              .tracking_env$tracking_count$status,
              c("initial import", "after cleaning", "after merging")
            )
          })

test_that("case_tracker_update warns if no specified cases are found in case list",
          {
            reset_tracking_env()
            case_tracker_add(c("case1", "case2"), type = "case_list")
            expect_warning(
              case_tracker_update("nonexistent_case", "excluded", type = "case_list"),
              "None of the specified cases were found"
            )
          })

# 3. Testing `case_tracker_report()` for case_list and case_count
test_that("case_tracker_report returns the correct tracking list", {
  reset_tracking_env()
  case_tracker_add(c("case1", "case2"), type = "case_list")
  report <- case_tracker_report(type = "case_list")

  expect_equal(nrow(report), 2)
  expect_equal(report$case_id, c("case1", "case2"))
  expect_equal(report$status, c("active", "active"))
})

test_that("case_tracker_report returns the correct case count tracking table",
          {
            reset_tracking_env()
            case_tracker_add(234, type = "case_count")
            case_tracker_update(230, "after cleaning", type = "case_count")

            report <- case_tracker_report(type = "case_count")
            expect_equal(nrow(report), 2)
            expect_equal(report$case_count, c(234, 230))
            expect_equal(report$status, c("initial import", "after cleaning"))
          })

test_that("case_tracker_report stops with an error if tracking list is not initialized",
          {
            reset_tracking_env()
            expect_error(case_tracker_report(type = "case_list"),
                         "Tracking list not initialized")
          })

test_that("case_tracker_report stops with an error if tracking count is not initialized",
          {
            reset_tracking_env()
            expect_error(case_tracker_report(type = "case_count"),
                         "Tracking count not initialized")
          })

# 4. Testing `case_tracker_save()` for case_list and case_count
test_that("case_tracker_save writes the tracking list to a CSV file", {
  reset_tracking_env()
  case_tracker_add(c("case1", "case2"), type = "case_list")
  temp_file <- tempfile(fileext = ".csv")

  case_tracker_save(temp_file, type = "case_list")

  saved_data <- read.csv(temp_file, stringsAsFactors = FALSE)
  expect_equal(saved_data$case_id, c("case1", "case2"))
  expect_equal(saved_data$status, c("active", "active"))

  # Clean up
  file.remove(temp_file)
})

test_that("case_tracker_save writes the tracking count to a CSV file", {
  reset_tracking_env()
  case_tracker_add(234, type = "case_count")
  case_tracker_update(230, "after cleaning", type = "case_count")
  temp_file <- tempfile(fileext = ".csv")

  case_tracker_save(temp_file, type = "case_count")

  saved_data <- read.csv(temp_file, stringsAsFactors = FALSE)
  expect_equal(saved_data$case_count, c(234, 230))
  expect_equal(saved_data$status, c("initial import", "after cleaning"))

  # Clean up
  file.remove(temp_file)
})

test_that("case_tracker_save stops with an error if tracking list is not initialized",
          {
            reset_tracking_env()
            temp_file <- tempfile(fileext = ".csv")
            expect_error(case_tracker_save(temp_file, type = "case_list"),
                         "Tracking list not initialized")
          })

test_that("case_tracker_save stops with an error if tracking count is not initialized",
          {
            reset_tracking_env()
            temp_file <- tempfile(fileext = ".csv")
            expect_error(case_tracker_save(temp_file, type = "case_count"),
                         "Tracking count not initialized")
          })
