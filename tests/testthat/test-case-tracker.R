library(testthat)

# Utility function to reset the environment for each test
reset_tracking_env <- function() {
  .tracking_env$tracking_list <- NULL
  .tracking_env$tracking_count <- NULL
  .tracking_env$last_status <- "active"
}

# 1. Testing `case_tracker_add()` for custom initial status and additional cases
test_that("case_tracker_add initializes a new tracking list with custom status",
          {
            reset_tracking_env()
            case_tracker_add(c("case1", "case2"), status = "imported", type = "case_list")

            expect_true(!is.null(.tracking_env$tracking_list))
            expect_equal(nrow(.tracking_env$tracking_list), 2)
            expect_equal(.tracking_env$tracking_list$status, rep("imported", 2))
            expect_equal(.tracking_env$tracking_list$case_id, c("case1", "case2"))
          })

test_that("case_tracker_add allows adding new cases later with a different custom status",
          {
            reset_tracking_env()
            case_tracker_add(c("case1", "case2"), status = "initial import", type = "case_list")
            case_tracker_add(c("case3", "case4"), status = "additional cases", type = "case_list")

            expect_equal(nrow(.tracking_env$tracking_list), 4)
            expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case3"], "additional cases")
            expect_equal(.tracking_env$tracking_list$case_id,
                         c("case1", "case2", "case3", "case4"))
          })

test_that("case_tracker_add initializes a new tracking count with initial count",
          {
            reset_tracking_env()
            case_tracker_add(234, status = "initial import", type = "case_count")

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
            case_tracker_add(initial_case_ids, status = "imported", type = "case_list")

            # Step 2: After cleaning, remove "case3"
            cleaned_case_ids <- setdiff(initial_case_ids, "case3")
            case_tracker_update(cleaned_case_ids, "after cleaning", type = "case_list")

            # Step 3: After merging, remove "case2"
            merged_case_ids <- setdiff(cleaned_case_ids, "case2")
            case_tracker_update(merged_case_ids, "after merging", type = "case_list")

            # Expectations
            expect_equal(nrow(.tracking_env$tracking_list), 5)

            # Verify correct statuses
            expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case1"], "after merging")
            expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case4"], "after merging")
            expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case5"], "after merging")
            expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case3"],
                         "Dropped at after cleaning")
            expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case2"],
                         "Dropped at after merging")
          })

test_that("case_tracker_update adds new cases to the tracking list at a checkpoint",
          {
            reset_tracking_env()
            initial_case_ids <- c("case1", "case2")
            case_tracker_add(initial_case_ids, status = "initial import", type = "case_list")

            # New cases "case3" and "case4" appear at the checkpoint
            updated_case_ids <- c("case1", "case2", "case3", "case4")
            case_tracker_update(updated_case_ids, "data cleaning", type = "case_list")

            # Verify new cases are added with the checkpoint status
            expect_equal(nrow(.tracking_env$tracking_list), 4)
            expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case3"], "data cleaning")
            expect_equal(.tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == "case4"], "data cleaning")
          })

test_that("case_tracker_update updates the case count at various pipeline stages",
          {
            reset_tracking_env()
            case_tracker_add(234, status = "initial import", type = "case_count")
            case_tracker_update(230, "after cleaning", type = "case_count")
            case_tracker_update(215, "after merging", type = "case_count")

            expect_equal(nrow(.tracking_env$tracking_count), 3)
            expect_equal(.tracking_env$tracking_count$case_count, c(234, 230, 215))
            expect_equal(
              .tracking_env$tracking_count$status,
              c("initial import", "after cleaning", "after merging")
            )
          })

# 3. Testing `case_tracker_report()` for case_list and case_count
test_that("case_tracker_report returns the correct tracking list", {
  reset_tracking_env()
  case_tracker_add(c("case1", "case2"), status = "imported", type = "case_list")
  report <- case_tracker_report(type = "case_list")

  expect_equal(nrow(report), 2)
  expect_equal(report$case_id, c("case1", "case2"))
  expect_equal(report$status, c("imported", "imported"))
})

test_that("case_tracker_report returns the correct case count tracking table",
          {
            reset_tracking_env()
            case_tracker_add(234, status = "initial import", type = "case_count")
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
  case_tracker_add(c("case1", "case2"), status = "imported", type = "case_list")
  temp_file <- tempfile(fileext = ".csv")

  case_tracker_save(temp_file, type = "case_list")

  saved_data <- read.csv(temp_file, stringsAsFactors = FALSE)
  expect_equal(saved_data$case_id, c("case1", "case2"))
  expect_equal(saved_data$status, c("imported", "imported"))

  # Clean up
  file.remove(temp_file)
})

test_that("case_tracker_save writes the tracking count to a CSV file", {
  reset_tracking_env()
  case_tracker_add(234, status = "initial import", type = "case_count")
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
