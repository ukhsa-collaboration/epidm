# Load libraries
library(testthat)
library(patrick)
library(data.table)

# Sample data for tests
episode_test <- structure(
  list(
    pat_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L),
    species = c(rep('E. coli',7),rep('K. pneumonia',7)),
    spec_type = c(rep('Blood',7),rep('Blood',4),rep('Sputum',3)),
    sp_date = structure(c(18262, 18263, 18281, 18282, 18262, 18263, 18281,
                          18265, 18270, 18281, 18283, 18259, 18260, 18281),
                        class = 'Date')
  ),
  row.names = c(NA, -14L), class = 'data.frame')

# Tests

test_that("group_time fails when 'date_start' column is missing", {
  expect_error(group_time(x = episode_test, date_start = "missing_col", window = 14, window_type = "static", group_vars = c("pat_id")), "Column")
})


test_that("group_time fails when 'date_end' column is missing", {
  expect_error(group_time(x = episode_test, date_start = "sp_date", date_end = "missing_col", window = 14, window_type = "static", group_vars = c("pat_id")), "Column")
})


test_that("group_time fails when some group_vars are missing", {
  expect_error(group_time(x = episode_test, date_start = "sp_date", window = 14, window_type = "static", group_vars = c("pat_id", "missing_col")), "Some group_vars not found")
})


test_that("group_time fails when 'window' is invalid", {
  expect_error(group_time(x = episode_test, date_start = "sp_date", window = -1, window_type = "static", group_vars = c("pat_id")), "window")
  expect_error(group_time(x = episode_test, date_start = "sp_date", window = "abc", window_type = "static", group_vars = c("pat_id")), "window")
})


test_that("group_time fails when 'window_type' is invalid", {
  expect_error(group_time(x = episode_test, date_start = "sp_date", window = 14, window_type = "invalid", group_vars = c("pat_id")), "window_type")
})


test_that("group_time fails when date_start column is not Date class", {
  bad_df <- episode_test
  bad_df$sp_date <- as.character(bad_df$sp_date)
  expect_error(group_time(x = bad_df, date_start = "sp_date", window = 14, window_type = "static", group_vars = c("pat_id")), "must be of class Date")
})


test_that("group_time warns and returns original when all start dates are NA", {
  na_df <- episode_test
  na_df$sp_date <- as.Date(NA)
  expect_warning(result <- group_time(x = na_df, date_start = "sp_date", window = 14, window_type = "static", group_vars = c("pat_id")), "No valid rows")
  expect_identical(result, na_df)
})

# Test renaming of columns
test_that('group_time renames columns correctly', {
  result <- group_time(x = episode_test,
                       date_start = 'sp_date',
                       window = 14,
                       window_type = 'static',
                       indx_varname = 'custom_indx',
                       min_varname = 'start_date',
                       max_varname = 'end_date',
                       group_vars = c('pat_id','species','spec_type'))

  expect_true('custom_indx' %in% names(result))
  expect_true('start_date' %in% names(result))
  expect_true('end_date' %in% names(result))
})


# Parameter combinations for testing
params <- expand.grid(
  window_type = c('rolling', 'static'),
  window = c(7, 14),
  .forceCopy = c(TRUE, FALSE)
)

with_parameters_test_that('group_time works with different window types and sizes',
                          .cases = params,
                          {
                            result <- group_time(x = episode_test,
                                                 date_start = 'sp_date',
                                                 window = window,
                                                 window_type = window_type,
                                                 indx_varname = 'episode_id',
                                                 group_vars = c('pat_id','species','spec_type'))

                            # Tests
                            expect_s3_class(result, 'data.table')
                            expect_true('episode_id' %in% names(result))
                            expect_false(any(is.na(result$episode_id)))
                          }
)
