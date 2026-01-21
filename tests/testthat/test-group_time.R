# Load libraries

library(testthat)
library(data.table)
library(patrick)

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


spell_test <- data.frame(
  id          = c(rep(99, 6), rep(88, 4), rep(3, 3)),
  provider    = c("YXZ", rep("ZXY", 5), rep("XYZ", 4), rep("YZX", 3)),
  spell_start = as.Date(c(
    "2020-03-01", "2020-07-07", "2020-02-08", "2020-04-28", "2020-03-15", "2020-07-01",
    "2020-01-01", "2020-01-12", "2019-12-25", "2020-03-28",
    "2020-01-01", NA, NA
  )),
  spell_end   = as.Date(c(
    "2020-03-10", "2020-07-26", "2020-05-22", "2020-04-30", "2020-05-20", "2020-07-08",
    "2020-01-23", "2020-03-30", "2019-01-02", "2020-04-20",
    "2020-01-01", NA, NA
  ))
)

# Tests

test_that("group_time fails when x is missing", {
  expect_error(
    group_time(date_start = "sp_date", window = 14, window_type = "static", group_vars = c("pat_id")),
    "x must be supplied"
  )
})

test_that("group_time fails when x is not a data.frame or data.table", {
  expect_error(
    group_time(x = 123, date_start = "sp_date", window = 14, window_type = "static", group_vars = c("pat_id")),
    "must be a data.frame or data.table"
  )
})

test_that("group_time fails when date_start argument is missing", {
  expect_error(
    group_time(x = episode_test, window = 14, window_type = "static", group_vars = c("pat_id")),
    "date_start must be supplied"
  )
})

test_that("group_time fails when 'date_start' column is missing", {
  expect_error(group_time(x = episode_test, date_start = "missing_col", window = 14, window_type = "static", group_vars = c("pat_id")), "Column")
})


test_that("group_time fails when 'date_end' column is missing", {
  expect_error(group_time(x = episode_test, date_start = "sp_date", date_end = "missing_col", window = 14, window_type = "static", group_vars = c("pat_id")), "Column")
})


test_that("group_time fails when group_vars argument is missing", {
  expect_error(
    group_time(x = episode_test, date_start = "sp_date", window = 14, window_type = "static"),
    "group_vars must be supplied"
  )
})

test_that("group_time fails when some group_vars are missing", {
  expect_error(group_time(x = episode_test, date_start = "sp_date", window = 14, window_type = "static", group_vars = c("pat_id", "missing_col")), "Some group_vars not found")
})

test_that("group_time fails when window is missing for event grouping", {
  expect_error(
    group_time(x = episode_test, date_start = "sp_date", window_type = "static", group_vars = c("pat_id")),
    "window parameter must be supplied"
  )
})

test_that("group_time fails when 'window' is invalid", {
  expect_error(group_time(x = episode_test, date_start = "sp_date", window = -1, window_type = "static", group_vars = c("pat_id")), "window")
  expect_error(group_time(x = episode_test, date_start = "sp_date", window = "abc", window_type = "static", group_vars = c("pat_id")), "window")
})

test_that("group_time fails when window_type is missing for event grouping", {
  expect_error(
    group_time(x = episode_test, date_start = "sp_date", window = 14, group_vars = c("pat_id")),
    "window_type must be specified as either rolling or static"
  )
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

params_events <- expand.grid(
  window_type = c("rolling", "static"),
  window = c(7, 14),
  forceCopy = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

patrick::with_parameters_test_that(
  "group_time groups events as for window_type={window_type}, window={window}, .forceCopy={forceCopy}",
  .cases = params_events,
  {

    input_x <- episode_test

    if (isTRUE(forceCopy)) {

      data.table::setDT(input_x)
      expect_true(data.table::is.data.table(input_x))
      input_names_before <- names(input_x)

    } else {

      input_x <- as.data.frame(input_x)
    }

    result <- group_time(
      x = input_x,
      date_start = "sp_date",
      window = window,
      window_type = window_type,
      indx_varname = "episode_id",
      group_vars = c("pat_id", "species", "spec_type"),
      .forceCopy = forceCopy
    )

    expect_s3_class(result, "data.table")
    expect_true("episode_id" %in% names(result))
    expect_false(any(is.na(result$episode_id)))

    if (isTRUE(forceCopy)) {
      expect_identical(names(input_x), input_names_before)
    } else {

      expect_true(data.table::is.data.table(input_x))
    }

    got_static <- NULL
    got_rolling <- NULL

    if (window_type == "static") {

      got_static <- result[!is.na(sp_date),
                           .(n_static = data.table::uniqueN(episode_id)),
                           by = .(pat_id, species, spec_type)
      ]

      static_key <- paste0("._last_static_w", window, "_fc", forceCopy)
      assign(static_key, got_static, envir = .GlobalEnv)

    } else {

      got_rolling <- result[!is.na(sp_date),
                            .(n_rolling = data.table::uniqueN(episode_id)),
                            by = .(pat_id, species, spec_type)
      ]

      rolling_key <- paste0("._last_rolling_w", window, "_fc", forceCopy)
      assign(rolling_key, got_rolling, envir = .GlobalEnv)

    }

    static_key <- paste0("._last_static_w", window, "_fc", forceCopy)
    rolling_key <- paste0("._last_rolling_w", window, "_fc", forceCopy)

    if (exists(static_key, envir = .GlobalEnv) &&
        exists(rolling_key, envir = .GlobalEnv)) {

      s <- get(static_key, envir = .GlobalEnv)
      r <- get(rolling_key, envir = .GlobalEnv)

      comp <- merge(s, r, by = c("pat_id", "species", "spec_type"), all = TRUE)

      expect_true(all(comp$n_rolling <= comp$n_static, na.rm = TRUE))

      rm(list = c(static_key, rolling_key), envir = .GlobalEnv)

    }


    assign("._last_static", got_static, envir = .GlobalEnv)
    assign("._last_rolling", got_rolling, envir = .GlobalEnv)
  }
)

test_that("group_time fails when date_end column is not Date class", {
  bad_df <- spell_test
  bad_df$spell_end <- as.character(bad_df$spell_end)
  expect_error(
    group_time(
      x = bad_df,
      date_start = "spell_start",
      date_end = "spell_end",
      group_vars = c("id", "provider")
    ),
    "must be of class Date"
  )
})

test_that("group_time groups overlapping intervals when date_end is provided", {
  res <- group_time(
    x = spell_test,
    date_start = "spell_start",
    date_end   = "spell_end",
    group_vars = c("id", "provider"),
    indx_varname = "spell_id",
    min_varname  = "spell_min_date",
    max_varname  = "spell_max_date"
  )

  expect_s3_class(res, "data.table")
  expect_true(all(c("spell_id", "spell_min_date", "spell_max_date") %in% names(res)))
  expect_false("indx" %in% names(res))
  expect_false("date_min" %in% names(res))
  expect_false("date_max" %in% names(res))

  got <- res[!is.na(spell_start),
             .(n_episodes = data.table::uniqueN(spell_id)),
             by = .(id, provider)][order(id, provider)]

  exp <- data.table::data.table(
    id = c(3, 88, 99, 99),
    provider = c("YZX", "XYZ", "YXZ", "ZXY"),
    n_episodes = c(1L, 2L, 1L, 2L)
  )
  data.table::setorder(exp, id, provider)
  expect_equal(got, exp)

  nas <- res[is.na(spell_start)]
  expect_true(all(is.na(nas$spell_id)))
})

test_that("group_time ignores window and window_type when date_end is provided", {
  res_base <- group_time(
    x = spell_test,
    date_start = "spell_start",
    date_end = "spell_end",
    group_vars = c("id", "provider"),
    indx_varname = "spell_id",
    min_varname = "spell_min_date",
    max_varname = "spell_max_date"
  )

  expect_warning(
    res_extra <- group_time(
      x = spell_test,
      date_start = "spell_start",
      date_end = "spell_end",
      window = 14,
      window_type = "static",
      group_vars = c("id", "provider"),
      indx_varname = "spell_id",
      min_varname = "spell_min_date",
      max_varname = "spell_max_date"
    ),
    "ignored"
  )

  data.table::setorder(res_base, id, provider, spell_start)
  data.table::setorder(res_extra, id, provider, spell_start)

  expect_equal(res_extra, res_base)
})

