library(testthat)
library(data.table)
library(patrick)

# Function helper to utilise throughout tests

run_proxy <- function(df,
                      .dropTmp = TRUE,
                      .forceCopy = FALSE) {
  proxy_episode_dates(
    x = df,
    group_vars = c("id", "provider"),
    spell_start_date = "spell_start",
    spell_end_date = "spell_end",
    discharge_destination = "disdest",
    .dropTmp = .dropTmp,
    .forceCopy = .forceCopy
  )
}


# Tests

test_that("proxy_episode_dates returns a data.table", {
  df <- data.frame(id = 1, provider = "A", spell_start = Sys.Date(), spell_end = Sys.Date(), disdest = 19)
  result <- proxy_episode_dates(df,
                             group_vars = c("id","provider"),
                             spell_start_date = "spell_start",
                             spell_end_date = "spell_end",
                             discharge_destination = "disdest")

  expect_true(is.data.table(result))
})


patrick::with_parameters_test_that("proxy_missing flags and date replacements behave correctly:", {

  result <- run_proxy(test_df, .dropTmp = FALSE)

  expect_equal(result$proxy_missing, expected_proxy_missing)

  if (!is.null(expected_spell_start)) {
    expect_equal(result$spell_start, expected_spell_start)
  }
  if (!is.null(expected_spell_end)) {
    expect_equal(result$spell_end, expected_spell_end)
  }

}, cases(
  unchanged_flag_0 = list(
    test_df = data.frame(
      id = c(1, 1),
      provider = c("A", "A"),
      spell_start = as.Date(c("2020-01-01", "2020-02-01")),
      spell_end = as.Date(c("2020-01-02", "2020-02-02")),
      disdest = c(19, 19)
    ),
    expected_proxy_missing = c(0, 0),
    expected_spell_start = as.Date(c("2020-01-01", "2020-02-01")),
    expected_spell_end = as.Date(c("2020-01-02", "2020-02-02"))
  ),

  missing_end_not_last_discharged_flag_3 = list(
    test_df = data.frame(
      id = c(1, 1),
      provider = c("A", "A"),
      spell_start = as.Date(c("2020-01-01", "2020-02-01")),
      spell_end = as.Date(c(NA, "2020-02-02")),
      disdest = c(19, 19)
    ),
    expected_proxy_missing = c(3, 0),
    expected_spell_start = as.Date(c("2020-01-01", "2020-02-01")),
    expected_spell_end = as.Date(c("2020-01-31", "2020-02-02"))
  ),

  missing_end_not_last_in_hospital_flag_2 = list(
    test_df = data.frame(
      id = c(1, 1),
      provider = c("A", "A"),
      spell_start = as.Date(c("2020-01-01", "2020-02-01")),
      spell_end = as.Date(c(NA, "2020-02-02")),
      disdest = c(51, 19)
    ),
    expected_proxy_missing = c(2, 0),
    expected_spell_start = as.Date(c("2020-01-01", "2020-02-01")),
    expected_spell_end = as.Date(c("2020-02-01", "2020-02-02"))
  ),

  missing_end_last_episode_flag_1 = list(
    test_df = data.frame(
      id = c(1, 1),
      provider = c("A", "A"),
      spell_start = as.Date(c("2020-01-01", "2020-02-01")),
      spell_end = as.Date(c("2020-01-02", NA)),
      disdest = c(19, 19)
    ),
    expected_proxy_missing = c(0, 1),
    expected_spell_start = as.Date(c("2020-01-01", "2020-02-01")),
    expected_spell_end = NULL
  ),

  overlap_swap_flag_4 = list(
    test_df = data.frame(
      id = 1,
      provider = "A",
      spell_start = as.Date("2020-02-01"),
      spell_end = as.Date("2020-01-31"),
      disdest = 19
    ),
    expected_proxy_missing = 4,
    expected_spell_start = as.Date("2020-01-31"),
    expected_spell_end = as.Date("2020-02-01")
  )
))

test_that("flag 1 replaces end date with Sys.Date", {
  df <- data.frame(
    id = c(1, 1),
    provider = c("A", "A"),
    spell_start = as.Date(c("2020-01-01", "2020-02-01")),
    spell_end = as.Date(c("2020-01-02", NA)),
    disdest = c(19, 19)
  )
  result <- run_proxy(df, .dropTmp = FALSE)
  expect_equal(result$proxy_missing[2], 1)
  expect_equal(result$spell_end[2], Sys.Date())
})

test_that("end dates are replaced correctly", {
  df <- data.frame(
    id = c(1,1),
    provider = "A",
    spell_start = as.Date(c("2020-01-01","2020-02-01")),
    spell_end   = as.Date(c(NA, "2020-02-02")),
    disdest     = c(19,19)
  )


  result <- proxy_episode_dates(df,
                             group_vars = c("id","provider"),
                             spell_start_date = "spell_start",
                             spell_end_date = "spell_end",
                             discharge_destination = "disdest")

  expect_equal(result$spell_end[1], as.Date("2020-01-31"))
})


test_that("unsorted input is ordered before proxying end dates", {
  df <- data.frame(
    id = c(1, 1),
    provider = c("A", "A"),
    spell_start = as.Date(c("2020-02-01", "2020-01-01")),  # reversed
    spell_end = as.Date(c("2020-02-02", NA)),
    disdest = c(19, 19)
  )
  result <- run_proxy(df, .dropTmp = FALSE)

  expect_equal(result$spell_start[1], as.Date("2020-01-01"))
  expect_equal(result$proxy_missing[1], 3)
  expect_equal(result$spell_end[1], as.Date("2020-01-31"))
})

test_that(".dropTmp TRUE removes tmp columns and .dropTmp FALSE keeps them", {
  df <- data.frame(
    id = c(1, 1),
    provider = c("A", "A"),
    spell_start = as.Date(c("2020-01-01", "2020-02-01")),
    spell_end = as.Date(c(NA, "2020-02-02")),
    disdest = c(19, 19)
  )

  result_drop <- run_proxy(df, .dropTmp = TRUE)
  expect_false(any(grepl("^tmp\\.", names(result_drop))))

  result_keep <- run_proxy(df, .dropTmp = FALSE)
  expect_true(any(grepl("^tmp\\.", names(result_keep))))
})

test_that(".forceCopy behaves as expected", {
  df <- data.frame(
    id = c(1, 1),
    provider = c("A", "A"),
    spell_start = as.Date(c("2020-01-01", "2020-02-01")),
    spell_end = as.Date(c(NA, "2020-02-02")),
    disdest = c(19, 19)
  )

  df_before <- df
  result_force <- run_proxy(df_copy, .forceCopy = TRUE)
  expect_equal(df, df_before)

})


test_that("errors when x is not a data.frame or data.table", {
  expect_error(
    proxy_episode_dates(
      x = 1,
      group_vars = c("id", "provider"),
      spell_start_date = "spell_start",
      spell_end_date = "spell_end",
      discharge_destination = "disdest"

   ),
    "`x` must be a data.frame or data.table.",
    fixed = TRUE
  )
})

test_that("errors when group_vars is empty", {
  df <- data.frame(
    id = c(1, 1),
    provider = c("A", "A"),
    spell_start = as.Date(c("2020-01-01", "2020-02-01")),
    spell_end = as.Date(c("2020-01-02", "2020-02-02")),
    disdest = c(19, 19)
  )

  expect_error(
    proxy_episode_dates(
      x = df,
      group_vars = character(0),
      spell_start_date = "spell_start",
      spell_end_date = "spell_end",
      discharge_destination = "disdest"
    ),
    "`group_vars` must be a non-empty character vector of column names.",
    fixed = TRUE
  )
})

test_that("errors when group_vars contains duplicates", {
  df <- data.frame(
    id = c(1, 1),
    provider = c("A", "A"),
    spell_start = as.Date(c("2020-01-01", "2020-02-01")),
    spell_end = as.Date(c("2020-01-02", "2020-02-02")),
    disdest = c(19, 19)
  )

  expect_error(
    proxy_episode_dates(
      x = df,
      group_vars = c("id", "id"),
      spell_start_date = "spell_start",
      spell_end_date = "spell_end",
      discharge_destination = "disdest"
    ),
    "`group_vars` contains duplicates. Provide unique column names.",
    fixed = TRUE
  )
})

test_that("errors when required columns are missing", {
  df <- data.frame(
    id = c(1, 1),
    provider = c("A", "A"),
    spell_start = as.Date(c("2020-01-01", "2020-02-01")),
    spell_end = as.Date(c("2020-01-02", "2020-02-02"))
  )

  expect_error(
    proxy_episode_dates(
      x = df,
      group_vars = c("id", "provider"),
      spell_start_date = "spell_start",
      spell_end_date = "spell_end",
      discharge_destination = "disdest"
    ),
    "Missing required columns:",
    fixed = FALSE
  )
})

test_that("errors when spell_start_date or spell_end_date are not Date", {
  df <- data.frame(
    id = c(1, 1),
    provider = c("A", "A"),
    spell_start = c("2020-01-01", "2020-02-01"),
    spell_end = as.Date(c("2020-01-02", "2020-02-02")),
    disdest = c(19, 19)
  )

  expect_error(
    proxy_episode_dates(
      x = df,
      group_vars = c("id", "provider"),
      spell_start_date = "spell_start",
      spell_end_date = "spell_end",
      discharge_destination = "disdest"
    ),
    "Columns for spell start and end dates must be of type Date.",
    fixed = TRUE
  )
})

test_that("errors when .dropTmp is not a single logical", {
  df <- data.frame(
    id = c(1, 1),
    provider = c("A", "A"),
    spell_start = as.Date(c("2020-01-01", "2020-02-01")),
    spell_end = as.Date(c("2020-01-02", "2020-02-02")),
    disdest = c(19, 19)
  )

  expect_error(
    proxy_episode_dates(
      x = df,
      group_vars = c("id", "provider"),
      spell_start_date = "spell_start",
      spell_end_date = "spell_end",
      discharge_destination = "disdest",
      .dropTmp = "yes"
    ),
    "`.dropTmp` must be a single TRUE/FALSE value.",
    fixed = TRUE
  )
})

test_that("errors when .forceCopy is not a single logical", {
  df <- data.frame(
    id = c(1, 1),
    provider = c("A", "A"),
    spell_start = as.Date(c("2020-01-01", "2020-02-01")),
    spell_end = as.Date(c("2020-01-02", "2020-02-02")),
    disdest = c(19, 19)
  )

  df <- data.table::setDT(df)

  expect_error(
    proxy_episode_dates(
      x = df,
      group_vars = c("id", "provider"),
      spell_start_date = "spell_start",
      spell_end_date = "spell_end",
      discharge_destination = "disdest",
      .forceCopy = "no"
    ),
    "`.forceCopy` must be a single TRUE/FALSE value.",
    fixed = TRUE
  )
})

test_that("errors when input has zero rows", {
  df <- data.frame(
    id = integer(),
    provider = character(),
    spell_start = as.Date(character()),
    spell_end = as.Date(character()),
    disdest = integer()
  )

  expect_error(
    proxy_episode_dates(
      x = df,
      group_vars = c("id", "provider"),
      spell_start_date = "spell_start",
      spell_end_date = "spell_end",
      discharge_destination = "disdest"
    ),
    "Input data has zero rows.",
    fixed = TRUE
  )
})



test_that("overlapping dates trigger flag 4", {
  df <- data.frame(
    id = 1,
    provider = "A",
    spell_start = as.Date("2020-02-01"),
    spell_end   = as.Date("2020-01-31"),
    disdest     = 19
  )
  result <- proxy_episode_dates(df,
                                group_vars = c("id","provider"),
                                spell_start_date = "spell_start",
                                spell_end_date = "spell_end",
                                discharge_destination = "disdest")
  expect_equal(result$proxy_missing[1], 4)
  expect_equal(result$spell_start[1], as.Date("2020-01-31"))
})

test_that("cleanup of tmp columns works", {
  df <- data.frame(id = 1, provider = "A", spell_start = Sys.Date(), spell_end = Sys.Date(), disdest = 19)
  result <- proxy_episode_dates(df,
                             group_vars = c("id","provider"),
                             spell_start_date = "spell_start",
                             spell_end_date = "spell_end",
                             discharge_destination = "disdest",
                             .dropTmp = TRUE)
  expect_false(any(grepl("^tmp.", names(result))))
})
