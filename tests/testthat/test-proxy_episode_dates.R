library(testthat)
library(data.table)

test_that("proxy_episode_dates returns a data.table", {
  df <- data.frame(id = 1, provider = "A", spell_start = Sys.Date(), spell_end = Sys.Date(), disdest = 19)
  result <- proxy_episode_dates(df,
                             group_vars = c("id","provider"),
                             spell_start_date = "spell_start",
                             spell_end_date = "spell_end",
                             discharge_destination = "disdest")

  expect_true(is.data.table(result))
})
# -----------------------------------------------------------
test_that("proxy_missing flags are set correctly", {
  df <- data.frame(
    id = c(1,1,1,1),
    provider = "A",
    spell_start = as.Date(c("2020-01-01","2020-02-01","2020-03-01","2020-04-01")),
    spell_end   = as.Date(c("2020-01-02", NA, "2020-03-02", NA)),
    disdest     = c(19,51,19,19)
  )
  result <- proxy_episode_dates(df,
                             group_vars = c("id","provider"),
                             spell_start_date = "spell_start",
                             spell_end_date = "spell_end",
                             discharge_destination = "disdest",
                             .dropTmp = FALSE)

  # 2nd row missing end date, discharge code 51: flag 2
  expect_equal(result$proxy_missing[2], 2)
  # 4th row missing end date, last episode: flag 1
  expect_equal(result$proxy_missing[4], 1)
})
# -----------------------------------------------------------
test_that("end dates are replaced correctly", {
  df <- data.frame(
    id = c(1,1),
    provider = "A",
    spell_start = as.Date(c("2020-01-01","2020-02-01")),
    spell_end   = as.Date(c(NA, "2020-02-02")),  # NA on first episode
    disdest     = c(19,19)
  )


  result <- proxy_episode_dates(df,
                             group_vars = c("id","provider"),
                             spell_start_date = "spell_start",
                             spell_end_date = "spell_end",
                             discharge_destination = "disdest")

  # First row missing end date, not last episode, discharge != 51/98 => flag 3
  # So end date should be next start date - 1
  expect_equal(result$spell_end[1], as.Date("2020-01-31"))
})
# -----------------------------------------------------------
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
# -----------------------------------------------------------
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
