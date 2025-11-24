library(testthat)

test_that("valid NHS number passes checksum", {
  # Example of a valid NHS number (9434765919 example give in function)
  expect_equal(valid_nhs(9434765919), 1)
})
# -----------------------------------------------------------
test_that("invalid NHS number fails checksum", {
  # Known wrong NHS number
  expect_equal(valid_nhs(9434765918), 0)
})
# -----------------------------------------------------------
test_that("NHS number with wrong length is invalid", {
  expect_equal(valid_nhs(123456789), 0)   # 9 digits
  expect_equal(valid_nhs(12345678901), 0) # 11 digits
})
# -----------------------------------------------------------
test_that("NA input returns 0", {
  expect_equal(valid_nhs(NA), 0)
})
# -----------------------------------------------------------
test_that("vector input returns vector of results", {
  nums <- c(9434765919, 9434765918, 1111111111, NA)
  result <- valid_nhs(nums)
  expect_equal(result, c(1, 0, 0, 0))
})
# -----------------------------------------------------------

