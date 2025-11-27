library(testthat)
library(data.table)

# -----------------------------------------------------------
test_that("All lookup tables exist in the epidm package", {

  # List of lookups expected in the package
  expected_objects <- c(
    "respeciate_organism",
    "specimen_type_grouping",
    "group_inpatient_admission_method",
    "group_inpatient_discharge_destination",
    "group_ecds_discharge_destination"
  )

  # Lookups may be internal or data(), so check the package environment
  pkg_env <- as.environment("package:epidm")

  for (obj in expected_objects) {
    expect_true(
      exists(obj, envir = pkg_env, inherits = FALSE),
      info = paste0("Lookup object '", obj, "' is missing from the epidm package environment.")
    )
  }
})

# -----------------------------------------------------------
# Test epidm::respeciate_organism
test_that("species lookup works correctly", {

  # Example: "ALCALIGENES DENITRIFICANS" → "ACHROMOBACTER DENITRIFICANS"
  result <- lookup_recode("ALCALIGENES DENITRIFICANS", type = "species")
  expect_equal(result, "ACHROMOBACTER DENITRIFICANS")

  # A value not in the lookup should return the original unchanged
  result2 <- lookup_recode("UNKNOWN BUG", type = "species")
  expect_equal(result2, "UNKNOWN BUG")
})
# -----------------------------------------------------------
# Test epidm::specimen_type_grouping
test_that("specimen lookup works correctly", {

  # Known mapping: BLOOD → Blood
  result <- lookup_recode("BLOOD", type = "specimen")
  expect_equal(result, "Blood")

  # Unknown specimen_type should return the original value
  result2 <- lookup_recode("Nonexistent", type = "specimen")
  expect_equal(result2, "Nonexistent")
})
# -----------------------------------------------------------
# Test lookup epidm::group_inpatient_admission_method
test_that("inpatient_admission_method lookup works correctly", {

  # Known mapping: 11 → Elective admission
  result <- lookup_recode("11", type = "inpatient_admission_method")
  expect_equal(result, "Elective admission")

  # Unknown value should return itself
  result2 <- lookup_recode("ZZ", type = "inpatient_admission_method")
  expect_equal(result2, "ZZ")
})
# -----------------------------------------------------------
# Test epidm::group_inpatient_discharge_destination
test_that("inpatient_discharge_destination lookup works correctly", {

  # Known mapping: 19 → Patient returned home
  result <- lookup_recode("19", type = "inpatient_discharge_destination")
  expect_equal(result, "Patient returned home")

  # Known mapping: 79 → Patient died
  result2 <- lookup_recode("79", type = "inpatient_discharge_destination")
  expect_equal(result2, "Patient died")

  # Unknown value: return original
  result3 <- lookup_recode("XYZ123", type = "inpatient_discharge_destination")
  expect_equal(result3, "XYZ123")
})
# -----------------------------------------------------------
# Test epidm::group_ecds_discharge_destination
test_that("ecds_destination_code lookup works correctly", {

  # Known mapping: 306689006 → Discharged
  result <- lookup_recode("306689006", type = "ecds_destination_code")
  expect_equal(result, "Discharged")

  # Known mapping: 305398007 → Died
  result2 <- lookup_recode("305398007", type = "ecds_destination_code")
  expect_equal(result2, "Died")

})
# -----------------------------------------------------------
test_that("manual lookup works correctly", {
  new_vals <- c("NEWNAME")
  old_vals <- c("OLDNAME")

  result <- lookup_recode("OLDNAME", type = "manual",
                          .import = list(new_vals, old_vals))
  expect_equal(result, "NEWNAME")

  # If not found, returns original
  result2 <- lookup_recode("NOTFOUND", type = "manual",
                           .import = list(new_vals, old_vals))
  expect_equal(result2, "NOTFOUND")
})
# -----------------------------------------------------------
test_that("manual lookup errors if .import missing", {
  expect_error(lookup_recode("X", type = "manual"),
               "supply a two item list")
})
# -----------------------------------------------------------
test_that("lookup handles NA values correctly", {

  result <- lookup_recode(NA_character_, "species")

  expect_true(is.na(result))
})
# -----------------------------------------------------------
test_that("lookup_recode() validates src correctly", {

  # Missing src error
  expect_error(
    lookup_recode(type = "species"),
    "'src' must be supplied\\."
  )
  # Invalid types should trigger your error message
  invalid_error <- "'src' must be a character vector, factor, atomic vector, or column\\."

  expect_error(
    lookup_recode(data.frame(x = "A"), type = "species"),
    invalid_error
  )

  # character
  expect_no_error(lookup_recode("ABC", type = "species"))

  # numeric
  expect_no_error(lookup_recode(12345, type = "species"))

  # factor
  expect_no_error(lookup_recode(factor("ABC"), type = "species"))

})
