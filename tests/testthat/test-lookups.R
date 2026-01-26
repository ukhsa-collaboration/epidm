library(testthat)

# -----------------------------------------------------------
#' Discover exported data objects in a package
#'
#' @param pkg_name Character scalar, package name (e.g., "epidm")
#' @param pattern Optional regex to filter names (NULL = no filter)
#' @return Character vector of dataset names exported by the package
discover_data_objects <- function(pkg_name, pattern = NULL) {
  stopifnot(length(pkg_name) == 1, is.character(pkg_name))

  # Get exported datasets via data()
  data_objects <- tryCatch(
    data(package = pkg_name)$results[, "Item"],
    error = function(e) character(0)
  )

  # Apply pattern filter if provided
  if (!is.null(pattern)) {
    data_objects <- grep(pattern, data_objects, value = TRUE)
  }

  return(data_objects)
}

# -----------------------------------------------------------
test_that("Exported data objects match expected", {
  expected_objects <- c(
    "genus_gram_stain",
    "group_ecds_discharge_destination",
    "group_inpatient_admission_method",
    "group_inpatient_discharge_destination",
    "lab_data",
    "respeciate_organism",
    "specimen_type_grouping"
  )

  actual_objects <- discover_data_objects("epidm")

  missing <- setdiff(expected_objects, actual_objects)
  extra   <- setdiff(actual_objects, expected_objects)

  expect(
    length(missing) == 0 && length(extra) == 0,
    paste0(
      "Mismatch in exported data objects.\n",
      if (length(missing)) paste0("- Missing: ", paste(missing, collapse = ", "), "\n") else "",
      if (length(extra))   paste0("- Extra: ", paste(extra, collapse = ", "), "\n") else ""
    )
  )
})
# -----------------------------------------------------------
test_that("All expected lookup tables exist in the epidm package environment", {
  expected_objects <- c(
    "genus_gram_stain",
    "group_ecds_discharge_destination",
    "group_inpatient_admission_method",
    "group_inpatient_discharge_destination",
    "lab_data",
    "respeciate_organism",
    "specimen_type_grouping"
  )

  pkg_env <- as.environment("package:epidm")
  data_objects <- tryCatch(
    data(package = "epidm")$results[, "Item"],
    error = function(e) character(0)
  )

  for (obj in expected_objects) {
    exists_in_env  <- exists(obj, envir = pkg_env, inherits = FALSE)
    exists_in_data <- obj %in% data_objects

    expect_true(
      exists_in_env || exists_in_data,
      info = paste0(
        "Expected lookup '", obj, "' was not found.\n",
        "Checked: package env = ", exists_in_env, ", data() = ", exists_in_data
      )
    )
  }
})


