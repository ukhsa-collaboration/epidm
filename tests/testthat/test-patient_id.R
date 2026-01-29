# Load libraries
library(testthat)
library(data.table)

# Sample data and helpers for tests

make_pd <- function() {
  data.frame(
    record_id = 1:8,
    nhs_number = c(9435817777, 9435817777, 9435773982, 9435797881, 9435797881, 9435754422, 9435754422, 9435802508),
    local_patient_identifier = c("HS45202", "HS45202", "KR2535", "NEW001", "IG12067", "IG12067", "IG12067", "UK8734"),
    patient_birth_date = as.Date(c("1962-06-14", "1962-06-14", "1927-06-24", "1938-10-05", "1938-10-05", "1930-01-01", "1930-02-01", "1989-01-01")),
    sex = c("Male", "Male", "Male", "Female", "Female", "Female", "Female", "Male"),
    surname = c("RODA", "RODA", "LINTON", "WILLIAMS", "WILLIAMS", "WILLIAMS", "WILLIAMS", "JAMILETH"),
    forename = c("TYLER", "TYLER", "KASHIEF", "JANE", "JAMILETH", "JAMILETH", "JAMILETH", "WILLIAMS"),
    specimen_date = as.Date(c("2023-08-26", "2023-08-27", "2023-02-25", "2024-01-01", "2024-02-02", "2024-03-03", "2024-04-03", "2024-01-01")),
    postcode = c("CW6 9TX", "CW6 9TX", "PL7 1LU", "BN14 9EP", "BN14 9EP", "BN14 9EP", "BN14 9EP", "BN14 9EP"),
    stringsAsFactors = FALSE
  )
}

id_map <- list(
  nhs_number = "nhs_number",
  hospital_number = "local_patient_identifier",
  date_of_birth = "patient_birth_date",
  sex_mfu = "sex",
  forename = "forename",
  surname = "surname",
  postcode = "postcode"
)

make_stage_data <- function(stage) {

  nhs_a <- 9435817777
  nhs_b <- 9435773982

  x <- data.frame(
    record_id = 1:3,
    nhs_number = c(NA, NA, NA),
    local_patient_identifier = c("H001", "H002", "H003"),
    patient_birth_date = as.Date(c("1980-01-05", "1980-02-06", "1981-03-07")),
    sex = c("Female", "Female", "Female"),
    forename = c("ALICE", "ALICE", "BOB"),
    surname = c("SMITH", "SMITH", "JONES"),
    postcode = c("AA1 1AA", "AA1 1AA", "BB2 2BB"),
    specimen_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    stringsAsFactors = FALSE
  )

  if (stage == 1) {
    x$nhs_number <- c(nhs_a, nhs_a, nhs_b)
    x$patient_birth_date <- as.Date(c("1970-01-01", "1970-01-01", "1970-01-02"))
  }

  if (stage == 2) {
    x$nhs_number <- c(nhs_a, nhs_b, nhs_b)
    x$local_patient_identifier <- c("H123", "H123", "H999")
    x$patient_birth_date <- as.Date(c("1970-01-01", "1970-01-01", "1970-01-02"))
  }

  if (stage == 3) {
    x$nhs_number <- c(nhs_a, nhs_a, nhs_b)
    x$local_patient_identifier <- c("H123", "H123", "H999")
  }

  if (stage == 4) {
    x$nhs_number <- c(nhs_a, nhs_a, nhs_b)
    x$surname <- c("BROWN", "BROWN", "BROWN")
  }

  if (stage == 5) {
    x$local_patient_identifier <- c("H123", "H123", "H999")
    x$surname <- c("BROWN", "BROWN", "BROWN")
  }

  if (stage == 6) {
    x$patient_birth_date <- as.Date(c("1970-01-01", "1970-01-01", "1971-01-01"))
    x$surname <- c("BROWN", "BROWN", "BROWN")
  }

  if (stage == 7) {
    x$sex <- c("Male", "Male", "Male")
    x$forename <- c("ALICE", "ALICE", "BOB")     # record 3 differs
    x$surname <- c("BROWN", "BROWN", "BROWN")
    x$patient_birth_date <- as.Date(c("1970-01-01", "1990-01-01", "1971-01-01"))
  }

  if (stage == 8) {
    x$sex <- c("Female", "Female", "Female")
    x$patient_birth_date <- as.Date(c("1980-01-05", "1980-01-20", "1980-02-01"))
    x$forename <- c("ALICE", "AMY", "ALICE")
    x$surname <- c("SMITH", "SMYTH", "SMITH")
  }

  if (stage == 9) {
    x$sex <- c(NA, NA, NA)
    x$patient_birth_date <- as.Date(c("1980-01-05", "1980-01-20", "1980-02-01"))
    x$forename <- c("ALICE", "AMY", "ALICE")
    x$surname <- c("SMITH", "SMYTH", "SMITH")
  }

  if (stage == 10) {
    x$forename <- c("ALICE", "ALICE", "ALICE")
    x$surname <- c("BROWN", "BROWN", "BROWN")
    x$postcode <- c("AA1 1AA", "AA11AA", "BB2 2BB")
  }

  if (stage == 11) {
    x <- x[1:2, ]
    x$nhs_number <- c(nhs_a, nhs_b)
    x$patient_birth_date <- as.Date(c("1962-06-14", "1962-06-14"))
    x$surname <- c("RODA", "TYLER")
    x$forename <- c("TYLER", "RODA")
    x$sex <- c("Male", "Male")
    x$postcode <- c("CW6 9TX", "CW6 9TX")
  }

  x
}

ids_by_record <- function(res, x) {
  setNames(res$id[match(x$record_id, res$record_id)], as.character(x$record_id))
}

stage_links <- function(stage) {

  x <- make_stage_data(stage)

  res <- uk_patient_id(
    x = x,
    id = id_map,
    .useStages = stage,
    .keepStages = TRUE,
    .forceCopy = FALSE,
    .sortOrder = if ("specimen_date" %in% names(x)) "specimen_date" else NULL
  )

  if (stage == 11) {
    expect_equal(res$id[1], res$id[2])
    expect_true(grepl("s11", res$stageMatch[1]))
    expect_true(grepl("s11", res$stageMatch[2]))
    return(invisible(NULL))
  }

  id_map_record <- ids_by_record(res, x)

  expect_equal(unname(id_map_record["1"]), unname(id_map_record["2"]))
  expect_false(unname(id_map_record["1"]) == unname(id_map_record["3"]))

  stage_by_record <- setNames(res$stageMatch[match(x$record_id, res$record_id)], as.character(x$record_id))
  expect_true(grepl(paste0("s", stage), stage_by_record["1"]))
  expect_true(grepl(paste0("s", stage), stage_by_record["2"]))

  invisible(NULL)
}

# Tests

test_that("Returns expected structure", {

  pd <- make_pd()

  res <- uk_patient_id(x = pd, id = id_map, .useStages = 1, .forceCopy = FALSE, .sortOrder = "specimen_date")

  expect_true(data.table::is.data.table(res))
  expect_true("id" %in% names(res))
  expect_equal(nrow(res), nrow(pd))
  expect_type(res$id, "integer")
})


patrick::with_parameters_test_that(
  "All stages link as expected with forceCopy FALSE",
  {
    stage_links(stage)
  },
  stage = 11,
  .test_name = paste0("stage_", 1:11)
)

test_that("keepValidNHS adds a valid_nhs column", {
  pd <- make_pd()

  data.table::setDT(pd)

  res <- uk_patient_id(
    x = pd,
    id = id_map,
    .useStages = 1,
    .keepValidNHS = TRUE,
    .forceCopy = TRUE
  )

  expect_true("valid_nhs" %in% names(res))
  expect_true(is.logical(res$valid_nhs))
})

test_that("Errors are raised for invalid inputs", {
  pd <- make_pd()

  data.table::setDT(pd)

  expect_error(
    uk_patient_id(x = NULL, id = id_map),
    "`x` must be a data.frame or data.table"
  )

  expect_error(
    uk_patient_id(x = pd, id = list(nhs_number = 1)),
    "character scalars"
  )

  expect_error(
    uk_patient_id(x = pd, id = id_map, .useStages = 99),
    "1 to 11"
  )

  expect_error(
    uk_patient_id(x = pd, id = id_map, .useStages = 1, .sortOrder = "not_a_col"),
    "Missing \\.sortOrder column"
  )

  pd2 <- pd
  pd2$id <- 1:nrow(pd2)
  expect_message(
    uk_patient_id(x = pd2, id = id_map),
    "`x` already contains a column named `id`. Please rename it to avoid overwrite."
  )
})

test_that("Expected error when .sortOrder isn't the correct type", {

  pd <- make_pd()

  expect_error(
    uk_patient_id(x = pd, id = id_map, .useStages = 1, .sortOrder = 1),
    "`.sortOrder` must be a character vector of one or more column names."
  )

})



patrick::with_parameters_test_that(
  "keepStages, .keepValidNHS and .forceCopy options behave as expected",
  {

    x <- make_stage_data(1)

    x_before <- x

    if (isTRUE(forceCopy)) {
      data.table::setDT(x)
      x_before <- data.table::copy(x)
    }

    res <- uk_patient_id(
      x = x,
      id = id_map,
      .useStages = 1,
      .keepStages = keepStages,
      .keepValidNHS = keepValidNHS,
      .forceCopy = forceCopy,
      .sortOrder = if ("specimen_date" %in% names(x)) "specimen_date" else NULL
    )

    id_map_record <- ids_by_record(res, x)
    expect_equal(unname(id_map_record["1"]), unname(id_map_record["2"]))
    expect_false(unname(id_map_record["1"]) == unname(id_map_record["3"]))


    if (isTRUE(keepStages)) {
      expect_true("stageMatch" %in% names(res))
      stage_by_record <- setNames(res$stageMatch[match(x$record_id, res$record_id)], as.character(x$record_id))
      expect_true(grepl("s1", stage_by_record["1"]))
      expect_true(grepl("s1", stage_by_record["2"]))
    } else {
      expect_false("stageMatch" %in% names(res))
    }

    if (isTRUE(keepValidNHS)) {
      expect_true("valid_nhs" %in% names(res))
      expect_true(is.logical(res$valid_nhs))
    } else {
      expect_false("valid_nhs" %in% names(res))
    }

    if (isTRUE(forceCopy)) {
      expect_equal(x, x_before)
    }

    expect_true("id" %in% names(res))
    expect_type(res$id, "integer")
  },

  keepStages = c(TRUE, FALSE),
  keepValidNHS = c(TRUE, FALSE),
  forceCopy = c(TRUE, FALSE),

  .test_name = paste0(
    "keepStages=", rep(c("TRUE", "FALSE"), each = 4),
    "_keepValidNHS=", rep(rep(c("TRUE", "FALSE"), each = 2), times = 2),
    "_forceCopy=", rep(c("TRUE", "FALSE"), times = 4)
  )
)
