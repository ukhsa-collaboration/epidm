library(testthat)
library(data.table)

# Sample minimal data
ae_data <- data.table(
  nhs_number = c("123", "456"),
  hospital_number = c("A1", "B2"),
  patient_birth_date = as.Date(c("1990-01-01", "1985-05-05")),
  organisation_code_of_provider = c("ORG1", "ORG2"),
  arrival_date = as.Date(c("2023-01-01", "2023-01-02")),
  departure_date = as.Date(c("2023-01-03", "2023-01-04"))
)

inp_data <- data.table(
  nhs_number = c("123", "789"),
  local_patient_identifier = c("A1", "C3"),
  date_birth = as.Date(c("1990-01-01", "1970-07-07")),
  organisation_code_code_of_provider = c("ORG1", "ORG3"),
  mega_spell_id = c("S1", "S2"),
  spell_start_date = as.Date(c("2023-01-01", "2023-01-05"))
)

test_that("Function runs with minimal valid input", {
  result <- link_ae_inpatient(
    ae = list(
      data = ae_data,
      nhs_number = "nhs_number",
      hospital_number = "hospital_number",
      patient_dob = "patient_birth_date",
      org_code = "organisation_code_of_provider",
      arrival_date = "arrival_date",
      departure_date = "departure_date"
    ),
    inp = list(
      data = inp_data,
      nhs_number = "nhs_number",
      hospital_number = "local_patient_identifier",
      patient_dob = "date_birth",
      org_code = "organisation_code_code_of_provider",
      spell_id = "mega_spell_id",
      spell_start_date = "spell_start_date"
    )
  )
  expect_s3_class(result, "data.table")
  expect_true("nhs_number" %in% names(result))
})

test_that("Function includes record_id if present in ae only", {
  ae_data[, unique_record_id := c("ae1", "ae2")]
  result <- link_ae_inpatient(
    ae = list(
      data = ae_data,
      record_id = "unique_record_id",
      nhs_number = "nhs_number",
      hospital_number = "hospital_number",
      patient_dob = "patient_birth_date",
      org_code = "organisation_code_of_provider",
      arrival_date = "arrival_date",
      departure_date = "departure_date"
    ),
    inp = list(
      data = inp_data,
      nhs_number = "nhs_number",
      hospital_number = "local_patient_identifier",
      patient_dob = "date_birth",
      org_code = "organisation_code_code_of_provider",
      spell_id = "mega_spell_id",
      spell_start_date = "spell_start_date"
    )
  )
  expect_true("unique_record_id_ae" %in% names(result))
})

test_that("Function includes record_id if present in inp only", {
  inp_data[, unique_record_id := c("inp1", "inp2")]
  result <- link_ae_inpatient(
    ae = list(
      data = ae_data,
      nhs_number = "nhs_number",
      hospital_number = "hospital_number",
      patient_dob = "patient_birth_date",
      org_code = "organisation_code_of_provider",
      arrival_date = "arrival_date",
      departure_date = "departure_date"
    ),
    inp = list(
      data = inp_data,
      record_id = "unique_record_id",
      nhs_number = "nhs_number",
      hospital_number = "local_patient_identifier",
      patient_dob = "date_birth",
      org_code = "organisation_code_code_of_provider",
      spell_id = "mega_spell_id",
      spell_start_date = "spell_start_date"
    )
  )
  expect_true("unique_record_id_inp" %in% names(result))
})

test_that("Function includes both record_id columns if present in both", {
  ae_data[, unique_record_id := c("ae1", "ae2")]
  inp_data[, unique_record_id := c("inp1", "inp2")]
  result <- link_ae_inpatient(
    ae = list(
      data = ae_data,
      record_id = "unique_record_id",
      nhs_number = "nhs_number",
      hospital_number = "hospital_number",
      patient_dob = "patient_birth_date",
      org_code = "organisation_code_of_provider",
      arrival_date = "arrival_date",
      departure_date = "departure_date"
    ),
    inp = list(
      data = inp_data,
      record_id = "unique_record_id",
      nhs_number = "nhs_number",
      hospital_number = "local_patient_identifier",
      patient_dob = "date_birth",
      org_code = "organisation_code_code_of_provider",
      spell_id = "mega_spell_id",
      spell_start_date = "spell_start_date"
    )
  )
  expect_true("unique_record_id_ae" %in% names(result))
  expect_true("unique_record_id_inp" %in% names(result))
})

test_that("Function throws error if required fields are missing", {
  expect_error(link_ae_inpatient(
    ae = list(data = ae_data),
    inp = list(data = inp_data)
  ), regexp = "Missing required fields")
})

test_that("Correct records are linked based on NHS number, DOB, org code, and link_date", {
  ae_data <- data.table(
    nhs_number = "1234567890",
    hospital_number = "H123",
    patient_birth_date = as.Date("1990-01-01"),
    organisation_code_of_provider = "ORG001",
    arrival_date = as.Date("2023-01-01"),
    departure_date = as.Date("2023-01-01")  # same as arrival to trigger fallback
  )

  inp_data <- data.table(
    nhs_number = "1234567890",
    local_patient_identifier = "H123",
    date_birth = as.Date("1990-01-01"),
    organisation_code_code_of_provider = "ORG001",
    mega_spell_id = "S123",
    spell_start_date = as.Date("2023-01-01")
  )

  result <- link_ae_inpatient(
    ae = list(
      data = ae_data,
      nhs_number = "nhs_number",
      hospital_number = "hospital_number",
      patient_dob = "patient_birth_date",
      org_code = "organisation_code_of_provider",
      arrival_date = "arrival_date",
      departure_date = "departure_date"
    ),
    inp = list(
      data = inp_data,
      nhs_number = "nhs_number",
      hospital_number = "local_patient_identifier",
      patient_dob = "date_birth",
      org_code = "organisation_code_code_of_provider",
      spell_id = "mega_spell_id",
      spell_start_date = "spell_start_date"
    )
  )


  # Confirm the linked NHS number is correct
  expect_equal(result$nhs_number[1], "1234567890")

  # Confirm the source is from both dfs
  expect_equal(result$source[1], "ECDS:SUS")
})

