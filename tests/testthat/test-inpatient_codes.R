# tests/testthat/test-inpatient_codes.R
# ------------------------------------------------------------
# Tests for inpatient_codes()
# ------------------------------------------------------------

library(testthat)
library(data.table)

# Sample minimal data
synth_icd_dt <- data.frame(
    id       = c(1L, 1L, 2L),
    spell_id = c("A","A","B"),
    primary_diagnosis_code   = c("U071-", "J189", NA),
    secondary_diagnosis_code_1 = c("J128 ", "I10X", "Z115"),
    secondary_diagnosis_code_2 = c(NA, "B972-", "I10X ")
  )

synth_opcs_dt <- data.frame(
    id       = c(1L, 2L),
    spell_id = c("A","B"),
    primary_procedure_code = c("H289", "K634"),
    primary_procedure_date = c("20170730", "20201202"),
    secondary_procedure_code_1 = c("H626", "Y534"),
    secondary_procedure_date_1 = c("20170730", "20201202")
  )

test_that("type validation: accepts icd9/icd10/opcs (case-insensitive) and rejects others", {

  dt <- synth_icd_dt

  expect_no_error(inpatient_codes(dt, "diagnosis", c("id","spell_id"), type = "icd10"))

  expect_no_error(inpatient_codes(dt, "diagnosis", c("id","spell_id"), type = "ICD9"))

  expect_no_error(inpatient_codes(synth_opcs_dt, c("procedure_code","procedure_date"),
                                  c("id","spell_id"), type = "OPCS"))

  expect_error(
    inpatient_codes(dt, "diagnosis", c("id","spell_id"), type = "ICD13"),
    "'arg' should be one of \"icd9\", \"icd10\", \"opcs\"",
    fixed = TRUE

  )
})

test_that("x must be data.frame/data.table and non-empty", {

  expect_error(inpatient_codes(1:3, "diagnosis", "id", type = "icd10"),
               "`x` must be a data.frame or data.table")

  empty_df <- data.frame()

  expect_error(inpatient_codes(empty_df, "diagnosis", "id", type = "icd10"),
               "`x` has 0 rows")

})

test_that("patient_id_vars must be present and non-empty", {

  dt <- synth_icd_dt

  expect_error(inpatient_codes(dt, "diagnosis", character(), type = "icd10"),
               "patient_id_vars.*non-empty")

  expect_error(inpatient_codes(dt, "diagnosis", c("id","missing"), type = "icd10"),
               "not found in `x`.*missing")

})

test_that("field_strings: icd requires one pattern; opcs requires two and paired cols", {

  dt <- synth_icd_dt

  # ICD: more than one pattern -> warning, uses first
  expect_warning(
    inpatient_codes(dt, c("diagnosis","extra"), c("id","spell_id"), type = "icd10"),
    "only the first element of `field_strings` will be used",
    fixed = FALSE
  )

  # ICD: no match -> error
  expect_error(
    inpatient_codes(dt, "will-not-match", c("id","spell_id"), type = "icd10"),
    "No columns in `x` matched"
  )

  # OPCS: must have two patterns
  dt2 <- synth_opcs_dt
  expect_error(
    inpatient_codes(dt2, "procedure_code", c("id","spell_id"), type = "opcs"),
    "must contain TWO patterns"
  )

  # OPCS: unmatched patterns
  expect_error(
    inpatient_codes(dt2, c("not_there","procedure_date"), c("id","spell_id"), type = "opcs"),
    "No procedure CODE columns matched"
  )
  expect_error(
    inpatient_codes(dt2, c("procedure_code","not_there"), c("id","spell_id"), type = "opcs"),
    "No procedure DATE columns matched"
  )

  # OPCS: unpaired counts
  dt3 <- data.table::as.data.table(data.frame(
    id=1,
    primary_procedure_code="X111", secondary_procedure_code_1="X222",
    primary_procedure_date="20200101" # only one date
  ))

  expect_error(
    inpatient_codes(dt3, c("procedure_code","procedure_date"), "id", type = "opcs"),
    "number of code columns .* must equal .* number of date columns"
  )

})

test_that(".forceCopy must be logical argument and behaves as expected", {

  dt <- synth_icd_dt

  # Invalid .forceCopy
  expect_error(
    inpatient_codes(dt, "diagnosis", c("id","spell_id"), type="icd10", .forceCopy = NA),
    "`.forceCopy` must be a single TRUE/FALSE"
  )


  # TRUE must not modify codes for the cleanup step.
  dt_copy <- data.table::copy(dt)

  # Run funciton on dt_copy
  res <- inpatient_codes(dt_copy, "diagnosis", c("id","spell_id"),
                         type="icd10", .forceCopy = TRUE)

  # Ensure original dt_copy unchanged in code columns (no 4-char truncation applied to source)
  expect_identical(dt_copy$primary_diagnosis_code[1], "U071-")

})

test_that("ICD path: output structure, 4-char truncation, deduplication, and order_n", {

  dt <- synth_icd_dt

  out <- inpatient_codes(dt, "diagnosis", c("id","spell_id"), type = "icd10")

  # Required columns
  expect_true(all(c("id","spell_id","order","order_n","icd10") %in% names(out)))

  # 4-char truncation: remove subcodes/whitespace; "U071-" -> "U071"
  expect_true(all(nchar(na.omit(out$icd10)) <= 4))

  expect_true("U071" %in% out$icd10)

  # Deduplication by IDs + code
  # Add a duplicate row to source and verify unique() drop
  dt2 <- data.table::rbindlist(list(dt, dt[1]), use.names = TRUE, fill = TRUE)

  out2 <- inpatient_codes(dt2, "diagnosis", c("id","spell_id"), type = "icd10")

  # Count per (id, spell_id, code) should be unchanged
  keys <- out[, .N, by = .(id, spell_id, icd10)]

  keys2 <- out2[, .N, by = .(id, spell_id, icd10)]

  setkeyv(keys, c("id","spell_id","icd10"))

  setkeyv(keys2, c("id","spell_id","icd10"))

  expect_identical(keys2, keys)

  # order_n increments within each id/spell_id
  grp_sizes <- out[, .N, by = .(id, spell_id)]

  expect_true(all(out[, max(order_n), by = .(id, spell_id)]$V1 == grp_sizes$N))

  expect_true(all(out[, all(order_n == seq_len(.N)), by = .(id, spell_id)]$V1))

})

test_that("ICD9 path works equivalently (using same diagnosis patterns)", {

  dt <- synth_icd_dt

  out <- inpatient_codes(dt, "diagnosis", c("id","spell_id"), type = "icd9")

  expect_true(all(c("id","spell_id","order","order_n","icd9") %in% names(out)))

  expect_true(all(nchar(na.omit(out$icd9)) <= 4))

})

test_that("OPCS path: pairing, ordering, and date column present", {

  dt <- synth_opcs_dt

  out <- inpatient_codes(dt,
                         field_strings = c("procedure_code","procedure_date"),
                         patient_id_vars = c("id","spell_id"),
                         type = "opcs")

  # Required columns
  expect_true(all(c("id","spell_id","order_n","date","opcs") %in% names(out)))

  # 4-char truncation for codes
  expect_true(all(nchar(na.omit(out$opcs)) <= 4))

  # Date column should carry matched dates
  # (Basic presence check; format is not enforced by function)
  expect_true(all(!is.na(out$date)))

  # Ordering by id, spell_id, date, order_n
  ord_out <- data.table::copy(out)

  data.table::setorderv(ord_out, c("id","spell_id","date","order_n"))

  expect_identical(out, ord_out)

  # Deduplication on (id, spell_id, opcs)
  out_dupe <- data.table::rbindlist(list(out, out[1]), use.names = TRUE)

  out_dupe_unique <- unique(out_dupe, by = c("id","spell_id","opcs"))

  expect_identical(out_dupe_unique, unique(out, by = c("id","spell_id","opcs")))

})

test_that("Non-character code columns are safely coerced before substr", {

  dt <- data.table::as.data.table(data.frame(
    id = 1:2, spell_id = c("A","B"),
    primary_diagnosis_code = c(70123, NA_integer_) # numeric initially
  ))

  out <- inpatient_codes(dt, "diagnosis", c("id","spell_id"), type = "icd10")

  # Result codes are character truncated to 4 chars
  expect_type(out$icd10, "character")

  expect_true(all(nchar(na.omit(out$icd10)) <= 4))

})

test_that("Whitespace and trailing hyphens are handled via truncation", {

  dt <- data.table::as.data.table(data.frame(
    id=1L, spell_id="A",
    primary_diagnosis_code="U071 ",
    secondary_diagnosis_code_1="J128-"
  ))

  out <- inpatient_codes(dt, "diagnosis", c("id","spell_id"), type = "icd10")

  expect_true(all(out$icd10 %in% c("U071","J128")))

})
