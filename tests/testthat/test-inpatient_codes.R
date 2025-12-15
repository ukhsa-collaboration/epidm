# tests/testthat/test-inpatient_codes.R

# Tests for inpatient_codes()


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

  expect_error(
    inpatient_codes(dt, "diagnosis", c("id", NA_character_), type = "icd10"),
    "contains NA/empty elements", fixed = TRUE
  )
  expect_error(
    inpatient_codes(dt, "diagnosis", c("id", ""), type = "icd10"),
    "contains NA/empty elements", fixed = TRUE)

})



test_that("field_strings: icd requires one pattern; opcs accepts one (codes) or two (codes+dates)", {

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

  # --- OPCS changes start here ---

  dt2 <- synth_opcs_dt

  # OPCS: single pattern (codes only) is allowed -> no error
  expect_silent(
    inpatient_codes(dt2, "procedure_code", c("id","spell_id"), type = "opcs")
  )

  # OPCS: codes pattern must match at least one column -> error if not
  expect_error(
    inpatient_codes(dt2, c("not_there"), c("id","spell_id"), type = "opcs"),
    "No procedure CODE columns matched"
  )

  # OPCS: two patterns supplied but code pattern unmatched -> error (still codes are mandatory)
  expect_error(
    inpatient_codes(dt2, c("not_there","procedure_date"), c("id","spell_id"), type = "opcs"),
    "No procedure CODE columns matched"
  )

  # OPCS: two patterns supplied but date pattern unmatched -> WARNING and proceed WITHOUT dates (not error)
  expect_warning(
    inpatient_codes(dt2, c("procedure_code","not_there"), c("id","spell_id"), type = "opcs"),
    "Proceeding WITHOUT dates",
    fixed = FALSE
  )

  # OPCS: unpaired counts (e.g., 2 code cols, 1 date col) -> WARNING and proceed WITHOUT dates (not error)
  dt3 <- data.table::as.data.table(data.frame(
    id = 1,
    spell_id = "s1",
    primary_procedure_code = "X111",
    secondary_procedure_code_1 = "X222",
    primary_procedure_date = "20200101" # only one date for two code cols
  ))

  expect_warning(
    inpatient_codes(dt3, c("procedure_code","procedure_date"), c("id","spell_id"), type = "opcs"),
    "do not pair 1:1 .* Proceeding WITHOUT dates",
    fixed = FALSE
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


test_that("ICD: 'order' reflects original column names", {
  dt <- data.table::as.data.table(synth_icd_dt)
  out <- inpatient_codes(dt, "diagnosis", c("id","spell_id"), type = "icd10")

  g1 <- out[id == 1L & spell_id == "A"]
  # First two entries should be primaries from the two rows, then secondary_1 etc.
  expect_true(grepl("primary", g1$order[1]))
  expect_true(grepl("primary", g1$order[2]))
  expect_true(grepl("secondary_diagnosis_code_1", g1$order[3]))
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

  out_dupe_unique <- unique(out_dupe, by = c("id","spell_id","opcs", "date"))

  expect_identical(out_dupe_unique, unique(out, by = c("id","spell_id","opcs", "date")))

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


test_that("ICD: Data is deduplicated per spell, and ordered by importance (primary then secondary index)", {
  dt <- data.table::as.data.table(synth_icd_dt)

  # Add a third secondary column with duplicates to exercise dedup
  dt$secondary_diagnosis_code_3 <- c("J128-", NA, "I10X")

  out <- inpatient_codes(
    x = dt,
    field_strings = "diagnosis",
    patient_id_vars = c("id","spell_id"),
    type = "icd10"
  )

  # Expect truncated 4-char codes in 'icd10' column
  expect_true("icd10" %in% names(out))
  expect_true(all(nchar(out$icd10) == 4))


  g1 <- out[id == 1L & spell_id == "A"]
  expect_equal(g1$icd10, c("U071", "J189", "J128", "I10X", "B972"))

  # Dedup check: no duplicates within spell (after truncation)
  expect_true(length(g1$icd10) == length(unique(g1$icd10)))

  # Dedup within spell: id=2/B should have Z115 then I10X (primary is NA)
  g2 <- out[id == 2L & spell_id == "B"]
  expect_equal(g2$icd10, c("Z115","I10X"))

  # Total rows after dedup:
  #  id=1/A -> 5 unique codes; id=2/B -> 2 unique codes => total 7
  expect_equal(nrow(out), 7L)

  # order_n increments within each spell according to melt order (primary across rows, then secondary_1, secondary_2, ...)
  expect_equal(g1$order_n, 1:5)
  expect_equal(g2$order_n, 1:2)

})


test_that("OPCS with dates: keeps date, sorts by date then importance, deduplicates by code+date", {
  dt <- data.table::as.data.table(synth_opcs_dt)

  # Extend OPCS with duplicates and an earlier/later date to test dedup & order
  dt$secondary_procedure_code_2 <- c("H626", "K634")          # duplicate codes
  dt$secondary_procedure_date_2 <- c("20170730", "20201203")  # same date for id=1; later date for id=2
  dt$secondary_procedure_code_3 <- c("Z999", NA)               # extra code for id=1 on earlier date
  dt$secondary_procedure_date_3 <- c("20170729", NA)           # earlier date for id=1

  out <- inpatient_codes(
    x = dt,
    field_strings = c("procedure_code","procedure_date"),
    patient_id_vars = c("id","spell_id"),
    type = "opcs"
  )

  # Date column present
  expect_true("date" %in% names(out))

  # id=1/A chronology: 2017-07-29 (Z999) first, then 2017-07-30 (H289 then H626)
  g1 <- out[id == 1L & spell_id == "A"]
  d1 <- as.character(g1$date)
  expect_equal(d1, c("2017-07-29", "2017-07-30", "2017-07-30"))
  expect_equal(g1$opcs, c("Z999", "H289", "H626"))  # primary before secondary on same date

  # Dedup: only one H626 on 2017-07-30 retained
  expect_equal(sum(g1$opcs == "H626" & d1 == "2017-07-30"), 1L)

  # id=2/B chronology: 2020-12-02 (K634 then Y534), 2020-12-03 (K634 again)
  g2 <- out[id == 2L & spell_id == "B"]
  d2 <- as.character(g2$date)
  expect_equal(d2, c("2020-12-02","2020-12-02","2020-12-03"))
  expect_equal(g2$opcs, c("K634","Y534","K634"))    # keep same code on different dates

  # Total rows after dedup: id=1/A -> 3; id=2/B -> 3 => total 6
  expect_equal(nrow(out), 6L)
})


test_that("OPCS without dates: no date column, ordered by importance, deduplicated by code only", {
  dt <- data.table::as.data.table(synth_opcs_dt)

  # Extend OPCS with duplicates and an earlier/later date to test dedup & order
  dt$secondary_procedure_code_2 <- c("H626", "K634")          # duplicate codes
  dt$secondary_procedure_date_2 <- c("20170730", "20201203")  # same date for id=1; later date for id=2
  dt$secondary_procedure_code_3 <- c("Z999", NA)               # extra code for id=1 on earlier date
  dt$secondary_procedure_date_3 <- c("20170729", NA)           # earlier date for id=1

  out <- inpatient_codes(
    x = dt,
    field_strings = "procedure_code",  # codes only
    patient_id_vars = c("id","spell_id"),
    type = "opcs"
  )

  # No date column
  expect_false("date" %in% names(out))

  # id=1/A: duplicates of H626 collapsed; importance order preserved (primary, then secondary indices)
  g1 <- out[id == 1L & spell_id == "A"]
  expect_equal(g1$opcs, c("H289","H626","Z999"))
  # order_n reflects melt index order within the spell (primary=1, secondary_1=2, secondary_3=4)
  expect_equal(g1$order_n, c(1L, 2L, 4L))

  # id=2/B: duplicate K634 collapsed; primary before secondary_1
  g2 <- out[id == 2L & spell_id == "B"]
  expect_equal(g2$opcs, c("K634","Y534"))

  # Total rows: id=1/A -> 3 unique; id=2/B -> 2 unique => total 5
  expect_equal(nrow(out), 5L)

  # Dedup check
  expect_true(length(g1$opcs) == length(unique(g1$opcs)))
  expect_true(length(g2$opcs) == length(unique(g2$opcs)))
})


test_that("OPCS warnings when date pattern supplied but unmatched or unpaired", {
  dt <- data.table::as.data.table(synth_opcs_dt)

  # Extend OPCS with duplicates and an earlier/later date to test dedup & order
  dt$secondary_procedure_code_2 <- c("H626", "K634")          # duplicate codes
  dt$secondary_procedure_date_2 <- c("20170730", "20201203")  # same date for id=1; later date for id=2
  dt$secondary_procedure_code_3 <- c("Z999", NA)               # extra code for id=1 on earlier date
  dt$secondary_procedure_date_3 <- c("20170729", NA)           # earlier date for id=1

  # Date pattern supplied but matches zero columns -> warn, proceed without dates
  expect_warning(
    inpatient_codes(dt, c("procedure_code","not_there"), c("id","spell_id"), type = "opcs"),
    "matched 0 columns. Proceeding WITHOUT dates",
    fixed = FALSE
  )

  # Unpaired counts -> warn, proceed without dates
  dt_unpaired <- data.table::as.data.table(data.frame(
    id = 1L, spell_id = "S",
    primary_procedure_code = "X111",
    secondary_procedure_code_1 = "X222",
    primary_procedure_date = "20200101"
  ))
  expect_warning(
    inpatient_codes(dt_unpaired, c("procedure_code","procedure_date"), c("id","spell_id"), type = "opcs"),
    "do not pair 1:1 .* Proceeding WITHOUT dates",
    fixed = FALSE
  )
})

