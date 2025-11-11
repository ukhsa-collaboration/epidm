library(testthat)
library(data.table)
library(stringr)

# Generate test dataset
set.seed(42)

df_respeciate <- data.frame(
  ptid = rep(1:6, each = 5),                 # 6 patients, 5 samples each (30 total)
  type = rep(c("BLOOD", "URINE"), length.out = 30),
  specdate = rep(seq.Date(Sys.Date() - 10, Sys.Date() - 6, by = "day"), length.out = 30)
)

# Assign genera/species in a way that ensures respeciation opportunities
df_respeciate$spec <- c(
  # Patient 1 (Klebsiella group – some SP within window)
  "KLEBSIELLA PNEUMONIAE", "KLEBSIELLA SP", "KLEBSIELLA UNNAMED", "KLEBSIELLA PNEUMONIAE", "KLEBSIELLA SP",
  # Patient 2 (Escherichia group – one respeciate opportunity)
  "ESCHERICHIA COLI", "ESCHERICHIA SP", "ESCHERICHIA UNNAMED", "ESCHERICHIA COLI", "ESCHERICHIA SP",
  # Patient 3 (Staphylococcus group – one UNNAMED near a species)
  "STAPHYLOCOCCUS SP", "STAPHYLOCOCCUS AUREUS", "STAPHYLOCOCCUS UNNAMED", "STAPHYLOCOCCUS SP", "STAPHYLOCOCCUS AUREUS",
  # Patient 4 (Mixed genera – no respeciation possible)
  "KLEBSIELLA PNEUMONIAE", "ESCHERICHIA SP", "STAPHYLOCOCCUS SP", "KLEBSIELLA UNNAMED", "ESCHERICHIA COLI",
  # Patient 5 (All fully speciated – no changes expected)
  "KLEBSIELLA OXYTOCA", "KLEBSIELLA OXYTOCA", "KLEBSIELLA OXYTOCA", "KLEBSIELLA OXYTOCA", "KLEBSIELLA OXYTOCA",
  # Patient 6 (All SP – no respeciation possible)
  "STAPHYLOCOCCUS SP", "STAPHYLOCOCCUS SP", "STAPHYLOCOCCUS SP", "STAPHYLOCOCCUS SP", "STAPHYLOCOCCUS SP"
)


test_that("returns a data.table with expected structure", {
  result <- suppressMessages(
    respeciate_generic(
      x = df_respeciate,
      group_vars = c("ptid", "type"),
      species_col = "spec",
      date_col = "specdate",
      window = 7
    )
  )

  expect_s3_class(result, "data.table")
  expect_true(all(c("ptid", "spec", "type", "specdate") %in% colnames(result)))
  expect_equal(nrow(result), nrow(df_respeciate))
})

# -----------------------------------------------------------
test_that("temporary columns are removed after processing", {
  result <- suppressMessages(
    respeciate_generic(
      x = df_respeciate,
      group_vars = c("ptid", "type"),
      species_col = "spec",
      date_col = "specdate",
      window = 7
    )
  )

  expect_false(any(grepl("^tmp\\.", names(result))))
})

# -----------------------------------------------------------
test_that("species column remains character and valid", {
  result <- suppressMessages(
    respeciate_generic(
      x = df_respeciate,
      group_vars = c("ptid", "type"),
      species_col = "spec",
      date_col = "specdate",
      window = 7
    )
  )

  expect_type(result$spec, "character")
})

# -----------------------------------------------------------
test_that("unspeciated isolates are respeciated when possible", {
  pre_unspec <- sum(stringr::str_detect(df_respeciate$spec, " SP$|UNNAMED$"))

  result <- suppressMessages(
    respeciate_generic(
      x = df_respeciate,
      group_vars = c("ptid", "type"),
      species_col = "spec",
      date_col = "specdate",
      window = 7
    )
  )

  post_unspec <- sum(stringr::str_detect(result$spec, " SP$|UNNAMED$"))
  expect_lte(post_unspec, pre_unspec)
})

# -----------------------------------------------------------
test_that("no change occurs when all isolates are already fully speciated", {
  df_full <- data.frame(
    ptid = c(1, 1, 2, 2),
    spec = c("ESCHERICHIA COLI", "ESCHERICHIA COLI",
             "KLEBSIELLA PNEUMONIAE", "KLEBSIELLA PNEUMONIAE"),
    type = "BLOOD",
    specdate = Sys.Date() - c(2, 1, 3, 0)
  )

  result <- suppressMessages(
    respeciate_generic(
      x = df_full,
      group_vars = c("ptid", "type"),
      species_col = "spec",
      date_col = "specdate",
      window = 5
    )
  )

  expect_equal(result$spec, df_full$spec)
})

# -----------------------------------------------------------
test_that("no change occurs when all isolates are unspecified", {
  df_sp <- data.frame(
    ptid = c(1, 1, 2, 2),
    spec = c("KLEBSIELLA SP", "KLEBSIELLA SP",
             "STAPHYLOCOCCUS SP", "STAPHYLOCOCCUS SP"),
    type = "BLOOD",
    specdate = Sys.Date() - c(2, 1, 3, 0)
  )

  result <- suppressMessages(
    respeciate_generic(
      x = df_sp,
      group_vars = c("ptid", "type"),
      species_col = "spec",
      date_col = "specdate",
      window = 5
    )
  )

  expect_equal(result$spec, df_sp$spec)
})
# -----------------------------------------------------------
test_that("respeciation does not change genus", {
  result <- suppressMessages(
    respeciate_generic(
      x = df_respeciate,
      group_vars = c("ptid", "type"),
      species_col = "spec",
      date_col = "specdate",
      window = 7
    )
  )

  # Extract genus from both before and after using the same pattern as function
  original_genus <- gsub("([A-Za-z]+).*", "\\1", df_respeciate$spec)
  new_genus <- gsub("([A-Za-z]+).*", "\\1", result$spec)

  # The genus should always match — respeciation shouldn't cross genera
  expect_equal(original_genus, new_genus)
})
# -----------------------------------------------------------
test_that("respeciation does not cross genus boundaries (tmp.genus included)", {
  df_fail_genus <- data.frame(
    ptid = c(1, 1, 1, 1),
    type = "BLOOD",
    spec = c(
      "KLEBSIELLA SP",
      "ESCHERICHIA COLI",
      "KLEBSIELLA UNNAMED",
      "ESCHERICHIA SP"
    ),
    specdate = as.Date("2024-01-01") + c(0, 1, 2, 3)
  )

  result <- suppressMessages(
    respeciate_generic(
      x = df_fail_genus,
      group_vars = c("ptid", "type"),
      species_col = "spec",
      date_col = "specdate",
      window = 5
    )
  )

  # Extract genus before and after
  original_genus <- gsub("([A-Za-z]+).*", "\\1", df_fail_genus$spec)
  new_genus <- gsub("([A-Za-z]+).*", "\\1", result$spec)

  # If tmp.genus is used correctly, genus must be identical
  expect_equal(original_genus, new_genus)
})
# -----------------------------------------------------------
test_that("no trailing UNNAMED remains after respeciation", {
  df_in <- data.frame(
    ptid = c(1,1,1,2,2,3,3,4,4),
    type = c("BLOOD","BLOOD","BLOOD","URINE","URINE","BLOOD","BLOOD","URINE","URINE"),
    specdate = as.Date("2025-11-01") + 0:8,
    spec = c(
      "ESCHERICHIA UNNAMED",
      "ESCHERICHIA UNNAMED ",
      "escherichia unnamed",
      "KLEBSIELLA   UNNAMED",
      "KLEBSIELLA UnNaMeD",
      "STAPHYLOCOCCUS SP",
      "STAPHYLOCOCCUS UNNAMED ",
      "ESCHERICHIA SP",
      "KLEBSIELLA PNEUMONIAE"
    ),
    stringsAsFactors = FALSE
  )

  res <- respeciate_generic(
    x = df_in,
    group_vars = c("ptid", "type"),
    species_col = "spec",
    date_col = "specdate",
    window = 7,
    .forceCopy = TRUE
  )

  # No species should end with the literal "UNNAMED" (case-insensitive)
  expect_false(
    any(str_detect(res$spec, stringr::regex("\\bUNNAMED\\s*$", ignore_case = TRUE))),
    info = "Found one or more species still ending with UNNAMED"
  )

  # Species column should be character and contain no NA values
  expect_type(res$spec, "character")
  expect_false(any(is.na(res$spec)), info = "Found NA in resulting species column")
})




