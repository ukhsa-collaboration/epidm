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

  # Reorder both input and output by ptid, type, specdate
  df_full_sorted <- df_full[order(df_full$ptid, df_full$type, df_full$specdate), ]
  result_sorted <- result[order(result$ptid, result$type, result$specdate), ]

  expect_equal(result_sorted$spec, df_full_sorted$spec)
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

  # Reorder both input and output by ptid, type, specdate
  df_sp_sorted <- df_sp[order(df_sp$ptid, df_sp$type, df_sp$specdate), ]
  result_sorted <- result[order(result$ptid, result$type, result$specdate), ]

  expect_equal(result_sorted$spec, df_sp_sorted$spec)
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
    window = 7
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
# -----------------------------------------------------------
test_that("respeciation never crosses genus boundaries, even with mixed genera", {
  set.seed(123)

  # dataset 1 — normal cases where respeciation *should* occur within genus
  df_respeciate_test <- data.frame(
    ptid = rep(1:4, each = 6),
    type = rep(c("BLOOD", "URINE"), each = 12),
    spec = c(
      # Patient 1 (Klebsiella, should respeciate within genus)
      "KLEBSIELLA PNEUMONIAE", "KLEBSIELLA SP", "KLEBSIELLA UNNAMED",
      "KLEBSIELLA OXYTOCA", "KLEBSIELLA SP", "KLEBSIELLA SP",

      # Patient 2 (E. coli, should respeciate within genus)
      "ESCHERICHIA COLI", "ESCHERICHIA SP", "ESCHERICHIA UNNAMED",
      "ESCHERICHIA SP", "ESCHERICHIA COLI", "ESCHERICHIA SP",

      # Patient 3 (Staph, should respeciate within genus)
      "STAPHYLOCOCCUS AUREUS", "STAPHYLOCOCCUS SP", "STAPHYLOCOCCUS UNNAMED",
      "STAPHYLOCOCCUS AUREUS", "STAPHYLOCOCCUS SP", "STAPHYLOCOCCUS SP",

      # Patient 4 (deliberate mixture to test genus isolation)
      "KLEBSIELLA SP", "ESCHERICHIA COLI", "STAPHYLOCOCCUS SP",
      "KLEBSIELLA PNEUMONIAE", "ESCHERICHIA UNNAMED", "STAPHYLOCOCCUS AUREUS"
    ),
    specdate = rep(seq.Date(Sys.Date() - 10, Sys.Date(), by = "2 days"), length.out = 24)
  )

  # dataset 2 — an explicit contamination test (would fail if tmp.genus not in group_by)
  df_cross_genus <- data.frame(
    ptid = 5:6,  # separate patients to isolate groups
    type = c("BLOOD", "BLOOD"),
    spec = c(
      "KLEBSIELLA SP",     # patient 5
      "ESCHERICHIA COLI"   # patient 6
    ),
    specdate = as.Date("2024-01-01") + c(0, 0)
  )

  # Combine datasets
  df_all <- data.table::rbindlist(list(df_respeciate_test, df_cross_genus), use.names = TRUE, fill = TRUE)

  # Run respeciation function
  result <- suppressMessages(
    respeciate_generic(
      x = df_all,
      group_vars = c("ptid", "type"),
      species_col = "spec",
      date_col = "specdate",
      window = 7,
      .forceCopy = TRUE
    )
  )

  # Sort both original and result the same way (ptid, type, specdate)
  df_all_sorted <- df_all[order(ptid, type, specdate), ]
  result_sorted <- result[order(ptid, type, specdate), ]

  # Extract genus
  original_genus <- gsub("([A-Za-z]+).*", "\\1", df_all_sorted$spec)
  new_genus <- gsub("([A-Za-z]+).*", "\\1", result_sorted$spec)

  # expect exact genus match — respeciation must never cross genera
  expect_equal(
    new_genus,
    original_genus,
    info = paste(
      "Respeciation changed genus at rows:",
      paste(which(new_genus != original_genus), collapse = ", ")
    )
  )

  # additional check — within each (ptid, type) group,
  # there should be only one genus after respeciation
  genus_by_group <- result_sorted[, .(unique_genera = unique(gsub("([A-Za-z]+).*", "\\1", spec))),
                                  by = .(ptid, type)]
  multi_genus_groups <- genus_by_group[lengths(unique_genera) > 1]

  expect_true(
    nrow(multi_genus_groups) == 0,
    info = paste("Groups with multiple genera detected:", paste(multi_genus_groups$ptid, collapse = ", "))
  )
})

test_that(".forceCopy behaves as expected", {

  df_before <- df_respeciate
  df_respeciate_df <- data.table::setDT(df_respeciate)
  result_force <- respeciate_generic(
    x = df_respeciate_df,
    group_vars = c("ptid", "type"),
    species_col = "spec",
    date_col = "specdate",
    window = 7,
    .forceCopy = TRUE
  )
  expect_equal(df_before, df_respeciate_df)

})

test_that("errors when .forceCopy is not a single logical", {

  df_respeciate_df <- data.table::setDT(df_respeciate)

  expect_error(
    respeciate_generic(
      x = df_respeciate_df,
      group_vars = c("ptid", "type"),
      species_col = "spec",
      date_col = "specdate",
      window = 7,
      .forceCopy = "no"
    ),
    "`.forceCopy` must be a single TRUE/FALSE value.",
    fixed = TRUE
  )
})



