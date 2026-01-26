library(testthat)
library(data.table)
library(patrick)

# Sample minimal data
sample_data <- data.table(
  id = c("1","1","2"),
  provider = c("A","A","B"),
  spell_start = as.Date(c("2020-01-01","2020-01-03","2020-02-01")),
  spell_end = as.Date(c("2020-01-02","2020-01-05","2020-02-02")),
  adm_meth = c("21","81","21"),
  adm_src = c("19","51","19"),
  dis_dest = c("51","51","19"),
  patclass = c("1","1","1")
)

#  Tests

test_that("Errors on non-data.frame input", {
  expect_error(cip_spells(x = "not_a_df",
                          group_vars = c("id","provider"),
                          spell_start_date = "spell_start",
                          admission_method = "adm_meth",
                          admission_source = "adm_src",
                          spell_end_date = "spell_end",
                          discharge_destination = "dis_dest",
                          patient_classification = "patclass"),
               "must be a data.frame")
})

test_that("Errors when required columns are missing; spell_start", {
  bad_data <- sample_data[, !"spell_start"]
  expect_error(cip_spells(x = bad_data,
                          group_vars = c("id","provider"),
                          spell_start_date = "spell_start",
                          admission_method = "adm_meth",
                          admission_source = "adm_src",
                          spell_end_date = "spell_end",
                          discharge_destination = "dis_dest",
                          patient_classification = "patclass"),
               "Missing required columns")
})

test_that("Errors when required columns are missing; spell_end", {
  bad_data2 <- sample_data[, !"spell_end"]
  expect_error(cip_spells(x = bad_data2,
                          group_vars = c("id","provider"),
                          spell_start_date = "spell_start",
                          admission_method = "adm_meth",
                          admission_source = "adm_src",
                          spell_end_date = "spell_end",
                          discharge_destination = "dis_dest",
                          patient_classification = "patclass"),
               "Missing required columns")
})

test_that("Errors when required columns are missing; adm_meth", {
  bad_data3 <- sample_data[, !"adm_meth"]
  expect_error(cip_spells(x = bad_data3,
                          group_vars = c("id","provider"),
                          spell_start_date = "spell_start",
                          admission_method = "adm_meth",
                          admission_source = "adm_src",
                          spell_end_date = "spell_end",
                          discharge_destination = "dis_dest",
                          patient_classification = "patclass"),
               "Missing required columns")
})

test_that("Errors when required columns are missing; adm_src", {
  bad_data4 <- sample_data[, !"adm_src"]
  expect_error(cip_spells(x = bad_data4,
                          group_vars = c("id","provider"),
                          spell_start_date = "spell_start",
                          admission_method = "adm_meth",
                          admission_source = "adm_src",
                          spell_end_date = "spell_end",
                          discharge_destination = "dis_dest",
                          patient_classification = "patclass"),
               "Missing required columns")
})

test_that("Errors when required columns are missing; dis_dest", {
  bad_data5 <- sample_data[, !"dis_dest"]
  expect_error(cip_spells(x = bad_data5,
                          group_vars = c("id","provider"),
                          spell_start_date = "spell_start",
                          admission_method = "adm_meth",
                          admission_source = "adm_src",
                          spell_end_date = "spell_end",
                          discharge_destination = "dis_dest",
                          patient_classification = "patclass"),
               "Missing required columns")
})

test_that("Errors when required columns are missing; patclass", {
  bad_data6 <- sample_data[, !"patclass"]
  expect_error(cip_spells(x = bad_data6,
                          group_vars = c("id","provider"),
                          spell_start_date = "spell_start",
                          admission_method = "adm_meth",
                          admission_source = "adm_src",
                          spell_end_date = "spell_end",
                          discharge_destination = "dis_dest",
                          patient_classification = "patclass"),
               "Missing required columns")
})

test_that("Errors when date columns are not Date type", {
  bad_data <- copy(sample_data)
  bad_data[, spell_start := as.character(spell_start)]
  expect_error(cip_spells(x = bad_data,
                          group_vars = c("id","provider"),
                          spell_start_date = "spell_start",
                          admission_method = "adm_meth",
                          admission_source = "adm_src",
                          spell_end_date = "spell_end",
                          discharge_destination = "dis_dest",
                          patient_classification = "patclass"),
               "must be of type Date")
})

test_that("Handles empty data", {
  empty_data <- sample_data[0]
  expect_error(cip_spells(x = empty_data,
                          group_vars = c("id","provider"),
                          spell_start_date = "spell_start",
                          admission_method = "adm_meth",
                          admission_source = "adm_src",
                          spell_end_date = "spell_end",
                          discharge_destination = "dis_dest",
                          patient_classification = "patclass"),
               "zero rows")
})

test_that("Returns expected columns", {
  result <- cip_spells(x = sample_data,
                       group_vars = c("id","provider"),
                       spell_start_date = "spell_start",
                       admission_method = "adm_meth",
                       admission_source = "adm_src",
                       spell_end_date = "spell_end",
                       discharge_destination = "dis_dest",
                       patient_classification = "patclass")

  expected_cols <- c("id","provider","spell_start","spell_end","adm_meth","adm_src",
                     "dis_dest","patclass","proxy_missing","cip_indx", "cip_spell_start", "cip_spell_end")
  expect_equal(expected_cols, names(result))
})

test_that("CIP spell grouping logic works", {
  result <- cip_spells(x = sample_data,
                       group_vars = c("id","provider"),
                       spell_start_date = "spell_start",
                       admission_method = "adm_meth",
                       admission_source = "adm_src",
                       spell_end_date = "spell_end",
                       discharge_destination = "dis_dest",
                       patient_classification = "patclass")

  expect_true(length(unique(result[id == "1", cip_indx])) == 2)
})


# Parameter combinations
cases <- cases(
  id_only_copy_true  = list(forceCopy = TRUE,  group_vars = c("id")),
  id_only_copy_false = list(forceCopy = FALSE, group_vars = c("id")),
  idprov_copy_true   = list(forceCopy = TRUE,  group_vars = c("id", "provider")),
  idprov_copy_false  = list(forceCopy = FALSE, group_vars = c("id", "provider"))
)

with_parameters_test_that(
  "Handles parameter combinations",
  .cases = cases,
  .interpret_glue = FALSE,
  {
    result <- cip_spells(
      x = sample_data,
      group_vars = group_vars,
      spell_start_date = "spell_start",
      admission_method = "adm_meth",
      admission_source = "adm_src",
      spell_end_date = "spell_end",
      discharge_destination = "dis_dest",
      patient_classification = "patclass",
      .forceCopy = forceCopy
    )
    expect_s3_class(result, "data.table")
  }
)

test_that("Warns on NA dates", {
  bad_data <- copy(sample_data)
  bad_data[1, spell_start := NA]
  expect_warning(cip_spells(x = bad_data,
                            group_vars = c("id","provider"),
                            spell_start_date = "spell_start",
                            admission_method = "adm_meth",
                            admission_source = "adm_src",
                            spell_end_date = "spell_end",
                            discharge_destination = "dis_dest",
                            patient_classification = "patclass"),
                 "missing values")
})

