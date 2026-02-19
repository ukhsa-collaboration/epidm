# Inpatient Codes cleanup

**\[experimental\]**

When HES/SUS ICD/OPCS codes are provided in wide format you may want to
clean them up into long for easier analysis. This function helps by
reshaping long as a separate table. Ensuring they're separate allows you
to retain source data, and aggregate appropriately later.

## Usage

``` r
inpatient_codes(
  x,
  field_strings,
  patient_id_vars,
  type = c("icd9", "icd10", "opcs"),
  .forceCopy = FALSE
)
```

## Arguments

- x:

  a data.frame or data.table containing inpatient data

- field_strings:

  a vector or string containing the regex for the the columns

- patient_id_vars:

  a vector containing colnames used to identify a patient episode or
  spell

- type:

  a string to denote if the codes are diagnostic or procedural

- .forceCopy:

  Logical (default `FALSE`). If `FALSE`, the input is converted to a
  `data.table` and modified by reference. If `TRUE`, the input must
  already be a `data.table`, and the function will create an explicit
  copy to avoid modifying the original object.

## Value

a separate table with codes and id in long form

## Examples

``` r
# Example inpatient dataset
inpatient_test <- data.frame(
  id = c(1053L, 5487L, 8180L),
  spell_id = c("dwPDw", "iSpUq", "qpgk5"),
  primary_diagnosis_code = c("K602", "U071-", "I501"),
  procedure_code = c("H201", "H251", NA),
  procedure_date = as.Date(c("2023-01-01", "2023-01-04", NA))
)

# ICD-10 cleaning example
inpatient_codes(
  x = inpatient_test,
  field_strings = "diagnosis",
  patient_id_vars = c("id", "spell_id"),
  type = "icd10"
)
#>       id spell_id                  order  icd10 order_n
#>    <int>   <char>                 <char> <char>   <int>
#> 1:  1053    dwPDw primary_diagnosis_code   K602       1
#> 2:  5487    iSpUq primary_diagnosis_code   U071       1
#> 3:  8180    qpgk5 primary_diagnosis_code   I501       1

# OPCS cleaning example
inpatient_codes(
  x = inpatient_test,
  field_strings = c("procedure_code", "procedure_date"),
  patient_id_vars = c("id", "spell_id"),
  type = "opcs"
)
#>       id spell_id order_n   opcs       date
#>    <int>   <char>  <char> <char>     <Date>
#> 1:  1053    dwPDw       1   H201 2023-01-01
#> 2:  5487    iSpUq       1   H251 2023-01-04
```
