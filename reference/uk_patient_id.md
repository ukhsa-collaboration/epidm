# Patient ID record grouping

**\[stable\]**

Assigns a **single integer `id`** to records that belong to the same
patient by applying a sequence of **deterministic matching stages**
across common identifiers (NHS number, hospital number, DOB, name, sex,
postcode). Identifiers are standardised, validated using NHS checksum
function, and fuzzy name keys are used in later stages. ' Matching is
performed in order through the following stages (first match is
applied):

1.  NHS number + date of birth

2.  Hospital number + date of birth

3.  NHS number + hospital number

4.  NHS number + surname

5.  Hospital number + surname

6.  Date of birth + surname (only where NHS is invalid/absent)

7.  Sex + full name (forename + surname)

8.  Sex + date of birth + fuzzy name (Soundex; surname + initial)

9.  Date of birth (YYYY-MM) + fuzzy name

10. Surname/forename + postcode

11. Name swaps (forename/surname reversed) + date of birth

Use `.useStages` to restrict which stages are applied (default: `1:11`).
The function generates a reproducible `id` per patient within the sort
order; you can provide `.sortOrder` (e.g., a date column) to make
assignment deterministic.

**Validity rules applied:**

- **NHS number** validated using the standard checksum
  ([`epidm::valid_nhs()`](valid_nhs.md)).

- **Hospital number**: excludes known placeholders (e.g., `"UNKNOWN"`,
  `"NO PATIENT ID"`).

- **DOB**: excludes proxy or missing dates (`"1900-01-01"`,
  `"1800-01-01"`, `NA`).

- **Sex**: normalised to `"M"` / `"F"`; others → `NA`.

- **Names**: uppercased, Latin characters normalised; Soundex used for
  fuzzy matching.

Identifiers are copied over where they are missing or invalid to the
grouped records.

## Usage

``` r
uk_patient_id(
  x,
  id = list(nhs_number = "nhs_number", hospital_number = "patient_hospital_number",
    date_of_birth = "date_of_birth", sex_mfu = "sex", forename = "forename", surname =
    "surname", postcode = "postcode"),
  .useStages = c(1:11),
  .keepStages = FALSE,
  .keepValidNHS = FALSE,
  .sortOrder,
  .forceCopy = FALSE
)
```

## Arguments

- x:

  A `data.frame` or `data.table` with patient identifiers.

- id:

  A **named list** of quoted column names:

  `nhs_number`

  :   NHS number.

  `hospital_number`

  :   Local patient identifier (hospital number).

  `date_of_birth`

  :   Date of birth.

  `sex_mfu`

  :   Sex/gender (M/F/Unknown).

  `forename`

  :   Forename / first name.

  `surname`

  :   Surname / last name.

  `postcode`

  :   Patient postcode.

- .useStages:

  optional, default 1:11; set to 1 if you wish patient ID to be assigned
  cases with the same DOB and NHS number, set to 2 if you wish patient
  ID to be assigned to cases with the same hospital number (HOS) and
  DOB, set to 3 if you wish patient ID to be assigned cases with the
  same NHS and HOS number, set to 4 if you wish patient ID to be
  assigned cases with the same NHS number and surname, set to 5 if you
  wish patient ID to be assigned cases with the same hospital number and
  surname, set to 6 if you wish patient ID to be assigned cases with the
  same DOB and surname, set to 7 if you wish patient ID to be assigned
  cases with the same sex and full name, set to 8 if you wish patient ID
  to be assigned cases with the same sex, DOB and fuzzy name, set to 9
  if you wish patient ID to be assigned cases with the same DOB and
  fuzzy name, set to 10 if you wish patient ID to be assigned cases with
  the same name and postcode, set to 11 if you wish patient ID to be
  assigned cases with the same first name or second name in changing
  order and date of birth.

- .keepStages:

  optional, default FALSE; to generate a new column (stageMatch) to
  retain the stage information for which the record matched the group.

- .keepValidNHS:

  optional, default FALSE; set TRUE if you wish to retain the column
  with the NHS checksum result stored as a BOOLEAN

- .sortOrder:

  optional; a column as a character to allow a sorting order on the id
  generation

- .forceCopy:

  optional, default FALSE; TRUE will force data.table to take a copy
  instead of editing the data without reference

## Value

A `data.table` with the original columns plus:

- `id`:

  Integer patient identifier assigned by staged matching.

- `valid_nhs`:

  (Optional) BOOLEAN NHS checksum flag; included when
  `.keepValidNHS = TRUE`.

## Workflow context

`uk_patient_id()` is typically used early to harmonise patient identity
across isolates before downstream tasks such as specimen episode
grouping ([`group_time()`](group_time.md)), dataset linkage (e.g., to
HES/SUS/ECDS), and epidemiological reporting.

## Examples

``` r
id_test <-
  data.frame(
    stringsAsFactors = FALSE,
    record_id = c(1L,2L,3L,4L,
                  5L,6L,7L,8L,9L,10L,11L,12L,13L,14L,15L,
                  16L,17L,18L,19L,20L,21L,22L,23L,24L),
    nhs_number = c(9435754422,
                   9435754422,NA,9435754422,5555555555,NA,
                   9435773982,NA,9999999999,NA,9435773982,NA,
                   9435802508,9435802508,NA,NA,9435802508,9435802508,NA,
                   3333333333,NA,9999999999,9435817777,
                   9435817777),
    local_patient_identifier = c(NA,"IG12067",
                                 NA,NA,"IG12067","IG12067","KR2535","KR2535",
                                 "KR2535",NA,NA,NA,"UK8734","UK8734",NA,NA,
                                 "UK8734","UK8734",NA,NA,"JH45204",
                                 "HS45202","HS45202","JH45204"),
    patient_birth_date = c("1993-07-16",
                           "1993-07-16","1993-07-16","1993-07-16",
                           "1993-07-16",NA,"1967-02-10",NA,"1967-02-10",NA,NA,
                           "1967-02-10",NA,NA,"1952-10-22","1952-10-22",
                           "1952-10-22",NA,"1947-09-14","1947-09-14",
                           "1947-09-14","1947-09-14","1947-09-14",
                           "1947-09-14"),
    sex = c("Male","Male",
            "Male","Male",NA,"Male","Female","Female",
            "Female","Female","Female","Female","Male",
            "Male","Male","Male","Male","Male","Male",
            "Male","Male","Male",NA,"Male"),
    forename = c(NA,"DENNIS",
                 NA,NA,"DENNIS",NA,"ELLIE","ELLIE",NA,
                 "ELLIE","ELLIE","ELLIE","IAN","IAN","MALCOLM",
                 "IAN","IAN",NA,"GRANT","ALAN","ALAN","ALAN",
                 "GRANT","ALAN"),
    surname = c(NA,"NEDRY",
                "NEDRY",NA,"NEDRY","NEDRY","SATTLER","SATTLER",
                NA,"SATTLER","SATTLER","SATTLER","M",NA,
                "IAN","MALCOLM","MALCOLM",NA,"ALAN","GRANT",
                "GRANT","GRANT","ALAN","GRANT"),
    postcode = c("HA4 0FF",
                 "HA4 0FF","HA4 0FF",NA,"HA4 0FF","HA4 0FF",
                 "L3 1DZ","L3 1DZ","L3 1DZ","L3 1DZ",NA,"L3 1DZ",
                 "BN14 9EP",NA,"BN14 9EP",NA,NA,NA,"CW6 9TX",
                 "CW6 9TX",NA,NA,NA,NA),
    specimen_date = c("2024-08-14",
                      "2023-02-03","2023-02-07","2023-02-04",
                      "2023-02-09","2024-08-14","2021-03-28","2021-03-28",
                      "2021-03-28","2021-03-28","2021-03-28",
                      "2021-03-28","2024-07-06","2024-07-06","2024-07-06",
                      "2023-10-31","2023-10-31","2023-10-31",
                      "2022-01-23","2022-01-24","2022-01-25","2022-01-26",
                      "2022-01-27","2022-01-28")
  )

data.table::setDT(id_test)

uk_patient_id(
  x = id_test,
  id = list(
    nhs_number = 'nhs_number',
    hospital_number = 'local_patient_identifier',
    date_of_birth = 'patient_birth_date',
    sex_mfu = 'sex',
    forename = 'forename',
    surname = 'surname',
    postcode = 'postcode'
  ),
  .sortOrder = 'specimen_date',
  .useStages = c(1:11),
  .keepStages = TRUE,
  .forceCopy = TRUE)[]
#> Warning: NHS number is missing or empty
#> Warning: NHS number is missing or empty
#> Warning: NHS number is missing or empty
#> Warning: NHS number is missing or empty
#> Warning: NHS number is missing or empty
#> Warning: NHS number is missing or empty
#> Warning: NHS number is missing or empty
#> Warning: NHS number is missing or empty
#> Warning: NHS number is missing or empty
#>        id        stageMatch record_id nhs_number local_patient_identifier
#>     <int>            <char>     <int>     <char>                   <char>
#>  1:     1 s2s4s5s6s7s8s9s10         7 9435773982                   KR2535
#>  2:     1           s5s7s10         8       <NA>                   KR2535
#>  3:     1                s2         9 9999999999                   KR2535
#>  4:     1             s7s10        10       <NA>                     <NA>
#>  5:     1              s4s7        11 9435773982                     <NA>
#>  6:     1       s6s7s8s9s10        12       <NA>                     <NA>
#>  7:    11              s6s9        19       <NA>                     <NA>
#>  8:    11          s6s7s8s9        20 3333333333                     <NA>
#>  9:    11      s2s5s6s7s8s9        21       <NA>                  JH45204
#> 10:    11        s2s6s7s8s9        22 9999999999                  HS45202
#> 11:    11          s1s2s6s9        23 9435817777                  HS45202
#> 12:    11    s1s2s5s6s7s8s9        24 9435817777                  JH45204
#> 13:    13     s1s2s5s6s9s10         2 9435754422                  IG12067
#> 14:    13                s1         4 9435754422                     <NA>
#> 15:    13             s6s10         3       <NA>                     <NA>
#> 16:    13       s2s5s6s9s10         5 5555555555                  IG12067
#> 17:    13                s1         1 9435754422                     <NA>
#> 18:    13             s5s10         6       <NA>                  IG12067
#> 19:    18                s3        18 9435802508                   UK8734
#> 20:    18       s6s7s8s9s11        16       <NA>                     <NA>
#> 21:    18     s3s6s7s8s9s11        17 9435802508                   UK8734
#> 22:    18                s3        13 9435802508                   UK8734
#> 23:    18                s3        14 9435802508                   UK8734
#> 24:    18               s11        15       <NA>                     <NA>
#>        id        stageMatch record_id nhs_number local_patient_identifier
#>     <int>            <char>     <int>     <char>                   <char>
#>     patient_birth_date    sex forename surname postcode specimen_date
#>                 <char> <char>   <char>  <char>   <char>        <char>
#>  1:         1967-02-10      F    ELLIE SATTLER    L31DZ    2021-03-28
#>  2:               <NA>      F    ELLIE SATTLER    L31DZ    2021-03-28
#>  3:         1967-02-10      F     <NA>    <NA>    L31DZ    2021-03-28
#>  4:               <NA>      F    ELLIE SATTLER    L31DZ    2021-03-28
#>  5:               <NA>      F    ELLIE SATTLER     <NA>    2021-03-28
#>  6:         1967-02-10      F    ELLIE SATTLER    L31DZ    2021-03-28
#>  7:         1947-09-14      M    GRANT    ALAN   CW69TX    2022-01-23
#>  8:         1947-09-14      M     ALAN   GRANT   CW69TX    2022-01-24
#>  9:         1947-09-14      M     ALAN   GRANT     <NA>    2022-01-25
#> 10:         1947-09-14      M     ALAN   GRANT     <NA>    2022-01-26
#> 11:         1947-09-14   <NA>    GRANT    ALAN     <NA>    2022-01-27
#> 12:         1947-09-14      M     ALAN   GRANT     <NA>    2022-01-28
#> 13:         1993-07-16      M   DENNIS   NEDRY   HA40FF    2023-02-03
#> 14:         1993-07-16      M     <NA>    <NA>     <NA>    2023-02-04
#> 15:         1993-07-16      M     <NA>   NEDRY   HA40FF    2023-02-07
#> 16:         1993-07-16   <NA>   DENNIS   NEDRY   HA40FF    2023-02-09
#> 17:         1993-07-16      M     <NA>    <NA>   HA40FF    2024-08-14
#> 18:               <NA>      M     <NA>   NEDRY   HA40FF    2024-08-14
#> 19:               <NA>      M     <NA>    <NA>     <NA>    2023-10-31
#> 20:         1952-10-22      M      IAN MALCOLM     <NA>    2023-10-31
#> 21:         1952-10-22      M      IAN MALCOLM     <NA>    2023-10-31
#> 22:               <NA>      M      IAN       M  BN149EP    2024-07-06
#> 23:               <NA>      M      IAN    <NA>     <NA>    2024-07-06
#> 24:         1952-10-22      M  MALCOLM     IAN  BN149EP    2024-07-06
#>     patient_birth_date    sex forename surname postcode specimen_date
#>                 <char> <char>   <char>  <char>   <char>        <char>

```
