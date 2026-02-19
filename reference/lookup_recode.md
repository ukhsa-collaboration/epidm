# Lookup table switch handler

**\[stable\]**

A function to recode values via named lookup tables (i.e call an epidm
lookup table and recode where we are aware of a new value). It routes to
a specific lookup based on type, returning a character vector where each
input value has been mapped to its corresponding replacement. If a value
is not found in the lookup then the original value is returned.

Built‑in lookups include:

- **`species`**: Uses the `respeciate_organism` dataset to standardise
  and reclassify organism names (e.g., historic → current nomenclature).
  This supports consistent reporting across SGSS and other laboratory
  datasets.

- **`specimen`**: Uses the `specimen_type_grouping` dataset to assign
  raw laboratory specimen types into harmonised specimen groups. This
  enables consistent grouping for reporting, aggregation, and filtering.

- **`genus_gram_stain`**: Uses the `genus_gram_stain` lookup table,
  which provides Gram stain classifications by bacterial genus. This
  reference is manually maintained against the UKHSA SGSS database and
  supports rapid filtering and high‑level organism categorisation. Users
  should raise an issue or submit a pull request to the `epidm` GitHub
  repository if an organism/genus is missing.

- **`lab_data`**: Uses the `lab_data` lookup dataset for harmonising
  laboratory code systems and internal SGSS mappings, supporting
  standardised laboratory result interpretation within surveillance
  pipelines.

- **`inpatient_admission_method`**: Uses the internal lookup table
  `epidm:::group_inpatient_admission_method` to categorise raw hospital
  admission method codes into operationally meaningful groups.

- **`inpatient_discharge_destination`**: Uses the internal table
  `epidm:::group_inpatient_discharge_destination` to group hospital
  discharge destination codes into standardised categories for inpatient
  pathway analysis.

- **`ecds_destination_code`**: Uses the internal table
  `epidm:::group_ecds_discharge_destination`, providing grouped mappings
  for ECDS (Emergency Care Data Set) discharge codes.

- **`manual`**: Allows the user to supply their own lookup through
  `.import = list(new, old)`. This is useful when working with local,
  provisional, or evolving code sets not yet included in the package’s
  centralised lookup tables.

## Usage

``` r
lookup_recode(
  src,
  type = c("species", "specimen", "inpatient_admission_method",
    "inpatient_discharge_destination", "ecds_destination_code", "manual"),
  .import = NULL
)
```

## Arguments

- src:

  Character vector (or column) of values to recode. Coerced to character
  if needed.

- type:

  Character scalar specifying the lookup to use. One of: `'species'`,
  `'specimen'`, `'inpatient_admission_method'`,
  `'inpatient_discharge_destination'`, `'ecds_destination_code'`,
  `'manual'`.

- .import:

  A two‑element list in the format `list(new, old)` used only when
  `type = 'manual'`. Each element must be a vector of equal length.

## Value

A character vector containing the recoded values, aligned 1:1 with
`src`. Values not present in the lookup are returned unchanged.

## Examples

``` r
df <- data.frame(
  spec = c(
    sample(grep(")",
                respeciate_organism$previous_organism_name,
                value=TRUE,
                invert = TRUE),
           9),
    "ESCHERICHIA COLI","SARS-COV-2","CANDIDA AUREUS"),
  type = sample(specimen_type_grouping$specimen_type,12),
  date = sample(seq.Date(from = Sys.Date()-365,
                         to = Sys.Date(),
                         by = "day"),12)
)
df <- df[order(df$date),]

# show the data before the changes
df
#>                              spec                 type       date
#> 6          ACTINOBACULUM SCHAALII               RECTUM 2025-03-15
#> 1         PROPIONIBACTERIUM ACNES       SYNOVIAL FLUID 2025-04-29
#> 12                 CANDIDA AUREUS    TRACHEAL ASPIRATE 2025-05-21
#> 9    TETRATHIOBACTER KASHMIRENSIS             DUODENUM 2025-07-29
#> 2         EUBACTERIUM AEROFACIENS            BRONCHIAL 2025-07-30
#> 7       CHRYSEOBACTERIUM MIRICOLA             PERIANAL 2025-08-02
#> 4            CORYNEBACTERIUM EQUI  LOWER GENITAL TRACT 2025-08-11
#> 3          ALCALIGENES PIECHAUDII   MIDDLE EAR/MASTOID 2025-08-16
#> 10               ESCHERICHIA COLI PUS (SOURCE UNKNOWN) 2025-09-30
#> 8  LEPTOSPIRA ICTEROHAEMORRHAGIAE                  EMU 2025-11-09
#> 5     STOMATOCOCCUS MUCILAGINOSUS           BIOPSY-NOS 2026-01-05
#> 11                     SARS-COV-2               SPLEEN 2026-01-23

# check the lookup tables
# observe the changes
head(respeciate_organism[1:2])
#>                  previous_organism_name       organism_species_name
#> 1             ALCALIGENES DENITRIFICANS ACHROMOBACTER DENITRIFICANS
#> 2                ALCALIGENES PIECHAUDII    ACHROMOBACTER PIECHAUDII
#> 3              ALCALIGENES XYLOSOXIDANS  ACHROMOBACTER XYLOSOXIDANS
#> 4 ALCALIGENES XYLOSOXIDANS XYLOSOXIDANS  ACHROMOBACTER XYLOSOXIDANS
#> 5                ACTINOBACULUM SCHAALII       ACTINOTIGNUM SCHAALII
#> 6          TETRATHIOBACTER KASHMIRENSIS      ADVENELLA KASHMIRENSIS
df$species <- lookup_recode(df$spec,'species')
df[,c('spec','species','date')]
#>                              spec                  species       date
#> 6          ACTINOBACULUM SCHAALII    ACTINOTIGNUM SCHAALII 2025-03-15
#> 1         PROPIONIBACTERIUM ACNES      CUTIBACTERIUM ACNES 2025-04-29
#> 12                 CANDIDA AUREUS           CANDIDA AUREUS 2025-05-21
#> 9    TETRATHIOBACTER KASHMIRENSIS   ADVENELLA KASHMIRENSIS 2025-07-29
#> 2         EUBACTERIUM AEROFACIENS  COLLINSELLA AEROFACIENS 2025-07-30
#> 7       CHRYSEOBACTERIUM MIRICOLA ELIZABETHKINGIA MIRICOLA 2025-08-02
#> 4            CORYNEBACTERIUM EQUI         RHODOCOCCUS EQUI 2025-08-11
#> 3          ALCALIGENES PIECHAUDII ACHROMOBACTER PIECHAUDII 2025-08-16
#> 10               ESCHERICHIA COLI         ESCHERICHIA COLI 2025-09-30
#> 8  LEPTOSPIRA ICTEROHAEMORRHAGIAE   LEPTOSPIRA INTERROGANS 2025-11-09
#> 5     STOMATOCOCCUS MUCILAGINOSUS      ROTHIA MUCILAGINOSA 2026-01-05
#> 11                     SARS-COV-2               SARS-COV-2 2026-01-23

head(specimen_type_grouping)
#>      specimen_type specimen_group
#> 1            BLOOD          Blood
#> 2       CORD BLOOD          Blood
#> 3            JOINT Bones & Joints
#> 4   SYNOVIAL FLUID Bones & Joints
#> 5             BONE Bones & Joints
#> 6 JOINT PROSTHESIS Bones & Joints
df$grp <- lookup_recode(df$type,'specimen')
df[,c('species','type','grp','date')]
#>                     species                 type                     grp
#> 6     ACTINOTIGNUM SCHAALII               RECTUM       Faeces & Lowergut
#> 1       CUTIBACTERIUM ACNES       SYNOVIAL FLUID          Bones & Joints
#> 12           CANDIDA AUREUS    TRACHEAL ASPIRATE           URT/Mouth/Ear
#> 9    ADVENELLA KASHMIRENSIS             DUODENUM       Faeces & Lowergut
#> 2   COLLINSELLA AEROFACIENS            BRONCHIAL Lower Respiratory Tract
#> 7  ELIZABETHKINGIA MIRICOLA             PERIANAL       Faeces & Lowergut
#> 4          RHODOCOCCUS EQUI  LOWER GENITAL TRACT                 Genital
#> 3  ACHROMOBACTER PIECHAUDII   MIDDLE EAR/MASTOID           URT/Mouth/Ear
#> 10         ESCHERICHIA COLI PUS (SOURCE UNKNOWN)                  Fluids
#> 8    LEPTOSPIRA INTERROGANS                  EMU            Urine/Kidney
#> 5       ROTHIA MUCILAGINOSA           BIOPSY-NOS           Swabs-General
#> 11               SARS-COV-2               SPLEEN                 Tissues
#>          date
#> 6  2025-03-15
#> 1  2025-04-29
#> 12 2025-05-21
#> 9  2025-07-29
#> 2  2025-07-30
#> 7  2025-08-02
#> 4  2025-08-11
#> 3  2025-08-16
#> 10 2025-09-30
#> 8  2025-11-09
#> 5  2026-01-05
#> 11 2026-01-23

# for a tidyverse use
# df %>% mutate(spec=lookup_recode(spec,'species))

# manual input of your own lookup
# .import=list(new,old)
lookup_recode(
  "ALCALIGENES DENITRIFICANS",
  type = 'manual',
  .import=list(respeciate_organism$organism_species_name,
               respeciate_organism$previous_organism_name)
  )
#> [1] "ACHROMOBACTER DENITRIFICANS"
```
