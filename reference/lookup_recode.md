# Lookup table switch handler

**\[stable\]** A function to call an epidm lookup table and recode where
we are aware of a new value.

Built in are the organism re-classifications and specimen_type groupings
and a manual mode.

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

  a character, vector or column containing the value(s) to be referenced

- type:

  a character value to denote the lookup table used

- .import:

  a list in the order list(new,old) containing the values for another
  lookup table existing in the environment

## Value

a list object of the recoded field

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
#> 6          ACTINOBACULUM SCHAALII               RECTUM 2025-01-02
#> 1         PROPIONIBACTERIUM ACNES       SYNOVIAL FLUID 2025-02-16
#> 12                 CANDIDA AUREUS    TRACHEAL ASPIRATE 2025-03-10
#> 9    TETRATHIOBACTER KASHMIRENSIS             DUODENUM 2025-05-18
#> 2         EUBACTERIUM AEROFACIENS            BRONCHIAL 2025-05-19
#> 7       CHRYSEOBACTERIUM MIRICOLA             PERIANAL 2025-05-22
#> 4            CORYNEBACTERIUM EQUI  LOWER GENITAL TRACT 2025-05-31
#> 3          ALCALIGENES PIECHAUDII   MIDDLE EAR/MASTOID 2025-06-05
#> 10               ESCHERICHIA COLI PUS (SOURCE UNKNOWN) 2025-07-20
#> 8  LEPTOSPIRA ICTEROHAEMORRHAGIAE                  EMU 2025-08-29
#> 5     STOMATOCOCCUS MUCILAGINOSUS           BIOPSY-NOS 2025-10-25
#> 11                     SARS-COV-2               SPLEEN 2025-11-12

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
#> 6          ACTINOBACULUM SCHAALII    ACTINOTIGNUM SCHAALII 2025-01-02
#> 1         PROPIONIBACTERIUM ACNES      CUTIBACTERIUM ACNES 2025-02-16
#> 12                 CANDIDA AUREUS           CANDIDA AUREUS 2025-03-10
#> 9    TETRATHIOBACTER KASHMIRENSIS   ADVENELLA KASHMIRENSIS 2025-05-18
#> 2         EUBACTERIUM AEROFACIENS  COLLINSELLA AEROFACIENS 2025-05-19
#> 7       CHRYSEOBACTERIUM MIRICOLA ELIZABETHKINGIA MIRICOLA 2025-05-22
#> 4            CORYNEBACTERIUM EQUI         RHODOCOCCUS EQUI 2025-05-31
#> 3          ALCALIGENES PIECHAUDII ACHROMOBACTER PIECHAUDII 2025-06-05
#> 10               ESCHERICHIA COLI         ESCHERICHIA COLI 2025-07-20
#> 8  LEPTOSPIRA ICTEROHAEMORRHAGIAE   LEPTOSPIRA INTERROGANS 2025-08-29
#> 5     STOMATOCOCCUS MUCILAGINOSUS      ROTHIA MUCILAGINOSA 2025-10-25
#> 11                     SARS-COV-2               SARS-COV-2 2025-11-12

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
#> 6  2025-01-02
#> 1  2025-02-16
#> 12 2025-03-10
#> 9  2025-05-18
#> 2  2025-05-19
#> 7  2025-05-22
#> 4  2025-05-31
#> 3  2025-06-05
#> 10 2025-07-20
#> 8  2025-08-29
#> 5  2025-10-25
#> 11 2025-11-12

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
