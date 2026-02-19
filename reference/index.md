# Package index

## Utility Functions

Functions for reading or writing data from various sources

- [`sql_clean()`](sql_clean.md) **\[stable\]** : Clean and Read a SQL
  query
- [`sql_connect()`](sql_connect.md) **\[stable\]** : Connect to a SQL
  database
- [`sql_read()`](sql_read.md) **\[stable\]** : Read a table from a SQL
  database
- [`sql_write()`](sql_write.md) **\[stable\]** : Write a table to a SQL
  database
- [`csv_from_zip()`](csv_from_zip.md) **\[stable\]** : Download a csv
  from a zip
- [`lookup_recode()`](lookup_recode.md) **\[stable\]** : Lookup table
  switch handler

## Data Management Functions

Functions for handling, cleaning and processing public health data

- [`valid_nhs()`](valid_nhs.md) **\[stable\]** : NHS Number Validity
  Check
- [`uk_patient_id()`](uk_patient_id.md) **\[stable\]** : Patient ID
  record grouping
- [`proxy_episode_dates()`](proxy_episode_dates.md) **\[stable\]** :
  Clean and Impute HES/SUS Episode Start and End Dates
- [`cip_spells()`](cip_spells.md) **\[stable\]** : Continuous Inpatient
  (CIP) Spells
- [`group_time()`](group_time.md) **\[stable\]** : Grouping of intervals
  or events that occur close together in time
- [`respeciate_generic()`](respeciate_generic.md) **\[stable\]** :
  Respeciate unspecified samples
- [`inpatient_codes()`](inpatient_codes.md) **\[experimental\]** :
  Inpatient Codes cleanup

## Lookup tables

Lookup tables used to reclassify, regroup, or translate data

- [`genus_gram_stain`](genus_gram_stain.md) : Bacterial Genus Gram Stain
  Lookup Table
- [`respeciate_organism`](respeciate_organism.md) : Respeciated
  organisms
- [`specimen_type_grouping`](specimen_type_grouping.md) : Specimen type
  grouping
- [`group_ecds_discharge_destination`](group_ecds_discharge_destination.md)
  : A&E attendance discharge destination
- [`group_inpatient_admission_method`](group_inpatient_admission_method.md)
  : Inpatient admission methods
- [`group_inpatient_discharge_destination`](group_inpatient_discharge_destination.md)
  : Inpatient discharge destination
- [`hospital_in_out_dates()`](hospital_in_out_dates.md)
  **\[experimental\]** : Hospital IN/OUT dates

## Data

Synthetic dataset for testing package functionality

- [`lab_data`](lab_data.md) : Synthetic Lab Data for epidm
