# Synthetic Lab Data for epidm

A dataset containing synthetic lab data for testing epidemiological data
transformation functions.

A dataset containing synthetic lab data for testing epidemiological data
transformation functions.

## Usage

``` r
data(lab_data)

data(lab_data)
```

## Format

A data frame with the following columns:

- nhs_number:

  NHS number

- local_patient_identifier:

  Patient identifier such as hospital number

- patient_birth_date:

  Date of birth of the patients.

- sex:

  Gender of the patients (Factor with levels: "Female", "Male").

- surname:

  Patient surname

- forename:

  Patient forename

- organism_species_name:

  Organism species name (Factor with levels: "KLEBSIELLA PNEUMONIAE").

- specimen_date:

  Date of specimen collection.

- specimen_type:

  Type of specimen: BLOOD or URINE.

- lab_code:

  Laboratory codes (Factor with unique levels).

- local_authority_name:

  Name of the local authority.

- local_authority_code:

  Code of the local authority.

- postcode:

  Postcode

A data frame with the following columns:

- nhs_number:

  NHS number

- local_patient_identifier:

  Patient identifier such as hospital number

- patient_birth_date:

  Date of birth of the patients.

- sex:

  Gender of the patients (Factor with levels: "Female", "Male").

- surname:

  Patient surname

- forename:

  Patient forename

- organism_species_name:

  Organism species name (Factor with levels: "KLEBSIELLA PNEUMONIAE").

- specimen_date:

  Date of specimen collection.

- specimen_type:

  Type of specimen: BLOOD or URINE.

- lab_code:

  Laboratory codes (Factor with unique levels).

- local_authority_name:

  Name of the local authority.

- local_authority_code:

  Code of the local authority.

- postcode:

  Postcode

## Examples

``` r
data(lab_data)
head(lab_data)
#>   nhs_number local_patient_identifier patient_birth_date    sex    surname
#> 1  576322970                  IG12067         05/10/1938 Female   WILLIAMS
#> 2  325999719                  XB29898         04/04/1957   Male EL-SHAHEED
#> 3  705649596                   KR2535         24/06/1927   Male     LINTON
#> 4  115685428                  HS45202         14/06/1962   Male       RODA
#> 5  705649596                   KR2535         24/06/1927   Male     LINTON
#> 6  115685428                  HS45202         14/06/1962   Male       RODA
#>   forename organism_species_name specimen_date specimen_type lab_code
#> 1 JAMILETH KLEBSIELLA PNEUMONIAE    24/05/2020         BLOOD  BI20985
#> 2   ZAAHIR KLEBSIELLA PNEUMONIAE    08/07/2023         BLOOD  JH70033
#> 3  KASHIEF STAPHYLOCOCCUS AUREUS    24/02/2023         BLOOD   CU5997
#> 4    TYLER STAPHYLOCOCCUS AUREUS    26/08/2023         BLOOD   ES3851
#> 5  KASHIEF STAPHYLOCOCCUS AUREUS    24/02/2023         BLOOD   CU5997
#> 6    TYLER STAPHYLOCOCCUS AUREUS    26/08/2023         BLOOD   ES3851
#>        local_authority_name local_authority_code postcode
#> 1                  Worthing            E07000229 BN14 9EP
#> 2                   Reading            E06000038  RG1 7QE
#> 3                  Plymouth            E06000026  PL7 1LU
#> 4 Cheshire West and Chester            E06000050  CW6 9TX
#> 5                  Plymouth            E06000026  PL7 1LU
#> 6 Cheshire West and Chester            E06000050  CW6 9TX

data(lab_data)
head(lab_data)
#>   nhs_number local_patient_identifier patient_birth_date    sex    surname
#> 1  576322970                  IG12067         05/10/1938 Female   WILLIAMS
#> 2  325999719                  XB29898         04/04/1957   Male EL-SHAHEED
#> 3  705649596                   KR2535         24/06/1927   Male     LINTON
#> 4  115685428                  HS45202         14/06/1962   Male       RODA
#> 5  705649596                   KR2535         24/06/1927   Male     LINTON
#> 6  115685428                  HS45202         14/06/1962   Male       RODA
#>   forename organism_species_name specimen_date specimen_type lab_code
#> 1 JAMILETH KLEBSIELLA PNEUMONIAE    24/05/2020         BLOOD  BI20985
#> 2   ZAAHIR KLEBSIELLA PNEUMONIAE    08/07/2023         BLOOD  JH70033
#> 3  KASHIEF STAPHYLOCOCCUS AUREUS    24/02/2023         BLOOD   CU5997
#> 4    TYLER STAPHYLOCOCCUS AUREUS    26/08/2023         BLOOD   ES3851
#> 5  KASHIEF STAPHYLOCOCCUS AUREUS    24/02/2023         BLOOD   CU5997
#> 6    TYLER STAPHYLOCOCCUS AUREUS    26/08/2023         BLOOD   ES3851
#>        local_authority_name local_authority_code postcode
#> 1                  Worthing            E07000229 BN14 9EP
#> 2                   Reading            E06000038  RG1 7QE
#> 3                  Plymouth            E06000026  PL7 1LU
#> 4 Cheshire West and Chester            E06000050  CW6 9TX
#> 5                  Plymouth            E06000026  PL7 1LU
#> 6 Cheshire West and Chester            E06000050  CW6 9TX
```
