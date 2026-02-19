<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# epidm 1.0.6

* This package is now being maintained by the EpiDS team in UKHSA and therefore has moved to the organisation repo and has changes in maintainers.

* Expanded error handling across multiple core functions.

* Improved function parameter validation.

* Various improvements to functions.

* Added tmp.genus to grouping and normalisation of UNNAMED to SP in respeciate_generic function.

* Fixes to link_ae_inpatient function.

* Improved date handling and window logic in group_time function.

* lookup_recode is now exported and fully validated with tests ensuring lookup availability and correct structure.

* Added case‑insensitive file matching along with enhanced error handling and tests in csv_from_zip.

* sql_readwrite / sql_connect: Re factored to support mocking with strengthened connection validation and full simulated‑DB tests.

* Comprehensive tests suites added for all functions.

* Major rewrite and expansion of function documentation across package.

* Added workflow details and vignette. 

* Added warning for incomplete function (hospital_in_out_dates)

* Consistency fixes, including parameter names and descriptions.

* Updated README including logo. 

* Updated DESCRIPTION metadata, additional authors, updated URLs and BugReports.

* R version updates and package version updates.

* Added and refined GitHub Actions.

* Updated pkgdown configuration.

# epidm 1.0.5

* Added ECDS destination_code lookups and use in lookup_recode to show where patients go after A&E attendance.

* Added new lookups for inpatient admission_method and discharge_destination; can be called using epidm::lookup_recode.

* Change from sapply to purrr::map_dbl to ensure type consistency and prevent creation of list cols.

* Implemented new steps for record matching, including first/last name field swaps.

* Add fuzzy match functionality to patient_id

* Add new package dataset "lab_data" for testing functions


# epidm 1.0.4

* fixed missing ';' character in SQL connection string


# epidm 1.0.3

* change trusted_connection=true to trusted_connection=yes to ensure cross-functionality with k8s and linux
* SCarnall fix for the infinite while loop case
* added pkgdown URL


# epidm 1.0.2

* remove a href links from Description field


# epidm 1.0.1

* Edits for CRAN submission to change print() to message() to prevent writing to console.
* Added additional webservice links to DESCRIPTION

# epidm 1.0.0

* Added a `NEWS.md` file to track changes to the package.

* This is the first release of the package for CRAN submission.
