# A&E attendance discharge destination

In order to group A&E discharge destination from SNOWMED into human
readable groups, a lookup table has been created. These work with
Emergency Care Dataset (ECDS) data with the destination_code field to
show where a patient goes after discharge from A&E.

## Usage

``` r
group_ecds_discharge_destination
```

## Format

- code:

  the ECDS destination_code

- destination_code:

  the destination grouping as a human readable string
