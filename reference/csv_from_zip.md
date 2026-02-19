# Download a csv from a zip

**\[stable\]** A convenience function to allow you to pull data from
NHS, ONS and ODR assets

## Usage

``` r
csv_from_zip(x)
```

## Arguments

- x:

  a zip file from the web

## Value

a zip file for ingestion into your chosen readr

## Examples

``` r
if (FALSE) { # \dontrun{
read.csv(csv_from_zip("https://files.digital.nhs.uk/assets/ods/current/pcodeall.zip"))
} # }
```
