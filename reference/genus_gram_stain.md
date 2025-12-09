# Bacterial Genus Gram Stain Lookup Table

A reference table of bacterial gram stain results by genus to allow
faster filtering of bacterial results. This dataset has been maintained
manually against the PHE SGSS database. If there are organisms missing,
please raise and issue or push request on the [epidm
GitHub](https://github.com/alexbhatt/epidm)

## Usage

``` r
genus_gram_stain
```

## Format

A data frame with four columns

- organism_genus:

  The bacterial genus

- gram_stain:

  A character string to indicate POSITIVE or NEGATIVE type

- gram_positive:

  A 0/1 flag to indicate if the genus is gram positive

- gram_negative:

  A 0/1 flag to indicate if the genus is gram negative
