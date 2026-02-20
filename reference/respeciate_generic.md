# Respeciate unspecified samples

**\[stable\]**

Some samples within SGSS are submitted by laboratories as "GENUS SP" or
"GENUS UNNAMED". However, they may also have a fully identified sample
taken from the same site within a recent time period. This function
captures species_col from another sample within X-days of an unspeciated
isolate. Respeciation is restricted to organisms of the same genus;
species will not be inferred from isolates belonging to a different
genus. Trailing "UNNAMED" is normalised to "SP" before any processing.

## Usage

``` r
respeciate_generic(
  x,
  group_vars,
  species_col,
  date_col,
  window = c(0:Inf),
  .forceCopy = FALSE
)
```

## Arguments

- x:

  a data.frame or data.table object

- group_vars:

  the minimum grouping set of variables for like samples in a character
  vector; suggest c('patient_id','specimen_type') - genus will
  automatically be included in the groupby. This is built from the
  species_col

- species_col:

  a character containing the column with the organism species_col name

- date_col:

  a character containing the column with the specimen/sample date_col

- window:

  an integer representing the number of days for which you will allow a
  sample to be respeciated

- .forceCopy:

  Logical (default `FALSE`). If `FALSE`, the input is converted to a
  `data.table` and modified by reference. If `TRUE`, the input must
  already be a `data.table`, and the function will create an explicit
  copy to avoid modifying the original object.

## Value

a data.table with a recharacterised `species_col` column

## Examples

``` r
df <- data.frame(
ptid = c(round(runif(25,1,5))),
spec = sample(c("KLEBSIELLA SP",
                "KLEBSIELLA UNNAMED",
                "KLEBSIELLA PNEUMONIAE",
                "KLEBEIELLA OXYTOCA"),
              25,replace = TRUE),
type = "BLOOD",
specdate = sample(seq.Date(Sys.Date()-21,Sys.Date(),"day"),25,replace = TRUE)
)

respeciate_generic(x=df,
                   group_vars=c('ptid','type'),
                   species_col='spec',
                   date_col='specdate',
                   window = 14)[]
#> 14-day round 1: 5 SP or UNNAMMED isolates respeciated
#> 14-day round 2: 2 SP or UNNAMMED isolates respeciated
#> 14-day round 3: 1 SP or UNNAMMED isolates respeciated
#> 14-day round 4: 1 SP or UNNAMMED isolates respeciated
#>      ptid                  spec   type   specdate
#>     <num>                <char> <char>     <Date>
#>  1:     1         KLEBSIELLA SP  BLOOD 2026-01-30
#>  2:     1         KLEBSIELLA SP  BLOOD 2026-02-16
#>  3:     1 KLEBSIELLA PNEUMONIAE  BLOOD 2026-02-17
#>  4:     2         KLEBSIELLA SP  BLOOD 2026-02-01
#>  5:     2         KLEBSIELLA SP  BLOOD 2026-02-04
#>  6:     2         KLEBSIELLA SP  BLOOD 2026-02-07
#>  7:     2         KLEBSIELLA SP  BLOOD 2026-02-09
#>  8:     2         KLEBSIELLA SP  BLOOD 2026-02-16
#>  9:     3    KLEBEIELLA OXYTOCA  BLOOD 2026-02-07
#> 10:     3    KLEBEIELLA OXYTOCA  BLOOD 2026-02-17
#> 11:     3    KLEBEIELLA OXYTOCA  BLOOD 2026-02-19
#> 12:     3 KLEBSIELLA PNEUMONIAE  BLOOD 2026-02-02
#> 13:     3 KLEBSIELLA PNEUMONIAE  BLOOD 2026-02-02
#> 14:     3 KLEBSIELLA PNEUMONIAE  BLOOD 2026-02-05
#> 15:     3 KLEBSIELLA PNEUMONIAE  BLOOD 2026-02-06
#> 16:     3 KLEBSIELLA PNEUMONIAE  BLOOD 2026-02-11
#> 17:     3 KLEBSIELLA PNEUMONIAE  BLOOD 2026-02-15
#> 18:     3 KLEBSIELLA PNEUMONIAE  BLOOD 2026-02-17
#> 19:     4    KLEBEIELLA OXYTOCA  BLOOD 2026-01-31
#> 20:     4         KLEBSIELLA SP  BLOOD 2026-01-31
#> 21:     4         KLEBSIELLA SP  BLOOD 2026-02-07
#> 22:     4         KLEBSIELLA SP  BLOOD 2026-02-10
#> 23:     4         KLEBSIELLA SP  BLOOD 2026-02-11
#> 24:     5 KLEBSIELLA PNEUMONIAE  BLOOD 2026-02-11
#> 25:     5 KLEBSIELLA PNEUMONIAE  BLOOD 2026-02-18
#>      ptid                  spec   type   specdate
#>     <num>                <char> <char>     <Date>
```
