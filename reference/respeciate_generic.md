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

  default FALSE; TRUE will force data.table to take a copy instead of
  editing the data without reference

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
#>  1:     1         KLEBSIELLA SP  BLOOD 2025-11-18
#>  2:     1         KLEBSIELLA SP  BLOOD 2025-12-05
#>  3:     1 KLEBSIELLA PNEUMONIAE  BLOOD 2025-12-06
#>  4:     2         KLEBSIELLA SP  BLOOD 2025-11-20
#>  5:     2         KLEBSIELLA SP  BLOOD 2025-11-23
#>  6:     2         KLEBSIELLA SP  BLOOD 2025-11-26
#>  7:     2         KLEBSIELLA SP  BLOOD 2025-11-28
#>  8:     2         KLEBSIELLA SP  BLOOD 2025-12-05
#>  9:     3    KLEBEIELLA OXYTOCA  BLOOD 2025-11-26
#> 10:     3    KLEBEIELLA OXYTOCA  BLOOD 2025-12-06
#> 11:     3    KLEBEIELLA OXYTOCA  BLOOD 2025-12-08
#> 12:     3 KLEBSIELLA PNEUMONIAE  BLOOD 2025-11-21
#> 13:     3 KLEBSIELLA PNEUMONIAE  BLOOD 2025-11-21
#> 14:     3 KLEBSIELLA PNEUMONIAE  BLOOD 2025-11-24
#> 15:     3 KLEBSIELLA PNEUMONIAE  BLOOD 2025-11-25
#> 16:     3 KLEBSIELLA PNEUMONIAE  BLOOD 2025-11-30
#> 17:     3 KLEBSIELLA PNEUMONIAE  BLOOD 2025-12-04
#> 18:     3 KLEBSIELLA PNEUMONIAE  BLOOD 2025-12-06
#> 19:     4    KLEBEIELLA OXYTOCA  BLOOD 2025-11-19
#> 20:     4         KLEBSIELLA SP  BLOOD 2025-11-19
#> 21:     4         KLEBSIELLA SP  BLOOD 2025-11-26
#> 22:     4         KLEBSIELLA SP  BLOOD 2025-11-29
#> 23:     4         KLEBSIELLA SP  BLOOD 2025-11-30
#> 24:     5 KLEBSIELLA PNEUMONIAE  BLOOD 2025-11-30
#> 25:     5 KLEBSIELLA PNEUMONIAE  BLOOD 2025-12-07
#>      ptid                  spec   type   specdate
```
