# Clean and Impute HES/SUS Episode Start and End Dates

**\[stable\]**

A utility for cleaning and imputing missing or inconsistent episode end
dates in HES/SUS–style inpatient data. The function identifies missing,
invalid, or overlapping spell dates within patient/provider groups and
applies deterministic rules to correct them. It also assigns a flag
(`proxy_missing`) indicating whether a value was modified and why.

## Usage

``` r
proxy_episode_dates(
  x,
  group_vars,
  spell_start_date,
  spell_end_date,
  discharge_destination,
  .dropTmp = TRUE,
  .forceCopy = FALSE
)
```

## Arguments

- x:

  A `data.frame` or `data.table`. Will be converted to a `data.table` if
  not already.

- group_vars:

  Character vector of grouping variables (e.g., patient ID, provider).
  At least one identifier must be supplied.

- spell_start_date:

  Name of the column containing the episode or spell start date.

- spell_end_date:

  Name of the column containing the episode or spell end date.

- discharge_destination:

  Name of the column containing the CDS discharge destination code.

- .dropTmp:

  Logical (default `TRUE`). If `TRUE`, temporary processing columns are
  removed before returning the result.

- .forceCopy:

  Logical (default `FALSE`). If `FALSE`, the input is converted to a
  `data.table` and modified by reference. If `TRUE`, the input must
  already be a `data.table`, and the function will create an explicit
  copy to avoid modifying the original object.

## Value

A `data.table` containing:

- Cleaned spell start and end dates.

- A flag variable (`proxy_missing`) indicating whether a date was
  modified and the rule applied (0–4).

## Examples

``` r
proxy_test <- data.frame(
  id = c(
    rep(3051, 4),
    rep(7835,3),
    rep(9891,3),
    rep(1236,3)
  ),
  provider = c(
    rep("QKJ", 4),
    rep("JSD",3),
    rep("YJG",3),
    rep("LJG",3)
  ),
  spell_start = as.Date(c(
    "2020-07-03", "2020-07-14", "2020-07-23", "2020-08-05",
    "2020-11-01", "2020-11-13", "2020-12-01",
    "2020-03-28", "2020-04-06", "2020-04-09",
    "2020-10-06", "2020-11-05", "2020-12-25"
  )),
  spell_end = as.Date(c(
    "2020-07-11", "2020-07-22", "2020-07-30", "2020-07-30",
    "2020-11-11", NA, "2020-12-03",
    "2020-03-28", NA, "2020-04-09",
    "2020-10-06", "2020-11-05", NA
  )),
  disdest = c(
    19, 19, 51, 19,
    19, 19, 19,
    51, 98, 19,
    19, 19, 98
  )
)


proxy_episode_dates(
  x=proxy_test,
  group_vars = c('id','provider'),
  spell_start_date = 'spell_start',
  spell_end_date = 'spell_end',
  discharge_destination = 'disdest'
)[]
#>        id provider spell_start  spell_end disdest proxy_missing
#>     <num>   <char>      <Date>     <Date>   <num>         <num>
#>  1:  1236      LJG  2020-10-06 2020-10-06      19             0
#>  2:  1236      LJG  2020-11-05 2020-11-05      19             0
#>  3:  1236      LJG  2020-12-25 2026-02-19      98             1
#>  4:  3051      QKJ  2020-07-03 2020-07-11      19             0
#>  5:  3051      QKJ  2020-07-14 2020-07-22      19             0
#>  6:  3051      QKJ  2020-07-23 2020-07-30      51             0
#>  7:  3051      QKJ  2020-07-30 2020-08-05      19             4
#>  8:  7835      JSD  2020-11-01 2020-11-11      19             0
#>  9:  7835      JSD  2020-11-13 2020-11-30      19             3
#> 10:  7835      JSD  2020-12-01 2020-12-03      19             0
#> 11:  9891      YJG  2020-03-28 2020-03-28      51             0
#> 12:  9891      YJG  2020-04-06 2020-04-09      98             2
#> 13:  9891      YJG  2020-04-09 2020-04-09      19             0
```
