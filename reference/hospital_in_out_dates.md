# Hospital IN/OUT dates

This function helps to determine when a patient has been in hospital
across spell aggregation. When retaining the final record the following
criteria is used:

- "1":

  Current admissions take priority

- "2":

  When conflicting on the same day, inpatient admissions take priority
  over A&E emergency care data

- "3":

  Where a patient has a linked A&E admission to a hospital inpatient
  stay, the A&E admission date is used

- "4":

  Where a patient has a positive test between two hospital stays the
  most recent completed hospital stay prior to the test is retained
  except if the time between these events is greater than 14 days, then
  the first admission following the test is retained

## Usage

``` r
hospital_in_out_dates(
  data,
  person_id = "id",
  hospital = list(org_code = "organisation_code_of_provider", event_date = "ev_date",
    ae_arrive = "arrival_date", ae_depart = "departure_date", ae_discharge =
    "ecds_discharge", in_spell_start = "spell_start_date", in_spell_end =
    "spell_end_date", in_discharge = "discharge_destination")
)
```

## Arguments

- data:

  the linked asset holding A&E and Inpatient data

- person_id:

  the column containing the unique patient ID

- hospital:

  a list containing the following items

  `org_code`

  :   the NHS trust organisation codes

  `event_date`

  :   the comparison date used; often `specimen_date`

  `ae_arrive`

  :   the ECDS arrival date

  `ae_depart`

  :   the ECDS discharge date

  `ae_discharge`

  :   the ECDS discharge status; recommend grouping from
      [`epidm::lookup_recode`](lookup_recode.md)

  `in_spell_start`

  :   the HES/SUS spell start date; recommend after
      [`epidm::group_time`](group_time.md)

  `in_spell_end`

  :   the HES/SUS spell end date; recommend after
      [`epidm::group_time`](group_time.md)

  `in_discharge`

  :   the HES/SUS discharge destination code; recommend grouping from
      [`epidm::lookup_recode`](lookup_recode.md)

## Value

new date columns on the data.table for `hospital_in` and `hospital_out`
and `hospital_event_rank`

## See also

epidm::lookup_recode()

epidm::group_time()

epidm::cip_spells()

## Examples

``` r
if (FALSE) { # \dontrun{
hospital_in_out_dates(link,
person_id = 'id',
hospital = list(
  org_code = 'organisation_code_of_provider',
  event_date = 'ev_date',
  ae_arrive = 'arrival_date',
  ae_depart = 'departure_date',
  ae_discharge = 'ecds_discharge',
  in_spell_start = 'spell_start_date',
  in_spell_end = 'spell_end_date',
  in_discharge = 'discharge_destination'
))[]
} # }
```
