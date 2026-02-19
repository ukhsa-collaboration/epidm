# Hospital IN/OUT dates

**\[experimental\]**

Derives per‑patient **hospital entry (`hospital_in`)** and **exit
(`hospital_out`)** dates by reconciling A&E (ECDS) attendances and
inpatient (HES/SUS) spells. Applies a simple ranking to determine the
most relevant hospital period around an index event date (e.g., a
specimen collection date).

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

  A linked table containing A&E and inpatient records (typically the
  output of [`link_ae_inpatient()`](link_ae_inpatient.md)), including
  person/event identifiers and date fields.

- person_id:

  Quoted column name for the unique patient identifier.

- hospital:

  A named **list** specifying column names (all quoted) for:

  `org_code`

  :   Organisation code (optional; used to scope grouping).

  `event_date`

  :   Index date to compare against (e.g., `specimen_date`).

  `ae_arrive`

  :   ECDS arrival date.

  `ae_depart`

  :   ECDS departure date.

  `ae_discharge`

  :   ECDS discharge status (use grouped values if available).

  `in_spell_start`

  :   Inpatient spell start date.

  `in_spell_end`

  :   Inpatient spell end date.

  `in_discharge`

  :   Inpatient discharge destination (grouped recommended).

## Value

A `data.table` equal to `data` with additional columns:

- `hospital_in`:

  Derived hospital admission date for the relevant stay.

- `hospital_out`:

  Derived hospital discharge date for the relevant stay.

- `hospital_event_rank`:

  Rank of suitability of the hospital window for the given person/event
  (1 = most suitable).

## Note

Work in progress — functionality is incomplete.

## Workflow context

Use `hospital_in_out_dates()` **after**:

- Linking A&E to inpatient spells (e.g., via
  [`link_ae_inpatient()`](link_ae_inpatient.md)),

- Constructing spells (e.g., [`group_time()`](group_time.md) or
  [`cip_spells()`](cip_spells.md)),

- Optional code standardisation (e.g., discharge groups via
  [`lookup_recode()`](lookup_recode.md))

## See also

epidm::lookup_recode()

epidm::group_time()

epidm::cip_spells()
