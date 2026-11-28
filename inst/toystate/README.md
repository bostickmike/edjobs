# Toyland: a toy state

A made-up state with four K-12 districts, one on each of four package platforms, run end to end through the package
the way a third state would run it. `tests/testthat/test-toy-state.R` runs it in package CI on every change: two
weekly runs and a same-day re-run, with HTTP answered from real captured fixtures (the district names are made up,
the postings are real) and the Apptegy page from a stand-in browser session. It is both the measure of "what would a
new state have to write" (2026-11-17 package review, step 3) and the shortest worked example of adding a state.

| File | What a state supplies |
|---|---|
| `k12_district_registry.csv` | One row per district, in the shared registry schema (`District`, `NCES_LEAID`, `City`, `County`, `Latitude`, `Longitude`, `Platform`, `Platform_Key`, `Job_Link`, `List_Start`, `List_Stop`, `List_Skip`) |
| `state_config.R` | `state_config()`: name, FIPS, time zone, its own OEWS areas, and how each platform is scraped (`scraper_registry()`: the package's specs, plus its own Apptegy spec) |
| `oews_areas.csv`, `oews_role_wages.csv` | The state's BLS OEWS areas and wages (a real state fetches the wages with `refresh_oews_wages()`) |
| `run_expectations.csv` | What a run's files should show (`evaluate_run_expectations()`) |
| `pipeline.R` | The weekly K-12 run: what the package doesn't do yet |

## What a new state still writes (the gaps)

Everything `pipeline.R` and `state_config.R` define as functions. `test-toy-state.R` lists them and fails when the
list changes, so a step that moves into the package shows up as a shorter list. Three today (the fourth, reading the
registry, is `read_k12_registry()` since 0.113.0):

1. `toy_raw_rows()`: each fetcher returns its own columns (`title`/`Title`, `url`/`Link`, `job_id`/`Posting_ID`),
   and AppliTrack's per-posting link has to be built from the job id. A package function that returns one raw-row
   shape for every platform would remove it.
2. `toy_posting_identity()`: the fallback id for a posting with no per-posting link (Apptegy and the other district
   site families). Each state has its own (MT `build_k12_posting_id()`, WY `assign_posting_identity()`).
3. `toy_k12_run()`: the stages in order (scrape, archive, combinedclean, teacher rows, totals, the accumulated files).
   Review steps 5 to 7 (pipeline stages as package functions).

Not code any more: how each platform is scraped. AppliTrack, SchoolSpring and TedK12 come from the package's
`default_scraper_specs()` (review step 4, 0.46.0), so a district on a known platform is one registry row. Toyland
replaces only Apptegy's spec (`scraper_specs()`), to read its fixtures without today's deadline cutoff.

Not yet in the toy: colleges, a statewide feed, posting spells, federal context, the app. Each is a later slice.
