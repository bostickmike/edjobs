# edjobs

Shared code for the [Montana](https://github.com/bostickmike/Montana-Education-Jobs-Dashboard) and
[Wyoming](https://github.com/bostickmike/Wyoming-Education-Jobs-Dashboard) Education Jobs Dashboards: scraping
platform job boards, classifying postings, posting spells and durations, the checks each weekly run makes on its own
output, the weekly run itself (a reusable workflow), and the dashboard's shared server code and tabs.

Each state repository pins a tagged version (`EDJOBS_VERSION` at its root) and installs it in its workflows. Code
moves here from the state repositories in phases, so a shared function is defined, tested and reviewed once. The
goal is a package that makes a third state mostly a matter of data (registries and a state configuration), and
state outputs that stack into one multistate dataset. This is infrastructure for those projects, not a
general-purpose scraping tool.

## Where to start reading

| You want to... | Read |
|---|---|
| Understand how the package and the state repos fit together | `vignette("edjobs")` ([source](vignettes/edjobs.Rmd)) |
| Understand posting spells and listing durations | `vignette("posting-spells")` ([source](vignettes/posting-spells.Rmd)) |
| Look up a function | `?function_name`, or `help(package = "edjobs")`. Exported: what a state calls (see `NAMESPACE`); helpers no state calls are internal (`edjobs:::name`, documented with `@keywords internal`) |
| See what changed in each version | [NEWS.md](NEWS.md) |
| See how code moved here, phase by phase | `vignette("edjobs")`, "How code moves here" |

## What's in `R/`

| File | What it does |
|---|---|
| `state_config.R` | `state_config()` / `set_state_config()`: a state's identity (name, FIPS, app folder), its own reference tables, statewide feed and scraper sources, as data |
| `scrape_helpers.R` | `safe_scrape()`, `perform_with_retry()`, scrape logging, reference-table guards (`write_reference_file()`, `reference_table_status()`) |
| `platform_scrapers.R` | Shared platform fetch/parse: AppliTrack, SchoolSpring details and ending reasons, PeopleAdmin page pay and dates, NEOGOV card pay |
| `platform_fetchers.R` | The platform fetchers: SchoolSpring list, TedK12, PeopleAdmin feed, NEOGOV classic careers page with each posting's page for its dates (`parse_jobposting_jsonld()`), and shared-board attribution from the registry |
| `llm_extract_scraper.R` | The quarantined LLM extraction tier (shadow pilot) and the drift check's title reader |
| `k12_he_classification.R` | Title repair, position / subject / HE job-type classification, posted pay extraction |
| `posting_spells.R` | One row per posting: starts, endings, flags, Kaplan-Meier durations, the first-seen lookup, frozen boards; state rules in `spell_rules()` |
| `posting_status.R` | Checks a posting's own page after it leaves its board (closed, open, stated closing date) |
| `coverage_calendar.R` | Which sources returned rows in each run (what "the source was healthy" means) |
| `scraper_dispatch.R` | Which function scrapes each source, as data (`scraper_registry()`, `scraper_spec()`, `resolve_scraper_call()`), and the auto-fix issue body built from it |
| `k12_history.R` | The accumulated K-12 files: one output schema (`K12_POSTING_COLUMNS`), `build_k12_combinedclean()`, `append_weekly_rows()` |
| `drift_check.R` | Per-source drift, data-quality and auto-fix helpers shared by both states' monitoring steps |
| `drift_repeats.R` | The top of each drift report: what is new since the last run and what has been flagged run after run, less what a state has acknowledged (`drift_acknowledged.csv`) |
| `schema_check.R`, `history_checks.R` | File schema checks, totals-match-parts, rebuild parity |
| `run_expectations.R` | Checks a run's files against a state's `run_expectations.csv` |
| `bls_oews_scraper.R`, `qcew_school_pay.R`, `ipeds_staff_salaries.R` | Federal pay benchmarks (BLS OEWS, BLS QCEW, IPEDS) |
| `app_helpers.R` | The dashboards' shared app helpers: pay, date and margin-of-error text, sparklines, trend and mover summaries, job search matching |
| `apptegy.R`, `cyberschool.R`, `edlio.R`, `finalsite.R`, `googledoc.R`, `googlesites.R`, `joomla.R`, `wordpress.R` | CMS-family routines: one reader per site family (district employment pages), with per-district list rules in the registry (`List_Start` / `List_Stop` / `List_Skip`) |
| `presence_check.R` | The daily presence check: each board with stable posting IDs and each statewide feed read once a day, recorded on each state's `presence` branch (not yet used by the posting spells; the plan is day-precision endings) |
| `he_campus.R` | Which town a college posting is in (branch campuses, outreach sites), from its location text, title, a department table or its own page |
| `ccd.R`, `saipe.R`, `census_acs.R`, `ipeds_he.R` | Federal context: NCES CCD staffing and enrollment, Census SAIPE child poverty, Census ACS county context, IPEDS faculty salaries, enrollment and Pell share |
| `k12_salary_archive.R` | Yearly snapshots of a state's current-year K-12 salary source, so a multi-year trend exists |
| `source_links.R`, `source_retrieved.R` | A link to each record's own page at its source; when each outside source was last retrieved |
| `csv_quoting.R` | Write a data frame with the quoting an existing CSV uses, so a rewrite changes only its changed rows |
| `pipeline_stages.R` | Pipeline chunks both states' Rmd wrote the same way, as one call each: `refresh_county_context()`, `refresh_file_columns()` (fetch, then swap a file's columns), `refresh_source_record_links()` |
| `run_checks.R`, `run_checks_canary.R`, `run_checks_drift.R`, `run_checks_salary.R`, `run_checks_census.R`, `run_checks_autofix.R` | The weekly run's own steps as functions (`*_step()`): the pre-flight canary, schema and row-count checks, drift, posted-pay coverage, title quality, salary and Census coverage, run expectations, filing auto-fix issues and the auto-fix PR's live check. Each state's `.github/scripts/*.R` is a short call to one of these |
| `app_server_shared.R` | `dashboard_server_shared()`: the dashboard server code both states share; each state's `server` calls it and keeps its own blocks |
| `app_server_configured.R` | The shared server blocks that take a state setting (map view, the Colleges tab's directory, HE slider); `bounded_timeline_value()`, `timeline_history_note()` |
| `app_server_districts.R` | The Districts tab's state-specific blocks and the map markers, driven by a `district_spec()` that names the state's pay and budget columns |
| `app_ui_helpers.R` | UI helpers the tabs share (`dash_card()`, `help_popover()`, ...) |
| `app_ui_home.R`, `app_ui_map.R`, `app_ui_jobs.R`, `app_ui_k12_trends.R`, `app_ui_k12_districts.R`, `app_ui_he_trends.R`, `app_ui_he_colleges.R`, `app_ui_spells.R`, `app_ui_data.R` | One function per dashboard tab (`home_tab_panel()`, `map_tab_panel()`, ...): the package builds the tab, the state passes its own text and any one-state piece |
| `app_feedback.R` | The public feedback form: checks a report and opens a GitHub issue in the state's repo |
| `globals.R`, `edjobs-package.R` | Column names for R CMD check; the package help page |

Outside `R/`: `.github/workflows/weekly-run.yml` is the weekly data refresh each state's `weekly-scrape.yml` calls at
its pinned tag (the state keeps its cron, secrets and state check); `presence.yml` is the daily presence check, which each state calls the same way;
`inst/toystate/` is a made-up four-district state run end to end in the package's tests (what a third state would
write); `inst/extdata/` holds the OEWS area definitions.

## Installing

```r
pak::pak(paste0("bostickmike/edjobs@v", readLines("EDJOBS_VERSION")))   # from a state repo: the version it pins
devtools::load_all("../edjobs")             # from a local checkout while changing it
```

## Changing the package

1. Branch, change, and keep `devtools::test()` and `devtools::check()` clean (CI runs `R CMD check --as-cran`
   on every PR; a warning fails it).
2. Document with roxygen comments above each function and run `devtools::document()`; never edit `man/` by hand.
3. Add a line to `NEWS.md`, bump `Version` in `DESCRIPTION`, merge, then tag `vX.Y.Z` on the merge commit and push
   the tag. Only tag a commit whose CI passed.
4. In each state repo: set `EDJOBS_VERSION`, delete any local copy of a moved function in the same change (a local
   definition masks the package's), run the state's suite and a rehearsal of the pipeline steps that call the moved
   code, then merge. The weekly runs don't gate a bump.

Rules for package code: no state names, FIPS codes, file names or URLs inside functions; what differs by state comes
in as data (a rules object, a registry, an argument). Functions take directories as arguments rather than assuming a
repo root. Tests use real captured fixtures (`tests/testthat/fixtures/`), not synthetic payloads.

## License

MIT. Data published by the dashboards is under their own `DATA_LICENSE.md` (CC BY 4.0).
