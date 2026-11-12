# edjobs

Shared code for the [Montana](https://github.com/bostickmike/Montana-Education-Jobs-Dashboard) and
[Wyoming](https://github.com/bostickmike/Wyoming-Education-Jobs-Dashboard) Education Jobs Dashboards: scraping
platform job boards, classifying postings, posting spells and durations, and the checks each weekly run makes on
its own output.

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
| Look up a function | `?function_name`, or `help(package = "edjobs")` |
| See what changed in each version | [NEWS.md](NEWS.md) |
| See the migration plan and phase status | the workspace's `claude-notes/2026-11-10_r-package-migration-plan.md` |

## What's in `R/`

| File | What it does |
|---|---|
| `state_config.R` | `state_config()` / `set_state_config()`: a state's identity (name, FIPS, app folder), its own reference tables, statewide feed and scraper sources, as data |
| `scrape_helpers.R` | `safe_scrape()`, `perform_with_retry()`, scrape logging, reference-table guards (`write_reference_file()`, `reference_table_status()`) |
| `platform_scrapers.R` | Shared platform fetch/parse: AppliTrack, SchoolSpring details and ending reasons, PeopleAdmin page pay and dates, NEOGOV card pay |
| `llm_extract_scraper.R` | The quarantined LLM extraction tier (shadow pilot) and the drift check's title reader |
| `k12_he_classification.R` | Title repair, position / subject / HE job-type classification, posted pay extraction |
| `posting_spells.R` | One row per posting: starts, endings, flags, Kaplan-Meier durations, the first-seen lookup, frozen boards; state rules in `spell_rules()` |
| `posting_status.R` | Checks a posting's own page after it leaves its board (closed, open, stated closing date) |
| `coverage_calendar.R` | Which sources returned rows in each run (what "the source was healthy" means) |
| `scraper_dispatch.R` | Which function scrapes each source, as data (`scraper_registry()`, `scraper_spec()`, `resolve_scraper_call()`), and the auto-fix issue body built from it |
| `drift_check.R` | Per-source drift, data-quality and auto-fix helpers shared by both states' monitoring steps |
| `schema_check.R`, `history_checks.R` | File schema checks, totals-match-parts, rebuild parity |
| `run_expectations.R` | Checks a run's files against a state's `run_expectations.csv` |
| `bls_oews_scraper.R`, `qcew_school_pay.R`, `ipeds_staff_salaries.R` | Federal pay benchmarks (BLS OEWS, BLS QCEW, IPEDS) |

## Installing

```r
pak::pak("bostickmike/edjobs@v0.7.0")      # the version a state pins
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
