# edjobs

Shared code for the [Montana](https://github.com/bostickmike/Montana-Education-Jobs-Dashboard) and
[Wyoming](https://github.com/bostickmike/Wyoming-Education-Jobs-Dashboard) Education Jobs Dashboards.

Each state repository pins a tagged version of this package (`EDJOBS_VERSION` at its root) and installs it in its
workflows. Code moves here from the state repositories in phases: shared functions are defined once, tested once,
and reviewed once. This is internal infrastructure for those two projects, not a general-purpose scraping tool.

## Contents
- **0.0.1 (phase 0):** run expectations. A state's `run_expectations.csv` lists what a person expects after a
  change lands (file, filter, measure, bounds, the run dates it applies to). The weekly run evaluates it and reports
  failures (`read_run_expectations()`, `validate_run_expectations()`, `evaluate_run_expectations()`,
  `build_run_expectations_report_markdown()`).

## Installing
```r
pak::pak("bostickmike/edjobs@v0.0.1")
# or, from a local checkout while developing:
devtools::load_all("../edjobs")
```

## Releasing
Change, test (`devtools::test()`), bump `Version` in DESCRIPTION, tag `vX.Y.Z`, push the tag; then update
`EDJOBS_VERSION` in each state repo, not within a day of that state's weekly run (MT Tuesday, WY Friday).

## License
MIT. Data published by the dashboards is under their own `DATA_LICENSE.md` (CC BY 4.0).
