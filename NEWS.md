# edjobs 0.10.0

Phase 4: the accumulated K-12 history, with one output schema for every state.

* `K12_POSTING_COLUMNS` (`combinedclean.csv`) and `K12_TEACHER_COLUMNS` (`k12jobanalysis.csv`): the columns every
  state writes, so the states' files stack. MT's `OPI_District` / `Also_On_OPI` are `Feed_District` /
  `Also_On_Feed`; WY's `posting_id` is `Posting_ID`, and WY gains `Posting_Source`, `Posting_Identity_Method`,
  `Feed_District`, `Also_On_Feed` and `Role_Hint` (NA where a state does not record one).
* `build_k12_combinedclean()` and `build_k12_teacher_rows()`: one builder. A state's raw rows and archives keep
  their own column names (`columns = `); feed counties, district-name corrections and the feed label come in as
  arguments. Posting ids are carried through, never rebuilt: the spell history bridges on their values.
* `append_weekly_rows()` (identical in both states) and `summarize_k12_posting_counts(id_col = )`.

# edjobs 0.9.0

Phase 4: scraper dispatch and the auto-fix issue body, the last drift-check functions MT and WY held differently.

* "Which function scrapes this source, with what arguments" is data. A state lists its sources in
  `state_config(scraper_sources = )`: `scraper_registry()` (a table with one row per source and a platform
  column, plus a `scraper_spec()` per platform: registry columns or functions of the row as arguments, an optional
  chromote session, a per-district dispatch map, a naming-convention fallback) and `scraper_calls()` (sources that
  are each one written-out call).
* `resolve_scraper_call(name)` looks a source up in them; `describe_scraper_call()` and `run_scraper_call()` follow.
  `build_source_url_lookup()` builds the corroboration step's name -> page lookup from the same sources and the feed.
* `gather_autofix_evidence()`, `autofix_evidence_markdown()` and `build_autofix_issue_body()`: one issue body for
  every state. `state_config(autofix = )` says which scraper files to read, how a source's fixtures are found
  (`"name"`: `fixtures_named_for_source()`; `"tests"`: `fixtures_used_by_tests()`) and what a new fixture is called.
* `collapse_statewide_feed_names()` (was MT's), reading the feed label from `state_config(feed = )`.
* The platform specs stay in the states for now: the two registries name platforms and arguments differently.
  Once they share a schema the package can hold the specs for the platforms it scrapes.

# edjobs 0.8.0

Phase 4: the drift-check functions MT and WY held differently, except dispatch and the auto-fix issue body (next).

* `build_historical_counts()`, `flag_drift()`, `summarize_central_safety_net()`, `build_drift_report_markdown()`,
  `summarize_live_check()` and `scraper_titles()` (WY's, which reads both `title` and `Title`).
* `state_config()` gains `feed` (the statewide feed's short name, label, URL and discrepancy file) and
  `drift_baseline_from` (the first archive that counts toward drift baselines; was each state's
  `BASELINE_VALID_FROM`). The drift functions default to them, so callers don't name the state's feed.
* `flag_drift(min_mean_count = 3)`: WY's noise floor is the default; MT keeps its own (0) by passing it.
* The default drift-report explanation is the shared sentence; WY passes its own longer one.

# edjobs 0.7.0

Phase 4: the two known places package code still named states.

* `state_config()`, `set_state_config()`, `current_state_config()`: a state's identity (name, FIPS, app folder)
  and its own reference tables, as data. `reference_table_rules()` gives the shared `REFERENCE_TABLE_RULES` plus the
  state's; `reference_rules_for()`, `write_reference_file()`, `log_reference_result()` and
  `reference_table_status()` use it. `REFERENCE_TABLE_RULES` keeps only the shared tables (BLS OEWS and QCEW, IPEDS
  staff pay); MT's district crosswalk and OPI district counties and WY's UW salary book move to the states'
  `state_config.R`. **Breaking for a state:** it must call `set_state_config()` before refreshing or checking one
  of its own tables.
* OEWS areas are data: `inst/extdata/oews_area_definitions.csv` (one row per county, keyed by state FIPS) and
  `oews_area_definitions()` replace the exported `OEWS_AREAS` list. `oews_state_areas()`,
  `oews_series_plan()` and `oews_area_for_county()` take a postal code or a FIPS code and return the same results
  as 0.6.0 for MT and WY.

* Documentation: README rewritten (where to start, what each `R/` file does, how to change and release), this
  NEWS file, `vignette("edjobs")` (how the package and the state repos fit together, adding a state) and
  `vignette("posting-spells")`, and a reference index grouped by topic (`_pkgdown.yml`). No code changes.

# edjobs 0.6.0 (2026-11-11)

Phase 4, first slice: posting spells.

* `R/posting_spells.R`: the posting-spells engine from both states. `bridge_posting_ids()` /
  `bridge_adjacent_runs()`, `build_posting_spells()`, `summarize_posting_durations()`, `use_earliest_posted_date()`,
  `frozen_board_runs()` and `build_frozen_board_report_markdown()`, `first_seen_key()` / `first_seen_lookup()`,
  `parse_board_posted_date()` (was WY's `parse_wy_posted_date()`), `parse_stated_close_date()`,
  `relisted_next_run()`, `spell_text()` / `spell_key()`.
* `spell_rules()`: what differed between the states is now a rules object (statewide-feed entity, feed rows in
  durations, the shared-title rule, partial-run and moved-board censoring, left-censoring after a gap, standing-ad
  and pool patterns). The defaults are WY's audited rules.
* Every rule set writes the same 36 columns, `POSTING_SPELL_COLUMNS`, so states' spells stack.
* New import: survival.

# edjobs 0.5.0 (2026-11-11)

Phase 3: shared platform scrapers.

* `R/platform_scrapers.R`: the 21 definitions both states held identically (AppliTrack fetch/parse and posting
  URLs, SchoolSpring posting details and ending reasons, PeopleAdmin page pay and dates, NEOGOV card pay,
  `format_pay_amount()`), with their tests and fixtures.
* Copilot setup steps and instructions, so a scraper auto-fix issue can be worked here.

# edjobs 0.4.0 (2026-11-11)

Phase 2: the classifier and shared checks (0.3.0 was never tagged; its changes are in this release).

* `R/k12_he_classification.R`: the 22 classifier definitions both states held identically (position, subject,
  broad category, HE job type and faculty category, role hint, title repair, title detail, posted pay and its
  plausibility range, placeholder postings). Each state keeps its own name corrections and posting identity.
* `R/ipeds_staff_salaries.R`.
* `R/drift_check.R`: 41 shared drift, data-quality and auto-fix definitions. `check_file_schema()`,
  `check_total_matches_parts()`, `rebuild_parity_row()`.
* CI on R 'release' with an apt-mirror fallback.

# edjobs 0.2.0 (2026-11-11)

Phase 1, part 2 (0.1.0, part 1, was never tagged; its changes are in this release).

* Scrape helpers (`safe_scrape()`, `perform_with_retry()`, scrape logging, `write_reference_file()`,
  `reference_table_status()`), posting status (`refresh_posting_status()`), the coverage calendar
  (`refresh_coverage_calendar()`).
* BLS OEWS role wages (`refresh_oews_wages()`, `attach_oews_estimates()`), BLS QCEW county school pay, the LLM
  extraction tier.

# edjobs 0.0.1 (2026-11-10)

Phase 0: run expectations (`read_run_expectations()`, `validate_run_expectations()`,
`evaluate_run_expectations()`, `build_run_expectations_report_markdown()`), and a Connect Cloud test app
(`inst/connect-spike/`).
