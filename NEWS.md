# edjobs 0.13.0

The daily presence check, phase 0 (shadow: written, read by nothing yet). Plan and scope:
`claude-notes/2026-11-09_daily-presence-check-plan.md`, `2026-11-12_twice-weekly-and-daily-presence-scope.md`.

* `refresh_presence()`: once a day, reads every board on the given registry platforms (list pages only, through
  each district's own `resolve_scraper_call()`, so the same call the weekly run makes) and updates
  `presence_checks.csv` (one row per board per day: ok / empty / error, postings, keyed postings, seconds) and
  `presence_sightings.csv` (one row per unbroken stretch of sightings of a posting). A stretch ends only when the
  board answered a check without the posting; a day it errored or the job didn't run doesn't break it. A second
  run the same day (Mountain time, `presence_today()`) does nothing. Parts: `run_presence_check()`,
  `presence_rows()`, `update_presence_sightings()`, `presence_summary_markdown()`.
* `k12_link_posting_id()` moved here from WY (`k12_he_classification.R`), so both states key postings the same
  way; it now also reads Tyler Portico links (`tylerportico:<tenant>:<id>`, the id a GUID or a number).
* New reusable workflow `.github/workflows/presence.yml`: runs the check from the caller's `main` and commits the
  two files to a side branch (`presence`, created on the first run), never `main`.
  `edjobs_ref` installs a branch instead of the caller's `EDJOBS_VERSION`, for testing before a tag. Tested from a
  WY branch (run 43510367152): first run created the branch and committed the files (2 min 16 s in all); a re-run the
  same day fetched it, found the day checked and committed nothing.
* First live run (2026-11-13, from the Mac): WY 30 boards (AppliTrack, SchoolSpring, TedK12), all answered, 528
  postings, all keyed, 43 s; MT 39 boards (adds Tyler Portico), all answered, 594 postings, all keyed, 64 s. Of
  WY's 530 keys on these boards in that morning's weekly archive, 523 were seen.

# edjobs 0.12.1

No R code changes.

* New reusable workflow `.github/workflows/upkeep.yml` for the state repos (self-sustaining checks 1 and 2 of
  `claude-notes/2026-11-12_twice-weekly-and-daily-presence-scope.md`): re-enables every active workflow through the
  API each run, so GitHub's 60-day inactivity rule can't silently stop a weekly schedule, and opens one issue per
  secret 30 days before its date in the caller's `.github/token-expiry.csv` (a PAT passed as `pat` is asked for its
  real expiry, which wins over the file).

# edjobs 0.12.0

Fixes from the phase 4 close-out review (2026-11-13, `claude-notes/2026-11-13_phase4-full-review.md`).

* `bridge_adjacent_runs()`: a stable ID that absorbed a fallback one no longer becomes movable, so it can't then be
  bridged into a different requisition number (fbA -> req:100 -> req:200 had become one spell). `Bridged_From` keeps
  the first ID a posting had.
* `bridge_posting_ids()`: a blank (`""`) posted date is no date; fallback rows never bridge on title alone.
* `use_earliest_posted_date()` and `build_posting_spells()` read dates as ISO only: card text ("Posted 3 weeks ago",
  which `fetch_neogov_postings()` keeps when a posting's page can't be read) is NA, never an error.
* `build_posting_spells(frozen_min_runs = )`: `Frozen_Board` uses the `min_runs` the frozen table was built with.
* `append_weekly_rows()`: an existing row with an NA date is kept as is (it had become an all-NA row).
* `build_historical_counts()` (drift baselines; Mike, 2026-11-13): a week a source had no rows counts as 0 between its
  first and last weeks with rows, so a district that posts only some weeks isn't judged against its busy weeks
  alone. Zero weeks after its last rows (an outage in progress) and weeks `failed =` names (new
  `failed_source_weeks()` of the coverage calendar: errored or not logged) are left out, so an outage never lowers
  its own baseline. Replayed since 09-01: WY 9 flags either way; MT 20 -> 19 (a Red Lodge false alarm).
* `build_k12_combinedclean()` says which `columns =` mappings found no column, and warns when no `state_config()`
  and no `feed_label` are given (feed rows would get no `Role_Hint`). `reference_table_status()` warns likewise.
* `fetch_neogov_postings()` / `fetch_schoolspring_postings()`: `max_pages` (default 40, with a warning) and a stop
  when a board repeats a page, instead of looping until the job's time limit.
* `parse_jobposting_jsonld()`: dates must be ISO; a date-time with a zone is read as the day in Mountain time
  (`2026-11-02T06:59:00Z` is 11-01). Other forms (`11/01/2026`, `20261101`) are NA.
* `check_posting_pages()` writes one line per page (platform, seconds, status, URL), so a weekly run's log shows
  where the time goes (the 2026-11-13 WY run hit its time limit in that step's chunk).
* `fixtures_named_for_source()`: "fremont1" no longer matches "fremont14_..." fixtures.
* Tests for `spell_text()`/`spell_key()`, `parse_stated_close_date()`, `source_health()`, `first_seen_lookup()`,
  `fetch_tedk12_postings()`.

# edjobs 0.11.0

Phase 4: the platform fetchers that still differed between the states, one version each.

* `fetch_neogov_postings(agency_slug)`: one NEOGOV platform for every state, reading the classic careers page
  (schooljobs.com / governmentjobs.com; NEOGOV redirects to the agency's own domain, so a registry needs only the
  slug). Measured 2026-11-12: every posting that only an Attract site listed (UM 11, Casper 1) had closed months
  earlier, and the classic page had none past its closing date, so MT's University of Montana moves off Attract.
  Each posting's own page is read for its exact posted date and closing date (`fetch_neogov_posting_details()`,
  `details = TRUE`): Posted_Date is exact where the page gives it, Close_Date the stated closing date or NA.
* `parse_jobposting_jsonld()`: a page's schema.org JobPosting block (dates, organization, address, pay) in the
  dashboards' terms. Built for NEOGOV; the same block appears on many district sites.
* `assign_shared_board_institutions()`: a board that carries several institutions' postings is registry data.
  Each covered institution's `SharesBoard` row names the board and a `Location_Pattern`; postings whose Location
  matches get that institution. Replaces MT's hard-coded street-address rule for UM's board.
* `fetch_schoolspring_postings()` / `parse_schoolspring_json()` (WY's: a `Posting_ID` column), `fetch_tedk12_postings()` /
  `parse_tedk12_postings()` (WY's: the board URL only where a row has no JobID link), `schoolspring_domain()`.
* `fetch_peopleadmin_atom()` / `parse_peopleadmin_atom()`: MT's (each entry's `<author>`, the department, as Location);
  `location_from_author = FALSE` keeps WY's single campus name per college.
* `NEOGOV_BASE`, `neogov_job_id()`, `parse_neogov_html()`, `parse_neogov_posting_page()`.
* Fixed before release (review, 2026-11-12): `parse_neogov_html()` reads each card's own Location and date, so a
  card missing one gives NA for that card only (matched by position, one card without its meta block blanked every
  Location on the page, which filed a shared board's other campuses under its owner); a card whose location line is
  missing gives NA, not its pay line. `fetch_neogov_postings()` warns when no posting on a board has a Location.
  `parse_jobposting_jsonld()` reads the text forms schema.org allows (a string `hiringOrganization` or address, a
  bare-number salary, an array `@type`, an `@graph` list) instead of erroring, and one block or page that still
  cannot be read gives NA for that page, not an error for the board.

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
