# edjobs 0.44.0

Hardening release from the 2026-11-17 package review (findings 4, 5 and 7; `claude-notes/2026-11-17_edjobs-package-review.md`).
No change to what the pipelines produce for MT or WY, except that Apptegy-style title lines with another state's
address are now dropped as addresses too.

* **Internal helpers (finding 4).** 170 exports that nothing outside the package names are now internal
  (`@keywords internal`, or only `@export` dropped where the name shares another function's help page; still
  documented, reachable as `edjobs:::name`): 0.43.0's 425 exports -> 256 (with the new `state_timezone()`). Searched: every
  tracked R, Rmd, YAML and shell file in both state repos (and MT's open `home-job-types` branch), the toolkit, the
  wayback repo, this package's workflows and `inst/`, and the states' CSV/JSON registries. `scraper_calls()` and
  `oews_area_definitions()` stay exported (the vignette uses them).
  **State change needed with the pin:** `scripts/reclassify_committed_data.R` compares the installed package with a
  checkout through `getExportedValue()`, which fails on an internal name; read `asNamespace("edjobs")` instead.
* **No state names in package code (finding 5).** New test `test-no-state-names.R` reads every string constant in
  `R/` (comments excluded) for a state name, a capitalized abbreviation, a statewide feed name or a US time zone;
  the stored `opi:` / `wsba:` presence key prefixes are the listed exceptions (renaming them would split the
  committed stretches). To pass it:
  - `state_config(timezone = )` and `state_timezone()` (new): `presence_today()` and the JSON-LD date reader default
    to the state's time zone instead of "America/Denver". **State change needed with the pin:** add
    `timezone = "America/Denver"` to each state's `state_config()`; without it `presence_today()` and NEOGOV's
    posting dates stop with "No state time zone set".
  - The Apptegy title filter's `\b(mt|montana) [0-9]{5}\b` is built from R's list of states (`US_STATE_ZIP_RX`).
    Every fixture line (11,238) and archived K-12 title (7,957, both states): 11 more lines dropped, all WY address
    lines ("Rawlins, WY 82301"); no title changes.
  - The presence README names "the state's `feed`" instead of MT's OPI and WY's WSBA.
* **Front matter (finding 7).** DESCRIPTION's Title and Description describe the package as it is; the README's
  file table lists all 38 files and its install line reads the state's `EDJOBS_VERSION`. Not changed: `Depends: R
  (>= 4.5.0)` (no older R has been tested) and `_pkgdown.yml` (needs a site URL; whether to publish a site is open).

# edjobs 0.43.0

The two postings that left MT's `k12_pay_has_unit` run expectation at 2 after 0.42.0 (both state their unit;
`claude-notes/2026-11-19_unitless-pay-browning-townsend.md`):

* `extract_posted_pay()` looks past a semicolon for pay words: "Salary: Hourly rate per Classified Salary Schedule,
  Lane 3/Step 0; $21.16" (Browning bus driver) is hourly. Bonus/fee words before a semicolon still don't veto the
  amount ("Hiring bonus available; $18/hour"). Replayed over every archived `Pay_Text` in both states (1,044
  distinct texts): only that posting changes (unit NA -> "hour").
* `continue_wrapped_pay()` (new): an Apptegy title line that ends partway through a pay range ("Custodian/Light
  Maintenance (immediate opening) - $13.52 -", with "$13.82/hour DOE." on the next line, Townsend) reads its pay
  from the line plus the next one, so the range keeps its high end and unit ($13.52-$13.82/hour). The title, its
  line, deadline and link matching are unchanged. The only such snippet in either state's archives.

# edjobs 0.42.0

* `extract_posted_pay()` reads a full or half day written before the amount as a day rate ("Full day/$100 and 1/2
  day/$50", Melstone 2026-11-17; MT's `k12_pay_has_unit` run expectation failed on it). Replayed over every archived
  `Pay_Text` in both states (1,044 distinct texts): only that posting changes (unit NA -> "day").
* `write_csv_keep_quoting()` and `csv_quoted_columns()` (new): write a data frame with the quoting an existing CSV
  file uses, so a file read with `colClasses = "character"` and written back changes only its changed rows. For the
  states' `reclassify_committed_data.R`, whose `write.csv()` quoted every date and number (MT PR 66 was patched by
  hand). Round-trip of the 9 files that script writes, both states: 18 of 18 byte-identical (plain `write.csv()`:
  14 of 18 differ).

# edjobs 0.41.0

* `classify_work_schedule()` (new): why a posting is clearly not a regular full-time job ("substitute", "extra
  duty", "part-time", "student job", "temporary or seasonal"), or `NA` when nothing marks it, for the Home page's
  job-type tables (Jobs_Dash_repos claude-notes `2026-11-17_home-job-types-plan.md`, piece A). Replayed over every
  archived title in both states (`2026-11-18_not-full-time/`). Open postings not marked: MT K-12 855 of 1,374,
  WY K-12 341 of 729; college (latest run) MT 256 of 380, WY 227 of 460. Applicant pools and a stipend pay unit
  are not reasons (pool titles say nothing about the schedule; `\bpool\b` also names swimming-pool jobs).
* `jobtype_run_totals()` (new): open postings per run by job type, all and not marked, for the new
  `k12_jobtype_weekly_totals.csv` / `he_jobtype_weekly_totals.csv` (plan piece B); one call for the pipeline and
  the archive rebuilds. Counts rows, or distinct `id_col` values (MT K-12 counts `Posting_ID`).
* `posting_run_totals()` (new): the same per-run `n` and `n_not_marked`, grouped by any column (district,
  institution), so the Home page's top district/college sparklines count on the same basis as their tables
  (`2026-11-18_sparkline-basis/PLAN.md`). `jobtype_run_totals()` is now this grouped by the type.
* `man/apptegy_passed_deadline.Rd` regenerated (0.40.0's roxygen change had not been written to it).

# edjobs 0.40.0

Classifier and pay fixes from the 2026-11-17 full review. Each was replayed over every archived title or pay
text in both states; every move listed is intended.

* Posted pay (bug 3): benefit words belong to their own amount; "K" and ".000" thousands; hourly "up to" ranges.
  1,007 archived texts with a $: 4 lost salaries recovered (Shelby, Cottonwood, Plevna, Hobson), nothing else moves.
* `PART_FTE_RX`: ".5 FTE", "0.50 FTE", "0.75 FTE" make a part-time teacher (3 archived titles).
* K-12 subjects (bug 10): school and place names (Woods Learning Center, Cheyenne East) don't pick a subject;
  "Second Language" isn't Elementary; Title I + math is Mathematics. 16 of 7,900 archived titles move.
* `classify_he_appointment()` (bug 9): "Part-time" makes an adjunct only beside a faculty word. Committed data:
  adjunct/part-time faculty MT 33 -> 28, WY 120 -> 102 (staff such as cooks and bus drivers leave the bucket).
* College job types (bug 11): chief officers -> Administration; campus police and security officers -> Support
  Services; pharmacy technicians -> Healthcare; library technicians/assistants/associates -> Support Services.
  24 archived college titles move.
* `apptegy_passed_deadline()` (item 4): "open until filled" postings, and first-review, priority or screening
  dates, are never a passed deadline.

# edjobs 0.39.0

Twice-weekly runs (2026-11-17): every history window and threshold that counted runs or files where weeks were
meant now counts days or ISO weeks, so it means the same with two full runs a week. On weekly runs each gives the
same result as before, except where an archive already holds two runs in one week (a re-run), which now count as
one week.

* `week_start()` (the Monday of a date's ISO week) and `last_run_each_week()` (each week's last run, from dates or
  dated file names).
* `build_historical_counts()` reads each week's last run, so `n_weeks` stays weeks and `mean_count` a weekly
  average. Callers of `flag_pay_coverage_drop()` and `flag_other_share_jump()` pass one history entry per week
  through `last_run_each_week()`.
* `recent_archive_files(days = 56)`: dated archives from the last `days` days; `recent_source_titles()` takes
  `days = 56` instead of the last 8 files.
* Frozen boards are counted in ISO weeks: `FROZEN_MIN_WEEKS` (12) replaces `FROZEN_MIN_RUNS`;
  `frozen_board_runs(min_weeks = )` adds a `Weeks` column and filters on it; `build_posting_spells(frozen_min_weeks
  = )`, and the spells column `Frozen_Weeks` replaces `Frozen_Runs`. The drift report's "for N weeks" prints weeks.
* Posting pages are re-checked for `POSTING_STATUS_RECHECK_DAYS` (28) days after the first check, replacing
  `POSTING_STATUS_MAX_CHECKS` (4); `posting_status_rechecks(run_date = )`.

# edjobs 0.38.0

Fixes from the 2026-11-17 full review (workspace `claude-notes/2026-11-17_edjobs-full-review.md`), each with a test.

* **Posting spells split at gaps, on the evidence** (Mike, 2026-11-17): `split_posting_gaps()`, run between
  `bridge_posting_ids()` and `use_earliest_posted_date()`. A title-identified posting missing for runs while its own
  board answered was one spell across the gap, usually two listings. Same posted date on both sides: one spell; a
  later posted date: split, the earlier piece ends; no date: split, the earlier piece censored ("censored: back
  after a gap"). Stable IDs never split. WY's archives: K-12 median 26.5 days (unchanged), college 59.5 -> 52.5;
  measured in the workspace's `claude-notes/2026-11-17_spells-gaps/`. Each state's `scripts/build_posting_spells.R`
  adds the call when it pins 0.38.0.
* `match_job_query()`: what a visitor types is literal text; "(" no longer breaks the map search.
* `validate_and_pad_schema()` and `build_k12_combinedclean()` pad missing columns on a zero-row frame.
* A 200 page with no HTML is a failed fetch in every family reader (Google Sites, Finalsite, WordPress, CyberSchool,
  as Edlio already did; shared `stop_if_no_html()`), so an outage is logged "error", not "empty".
* `fetch_peopleadmin_atom()`: a posting page with no HTML blanks only that posting's pay and dates.
* `parse_llm_extracted_postings()`: a model date like "March 20, 2023" is read instead of throwing.
* `attach_scrape_log_errors()`: a source matches its exact name or the name behind a platform prefix, not a
  substring ("... District 1" no longer takes "... District 14"'s error).
* `summarize_central_safety_net()`: the feed total comes from the feed's own log row (exact label, latest).
* Federal year searches (IPEDS staff and instructional salaries, the college directory, QCEW, ACS, SAIPE): only a
  year that isn't published yet (a 404, or no rows) moves the search back; a 5xx or timeout on the newest year now
  propagates, so the pipeline keeps last run's file and logs the failure instead of shipping an older year
  (`try_federal_year()`; each API's answer for 2027 checked 2026-11-17).
* `reference_table_problem()`: a refresh whose newest `year_col` is older than the current file's is rejected.
* `googledoc_preview_text()`: reads each value as a JSON string, so an escaped quote no longer cuts off the rest
  of the document and every escape (`\u0026`, `\"`) is decoded. Victor's preview reads identically.
* College places: a "Town, ST" title marker or page sentence counts only for the state being placed
  (`place_state_rx()`, from `place_he_postings(state_abbrev = )` or the state config), not MT/WY in either state; a
  third state's markers now read. Every archived MT and WY college title gives the same markers as before.
* Smaller: `merge_posting_status()` merges by column name; `parse_redrover_json()` reads a response whose every
  location is null; `safe_scrape()` logs a scraper that returns something other than a data frame as an error;
  `nces_district_url()` restores the leading zero a numeric LEAID loses (FIPS 01-09 states).

# edjobs 0.37.0

Package phase 6, slice 2: six more dashboard app helpers that both states' `app.R` defined identically but that read
an app global. The global becomes an argument; where both states set it to the same value, that value is the default.

* `validate_and_pad_schema(on_missing = )`: the apps pass a function that adds the message to their
  `DATA_LOAD_ISSUES` (the app copy assigned that global itself). Default: a warning.
* `in_pay_band(bands = )`: the state's `PAY_BANDS` (they differ by state).
* `dictionary_columns_help(dictionary = )` and `footer_retrieved(retrieved = )`: the app's data dictionary and
  `source_retrieved_dates.csv`.
* `relabel_spell_status(labels = )` and `map_marker_radius(min_radius = , max_radius = )`: defaults are both
  states' values ("Closed / Filled" shown as "No longer listed"; 6 and 24 px).

# edjobs 0.36.0

Package phase 6, slice 1: the dashboard apps' shared helpers. 22 functions both states' `app.R` defined identically
(measured in Jobs_Dash_repos claude-notes/2026-11-16_phase6/) move here with their code as it was; on both states'
committed data the package versions return identical output to the app copies (64 checks, 0 differences).

* `app_helpers.R`: `classify_he_appointment()`, `summarize_he_faculty_postings()`, `format_posted_or_listed_date()`,
  `fill_first_seen()`, `format_poverty_range()`, `format_moe_dollars()`, `format_moe_points()`,
  `indistinct_from_zero()`, `county_moe_summary()`, `format_posted_pay()`, `format_bls_estimate()`,
  `format_pay_estimate()`, `posting_pay_value()`, `make_sparkline_svg()`, `make_sparkline_svg_light()`,
  `build_current_trends_table()`, `latest_snapshot_per_week()`, `compute_wow_delta()`, `find_biggest_mover()`,
  `he_map_name()`, `match_job_query()`, `compute_vacancy_rate_domain()`.
* `fill_first_seen(lookup = )` is now required (the app's default was its own global table). The apps' copy of
  `first_seen_key()` is retired: it was already here.
* New Imports: htmltools, scales, tidyr (all already in the apps' deploy manifests).

# edjobs 0.35.0

A link to each district's and college's own page at its source (Mike 2026-11-16: "each row in each data set could
have a url link to its source whenever possible"; measured in Jobs_Dash_repos
claude-notes/2026-11-16_source-links-per-row.md). The posting files already link every posting.

* `nces_district_url()`: NCES CCD district page from a 7-digit `NCES_LEAID`.
* `college_navigator_url()`: IPEDS College Navigator page from a 6-digit `IPEDS_UNITID`.
* `add_source_record_links()`: adds `CCD_Link` / `IPEDS_Link` right after the id column; NA where the id is missing.
  Both NCES pages show NCES's newest year, which can be newer than the file's own year columns.

# edjobs 0.34.0

College postings at outreach sites and field offices, stage 2 (Mike 2026-11-16: "yes, build it"; measured on 360
live PeopleAdmin pages and MT's archives in claude-notes/2026-11-16_college-campus-locations.md, "Stage 2 measured").
Boards whose Location is a department (MSU's) or a fixed campus label (WY's four PeopleAdmin colleges) can now place
a posting from what the posting itself says.

* `place_he_postings(campus_departments = )`: an institution's Location value -> town (MT: MSU's research centers).
  The function now also reads the title's markers and the page's places, and a posting naming several towns stays at
  the main campus (`Place_Basis` "no_town"). The output columns are unchanged; `Page_Places` is dropped.
* `title_place_names()`: `*Havre, MT*`, `(Conrad, MT)`, `(Rock Springs Campus)`.
* `peopleadmin_page_places()`: only explicit phrases ("This position is located in <Town>, MT", "The Extension
  office is located in <Town>", "at the <Town> Campus|Center", "main campus"), never a bare town name.
* `fetch_peopleadmin_atom()` adds `Page_Places` from the pages it already reads for pay and dates (no new requests).

# edjobs 0.33.0

IPEDS faculty salary: the average over full-time instructional staff on 9- to 12-month contracts (Mike
2026-11-16), from `ipeds_avg_salary_9to12()`, for `Faculty_Avg_Salary`, `Faculty_Avg_Salary_Professor` and the
salary trend. The contract_length 99 "Total" row it replaces counts staff on contracts under 9 months, whose salaries
IPEDS doesn't collect, so it divided the others' pay by too many people: Blackfeet CC 2024 $25,621 -> $54,317 (28 of
53 under 9 months), University of Montana $93,938 -> $95,013 (5 of 442); the other 27 MT/WY colleges are unchanged.
`Faculty_Count` stays the Total row's count (all full-time instructional staff).

# edjobs 0.32.0

College postings at branch campuses and outreach sites (Mike 2026-11-16: "we need the locations to be accurate on
the map"; measured on both states' archives in claude-notes/2026-11-16_college-campus-locations.md).

* `place_he_postings(postings, institutions, places, state_abbrev)`: adds `Campus_Town`, `Campus_County`,
  `Campus_Latitude`, `Campus_Longitude` and `Place_Basis` ("main", "other_town", "out_of_state", "no_town"). A
  posting is placed at another town only when its Location writes a town in the state other than the institution's
  main campus town (the HE registry's new `Town` column); anything else stays at the main campus.
* `location_town()`: the town a Location writes (the whole value, before ", <ST>", after "US-<ST>-", either side of
  " - "); never a word inside other text, and never a name two places in the state share.
* `location_out_of_state()`, `read_state_places()` (a state's `places.csv`: every Census place, its internal point
  and county). New import: datasets (`state.abb`).
* `HE_PLACE_COLUMNS`: the columns `place_he_postings()` adds, for leaving them out of accumulated histories
  (facultydata.csv) the way the pay columns are.

# edjobs 0.31.0

One registry schema for both states (plan 2b gap 2; Mike 2026-11-16).

* `scraper_spec(fixture_prefix = )`: for fixtures named `<prefix>_<district>...` when the prefix isn't the
  lower-cased platform name. WY's own-fetcher platforms take CamelCase names (`EducationalNetworks`,
  `GoogleSitesBespoke`, ...) while their fixtures keep their names. `resolve_scraper_call()` returns it and
  `fixtures_named_for_source()` uses it.
* The auto-fix issue's new-fixture name (`autofix$fixture_example`'s `{platform}`) is that prefix too, else the
  lower-cased platform, so it names files the lookup finds (WY's `Apptegy` rows: `apptegy_<district>_...`).

# edjobs 0.30.0

Phase 5 close-out: the last pipeline helpers both states defined.

* `standardize_date()`: the posted-date parser from both states' pipeline Rmd (same code in both). New import:
  lubridate.
* `archive_k12_salary_snapshot(salarymap2, history_path, columns)` and `missing_k12_salary_snapshot_rows()`: the
  K-12 salary history, one row per district per salary year, with the state's salary `columns`. Takes MT's rule
  (append any district/year pair the archive lacks, which also repairs a partly written year); WY's rule appended
  only a new year. Measured 2026-11-16 on WY's committed files: 0 rows differ (48 of 48 districts archived).
  `needs_k12_salary_archive_update()` (WY's year check) is retired, not moved.

# edjobs 0.29.0

A retrieval date for every outside data source (Mike, 2026-11-16: BLS asks citations to give the date data were
accessed; show one for every source).

* `record_source_retrieved()`, `read_source_retrieved()`, `source_retrieved_path()`, `source_retrieved_label()`:
  one row per source in the app folder's `source_retrieved_dates.csv` (Source, Retrieved), and the footer text.
* `safe_scrape()` (with `app_dir`) and `write_reference_file()` take an optional `retrieved_source`: a kept,
  non-empty result records today. A failed, empty or rejected refresh keeps the earlier date, since the page still
  shows that data.
* `refresh_oews_wages()` passes it through only when it downloads a new year; "already on file" keeps the date of
  the download that produced the file.

# edjobs 0.28.0

Margins of error for the ACS county context (Mike's decision 2026-11-16, option B).

* `parse_census_income_rent_population()` adds Median_Household_Income_MOE and Median_Gross_Rent_MOE (Census's 90%
  MOEs, B19013_001M / B25064_001M); `parse_census_mining_employment_share()` adds Mining_Employment_Share_MOE from
  the new `acs_proportion_moe()` (ACS handbook 2020 formula (6), with its plus-sign fallback). The fetchers request
  the `_M` variables; an estimates-only response gives NA MOEs.
* No CV tiers: a figure whose MOE is at least its estimate is "not distinguishable from zero" (Census's CV > 0.61
  line). On 2024 data that is 19 of Montana's 56 county mining shares and nothing else in either state. County
  population totals are controlled to the population estimates, so the population change has no MOE.

# edjobs 0.27.0

Census's error band beside SAIPE child poverty (Mike's decision 2026-11-15, option A).

* `fetch_saipe_by_leaid()` also reads each district's total population (`SAEPOVALL_PT`) as `SAIPE_Population`;
  `parse_census_saipe_child_poverty()` and `surrounding_district_poverty()` carry it, a pair taking the larger
  district's (`pool_saipe_population()`).
* `SAIPE_RELATIVE_ERROR`, `saipe_relative_error()` and `add_saipe_range()`: Census publishes no margin of error for
  one district, only median relative errors by district size, "an approximate upper bound"; the approximate 90% range
  is rate x (1 -/+ 1.645 x CV) per Census's own rule, floored at 0 and capped at 1 (Child_Poverty_Low / _High).

# edjobs 0.26.0

Package phase 5, second slice: CCD staffing and SAIPE child poverty move from both states, matched by NCES id.

* New `R/ccd.R`: `fetch_ccd_teacher_fte(district_ids)` / `parse_ccd_teacher_fte()` (a district's LEAIDs summed),
  `find_latest_ccd_directory_year()`, and the functions both states already shared (free/reduced-price lunch share,
  school-level staffing, `geocode_school_district()`).
* New `R/saipe.R`: `fetch_saipe_by_leaid()` (every SAIPE district with its rate and both counts),
  `fetch_census_saipe_child_poverty(district_ids)`, `pool_child_poverty()` and `surrounding_district_poverty()`.
* `district_ids` replaces the states' name matching (MT's hand-written pairs, WY's name rewrites). On the 2024 data
  the figures are the same by id as by name for every district in both states.
* **Combined districts pool the counts** (Mike's decision 2026-11-15): a district made of an elementary and a high
  school district gets children in poverty / relevant children summed over both (the combined area's share),
  instead of MT's equal average of the two rates, which weighted the high school grades the same as K-8 (Census
  splits school-age children between such districts by grade range: SAIPE "2010 - 2024 Overview of School District
  Estimates"). On 2024 data MT's 90 combined districts move by a
  median 0.4 and at most 2.5 percentage points (Billings 7.5% -> 7.9%); single districts keep SAIPE's published rate.
  `surrounding_district_poverty()` pools a split town's districts the same way.

# edjobs 0.25.0

Package phase 5, first slice: the federal higher-ed and county context fetchers move from both states.

* New `R/ipeds_he.R`: IPEDS instructional staff salaries (`fetch_ipeds_he_salaries()`, `fetch_ipeds_he_salary_trend()`),
  fall enrollment FTE (`fetch_ipeds_he_enrollment()`, `fetch_ipeds_he_enrollment_trend()`), the FSA Pell recipient share
  (`fetch_ipeds_he_pell_share()`) and `find_latest_ipeds_directory_year()`, with their parse functions. Each takes the
  state's `unitid_map` (`unitid`, `Name`, optional `Salary_Note`, carried into the salary output) and `fips`.
* New `R/census_acs.R`: the ACS 5-year county context (`fetch_census_county_context()` and its parts). `county_suffix`
  strips text from Census's county name (MT passes ", Montana"; WY keeps "Albany County, Wyoming").
* New `state_fips()`: the current state config's FIPS code, the default `fips` for these fetchers.
* The states' copies differed only in the FIPS code, the map and the county suffix; on every state fixture the package
  output is identical to the old state functions' (18 comparisons). Tests and fixtures moved with the code.

# edjobs 0.24.0

WY's own-page districts on the CMS families (notes: `claude-notes/2026-11-14_wy-families/`).

* Apptegy: a posting whose own text gives an application deadline that has passed is left out (a candidate can no
  longer apply), with a message naming it. New `apptegy_passed_deadline()`; `fetch_apptegy_postings()` and
  `parse_apptegy_postings()` take `deadline_date` (default today; `NA` keeps every posting). A deadline needs a cue
  word and a full date with its year. Measured on every MT and WY Apptegy page (228 postings, 50 districts): it drops
  one, Weston County SD 7's School Maintenance Technician (closed 2026-10-29); no other page states a deadline
  (`claude-notes/2026-11-14_deadline-check/`).
* The shared title filter: a posting named by its job-description file keeps the job ("Speech Language Pathologist Job
  Description" -> "Speech Language Pathologist", "Job Description - Music Teacher K12" -> "Music Teacher K12"); a
  camel-case file name reads as words ("AssistantBoysSoccerCoach", "SubsMusicTeachingPosition.pdf"). On every cached
  live page (MT and WY, all families) this changes 4 titles: Weston 1's two (same postings, cleaner titles), Washakie
  2's "Music Teacher" (excluded there by a stop rule) and Fromberg's substitute music posting, which the Finalsite
  routine missed (Fromberg's own scraper reads it; with this fix the routine matches it).
* A "Substitute <job> Positions" line with nothing listed under it is a posting, not a section header (WY Sheridan 3's
  "Substitute Custodial Positions"); no other cached page changes.
* A numbered list item on its own line loses its number ("1. Maintenance/Custodial Supervisor", MT's Helena Flats).
  On every cached page, the only other change is Wolf Point read without its rules (2 junk lines fewer).
* `fetch/parse_cyberschool_postings(mode = "text", start, stop, skip_section)`: a CyberSchool page whose list is
  written as text in the item body (MT's Nashua, Frazer, Lavina, Hinsdale) is read with the shared title line filter;
  links come from the body only. Same titles as those districts' old parsers on fixtures and live pages.
* The shared title filter: childcare/daycare, "sciences", business education and world languages are job words; a
  coach's required trainings ("... course", "... certification", "... Clinic") and "are hired pending ..." are not
  titles; an all-caps "... VACANCIES" line is a header; a role line between a contact's name and phone is a contact.
  On every cached page these change only the four CyberSchool text pages above.
* `fetch/parse_googledoc_postings(doc_ids, location, link, start, stop, skip_section)` and `googledoc_preview_text()`:
  job lists kept in published Google Docs (MT's Victor, Superior, Bainville, Creston), read from the plain-text export
  or, for an uploaded file, the preview page; ids from the registry `Slug` (";" between docs). A doc's "updated
  M/D/YYYY" line is each posting's Posted_Date, as Victor's old scraper read it.
* The shared title filter: "Athletics/Coaching" is a header; "Substitutes for the following positions:" over a
  lowercase list is one "Substitutes" posting; "3 Paraprofessional positions" is "Paraprofessional"; "2026-27" alone is
  not a title. No other cached page changes.

# edjobs 0.23.0

Joomla district pages (notes: `claude-notes/2026-11-14_small-families/joomla/`).

* `fetch_joomla_postings()` / `parse_joomla_postings()`: the Edlio reading, on Joomla pages (5 MT districts).
* `district_page_text()`: each block end (table cell, list item, paragraph, div, heading, `<br>`) is a line break, so
  cells no longer run together ("CustodianFull Time Custodial ...", Roberts, Joliet); Joomla module menus and the
  UIkit/YOOtheme navigation, toolbar and footer are chrome. A body with no HTML ("site under maintenance") is an
  error, not zero postings.
* The shared title filter: an all-caps "X NEEDED" / "X WANTED" heading keeps the job ("BUS DRIVER NEEDED" -> "BUS DRIVER"); a
  sentence ending in "are NEEDED." is not cut to a title; lone "Online" link text, free/reduced-lunch headings and
  menus are not titles; "Job Descriptions (* requires a license):" opens a job-description section; "Click here"
  run onto a title is cut.

# edjobs 0.22.0

Edlio district pages (notes: `claude-notes/2026-11-14_small-families/edlio/`).

* `fetch_edlio_postings()` / `parse_edlio_postings()` (same arguments and output as the other page-text routines),
  on `district_page_text()`: the body without navigation, header, footer, sidebars, menus and scripts. Per-posting
  document links as the old scrapers had them.
* The shared title filter, with every earlier measure unchanged: an all-caps line naming no role is a section
  header (Livingston's school names); W-4 / I-9 forms and agreements are not titles; a document's file name reads
  as words without the school year ("Baker_Cook 2026 27" -> "Baker Cook"); cashier and dish(washer) are roles;
  "Position(s)" closes a header.

# edjobs 0.21.0

The fifth CMS-family routine: WordPress district pages (notes: `claude-notes/2026-11-14_wordpress-family/`).

* `fetch_wordpress_postings()` / `parse_wordpress_postings()` (same arguments and output as the Google Sites
  routine): the post/page content (`.entry-content`, else Divi's main area, `main`, `article`, `#content`) without
  navigation, header, footer, sidebars and widgets (`wordpress_page_text()`).
* The shared title filter, with every earlier measure unchanged: handbooks, CBAs, questions, job-duty lines
  ("Prep, cook and deliver breakfast"), "... wage" lines and "School District No. 38" names are not titles; "We are
  hiring X" and "Foundations Hiring for X" give X; "(PDF)" suffixes and "Name, Role" contacts with names like
  McAllister.

# edjobs 0.20.0

The fourth CMS-family routine: Finalsite district pages (notes: `claude-notes/2026-11-14_finalsite-family/`).

* `fetch_finalsite_postings()` / `parse_finalsite_postings()` (same arguments and output as the Google Sites
  routine): Finalsite's page content without the site chrome, tab navigation and hidden copies, element headers
  kept, resource links read by their `data-resource-title` (`finalsite_page_text()`).
* Both page-text routines now keep a title's own link when the title is a link on the page (a posting's document:
  Manhattan's and Hellgate's links as their old scrapers had them; new stable links at Shields Valley, Thompson
  Falls, Lincoln and Big Timber).
* The shared title filter, measured on Finalsite with every earlier measure unchanged: titles marked "Filled" go
  (open = a candidate can apply); closed notices ("We currently have NO ... positions") go; "(opens in new
  window/tab)", "Click Here ...", "(... open until filled)" and trailing "link"/"-" come off a title; a "Job
  Descriptions" section is skipped; a short "... Openings" line naming only a subject is a header; a line that only
  strings other titles together goes; more boilerplate (Load More, job-description field labels, Attn:, document
  file names, "Updated ..."), and "Role: Dr. Name" contacts.

# edjobs 0.19.0

The third CMS-family routine: Google Sites district pages (notes: `claude-notes/2026-11-14_googlesites-family/`).

* `fetch_googlesites_postings(url, location, start, stop, skip_section)` / `parse_googlesites_postings()`: a plain
  request, the page text without the site's menu, header, footer and Google's "Report abuse" footer
  (`googlesites_page_text()`), and the shared title line filter with the same optional per-district rules. Output
  as `parse_apptegy_postings()` (posted pay from each title's line).
* The shared title filter, measured on MT's Google Sites pages and WY's Apptegy pages with MT's Apptegy parity
  unchanged: titles inside a sentence ("... is accepting applications for a SPED Teacher."), unless the sentence
  restates a heading title just above; a long line ending in ":" is a sentence, not a header; "Role: First Last"
  and a bare name beside a contact line are contacts; ".pdf" names, a pay-stating aside and pay after the title
  come off a title; glued "<Word> Positions:" headers are split; single all-caps words and "X, Y, AND Z" lines
  are labels; state-neutral boilerplate (addresses, greetings, document and resource links). On WY's Apptegy
  pages this now matches WY's own parser on Niobrara 1 and Platte 2 and finds Weston 7's two postings it misses.

# edjobs 0.18.0

The second CMS-family routine: Apptegy district sites (notes: `claude-notes/2026-11-14_apptegy-family/`). Mike's
decision (11-13): the shared title line filter wins where it disagrees with a district's old parser; substitute
lines are kept; Shelby stays bespoke.

* `fetch_apptegy_postings(session, url, location, mode, start, stop, skip_section)` /
  `parse_apptegy_postings()`: one routine for the Apptegy district sites, through a chromote session. Mode "text"
  reads the rendered text; "pagebuilder" reads the page-builder JSON for pages whose jobs sit in collapsed
  accordions, buttons or cards. Returns `Title`, `Location`, `Posted_Date` (NA: Apptegy pages give none), `Link`
  and the posted pay columns, read from a title's own line ("Substitute Route Drivers (CDL): $23.00/hour").
* `apptegy_title_lines()`: the shared title line filter (filter_v14 as measured, plus title-keeping fixes found
  in the build's exact-title parity: a "Title - rest" line is cut only when the rest is details, never inside
  parentheses, so "Music - Instrumental" and "Music - Vocal", or "JH Football: Head Coach" and "...: Asst. Coach",
  stay separate postings; a "Job Title:" label is dropped; an all-caps title wrapped onto an "AND ..." line is
  joined; a line naming no role under a header that does takes the header's role, "Track Coach - High School -
  Head"). Optional per-district `start`, `stop` and `skip_section` regexes.
* `decode_apptegy_pagebuilder_nodes()`, `apptegy_html_fragment_to_lines()` (moved from MT) and
  `apptegy_pagebuilder_lines()`; `apptegy_split_line()`.
* Auto-fix evidence for a routine edjobs hosts: `scraper_reads_inner_text()` reads the package's own function when
  the state's scraper files don't define it, and `fixtures_used_by_tests(fetch_fn, test_files, source_name)` keeps
  only the test blocks that name the source when a family's per-district tests do ("parse_apptegy_postings: Lone
  Rock School District"), so the issue hands the agent that district's fixtures, not the whole family's.
* WY's own Apptegy parser is renamed `fetch/parse_wy_apptegy_postings()` in WY's pin-bump change (no output
  change); moving WY's three Apptegy districts onto this routine is a separate, measured decision.

# edjobs 0.17.0

The first CMS-family routine: CyberSchool (IES) district sites (notes: `claude-notes/2026-11-14_cyberschool-family/`;
plan: `2026-11-12_cms-families.md`). Mike, 11-14: grouping the district scrapers by site family is to be done "asap".

* `fetch_cyberschool_postings(url, location)` / `parse_cyberschool_postings()`: one routine for every district on
  IES's CyberSchool CMS, replacing a scraper per district. Reads both generations of the platform: classic
  CyberSchool 2.0 (`.listItem_holder` items with "Posted on" and "Deadline:") and IES's newer platform, which
  Vaughn moved to in 2026-11 (its `/District/Jobs/` now redirects to `/jobs`; each job is a list item linking to
  `/jobs/<slug>` with "Published: MM/DD/YYYY"). Returns `Title`, `Location`, `Posted_Date` and `Closing_Date` (ISO,
  or NA; "until filled" is NA) and `Link`. Links resolve against the page actually served (after a redirect).
* On the same pages it returns the same titles, links and locations as MT's Highwood, Power and Vaughn scrapers
  (fixtures and 11-14 live pages), adds posted dates for Power and Vaughn, and reads Vaughn's moved page, where the
  old scraper finds nothing. Pages that hold the whole list as text in one item (Nashua, Frazer, Lavina, Hinsdale)
  are not read here: they need the shared title line filter.

# edjobs 0.16.0

Drift check: an LLM's reading of a flagged page no longer counts titles the scraper already returned (Mike, 11-13).

* `combine_verdict_with_llm()` gains `current_titles` (this run's titles for the source): only a title the scraper
  missed promotes to "likely_broken"; if the LLM read nothing else, an inconclusive page becomes
  "looks_genuinely_empty". New helpers `drop_titles_already_scraped()` and `title_match_key()`.
* Why: WY's Sublette County SD 1 returned its one item, "Substitutes"; the LLM read the same "Substitutes", which
  made it "likely_broken" on 11-11 and 11-13 and sent an auto-fix issue (WY issue 7) to Copilot and Claude for a
  board that is just quiet. Replayed on every past flag with an LLM reading (8: MT 6, WY 2): the 6 MT flags are
  unchanged (each scraper returned 0), the 2 Sublette flags are no longer filed.
* States pass `current_titles` from `corroborate_drift.R` (combinedclean.csv / hedata.xlsx); without it, the old
  behaviour.

# edjobs 0.15.0

Statewide feeds in the daily presence check: MT's OPI and WY's WSBA (notes:
`claude-notes/2026-11-13_presence-redrover-opi-wsba.md`). Mike, 11-13: "look a bit harder at OPI and WSBA".

* `presence_feed(board, platform, fetch)`: a feed a state passes to `refresh_presence(feeds = )`; `fetch(known)` gets
  the feed's own stretches so far (to reuse keys without reading detail pages). The reusable workflow passes the
  setup files' `PRESENCE_FEEDS`, and installs the packages named in its new `packages` input. A new `checkout_ref` input (default `main`) lets a
  state test new setup files from a branch before they merge.
* The presence files gain `Board` (the board or feed a posting was seen on; for a district's own board, the
  district) and, in the sightings, `List_Key` (what a feed's list shows of the posting). `District` is the
  posting's district. Phase 0 had not started, so no file is converted.
* Fingerprints for feeds without posting IDs: `opi_posting_key()` (OPI detail page's district and school, title,
  posted date, the archived description excerpt) and `wsba_vacancy_key()` (district, title, WSBA's posted date),
  with `presence_occurrence_keys()` for repeats in one day's list, `presence_iso_date()` and `presence_norm()`. The
  weekly archives' columns give the same keys. Measured: OPI repeats within a week are only true double listings,
  and no description changed for the 324 postings listed both 11-03 and 11-10; WSBA repeats 2 in 47.
* Live, from the Mac (2026-11-13): MT's OPI feed read 353 detail pages on its first day (270 s) and 1 on a second
  (4 s; the other 353 keys reused); 336 of the 346 fingerprints in MT's 11-10 weekly archive were among the day's.
  WY with Red Rover and WSBA: 39 boards, all answered, 701 postings, all keyed; 46 of the 47 WSBA rows in WY's 11-13
  archive had the same key.
* Needs R 4.5 (`tools::md5sum(bytes = )`).

# edjobs 0.14.0

Red Rover through its API in both states (notes: `claude-notes/2026-11-13_presence-redrover-opi-wsba.md`).

* `fetch_redrover_postings(org_id, org_slug)`, `parse_redrover_json()`, `redrover_org_slug()` moved here from WY:
  the unauthenticated GraphQL API Red Rover's own board calls. MT switches from rendering its three boards in Chrome
  to this, which gives every posting its own `/opening/<n>` link (MT's Chrome scrape got one for 8 of Columbia
  Falls's 23 postings on 2026-11-10) and its posted pay.
* `parse_redrover_json()` drops the standing application forms (`REDROVER_PLACEHOLDER_TITLES`: "Classified
  Application", "Certified Application", "Volunteer Application"), as MT's Chrome parser did. St. Ignatius lists two;
  no WY archive has ever carried one.
* Tests and a real captured response (St. Ignatius, 2026-11-13) for the parser.

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
