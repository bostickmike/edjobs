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
