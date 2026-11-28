# edjobs 0.107.0

* "New This Week" covers every role (full review 2026-11-27; Mike's call). It compared the teaching (K-12) and faculty
  (college) history files only, so the Jobs tabs' button on an all-roles table dropped every new paraprofessional,
  custodian, coach or staff posting, and the map's and summary tables' "New This Week" counted teachers and faculty
  only. `write_k12_history()` and `write_he_history()` now also keep `k12_recent_postings.csv` and
  `he_recent_postings.csv` in the app folder (`write_recent_postings()`: the last two weeks' latest updates, every
  role, only the columns compared), which the reusable weekly workflow commits. The K-12 rule is unchanged
  (`find_k12_new_postings()`, rule A); the college rule both apps had is now `find_he_new_postings()` (title,
  location, institution), and the college Jobs filter matches on all three (it left out location).

# edjobs 0.106.0

* Posted pay (full review 2026-11-27): a monthly amount counts from $50 (was $300), so UW's real $100-a-month
  resident-assistant stipend isn't blanked; over both states' archives it and one misread hourly range (which the
  parser no longer makes) were the only monthly amounts under $300. Weekly pay has a range ($100-$10,000) instead of
  being blanked as an unknown unit, and JSON-LD `unitText` spellings HOURLY, DAILY, WEEKLY, MONTHLY, YEARLY, ANNUAL
  map to their units. The review's other two pay cases (WY Carbon #1's "$23.8512 month", MT Missoula's "2/hour
  minimum") already parse right since 0.99.0; the archived values were written by older versions.

# edjobs 0.105.0

* Dashboard fixes from the 2026-11-27 full review. Home "Last 12 Weeks" sparklines count a week a district, college
  or job type had no postings as 0 instead of skipping it and reaching further back (`group_weekly_series()`; WY's
  Fremont County SD 25 skipped 10-09 and 10-16). Clickable names with an apostrophe work: the Home tables and map
  popups escaped `'` as `&#39;`, which the browser decodes before the JS runs, so MT's "St Mary's School" link
  failed (`js_attr_string()`). Compare cards no longer read "Gallatin County County" or "Albany County, Wyoming
  County" (`county_subtitle()`). The K-12 and college trends plots show about 8 date ticks, labeled "Jan 2026" when a month or more apart and
  "Nov 09" when closer, instead of a tick on each of WY's 121 run dates with no year (K-12).

# edjobs 0.104.0

* Stale board ads, restated: `stale_flags_for_archive()` gives an archive written before the rule its flags, judged
  with that run's own date (the cutoff moves with each run), and leaves an archive the rule wrote as it is. A state's
  archive reader calls it before `drop_stale_rows()`, so every week shown is counted under one rule instead of the
  rule's first run dropping the old ads all at once. Measured (both states' archives, 2026-11-26): MT -1.0% to -4.0%
  of counted rows a week since 08-06 (about 23 of 328 teaching postings on 11-23); WY 0.3% to 3.2% a week since
  2024-08-02 (up to 10 teaching postings a week; 264 of the 291 flagged teaching rows over all weeks are Fremont County SD 38 ads).

# edjobs 0.103.0

* Filled postings by public record: `filled_record()` reads and checks a state's table of postings a public record
  shows as filled (district, title, optional posted date, evidence without names, source URL, the record's date, and an
  `Until` date), and `filled_record_reason()` gives each matching row its reason, for `Stale_Reason` or a feed's drop
  reason. One posting at a time, each with its source; an entry expires so a job advertised again next year counts.
  Built for MT's spring-postings check of 2026-11-26 (Custer's board still listing a PE and a Vo-Ag job its board
  agendas record as hired).

# edjobs 0.102.0

* `check_drift_step(skip_platforms = )`: K-12 districts whose registry Platform has no board (MT's "OPIOnly") aren't
  judged, as `check_silent_sources_step()` already does. A district moved to one kept its old board's baseline and read as
  a scraper gone quiet: MT's Lodge Grass (OPI only since 11-22) filed auto-fix issue #156 on 11-26. Replayed on MT's
  11-26 data: 4 flagged without it, 3 with it (Lodge Grass out, the rest unchanged).

# edjobs 0.101.0

* CSS boards: `Pay_Page_CSS` (rules column) reads posted pay from each posting's own page, the text of the elements it
  names, for a board whose cards cut the pay off or leave it out (`css_board_page_pay()`). Only links that are their own
  web page are fetched (not the board, not a PDF or image); a page that can't be fetched leaves that posting without pay.
  Measured on MT's 11-23 archive: 7 of 12 such postings (Charlo 2, Rocky Mountain College 5) state their pay there.

# edjobs 0.100.0

* K-12 places from CCD: `fetch_ccd_district_towns()` / `ccd_district_towns()` give each district's office town and its
  schools' towns, `refresh_district_towns()` keeps them in the app folder's `district_towns.csv` (a reference table: a
  failed refresh keeps the file), and `fill_k12_locations()` gives a posting with no place its district's town when all
  the district's schools are in one town, and corrects a statewide feed's town that is none of the district's to its
  office town.

# edjobs 0.99.0

* Text boards (review step 8): `fetch_text_board_postings()` / `parse_text_board()` read a job page with no useful markup
  by rules on its lines of text (`text_board_rules()`: section start and stop, which lines are titles, what to strip,
  which lines state pay), one row or more per source in the state's own file; `text_board_spec()` is its dispatch spec. A
  missing start or stop line is an error rather than an empty board. Posted pay is read from the lines a rule names.
* CSS boards: `Strip_Regex` (removed from titles) and `Link_CSS = "@item"` (an item that is itself the link).
* `extract_posted_pay()` reads "$16.97 per hour to $20.04 per hour" as a range (it read the low end only).

# edjobs 0.98.0

* Reference and salary stages (review step 5, slice E): `refresh_pay_benchmarks()` (BLS OEWS and QCEW),
  `refresh_ipeds_staff_pay()`, `refresh_ipeds_he_salaries()` (with the prior-year columns, `widen_salary_trend()`),
  `refresh_ccd_teacher_fte()` and `refresh_data_coverage()`, the blocks both states' Rmds wrote out the same way. The salary
  trend is still fetched only when the current salaries came back. The state's own sources (DLI, WSBA, UW's salary
  book) stay with the state.

# edjobs 0.97.0

* The higher-ed stage after the scrapes (review step 5, slice D): `combine_he_frames()` (scraped frames to one table),
  `finish_he_rows()` (placeholders out, implausible pay blanked, IPEDS staff estimates) and `build_he_history()` /
  `write_he_history()` (faculty rows, institution and job-type weekly totals, `allsum_he`, `allnow_he`), built one way by
  the weekly run and the from-archive rebuild. `HE_FACULTY_COLUMNS` names `facultydata.csv`'s columns. Placing on the map,
  a college's own salary book and the institution spellings stay with the state. Both states' old steps replay identically on
  the newest committed archives (tables identical, 5 of 5 history files byte for byte).

# edjobs 0.96.0

* `build_k12_history()` and `write_k12_history()` (review step 5, slice C1): the five K-12 history tables from counted
  postings (`k12jobanalysis`, district and job-type weekly totals, `allsum`, `allnow`), built one way by the weekly
  run and the from-archive rebuild. `id_col` keeps each state's counting (MT distinct `Posting_ID`, WY rows). Every file's
  append is checked before any is written. Both states' old steps replay byte for byte (5 of 5 files, same-day re-run and
  a new run date). The toy state uses it.

# edjobs 0.95.0

* `scrape_registry_platforms(platforms = NULL)`: every registry row is its own group, in the registry's order, named by
  its name column (Wyoming's nine colleges keep their file order through it). Wyoming's higher-ed chunk replays
  identically (9 of 9 frames, same log order).

# edjobs 0.94.0

* **Direct-board stage, higher ed** (review step 5, slice B): `scrape_registry_platforms(shape = "raw")` keeps each
  scraper's own columns and adds the registry's name column (`Institution`) unless the scraper set it (JazzHR's parser
  splits Montana Tech and Highlands College). `after` runs a hook per platform on a non-empty result (NEOGOV's shared
  board assigned to campuses). `source` may name a registry by its `where` ("he_institution_registry.csv"), and
  `expected_cols` may be a list by platform. Montana's higher-ed chunk (12 platforms) replays identically through it.

# edjobs 0.93.0

* **Trends tabs (K-12 and higher ed):** the multi-year plot's legend sits under the date axis, anchored left, titled
  "Category" above its items (`trends_plotly()`, internal). `ggplotly()`'s own horizontal legend put the raw column
  name to the left of the items, so on a full-width plot the title was clipped ("road_Category") and the last item
  ran off the right edge ("Librai"). Checked by screenshot on both states' committed data.

# edjobs 0.92.0

* **K-12 direct-board stage** (2026-11-17 review, step 5): `scrape_registry_platforms(registry, platforms)` scrapes
  every registry row on the listed platforms through its `scraper_spec()` (the call the live check and repro script
  already make), under `safe_scrape()` with the district as the source, and returns one K-12 frame per platform
  group. `registry_posting_source()` labels a row from the registry's new optional `Posting_Source` column (blank:
  "<District> site (direct board)"). Each group's dates are parsed together, because `standardize_date()` picks
  formats from the whole vector. Montana's Rmd replaces 46 per-platform blocks with one call; a stub replay of
  the old blocks gives identical frames.
* The default `CyberSchool` spec passes the mode ("items") and the registry list rules, as the Rmds did, so a rule
  added to a CyberSchool row is read by the live check too.
* `resolve_scraper_call()`'s per-row lookup is now `scraper_call_for_row()` (internal), shared with the stage.

# edjobs 0.91.0

* **K-12 Trends tab:** `k12_trends_panel(counting_note = )` is now a footnote at the bottom of the tab (small muted
  text, above the site's data-sources footer) instead of a box under the multi-year plot, where it covered the plot's
  date axis. Cause: the server draws both Trends plots 500px tall (`ggplotly(height = 500)`) while their containers
  were plotly's default 400px, so the axis and legend spilled onto what came next. Both containers (K-12 and higher
  ed) are now 500px.
* K-12 Trends help popovers (Current and Multi-Year) now say what the counts leave out: "Counts full teaching
  positions only: part-time teacher, substitute and coaching postings are not included." The charts count postings
  classified Teacher; Part-Time Teacher, Substitute and Athletics are separate classes (Montana 2026-11-23: 318 Teacher
  postings counted; 15 part-time teacher, 264 substitute and 183 athletics postings not).

# edjobs 0.90.0

* **Stale board ads** (`flag_stale_board_postings()`, `stale_board_rule()`, `stale_board_reason()`,
  `stale_cutoff_date()`, `mark_stale_feed_rows()`, `drop_stale_rows()`, `is_stale_flagged()`; Mike, 2026-11-24).
  A board platform keeps an ad listed, and takes applications for it, until the district deactivates it, whatever its
  dates: AppliTrack's "Closing Date" is free text the district types. A dated board posting whose posted date, or a
  stated closing date that parses as a date, is before the start of the previous school year (with the defaults the
  July 1 that began it: 2025-07-01 during 2026-27) is flagged in `Stale_Reason` and not counted. Never flagged:
  standing pools (`STANDING_POOL_PATTERN`), ongoing roles (`STALE_ONGOING_ROLE_PATTERN`: bus drivers and aides,
  paraprofessionals and aides, custodians and maintenance, food service) and athletic coaches
  (`STALE_ATHLETIC_COACH_PATTERN`; an instructional, academic or literacy coach is judged by its dates). The
  school-year start, how many years back the cutoff is, and the exempt titles are a `stale_board_rule()`, set per
  state with `state_config(stale_rule = )`. It works for any source with a posted or closing date (AppliTrack,
  SchoolSpring, Tyler Portico, Red Rover, TedK12, dated district pages). Flag, don't delete: the raw archive keeps
  the rows, and `drop_stale_rows()` leaves them out of what is counted; an archive without the column is unchanged.
  A feed row that is the same ad as a flagged board posting is marked with `mark_stale_feed_rows()` (the match is
  the state's own). The toy state runs it. Replayed on the latest archives: 43 of 1,164 Montana board rows on
  2026-11-23 (2 of them placeholders already not counted), releasing the 5 OPI postings matched to them; 6 of 688
  Wyoming board rows on 2026-11-20. Measured in the workspace's `claude-notes/2026-11-24_stale-ats-ads/README.md`
  (option C).

# edjobs 0.89.0

* **`scraper_spec(parse_fn = )`**: a platform's dispatch entry can name its parser when it isn't the fetch
  function's `parse_*` twin. `resolve_scraper_call()` returns it, and the auto-fix evidence
  (`gather_autofix_evidence()`, `fixtures_by = "tests"`) finds a source's fixtures from the tests of the fetch
  function and of that parser. Until now the parser was always guessed by swapping `fetch_` for `parse_`, which
  for `fetch_css_board_postings()` gave `parse_css_board_postings` (no such function), so a test block titled
  `parse_css_board: <source>` gave the issue no fixtures. Named: `css_board_spec()` (`parse_css_board`),
  AppliTrack (`parse_applitrack_output`), SchoolSpring (`parse_schoolspring_json`), Red Rover
  (`parse_redrover_json`), NEOGOV (`parse_neogov_html`), Trakstar (`parse_trakstar_list`). A test checks every
  default spec's parser exists and is the one its fetch function calls.

# edjobs 0.88.0

* **SmartSites (ParentSquare) district pages** (`fetch_smartsites_postings(url, location)`,
  `parse_smartsites_postings()`, spec `SmartSites` on `Job_Link` and `City`). One reader for the site family's
  three posting widgets, each read wherever it appears on the page: the document list (title in `data-searchText`,
  date "added YYYY-MM-DD" in the link's label), the image gallery (each flyer's alt text, no date) and posts (title
  and "July 10, 2026" date). SmartSites renders them the same way on every site, so there are no per-district
  rules. Application forms and "logo" images are dropped; Link is the page. A SmartSites page with no widget reads
  as zero rows; a page with no SmartSites markup at all is an error (the site has moved), so it isn't logged as an
  empty board. Replaces three state scrapers: one board on each widget (measured 2026-11-23: on their fixtures and
  that day's pages the reader gives the same rows, titles, dates and links as the old parsers). Free text typed
  into an editor block isn't read (`parse_css_board()` reads that with a per-source selector).

# edjobs 0.87.0

* **CSS boards** (`parse_css_board()`, `fetch_css_board_postings()`, `css_board_rules()`, `css_board_rule()`,
  `css_board_spec()`; package review step 8, slice 4). One reader for job pages with no platform behind them whose
  one-off parsers differed only in their selectors. Each source's selectors are a row of the state's own
  `css_board_rules.csv`, keyed by its registry name: `Title_CSS`, optional `Item_CSS` (one element per posting; its
  title, link and location are read inside it), `Link_CSS` (blank: the page; `@title`: the title's own `href`; or a
  selector in the item), `Location_CSS` (falls back to the registry's place), `Board_CSS` (an element the board
  always has: missing, the scrape stops with an error instead of reporting no postings), `Exclude_Regex`,
  `Include_Regex`, `Max_Title_Chars` and `Max_Tries`. Links are made absolute against the page; titles are trimmed
  and blanks dropped; repeated titles are kept. A state's registry rows use `Platform = CSSBoard` and
  `css_board_spec(rules, url_col, place_col, name_col)` in its `scraper_specs()`. First used for five Montana
  college pages, whose old parsers give the same rows on every captured fixture.

# edjobs 0.86.0

* **Live dashboard check** (`live_check_step()`, `check_live_dashboard()`; reusable workflow
  `.github/workflows/live-check.yml`). Connect Cloud redeploys a state's app on every push to main, and every other
  check runs before that push. This one reads the live page's "Refreshed on" date, from the HTML the server sends,
  until it equals the latest `Archive_Date` in main's `allsum.csv` and `allsum_he.csv`. It reads again while
  the page is Connect Cloud's "Loading..." page or still shows the previous date, for up to 40 minutes. A failure
  (`stale`, `no_app`, `unreachable`) files one issue, or comments on the open one, and fails the run. A pass closes
  that issue. On 2026-11-23 both live dashboards passed (MT November 23, WY November 20, the dates on their mains).
  It checks the date only. After a change to code alone the date doesn't move, so a pass then shows the page loads
  with the right data, not that it runs the new code.

# edjobs 0.85.0

* **Trakstar Hire boards** (`fetch_trakstar_postings()`, spec `Trakstar` on `Job_Link`; first used for St. Labre
  Indian School, MT, 2026-11-23). Reads the list pages (`?p=2`, ... until a page has no cards) and each posting's
  page for its posted date (the JobPosting block's `datePosted`), closing date (`validThrough`, none on St. Labre's
  board) and pay (the description text). Titles come from the list cards. Posting_ID is `trakstar:<org>:<id>`.
* **`parse_jobposting_jsonld()` reads blocks with raw line breaks in their strings.** Strict JSON forbids them and
  Trakstar writes them on every posting page; a block that fails to parse is retried with them read as spaces. Only
  NEOGOV used the function before; its blocks parse on the first try, so nothing changes there.
* **A pay grade's low-mid-high steps read as first to last** (`extract_posted_pay()`): "Salary Grade: 13A
  ($100,323-$125,404-$150,812)/yr" is $100,323-$150,812, not $100,323-$125,404. Only a rising chain whose top is at
  most twice the middle counts ("$20 - $25 - $3,000 signing bonus" stays $20-$25). No archived text in either
  state has such a chain (787 pay texts in 145 archives compared with 0.84.0: none changed).

# edjobs 0.84.0

* **One week rule, not two.** 0.83.0's `latest_run_per_week()` repeated `latest_snapshot_per_week()`, which the
  weekly-totals downloads already used (the latest snapshot in each ISO week). The sparklines now call that function
  and take the last 12 rows; `latest_run_per_week()` is removed (no state used it). Same points as 0.83.0.

# edjobs 0.83.0

* **"Last 12 Weeks" sparklines plot one point per week** (`latest_run_per_week()`; Mike, 2026-11-23). The Home
  tab's top-districts, top-colleges and top-job-type tables and the Trends tabs' current tables took the last 12
  runs; since both states update twice a week (2026-11-17) that covered about 6 weeks (69 days in MT on 11-23), and
  an extra manual run added a point. Each sparkline now takes the latest count in each of the last 12 ISO weeks.
* **Plain time words in the dashboard text.** "This week's scrape", "weekly scrapes" and "scraped weekly" read
  "the latest update", "each of the last 12 weeks (the latest count each week)", "read twice a week"; the Listing
  Duration notes and the Data tab's download sources no longer say "scrape" (Mike: visitors don't know what a run
  or a scrape is; units are days, weeks, months).

# edjobs 0.82.0

* **An acknowledged silent source must still read as empty** (`check_ack_empty_text()`, `page_visible_text()`,
  used by `check_silent_sources_step()`; MT step 8 finding 1, 2026-11-22). `silent_sources_acknowledged.csv` takes an
  optional `Empty_Text` column: a pattern (Perl regex, case-insensitive) the board's visible text matches while it
  is empty ("There are currently no job openings", or a heading running straight into the footer for a blank page).
  Each run fetches the board (`Job_Link` in the registry) and checks it; a page that no longer matches, fails to
  load, or has no link brings the source back in the drift report as "needs a look". Without the column, or with it
  blank, an acknowledgement holds as before (while the scrape doesn't error). The fetch retries gateway errors
  like the scrapers do and caps a server-asked wait at 60 s, and the workflow step has a 10-minute timeout. The
  visible text drops zero-width characters and folds thin and non-breaking spaces. Write a pattern that runs from
  the board's heading to its footer, so a posting added anywhere in between breaks it (review, 2026-11-22: a bare
  "no openings" sentence still matched with a posting typed beside it).

# edjobs 0.81.0

* **A feed title that names no job gives way to the description's** (`feed_description_title()`, used by
  `build_k12_combinedclean()` for feed rows; MT drift triage, 2026-11-22). OPI Jobs for Teachers' title is its
  category field, and "Other" / "Administrative - Other" name no job: a cook, a district clerk, St. Labre's band,
  theology and VoAg teachers. For those two titles the row's `title` becomes the job named in the description: a
  labeled title ("Job Title: Band Teacher", "POSITION: CULTURE TEACHER"), else the role after "looking for",
  "seeking", "recruiting" or "accepting applications for", without leading adjectives; a name that classifies as
  "Other" is not used. `Posting_ID` is set before and doesn't change. Measured on the 18 distinct generic-titled OPI
  postings in MT's archives (hand-labeled): position and subject right for 16, was 9, none made worse; on 224 OPI
  postings with specific titles (held out) the extracted name never disagreed with the title's bucket, and no
  specific title changes. WY's feed has no descriptions: no change there. A labeled name that is only a building
  ("Position: HC - Davis Elementary") is not used (review, 2026-11-22). The archive diff and
  `reclassify_committed_data.R`'s guard see only `R/k12_he_classification.R`; this change is in
  `build_k12_combinedclean()`, so restate a state's history with 0.81.0 installed (rebuild, then `--skip-guard`).
* **"Family Consumer Science", "Family & Consumer Science" and "Family – Consumer Science" are their subject** (`classify_k12_subject()`), not
  Science Education. Archive diff, both states: 9 titles move, all to Family and Consumer Science.

# edjobs 0.80.0

* **An acknowledged drift item covers only what was checked** (`classify_drift_repeats()`, `parse_drift_report()`,
  `build_drift_glance_markdown()`; MT drift triage, 2026-11-22). `drift_acknowledged.csv` takes an optional `Details`
  column: what the report listed when a person checked it, "; "-separated (the postings at a district, as "Title
  (posted YYYY-MM-DD)", or the figure flagged). Such an entry acknowledges the item only while every "; "-separated
  part of the bullet's text after its name is one of those. A new posting at an acknowledged district, or a changed
  figure, brings the item back under "New this run", naming what isn't checked yet (`unchecked`). Without `Details`,
  or with it empty, an entry covers the whole item, as before. `parse_drift_report()` items gain a `details` column.
  This keeps an acknowledgement from hiding a later scraper miss at the same district.
  A line with no details of its own (postings on nested lines) is not acknowledged by a `Details` entry, and an en
  dash after the name is stripped like a colon (review, 2026-11-22).

# edjobs 0.79.0

* **A "$"-less number below a pay amount no longer makes a range** (`extract_posted_pay()`; MT drift triage,
  2026-11-22). Missoula County's "$24.00/hour - 2/hour minimum" (a two-hour minimum shift) read as $2-$24 an hour,
  which `drop_implausible_pay()` then blanked, so the posting showed no pay. A high end written without "$" that is
  smaller than the low end is now dropped: $24 an hour. Replayed on every archived `Pay_Text` in both states
  (1,095 distinct): this is the only change.

# edjobs 0.78.0

* **The drift issue says what is new and what keeps coming back** (`mark_drift_repeats_step()`,
  `classify_drift_repeats()`, `parse_drift_report()`, `drift_report_items()`, `build_drift_glance_markdown()`;
  test-kit plan item 3, Mike's option B, 2026-11-22). Before commenting, the weekly workflow reads the issue's earlier
  automated reports (its body and the workflow's own comments, the last 30) and puts a block at the top of this
  run's report: items not there on the last run day ("New this run") and items flagged on 3 or more run days in a
  row ("Still unresolved", with the count and the first date). The full report below is unchanged; if the step
  fails the report is filed as before.
  - An item is a top-level `- **Name**` bullet keyed by its section and name, so its numbers can change from run to
    run. Summary sections (feed totals, the safety-net summary) are not items. Sections that flag something without
    per-source bullets (rebuild parity, run expectations, a crashed monitoring step, keyword conflicts) are named
    in an "Also in this report" line, so "0 new" can't hide them.
  - Run days, not reports: of the earlier reports only the last on each date counts, and today's earlier re-runs or
    test dispatches are left out. A report without the item's section (a crashed check) or with it cut short ("...
    and N more", MT's 15-district OPI list) neither extends nor breaks an item's run.
  - A state's `drift_acknowledged.csv` (Section, Item, Reason, Acknowledged) takes items a person accepts out of both
    lists. A report too long for GitHub's comment limit is filed without the block.
* Replayed on MT drift issue #11's 10 run comments and WY #4's 6 (`claude-notes/2026-11-22_drift-repeats/`; the
  workflow also reads the issue body, MT's 10-27 report, which the replay doesn't): the last MT report would open with
  0 new and 23 still unresolved, led by 10 items flagged on all 6 run days since 11-03 (8 "on OPI but not on our
  scrape", Frontier's empty board, Swan River's DLI salary) and 5 on 5 run days (3 more OPI misses, Ulm's notice,
  OPI's own title lint); WY with Carbon 1's blanked pay.

# edjobs 0.77.0

* **A classic CyberSchool page with no jobs list is an error, not an empty board** (`parse_cyberschool_postings()`;
  test-kit plan item 6, 2026-11-22). An empty classic board keeps its list container (`div.pageData >
  div.listItem_list`: MT's Frontier, Saco, Montana City, Denton and Clancy); a classic page (generator
  `IESAres404`) without it, read in mode "items", is a non-jobs page or a changed site, so it stops and the scrape
  log records an error. Mode "text" and the newer IES layout (no marker when empty) are not checked. Live check
  11-22 on all 12 MT CyberSchool boards: no new errors (5 empty boards 0 rows, the other 7 unchanged).
  In drift corroboration this error (`PAGE_CHANGED_ERROR_PATTERN`) is not `confirmed_broken`: the page is still
  rendered, and the source is `likely_broken`, so it can get an auto-fix issue (a redesign needs a code or registry
  fix; a site failure doesn't).
* `attach_scrape_log_errors()` ignores scrape-log rows that aren't a source's own scrape (`LLMExtract: ...`, MT's
  shadow pilot; `Reference: ...`): a Gemini error was matched to the district (review of this release).
* **The silent-source check reads this run's scrape log** (`check_silent_sources_step(scrape_log_path = )`). A
  source with no rows in its last runs whose latest scrape errored is listed with the error even when
  `silent_sources_acknowledged.csv` lists it: the acknowledgement says the board is empty, not that it may break.
  Without this the CyberSchool error above would not show for the five acknowledged boards (an always-empty source
  has no drift baseline). On MT's archives today the list is unchanged (9 listed, 6 acknowledged).
* Fixed: with no statewide feed configured, `check_silent_sources_step()` dropped every K-12 row (`x != NULL` is
  `logical(0)`), so every K-12 source looked silent. Both states have a feed; a third state might not.
* Text-family boards (Edlio, WordPress, Google Sites, ...) get no "board found" signal: on MT's fixtures only
  Cottonwood's page says it has no openings, and the near-empty shells (Sun River Valley, Fishtail, Nye: 68-83
  characters) are too close to a real one-posting page (Billings, 117) for a length rule
  (`claude-notes/2026-11-22_board-found/`).

# edjobs 0.76.0

* **The drift report names sources judged genuinely empty** (`build_drift_report_markdown(judged_empty = )`,
  `corroborate_drift_step()`; test-kit plan item 7, 2026-11-22). A flagged source whose live page reads as having
  no openings (`looks_genuinely_empty`) used to be dropped from the report, so a source could fall to 0 and appear
  in no drift comment (MT's University of Providence, 4 rows on 11-03, 0 from 11-10). They are now listed last,
  under "Judged genuinely empty", and counted in the opening line; a run where every flagged source is judged
  empty still writes the report, with its own opening line. A source stays listed every run it is flagged. No
  verdict changes, and judged-empty sources still get no auto-fix issue.

# edjobs 0.75.0

* **Title lint: "document, not a job"** (`lint_title_shape()`, `TITLE_DOCUMENT_PATTERN`; test-kit plan item 2,
  2026-11-21). A title naming a notice, notification, handbook, lunch or breakfast menu, minutes, agenda,
  newsletter, calendar, checklist, a Title I parent letter or an enrollment or application form, or a title that is
  a heading or a no-openings line ("Job Description", "Requirements:", "Summary:", "Click here", "No open/current
  ..."), goes into the drift report's title section. MT's Ulm counted a Title I parent notice as a posting from 11-03
  to 11-19. The lint only reports; no row is dropped.
* Replayed on every distinct archived title in both states (14,749 source-title pairs, all K-12 and HE archives):
  5 newly flagged, all real non-jobs: Deer Lodge "No current openings", Terry "No open positions at this time",
  Fair-Mont-Egan "Job Description", Ennis's "Summary: ..." paragraph and Stone Child College's "H1 B Hiring
  Notification". Ulm's notice and Judith Gap's "Requirements: ..." were already flagged by other rules. Words that
  hit real jobs in the replay are left out: "policy", "I-9", "W-4", "registration", a bare "menu", and "Job
  Description" with a job after it ("Job Description - Music Teacher K12").

# edjobs 0.74.0

* **New weekly check: sources with no rows in their last weekly runs** (`check_silent_sources_step()`,
  `find_silent_sources()`, `build_silent_sources_report_markdown()`; test-kit plan item 1, 2026-11-21). A K-12
  registry district or HE institution with no rows in any of the last 4 weekly runs (one per week, the week's last
  run, today's included) goes into the drift report, unless it is listed with a reason in the repo's
  `silent_sources_acknowledged.csv` (`Type`, `Source`, `Reason`, `Acknowledged`). The drift check judges a source
  against its own average, so a source that never had rows was never flagged: MT's Ulm read a parent notice for
  weeks, and five CyberSchool parsers were written against empty boards. K-12 counts the board's rows, not the
  statewide feed's; an archive without `Posting_Source` counts every row under its district (it can only make a
  source look less silent). `skip_platforms` leaves out registry rows with no board (MT's `OPIOnly`). The weekly
  workflow runs it after the title-quality check when the repo has `.github/scripts/check_silent_sources.R`, and
  the self-check lists it if it crashes.
* Measured on the archives (2026-11-22): MT 14 K-12 districts and Missoula College (the review's list A, less the
  three districts now `OPIOnly`); WY none yet. Its archives before 11-13 have no `Posting_Source`, so WSBA rows
  still count for Sheridan County SD 3 in two of the four runs; it can be listed from WY's fourth labeled run.

# edjobs 0.73.0

* **HE faculty category fixes** (`classify_he_faculty_category()`), from the University of Montana's Interfolio faculty
  titles (MT, 2026-11-21), 6 of whose 19 were mislabeled:
  * "Clinical" as a faculty rank ("Clinical Assistant Professor, Communication Studies") no longer makes a title
    Health Sciences; the field decides. Clinical instructors, labs and medicine stay Health Sciences.
  * "Speech, Language, Hearing" (not only "speech-language") is Health Sciences, not Humanities.
  * Forestry, forest management and silviculture are Science, ahead of Business's "management"; the biological
    sciences and biological stations are Science, ahead of Health's "biomed".
  * The Wyoming Institute for Disabilities is Health Sciences (an academic unit of UW's College of Health Sciences,
    uwyo.edu/wind), not Education from "disabilit".
* Replayed over every archived title in both states (3,804 HE titles, 141 archives): 15 move, all WY. 14 Wyoming
  Institute for Disabilities titles go from Education to Health Sciences, and "Clinical Assistant Professor -
  Educational Health Center of Wyoming" (UW College of Education's clinic) goes from Health Sciences to Education.
  No MT archived title moves; MT's UM Interfolio titles are first archived 2026-11-23.

# edjobs 0.72.0

* **Coverage calendar: an entity can have more than one source** (`build_coverage_calendar()` /
  `refresh_coverage_calendar()`, new argument `extra_sources`: entity -> the other names it logs under). Montana's
  University of Montana gets its faculty jobs from Interfolio as well as its staff jobs from NEOGOV. An entity is
  healthy only when every one of its sources is, so a failed Interfolio scrape leaves those postings' ends unknown
  (censored) instead of reading them as closed. The row's Status is the first unhealthy one (own source first),
  Source joins the logged names with "; ", N_Rows is their total, and Error the first error. Institutions on its
  board (`shared_boards`: Missoula College, Helena College, UM Western) take the combined status. With no
  `extra_sources`, rows are as before.
* **Posted pay: an amount in millions or billions is not pay** (`extract_posted_pay()`). UM's Flathead Lake
  Biological Station director posting mentions "the $1M Jessie Bierman endowment", which was read as pay of $1. An
  amount followed by M, MM, mil, million, billion or B is now skipped like a bonus or fee, so a salary in the same
  posting is still read. "$45,000 MA degree" and "$18.50 per hour M-F" are unaffected. No archived posting in
  either state has a Pay_Text with such an amount (4,338 archived rows with pay, 2026-11-21), so no committed value
  changes.

# edjobs 0.71.0

* **`dashboard_server_shared()` takes the Districts tab and the map markers** (step 7 slice 3, part 3;
  `R/app_server_districts.R`): the District Summary table and its footnote, the compare cards, the compare salary
  plot, and the map marker observer (hover labels and popups). What differed between the states, their pay and
  budget columns and those columns' source notes, now comes in through a new required argument, `district_spec`:
  * `district_spec()`: the columns, the popup pay lines, the compare plot's bars and y-axis title, the footnote's
    source paragraphs, the poverty paragraph's state sentences, an optional extra footnote paragraph, the table's
    page length, and an optional remark for a vacancy rate reported for two colleges together
    (`Vacancy_Rate_Shared`).
  * `district_column()`: a table column (dollar amount or a low-high range), its header with a year label from a
    column (or the school year before it), and its compare-card row (above Enrollment or below Students / Teacher).
  * `district_popup_line()` and `district_source_note()`: a popup pay line and a footnote source paragraph.
  * `dashboard_server_shared()` checks that every column the spec names is in `combined_map_data`.
* The builders are exported for tests: `district_summary_df()`, `district_summary_footnote()`,
  `district_compare_cards()`, `district_compare_salary_plot()`. `prior_school_year_label()` (from Wyoming's `app.R`).
* Both states' outputs were compared with their old `app.R` code on the committed data: the table data and widget,
  the footnote, the cards and plot (none, four and two districts selected) and every popup and label of the opening
  and a search view are identical. One rule changed: a popup pay line's " &middot; year" is left out when the year
  is missing (it printed "NA"; no row in either state's data has a pay amount without its year).
* `data` now also needs `qcew_school_pay`.

# edjobs 0.70.0

* **One "new this week" rule for K-12, `find_k12_new_postings()`** (step 7 slice 3, part 2; Mike chose rule A in
  its paired form, 2026-11-21): a posting is new when its `Posting_ID` was not in the previous weekly snapshot,
  except an ID change: a new ID taking the place, one for one, of a previous posting with the same title, location
  and district whose ID is gone. Montana counted every new `Posting_ID` (ID churn showed as new: MT 11-03, 159
  postings); Wyoming a new title + location + district (a title edit showed as new: WY 11-13, 5; a second same-title
  opening did not), and its Jobs filter also kept every current posting sharing a new one's title and district
  (198 rows over the archive). Over the archives: MT 889 by ID -> 578, WY 2,371 by its rule -> 2,439.
* `output$k12_jobs` (the K-12 Jobs table) moves into `dashboard_server_shared()`, filtering "new this week" by
  `Posting_ID`. `data` now also needs `k12_new_this_week`, and `combineddata` needs `Posting_ID`.

# edjobs 0.69.0

* **`dashboard_server_shared()` takes the server blocks that differed only by a setting** (step 7 slice 3, part 1;
  `R/app_server_configured.R`): the map's basemaps, legend and opening view (`map_view`, also where "All counties"
  returns), the county zoom, the Institution Summary table (`he_page_length`, and `he_shared_salary_note` for
  Wyoming's `Note` column), the HE timeline slider's opening range, and both slider labels. New arguments
  `map_view` and `he_page_length` are required. `data` now also needs `vacancy_rate_domain`,
  `vacancy_rate_palette` and `WINDOW_WEEKS`.
* `bounded_timeline_value()` and `timeline_history_note()` (from Montana's `app.R`): a slider's opening range bounded
  by the archive's first date, and a note when the archive covers under a year. Wyoming's HE slider now uses them:
  on its data the opening range is the same, and the note is empty.
* The Feedback form's report time takes the state's time zone: `dashboard_server_shared(timezone = )` and
  `feedback_server(timezone = )` (required; both states pass "America/Denver", so the issue text is unchanged).
  `app_feedback.R` named the zone since 0.60.0; `test-no-state-names.R` caught it, but only from source
  (`devtools::test()`): under R CMD check the test skips, which is why CI passed.
* `output$he_new_table` removed: no tab in either app showed it (its `he_new_appointment` filter had no input).

# edjobs 0.68.0

* **`k12_districts_panel()`: the K-12 Districts tab's UI** (step 7 slice 2b, the last tab). Both states' `app.R`
  had the same District Directory and Head-to-Head Comparator; the state passes its text (Openings note, the
  directory's pay/finance/county paragraphs and source, the comparator blurb, the openings source), the comparator's
  districts and opening pair, and its salary chart's card.

# edjobs 0.67.0

* **`home_tab_panel()`: the Home tab's UI** (step 7 slice 2b). Both states' `app.R` had the same KPI cards, switch
  and four "top this week" tables. The state passes its name, its K-12 source line, the refreshed date, the credit
  line and an optional card of its own (`extra`; Montana's K-12 coverage card). The switch label and the two notes on
  what it leaves out move here from both states' `app.R`, where only Home used them.

# edjobs 0.66.0

* **`k12_trends_panel()`: the K-12 Trends tab's UI** (step 7 slice 2b). Both states' `app.R` had the same Current and
  Multi-Year Trends sub-tabs. The state passes `k12nowsum`, `k12sum` and the slider's opening window; Montana also
  turns on its slider label (`slider_label = TRUE`) and passes its note on how postings are counted
  (`counting_note`).

# edjobs 0.65.0

* **`he_colleges_panel()`: the Higher Ed Colleges tab's UI** (step 7 slice 2b). Both states' `app.R` had the same
  Institution Directory and Head-to-Head Comparator; the state passes the comparator's institutions and opening pair,
  and what its directory's Salaries and County columns hold.

# edjobs 0.64.0

* **`he_trends_panel()`: the Higher Ed Trends tab's UI** (step 7 slice 2b). Both states' `app.R` had the same
  Current and Multi-Year Trends sub-tabs; only the timeline slider's opening window differed, so the state passes it
  (`timeline_value`; the default is the last 365 days, Wyoming's). The state also passes `henowsum` and `hesum`.

# edjobs 0.63.0

* **`jobs_tab_panel()`: the K-12 and Higher Ed Jobs tabs' UI** (step 7 slice 2b). Both Jobs tabs were the same
  `nav_panel()` in both states' `app.R`: quick filter, filter status, pay line and table; Higher Ed adds the
  Appointment filter. The state passes the level and its "About the pay columns" note.

# edjobs 0.62.0

* **`map_tab_panel()`: the Map tab's UI** (step 7 slice 2b, second tab). Both states' `app.R` had the same filters
  and map; the state passes its card title (`state_name`), the "About this map" paragraphs before the shared one on
  vacancy rates, what its red "Partial" badge means, the note under the map, and the county choices, `PAY_BANDS`
  and college placement note it already builds. leaflet joins the Suggests. Each state's page HTML is the same as
  before (`ui_parity.R`).

# edjobs 0.61.0

* **`data_tab_panel()`: the Data tab's UI** (package review step 7, slice 2b; Mike, 2026-11-21: the package builds
  each tab and the state passes its text, one tab per PR, Data first). Both states' `app.R` had this `nav_panel()`
  with the same layout and per-state prose. The package keeps the layout and the text both states shared; each
  `app.R` passes its own tiles (`data_tile()`: fields it leaves out take the shared default for that download),
  provenance rows (`data_provenance_row()`, four shared rows by key), table title, intro, headers and class, and its
  citation and repository. Each state's page HTML is the same as before (checked with the workspace's
  `claude-notes/2026-11-21_step7-slice2b-data/ui_parity.R`).
* `download_rows()` and `rows_phrase()`: the row count a download hands out, as " (8,400+ records)", so the Data
  tiles' counts come from the files at app start. The hand-written ones had gone stale: Montana's tiles said 3,200+
  K-12 history records and 3,700+ weekly observations for downloads of 8,401 and 2,891 rows; Wyoming's said 76,000+
  K-12 and 29,000+ higher ed records for 13,513 and 18,532.

# edjobs 0.60.0

* **`dashboard_server_shared()`: the dashboard server code both states share** (package review step 7, slice 1;
  Mike's choice A, 2026-11-20: one package server function with state hooks). It holds the 101 server blocks that
  were identical in both states' `app.R` (90) or differed only by the state's name (11): the Home KPIs and tables,
  the map filters, the Jobs tables, Trends, both Listing Duration tabs, Higher Ed Jobs, most of Higher Ed Colleges,
  the downloads and the Feedback form. Generated from Montana's `app.R` (script in the workspace's
  `claude-notes/2026-11-21_step7-slice1/`): calls into other packages are written `pkg::fn()`, the download file
  names take `state_name`, and the 27 app-level objects it reads come in as `data`
  (`dashboard_server_data_names()`). The 14 blocks that differ by state (district pay and finance columns, the map
  center, the timeline labels, WY's WDE staff download, and `outputOptions()` on the state's map) stay in each
  `app.R` and read 7 of the reactives it returns.
* The UI helpers `spells_footnote()`, `kpi_value_box()`, `dash_card()`, `help_popover()`, `info_popover()`,
  `download_tile()` and the Feedback form (`feedback_nav_item()`, `feedback_server()`; was each app's `feedback.R`,
  identical in both) move here (phase 6's slice 3; unchanged but for `pkg::fn()` calls and `\u` escapes for
  non-ASCII characters in strings). shiny, bslib, DT, plotly, ggplot2, shinyWidgets, grid, curl and codetools
  are Suggests (Mike, 11-17 "B"): the apps load them already, and jobs that install edjobs without drawing a
  page (auto-fix, Copilot setup) don't need them. The Feedback tests moved here.
* `k12_spells_panel()` and `he_spells_panel()`: the two Listing Duration tabs' UI, identical in both states' `app.R`
  (step 7 slice 2a; generated the same way, `make_ui_spells.R`). shinycssloaders joins the Suggests.
* `extract_posted_pay()` splits an amount run into the next number after its cents. Carbon County SD 1 (WY) posts
  "Hourly Rate: $18.85-$23.8512 month work year"; it was read as $23.851 a month and blanked as implausible every
  run since 11-11. Now $18.85-$23.85 an hour. Replayed on all 669 distinct archived pay snippets in both states: this
  is the only one that changes (and the only 4+ decimal amount in either state's archives).

# edjobs 0.59.0

* **`.github/workflows/weekly-run.yml`: the states' weekly data refresh as one reusable workflow** (package review
  step 6, second half). Both states' `weekly-scrape.yml` had the same 37 steps in the same order and differed only in
  state data (the app folder, the Rmd, one extra package, whether the pipeline gets `GEMINI_API_KEY`, the state's own
  check, its commit files), which are now inputs; each state keeps a short caller with its cron, `concurrency`,
  `permissions` and secrets. Secrets are passed one by one (GitHub documents `secrets: inherit` for organizations
  only), and `AUTOFIX_ENGINE` and the time zone are inputs. The state's own checks run in one step, each even if one
  before it crashed, and a crash is named in the drift issue. No R code changed. Montana's test dispatch on a branch
  (run 44675428576) gave the same step outcomes as its 11-19 run and a commit dated in Mountain time.

# edjobs 0.58.0

* **A salary value range can be per unit of another column.** `salary_checks(bounds = )` takes optional `per` (divide
  `column` by this column; rows without a positive value are skipped) and `unless` (skip rows with a value in this
  column, so a dollar range covers only what a `per` range can't). Montana's general-fund floor of $500k, set when its
  smallest of 32 districts spent $2.1M, flagged 13 real districts of 4-46 students every week since the 11-10 registry
  growth; per enrolled student ($3k-$60k) it flags none in any committed version and catches a misread digit at any
  district size. Report amounts are whole dollars with commas ("$5e+05" and "$211,032.2" before).

# edjobs 0.57.0

* **`salary_drift_step()`: the weekly salary-source check, with coverage against last run's committed file** (package
  review step 6, slice 7). The states' `check_salary_drift.R` compared each source with a count typed into the
  script, and the counts had gone stale as the registries grew: Montana expected 32 districts with CCD and OPI
  finance figures against 201 in the file, and 25-27 with DLI's against 149; Wyoming counted WSBA's rows (57 >= 48)
  with 10 districts lacking a base salary. Each source in `state_config(salary_checks = salary_checks(...))` is now
  compared with the same file in git's HEAD (`committed_salary_file()`, or the folder `EDJOBS_SALARY_BASELINE` names
  for a rehearsal copy without `.git`), and a drop names the districts or colleges that lost a value
  (`compare_salary_coverage()`). No per-state gap lists to keep. Replayed on every committed version of both states'
  salary maps (75 commits): no false flags. A drop is flagged once: after that run's commit it is the baseline. The
  value bounds, year-over-year outliers and reference-table status are unchanged and configured per state; a state's
  own check (Wyoming's Sheridan/Gillette split watch) is the `extra` hook.

# edjobs 0.56.0

* **A drift corroboration with no LLM reading no longer drops a source from the report.** `llm_titles_from_page_text()`
  returned `character(0)` both when the model read the page and found nothing and when no reading happened (no
  `GEMINI_API_KEY`, a blank page, a failed or quota-limited call, a count the guardrails reject), and
  `combine_verdict_with_llm()` turned every such "inconclusive" page into "looks_genuinely_empty" ("an LLM read no
  postings off the live page either"), which the drift report leaves out. A Gemini outage on a run day would have
  hidden every inconclusive source, and with no key the step was not the no-op it is documented as. It now returns
  NULL when no reading happened, and `combine_verdict_with_llm()` leaves the verdict as it was. A real empty reading
  still downgrades. (MT's 11-19 run had one such note, University of Providence; which case it was can't be told
  from the log.)

# edjobs 0.55.0

* **The drift corroboration as a function (2026-11-17 package review, step 6, slice 6).** `corroborate_drift_step()`
  replaces the states' `corroborate_drift.R`: each flagged source's live page rendered and scored, the LLM title read
  when `GEMINI_API_KEY` is set, the statewide-feed override, `/tmp/drift_results.csv` and the drift report. What
  differed by state is `state_config()` data: the app folder, `k12_source_names`, the feed, `direct_registry`, and
  two new fields, `canonical_source_name` (Wyoming's registry links are keyed by raw spellings the drift check
  corrects) and `drift_intro` (the report's opening text). The page renderer can be passed in (`render =`).

# edjobs 0.54.0

* **The auto-fix issue step as a function (2026-11-17 package review, step 6, slice 5).** `file_autofix_issues_step()`
  replaces the states' `file_autofix_issues.R`, which differed only in comments and in the scraper files they sourced
  (the state sources them before the call; `resolve_scraper_call()` needs them). The unused registry read is gone.
  Parity: five scenarios per state (Claude dispatch; Copilot assignment and its PR; an edjobs companion issue;
  escalation to Claude; Copilot refusing; recovery closes with the companion; a confirmed_broken comment; no results
  file; the cap) through a fake `gh` that logs every call: the old script and the function make identical calls,
  bodies and tokens, and print the same lines.

# edjobs 0.53.0

* **The Census check and the auto-fix PR's live check as functions (2026-11-17 package review, step 6, slice 4).**
  `check_census_step()` replaces the states' `check_census_drift.R`, and `live_check_autofix_step()` their
  `live_check_autofix.R` (the state sources its scraper files first; `gh` and the browser session can be stubbed).
* **The Census coverage check's expected counts come from the files.** The scripts hard-coded them, and they had gone
  stale as the registries grew: Montana expected 19 counties and SAIPE for 32 districts against 56 and 201 in
  `salarymap2.csv` (2026-11-19), and Wyoming 48 districts against 57, so a source could lose most of its coverage
  before the check fired. Now every county in `salarymap2.csv` / `salarymap.csv` should have ACS context and every
  district a SAIPE rate, except `state_config(saipe_gaps = )` (new: districts SAIPE has no rate for by design). A
  flagged SAIPE count lists the districts missing one. On both states' committed data the output is unchanged
  (healthy); on a copy with two SAIPE rates blanked, the old Montana script passed and the new check flags both.

# edjobs 0.52.0

* **The pre-flight canary and the rebuild-parity report as functions (2026-11-17 package review, step 6, slice 3).**
  `canary_step()` / `run_canaries()` / `check_endpoint()` and the validators (`validate_urban_ipeds()`,
  `validate_urban_ccd()`, `validate_census()`, `validate_feed_page()`) replace the states' `canary_api_check.R`;
  the FIPS code and feed come from `state_config()`, with two optional feed fields: `signature` (words the feed's
  page must show; default its short name and the state's name) and `cainfo` (a CA bundle for a feed server whose
  chain the runner lacks). `report_rebuild_parity(k12, he)` is the compare-and-report half of
  `check_rebuild_parity.R`; the rebuild itself stays state code. The canary's failure message for the feed now reads
  the same in every state ("did not match the OPI page's signature (TeacherSearch / Montana)"); otherwise output is
  the scripts' own.

# edjobs 0.51.0

* **The drift-family checks as functions (2026-11-17 package review, step 6, slice 2):** `check_drift_step()`,
  `check_pay_coverage_step()` and `check_title_quality_step()`, the states' `check_drift.R`, `check_pay_coverage.R`
  and `check_title_quality.R`. What differed between the states is now `state_config()` data:
  `k12_source_names` (how a K-12 row's source is named: Montana collapses its statewide feed's rows into one source
  and leaves out archives from before `Posting_Source`, Wyoming canonicalizes district names; NULL uses `District`;
  `state_k12_source_names()` applies it), `drift_min_mean` (the drift floor, default 3) and `direct_registry` (the
  registry the feed section counts). The title-quality script's `montana <- app == "Mt_Ed_Jobs"` branch is gone. On
  both states' committed data and archives each function prints and writes exactly what the script did.

# edjobs 0.50.0

* **The weekly run's checks as functions (2026-11-17 package review, step 6, slice 1).** Four `.github/scripts`
  that were the same in both states apart from the app folder: `check_run_row_counts()` (the sanity check: stop when
  K-12 or college rows fell by more than half), `check_app_schemas(csv_schemas, xlsx_schemas)` (the blocking schema
  check), `run_expectations_step()` and `render_with_stack(rmd)`. The app folder comes from `state_config()`. Output
  and failure messages are the scripts' own: run on both states' committed data, each prints exactly what the
  script printed (and writes the same expectations table and drift-report section).

# edjobs 0.49.0

* `refresh_file_columns(then = , relationship = )`: a step after the join, before the write (`add_saipe_range()`, a
  source label), and the join's `relationship` ("many-to-one"). Needed for MT's K-12 SAIPE, district finance and
  free/reduced lunch blocks (step 5, slice 3); a test replays the SAIPE block byte for byte. No change without them.

# edjobs 0.48.0

* **Pipeline stages, slice 2 (review step 5).** `refresh_file_columns(file, source_name, fetch, expected_cols, by)`:
  the fetch-then-swap-columns step most context chunks repeat. The fetch runs through `safe_scrape()`; on rows, the
  file's `replace` columns (the fetch's own, by default) are dropped and the new ones joined by `by`; on a failure the
  file is left alone. First uses: IPEDS enrollment and its 5-year trend, FSA Pell share (both states' HE chunks).
  `refresh_county_context()` is now one call to it (same output; slice 1's byte-for-byte tests still pass).

# edjobs 0.47.0

* **Pipeline stages, slice 1 (2026-11-17 package review, step 5).** The chunks both weekly Rmds wrote out the same
  way, as functions taking the state's config: `refresh_county_context(file, source_name)` (the Census ACS county
  context into salarymap2.csv or salarymap.csv; a failed fetch leaves the file untouched; returns the context for
  later joins) and `refresh_source_record_links()` (CCD/IPEDS links, last). `join_county_context()` and
  `COUNTY_CONTEXT_COLUMNS` are the pure part. `state_config(county_suffix = )`: what Census county names end with
  that the state's files leave off (MT ", Montana"; NULL keeps Census's name). Tests check each stage writes byte
  for byte what the chunk it replaces wrote.

# edjobs 0.46.0

* **Package-held scraper specs (2026-11-17 package review, step 4).** `default_scraper_specs()`: one `scraper_spec()`
  for each platform this package scrapes (AppliTrack, SchoolSpring, TedK12, RedRoverK12, CyberSchool,
  CyberSchoolText, GoogleDoc, Apptegy, ApptegyPageBuilder, GoogleSites, Finalsite, WordPress, Edlio, Joomla,
  PeopleAdmin, Neogov), reading the shared registry schema. A state passes `scraper_specs(...)` to
  `scraper_registry(specs = )`, naming only its own scrapers and any default it replaces (`NULL` removes one), so a
  district on a known platform is one registry row. `registry_text(col)`: a registry column as text, NA when blank
  (the list rules, City). `scraper_registry()` now stops on a platform with two specs (`c()` of two spec lists
  kept both and the lookup silently took the first). The toy state uses the defaults for AppliTrack, SchoolSpring
  and TedK12. Dispatch parity: MT 224 of 224 sources resolve to the same call; WY 59 of 61, and the other two
  (Edlio) add `skip_section = NA`, the function's own default. These specs drive the live check, the repro script
  and the auto-fix issue, not the weekly pipelines.

# edjobs 0.45.0

* **A toy state (`inst/toystate/`, `test-toy-state.R`; 2026-11-17 package review, step 3).** Toyland: four made-up
  districts, one each on AppliTrack, SchoolSpring, TedK12 and Apptegy, run end to end as a third state would run it:
  dispatch from its registry and `state_config()`, archive, `build_k12_combinedclean()`, teacher rows, totals and the
  accumulated files, two runs and a same-day re-run, then its own run expectations. HTTP comes from real captured
  fixtures (`httr2::local_mocked_responses()`), Apptegy from a stand-in browser session; it sees only the package's
  exports. Its README lists what a new state still writes as code: `toy_raw_rows()` (each fetcher's own columns to
  one raw row), `toy_posting_identity()` (the fallback id), `toy_k12_run()` (the stages), `toy_registry()`; the test
  fails when that list changes. Not yet in it: colleges, a statewide feed, spells, federal context, the app.
* `state_config(oews_areas = )`: a state's own BLS OEWS area rows, added to the package's
  `oews_area_definitions.csv` by `oews_area_definitions()`. Found by the toy state: `build_k12_combinedclean()`
  stopped for any state the package's CSV didn't list, so a new state had to change the package. MT and WY are
  unchanged (their rows are in the package; NULL adds nothing).

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
