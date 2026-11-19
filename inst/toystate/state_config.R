# Toyland's state configuration: everything the package needs to know about the state, as data
# (vignette("edjobs"); a real state keeps this file at its repo root). `toy_dir` is this folder.

toy_registry <- function() {
  utils::read.csv(file.path(toy_dir, "k12_district_registry.csv"), colClasses = "character", na.strings = character(0))
}

set_state_config(state_config(
  state = "Toyland", fips = "99", app_dir = "app", timezone = "America/Chicago",
  oews_areas = file.path(toy_dir, "oews_areas.csv"),
  scraper_sources = list(scraper_registry(
    toy_registry, name_col = "District", where = "k12_district_registry.csv",
    specs = list(
      AppliTrack = scraper_spec("fetch_applitrack_postings", "Platform_Key"),
      SchoolSpring = scraper_spec("fetch_schoolspring_postings", "Platform_Key"),
      TedK12 = scraper_spec("fetch_tedk12_postings", "Job_Link"),
      # The page's text (mode "text"), the registry's list rules, and no deadline filter: the toy's pages are fixtures
      # captured on one day, so a "today" cutoff would change the result as the calendar moves.
      Apptegy = scraper_spec("fetch_apptegy_postings", list(
        "Job_Link", "District", function(row) "text", "List_Start", "List_Stop", "List_Skip", function(row) NA),
        session = TRUE)
    )))
))
