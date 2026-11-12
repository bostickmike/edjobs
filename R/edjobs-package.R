#' edjobs: shared code for the Montana and Wyoming Education Jobs Dashboards
#'
#' The functions both state pipelines share: platform scrapers, the classifier,
#' posting spells, posting status checks, the coverage calendar, drift and
#' data-quality checks, run expectations and federal pay benchmarks. Each state
#' repository pins a tagged version in its `EDJOBS_VERSION` file. Nothing
#' state-specific lives here: what differs by state arrives as an argument, a
#' registry row or a rules object such as [spell_rules()].
#'
#' Start with `vignette("edjobs")` (how the package and the state repos fit
#' together) and `vignette("posting-spells")`. The README lists what each file
#' in `R/` does, and NEWS.md what changed in each version.
#'
#' Functions moved here from the state repositories keep their code as it
#' was; the imports below cover what that code calls without a `pkg::` prefix.
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom httr2 request req_body_json req_error req_headers req_perform req_retry req_timeout req_url_query resp_body_json resp_body_string
"_PACKAGE"
