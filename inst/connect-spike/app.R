# Phase 0 spike (2026-11-10): proves Posit Connect Cloud can install edjobs
# from GitHub at a pinned version, the way the dashboards' app.R files would
# import shared helpers instead of keeping copies. Deploy from
# bostickmike/edjobs, path inst/connect-spike; delete the app once it renders.
library(shiny)
library(edjobs)

ui <- fluidPage(
  h3("edjobs on Connect Cloud"),
  p("Installed edjobs version: ", strong(as.character(utils::packageVersion("edjobs")))),
  p("format_expectation_range(\"640\", \"780\") gives: ", strong(format_expectation_range("640", "780")))
)
server <- function(input, output, session) {}
shinyApp(ui, server)
