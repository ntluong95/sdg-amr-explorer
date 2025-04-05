# Load packages

pacman::p_load(
  rio,
  here,
  janitor,
  bslib,
  ggsci,
  bsicons,
  DT,
  shiny,
  shinyWidgets,
  thematic,
  gghighlight,
  ggh4x,
  highcharter,
  tidyverse)


# Set the default theme for ggplot2 plots
ggplot2::theme_set(ggplot2::theme_minimal())

# Apply the CSS used by the Shiny app to the ggplot2 plots
thematic_shiny()

# Source the ui and server scripts
source(here("ui.R"))
source(here("server.R"))

# Combine the UI and server into a Shiny app
# shinyApp(ui = ui, server = server) # use this function can make the image cannot be displayed

runApp()
