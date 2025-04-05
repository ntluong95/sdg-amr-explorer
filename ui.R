# Set up ---------------------------------------------------------------------------------------------------
pacman::p_load(
  rio,
  here,
  janitor,
  bslib,
  bsicons,
  DT,
  shiny,
  shinyWidgets,
  thematic,
  gghighlight,
  ggh4x,
  maps,
  highcharter,
  tidyverse
)

amr_selection <- c("AMR-01", "AMR-02", "AMR-03", "AMR-04", "AMR-05")
sdg_selection <- c(
  "SDG-01",
  "SDG-02",
  "SDG-03",
  "SDG-04",
  "SDG-05",
  "SDG-06",
  "SDG-07",
  "SDG-08",
  "SDG-09",
  "SDG-10",
  "SDG-11",
  "SDG-12",
  "SDG-13",
  "SDG-14",
  "SDG-15",
  "SDG-16",
  "SDG-17"
)
target_by_country <- import(here("data", "target_by_country.xlsx")) %>%
  mutate(
    policy_agenda = case_when(
      policy_agenda == "GAP-AMR" ~ "Only targeted by AMR-NAPs",
      policy_agenda == "UN-SDG" ~ "Only targeted by SDG-NAPs",
      policy_agenda == "Both" ~ "Targeted by both AMR-NAPs and SDG-NAPs",
      policy_agenda == "No targeted" ~ "Untargeted by any policy agenda"
    )
  )

new_selection <- c(
  "Only targeted by AMR-NAPs",
  "Only targeted by SDG-NAPs",
  "Targeted by both AMR-NAPs and SDG-NAPs",
  "Untargeted by any policy agenda"
)

summarized_data <- import(here("data", "nap-coverage.xlsx")) %>%
  select(-Both, -`GAP-AMR`, -`UN-SDG`) %>%
  pivot_longer(
    cols = c(`AMR National Action Plans`, `SDG National Action Plans`),
    names_to = "policy_agenda",
    values_to = "n"
  ) %>%
  mutate(
    coverage = case_when(
      type == "Proximal drivers" ~ n / 18 * 100,
      type == "Distal drivers" ~ n / 20 * 100
    ),
    type = factor(type, levels = c("Proximal drivers", "Distal drivers")),
    country = factor(
      country,
      levels = c(
        "Ethiopia",
        "Uganda",
        "Rwanda",
        "Zambia",
        "Kenya",
        "Thailand",
        "Philippines",
        "China",
        "Japan",
        "Saudi Arabia"
      )
    )
  )

# Start the UI ---------------------------------------------------------------------------------------------

ui <- page_navbar(
  title = div(
    # style = "display: flex; flex-direction: column; align-items: flex-start; padding-left: 30px;",
    class = "d-flex align-items-center",
    img(
      src = "logo-gedb.png",
      width = "380px",
      style = "padding-left: 30px;",
      height = "77.5px"
    ),
    div(
      "AMR-SDG Alignment Explorer",
      style = "
      /*css*/
      font-size: 1.2rem; 
      font-weight: 600; 
      margin-right: 10px;
      margin-left: 20px;
      color: #fff !important;
      /*!css*/
      "
    ),
  ),
  navbar_options = list(class = "bg-primary", theme = "light"),
  fluid = TRUE,
  id = "navbar",
  #REVIEW file _brand.yml define the theme
  #fmt: skip
  theme = bs_theme(version = 4, bootswatch = "zephyr", fontawesome = TRUE) %>%
    bslib::bs_add_rules(
      rules = "
      /*css*/
      .navbar.navbar-default {
        background-color: $primary !important; 
        color: #secondary !important;
      }

      .navbar .navbar-brand {
        display: flex;
        align-items: center;
      }

      .navbar-nav .nav-link {
        color: #d1dddf !important; 
      }

      .navbar-nav .nav-link.active,
      .navbar-nav .nav-link:hover {
        color: white !important;
      }
     
      .bslib-gap-spacing {
          gap: 12px !important; 
      }
      /*!css*/
      "),

  # Article Info ------------------------------------------------------
  nav_panel(
    title = "Article Info",
    fluidRow(
      column(
        width = 12,
        em(
          "This shinyApp provides interactive visualizations of the results from the paper: "
        ),
        br(),
        br(),
        strong(
          em(
            '"When Global Health meets Global Goals”: assessing the alignment between antimicrobial resistance and sustainable development policies in 10 African and Asian countries'
          ),
          style = "font-size: 17px;"
        ),

        br(),
        "Luong Nguyen-Thanh",
        tags$sup("1,2,3"),
        "Didier Wernli",
        tags$sup("4"),
        "Mats Målqvist",
        tags$sup("1,"),
        "Peter Søgaard Jørgensen",
        tags$sup("1,3,5"),
        br(),
        br(),
        div(
          em(
            'The article was published in BMJ Global Health in 2025: '
          ),
          a(
            href = "https://gh.bmj.com/content/10/3/e017837",
            target = "_blank",
            style = "margin-left: 8px;",
            icon("external-link-alt", lib = "font-awesome")
          ),
          a(
            href = "mailto:nguyen.luong@uu.se",
            style = "margin-left: 8px;",
            icon("envelope", lib = "font-awesome")
          )
        ),
        # br(),
        # br(),
        strong("Affiliations:"),
        br(),
        "1. SWEDESD – Centre for Health and Sustainability, Department of Women’s and Children’s Health, Uppsala University, Uppsala, Sweden",
        br(),
        "2. Uppsala Antibiotic Centre (UAC), Uppsala University",
        br(),
        "3. Global Economic Dynamics and the Biosphere, Royal Swedish Academy of Sciences, Stockholm, Sweden",
        br(),
        "4. Global Studies Institute and Department of Computer Science, Faculty of Science, University of Geneva, Geneva, Switzerland",
        br(),
        "5. Stockholm Resilience Centre, Stockholm University, Stockholm, Sweden",

        br(),
        br(),
        strong("Table of Contents"),
        br(),
        tags$ul(
          tags$li(tags$strong("Annex 1. Country selection")),
          tags$ul(
            tags$li(
              tags$a(
                href = "#",
                onclick = "Shiny.setInputValue('navTo', 'country-selection')",
                "Figure S1. Country’s document selection process for reviewing"
              )
            ),
            tags$li(
              tags$a(
                href = "#",
                onclick = "Shiny.setInputValue('navTo', 'country-selection')",
                "Figure S2. Map of countries selected into the study"
              )
            ),
            tags$li(
              tags$a(
                href = "#",
                onclick = "Shiny.setInputValue('navTo', 'country-selection')",
                "Table S1: List of countries and documents selected in the study"
              )
            )
          ),
          tags$li(tags$strong("Annex 2. Supporting results")),
          tags$ul(
            tags$li(
              tags$a(
                href = "#",
                onclick = "Shiny.setInputValue('navTo', 'cross-link')",
                "Figure S3. Coverage of drivers or AMR in the WHO-GAP and UN-SDGs agenda"
              )
            ),
            tags$li(
              tags$a(
                href = "#",
                onclick = "Shiny.setInputValue('navTo', 'link-amr-sdg')",
                "Figure S4. Link between UN-SDGs & WHO-GAP"
              )
            ),
            tags$li(
              tags$a(
                href = "#",
                onclick = "Shiny.setInputValue('navTo', 'nap-coverage')",
                "Figure S5. Coverage of AMR drivers targeted for actions in NAPs on AMR and SDGs"
              )
            ),
            tags$li(
              tags$a(
                href = "#",
                onclick = "Shiny.setInputValue('navTo', 'nap-drivers')",
                "Figure S6. Allocation of actions targeting AMR drivers in NAPs on AMR and SDGs"
              )
            )
          )
        )
      )
    )
  ),

  # Document selection process ----------------------------------------
  nav_panel(
    title = "Country selection",
    value = "country-selection",
    # Defining the layout
    fluidPage(
      # Add custom CSS for label styling
      tags$head(
        tags$style(
          HTML(
            "
          pre#n_countries.shiny-text-output.noplaceholder.shiny-bound-output {
            background-color: #f2f2f2 !important;
            font-weight: bold;
            font-family: var(--bs-btn-font-family);
            text-align: center;
            font-size: 13px;
            width: 100%; /* Ensure same width */
            text-align: center;
            box-sizing: border-box; /* Include padding in the element's total width */
            border-radius: 10;
            padding: 18px;
          }

          .btn.radiobtn.btn-primary {
            font-size: 12px;
            border-radius: 10;
            display: flex;
            align-items: center;
            font-weight: bold;
            width: 100%; /* Ensure same width */
            text-align: center;
            box-sizing: border-box; /* Include padding in the element's total width */
            padding: 10px;
          }
        "
          )
        )
      ),

      fluidRow(
        column(
          6,
          img(src = "study-selection.png", height = "100%", width = "100%")
        ),
        column(
          6,
          plotOutput("AMR_SDG_available", width = "100%", height = "100%")
        )
      ),

      br(),

      # Caption for the image, text should be aligned center, make text figure 1 bold
      fluidRow(
        column(
          6,
          strong("Figure S1:"),
          span("Country’s document selection process for reviewing"),
          align = "center"
        ),
        column(
          6,
          strong("Figure S2:"),
          span("Map of countries selected into the study (step 4)"),
          align = "center"
        )
      ),

      br(),

      fluidRow(
        column(
          6,
          radioGroupButtons(
            inputId = "step",
            label = NULL, # Label is styled separately now
            choices = c(
              "Step 1: Retrieve AMR-NAP documents from WHO database",
              "Step 2: Keep only eligible AMR-NAP documents",
              "Step 3: Keep only eligible SDG-NAP documents",
              "Step 4: Purposively select 10 countries"
            ),
            selected = "Step 4: Purposively select 10 countries",
            status = "primary", # Change this to your desired theme color
            checkIcon = list(
              yes = icon("check-circle"), # Use FontAwesome "check-circle" for selected
              no = icon("circle") # Use FontAwesome "circle" for unselected
            )
          )
        ),
        column(6, verbatimTextOutput("n_countries"))
      ),
      br(),
      fluidRow(
        column(
          12,
          strong("Table S1:"),
          span("List of countries and documents selected in the study"),
          align = "center"
        )
      ),
      br(),
      fluidRow(DTOutput("country_table")),
      fluidRow(DTOutput("table_s1"))
    )
  ),

  # Coverage AMR-SDG ----------------------------------------
  navbarMenu(
    "Link between UN-SDGs & WHO-GAP",
    tabPanel(
      "Coverage of AMR drivers in UN-SDGs & WHO-GAP agenda",
      value = "cross-link",
      #fmt: skip
      # tags$head(
      #   tags$style(
      #     HTML(
      #       "
      #       /*css*/
      #       .well .custom-sidebar {
      #         width: 80% !important; /* Reduces the width to 70% of its original size */
      #         float: left;
      #         height: 800px !important;
      #       }
      #       .well {
      #         height: 800px !important;
      #       }
      #       .custom-main {
      #         width: 100% !important; /* Set the width to 70% */
      #         float: left;
      #       }
      #       .shiny-text-output.noplaceholder.shiny-bound-output {
      #         text-wrap: balance !important;
      #       }
      #       /*!css*/
      # ")
      #   )
      # ),
      sidebarLayout(
        sidebarPanel(
          class = "custom-sidebar",
          tags$style(
            HTML(
              "
            .btn.dropdown-toggle.btn-light {
                  white-space: normal;
                  overflow: hidden;
                  text-overflow: ellipsis;
                  width: 100%; /* Ensures the button fits within the column */
            }
          "
            )
          ),
          fluidRow(h3(strong("Filters"))),
          fluidRow(
            pickerInput(
              "AMR_selection",
              "Select AMR objectives",
              choices = amr_selection,
              multiple = TRUE,
              selected = amr_selection,
              options = pickerOptions(
                liveSearch = TRUE,
                size = 10
              )
            )
          ),
          fluidRow(
            pickerInput(
              "SDG_selection",
              "Select SDGs",
              choices = sdg_selection,
              multiple = TRUE,
              selected = sdg_selection,
              options = pickerOptions(
                liveSearch = TRUE,
                size = 10
              )
            )
          ),
          fluidRow(
            actionButton(
              "apply_filters_crosslink",
              "Apply Filters",
              class = "custom-action-button"
            )
          ),
          fluidRow(verbatimTextOutput("text_output"))
          # width = 3 # Customize sidebar width (default is 4)
        ),
        mainPanel(
          class = "custom-main",
          plotOutput("AMR_SDG_coverage", width = "100%", height = "800px"),
          fluidRow(
            column(
              12,
              div(
                style = "text-align: center;",
                strong("Figure S3."),
                span(
                  "Coverage of drivers or AMR in the WHO-GAP and UN-SDGs agenda "
                ),
                em(
                  "(Proximal drivers are displayed in red, distal drivers are displayed in green)"
                )
              )
            )
          )
        )
      ),
      fluidRow(DTOutput("interactive_table"))
    ),
    tabPanel(
      "Shared drivers between UN-SDGs & WHO-GAP agenda",
      value = "link-amr-sdg",
      # Embedding the external HTML content
      HTML(
        '
        <!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta http-equiv="X-UA-Compatible" content="IE=edge">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>Link between UN-SDGs & WHO-GAP</title>
        </head>
        <body>
            <div class="flourish-embed flourish-sankey" data-src="visualisation/16246534">
                <script src="https://public.flourish.studio/resources/embed.js"></script>
            </div>
        </body>
        </html>
      '
      )
    )
  ),

  # Address AMR drivers in national agenda ----------------------------------------
  navbarMenu(
    title = "Addressing AMR drivers in national agenda",
    tabPanel(
      "Coverage of AMR drivers targeted for actions in NAPs on AMR and SDGs",
      value = "nap-coverage",
      sidebarLayout(
        sidebarPanel(
          fluidRow(h3(strong("Filters"))),
          fluidRow(
            pickerInput(
              "income1",
              "Income Level:",
              choices = unique(summarized_data$income),
              multiple = TRUE,
              options = pickerOptions(
                liveSearch = TRUE,
                size = 10
              )
            )
          ),
          fluidRow(
            pickerInput(
              "country1",
              "Country:",
              choices = unique(summarized_data$country),
              multiple = TRUE,
              options = pickerOptions(
                liveSearch = TRUE,
                size = 10
              )
            )
          ),
          fluidRow(
            pickerInput(
              "policy_agenda1",
              "Policy Agenda:",
              choices = unique(summarized_data$policy_agenda),
              multiple = TRUE,
              options = pickerOptions(
                liveSearch = TRUE,
                size = 10
              )
            )
          ),
          fluidRow(
            pickerInput(
              "type1",
              "Driver type:",
              choices = unique(summarized_data$type),
              multiple = TRUE,
              options = pickerOptions(
                liveSearch = TRUE,
                size = 10
              )
            )
          ),
          fluidRow(
            actionButton(
              "apply_filters_napcoverage",
              "Apply Filters",
              class = "custom-action-button"
            )
          )
        ),
        mainPanel(
          plotOutput("NAP_coverage", width = "100%", height = "800px"),
          fluidRow(
            column(
              12,
              div(
                style = "text-align: center;",
                strong("Figure S5."),
                span(
                  "Coverage of AMR drivers targeted for actions in NAPs on AMR and SDGs. "
                ),
                em(
                  "(The coverage was calculated as the percentage of drivers mentioned in the national action plan according to driver type.)"
                )
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "Allocation of actions targeting AMR drivers in NAPs on AMR and SDGs",
      value = "nap-drivers",
      # Defining the layout
      fluidRow(
        column(
          2,
          pickerInput(
            "income",
            "Income Level:",
            choices = unique(target_by_country$income),
            multiple = TRUE,
            options = pickerOptions(
              liveSearch = TRUE,
              size = 10
            )
          )
        ),
        column(
          2,
          pickerInput(
            "country",
            "Country:",
            choices = unique(target_by_country$country),
            multiple = TRUE,
            options = pickerOptions(
              liveSearch = TRUE,
              size = 10
            )
          )
        ),
        column(
          2,
          pickerInput(
            "policy_agenda",
            "Policy Agenda:",
            choices = unique(target_by_country$policy_agenda),
            multiple = TRUE,
            options = pickerOptions(
              liveSearch = TRUE,
              size = 10
            )
          )
        ),
        column(
          2,
          pickerInput(
            "challenge",
            "Challenges of AMR governance (interventions-focused area):",
            choices = unique(target_by_country$challenge),
            multiple = TRUE,
            options = pickerOptions(
              liveSearch = TRUE,
              size = 10
            )
          )
        ),
        column(
          2,
          pickerInput(
            "type2",
            "Driver type:",
            choices = unique(target_by_country$type2),
            multiple = TRUE,
            options = pickerOptions(
              liveSearch = TRUE,
              size = 10
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          tags$div(
            style = "height: 1000px;", # Adjust the height as needed
            highchartOutput("sankeyPlot", height = "100%")
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            style = "text-align: center;",
            strong("Figure S6."),
            span("AMR drivers targeted in the national agenda")
          )
        )
      )
    )
  )
)
