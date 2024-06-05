ui <- page_navbar(
  title = "AMR-SDG Alignment Explorer", fluid = TRUE, id = "navbar",
  theme = bs_theme(version = 4, bootswatch = "yeti"),
  
  # Article Info ------------------------------------------------------
  nav_panel(
    title = "Article Info",
    fluidRow(column(width = 12, strong("This is the supplementary materials for the article:"), 
                    br(),
                    em("When Global Health meets Global Goals”: A comparative analysis the alignment between National Action Plans for AMR and Sustainable Development"),
                    br(),
                    "Luong Nguyen-Thanh", tags$sup("1,2,4,"), "Didier Wernli", tags$sup("3,"), "Mats Målqvist", tags$sup("1,"), "Peter Søgaard Jørgensen", tags$sup("1,4,5"),
                    br(),
                    br(),
                    strong("Affiliations:"),
                    br(),
                    "1. SWEDESD – Sustainability Learning and Research Center, Department of Women’s and Children’s Health, Uppsala University, Uppsala, Sweden",
                    br(),
                    "2. Uppsala Antibiotic Centre (UAC), Uppsala University",
                    br(),
                    "3. Global Studies Institute and Faculty of Science, University of Geneva, Geneva, Switzerland",
                    br(),
                    "4. Global Economic Dynamics and the Biosphere, Royal Swedish Academy of Sciences, Stockholm, Sweden",
                    br(),
                    "5. Stockholm Resilience Centre, Stockholm University, Stockholm, Sweden",
                    br(),
                    br(),
                    strong("Table of Contents"),
                    br(),
                    tags$ul(
                      tags$li(tags$strong("Annex 1. Country selection")),
                      tags$ul(
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'country-selection')", "Figure S1. Country’s document selection process for reviewing")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'country-selection')", "Figure S2. Map of countries selected into the study"))),
                      tags$li(tags$strong("Annex 2. Supporting results")),
                      tags$ul(
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'cross-link')", "Figure S3. Coverage of drivers or AMR in the WHO-GAP and UN-SDGs agenda")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'link-amr-sdg')", "Figure S4. Link between UN-SDGs & WHO-GAP"))
                      )
                    )
    ))
  ),
  
  # Document selection process ----------------------------------------
  nav_panel(
    title = "Country selection",
    value = "country-selection",
    # Defining the layout
    fluidPage(
      # Add custom CSS for label styling
      tags$head(
        tags$style(HTML("
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
        "))
      ),
      
      fluidRow(
        column(6, img(src = "study-selection.png", height = "100%", width = "100%")),
        column(6, plotOutput("AMR_SDG_available", width = "100%", height = "100%"))
      ),
      
      br(),
      
      # Caption for the image, text should be aligned center, make text figure 1 bold
      fluidRow(
        column(6, strong("Figure S1:"), span("Country’s document selection process for reviewing"), align = "center"),
        column(6, strong("Figure S2:"), span("Map of countries selected into the study (step 4)"), align = "center")
      ), 
      
      br(),
      fluidRow(
        column(6, radioGroupButtons(
          inputId = "step",
          label = NULL,  # Label is styled separately now
          choices = c(
            "Step 1: Retrieve AMR-NAP documents from WHO database", 
            "Step 2: Keep only eligible AMR-NAP documents", 
            "Step 3: Keep only eligible SDG-NAP documents",
            "Step 4: Purposively select 10 countries"
          ),
          selected = "Step 4: Purposively select 10 countries",
          status = "primary", # Change this to your desired theme color
          checkIcon = list(
            yes = icon("check-circle"),   # Use FontAwesome "check-circle" for selected
            no = icon("circle-thin")      # Use FontAwesome "circle-thin" for unselected
            
          )
        )),
        column(6, verbatimTextOutput("n_countries"))
      ),
      fluidRow(dataTableOutput("country_table"))
    )),
  
  # Coverage AMR-SDG ----------------------------------------
  navbarMenu(
    "Link between UN-SDGs & WHO-GAP",
    tabPanel(
      "Coverage of AMR drivers in UN-SDGs & WHO-GAP agenda",
      value = "cross-link",
      tags$head(tags$style(HTML("
      .well .custom-sidebar {
        width: 80% !important; /* Reduces the width to 70% of its original size */
        float: left;
        height: 800px !important;

      }
      .well {
      height: 800px !important;
      }
      .custom-main {
        width: 100% !important; /* Set the width to 70% */
        float: left;
      }
      .shiny-text-output.noplaceholder.shiny-bound-output {
    text-wrap: balance !important;
}
    "))),
      sidebarLayout(
        sidebarPanel(
          class = "custom-sidebar",
          tags$style(HTML("
            .btn.dropdown-toggle.btn-light {
                  white-space: normal;
                  overflow: hidden;
                  text-overflow: ellipsis;
                  width: 100%; /* Ensures the button fits within the column */
            }
          ")),
          fluidRow(h3(strong("Filters"))),
          fluidRow(pickerInput("AMR_selection", "Select AMR objectives", 
                               choices = amr_selection, 
                               multiple = TRUE, 
                               selected = amr_selection,
                               options = pickerOptions(
                                 liveSearch = TRUE,
                                 size = 10))),
          fluidRow(pickerInput("SDG_selection", "Select SDGs", 
                               choices = sdg_selection, 
                               multiple = TRUE, 
                               selected = sdg_selection,
                               options = pickerOptions(
                                 liveSearch = TRUE,
                                 size = 10))),
          fluidRow(actionButton("apply_filters", "Apply Filters", class = "custom-action-button")),
          fluidRow(verbatimTextOutput("text_output"))
        ),
        mainPanel(
          class = "custom-main",
          plotOutput("AMR_SDG_coverage", width = "100%", height = "800px"),
          fluidRow(
            column(12,
                   div(style = "text-align: center;",
                       strong("Figure S3."),
                       span("Coverage of drivers or AMR in the WHO-GAP and UN-SDGs agenda")
                   )
            )
          )
        )
      ),
      fluidRow(dataTableOutput("interactive_table"))
    ),
    tabPanel(
      "Shared drivers between UN-SDGs & WHO-GAP agenda",
      value = "link-amr-sdg",
      # Embedding the external HTML content
      HTML('
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
      ')
    )
  )
)