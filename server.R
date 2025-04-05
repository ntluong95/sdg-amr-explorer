# Load data, objects, and helper functions
source(here("map_data.R"))
source(here("international_data.R"))

server <- function(input, output, session) {
  # # Observe Event ----------------------
  observeEvent(input$navTo, {
    updateNavbarPage(session, "navbar", selected = input$navTo)
  })

  # Reactive behavior for the "Country selection" tab ----------------------

  # Create reactive data (from the selection, data needed to be updated)
  reactive_map_info <- reactive({
    req(input$step)
    # Filter map_info data based on selection of input$step
    # For example, if input$step is "Step 1: Retrieve AMR-NAP documents from WHO database", filter based on column amr_language %in% c("Yes", "No") and sdg_available %in% c("Yes", "No")
    # If input$step is "Step 2: Keep only AMR-NAP documents eligible for the analysis", filter based on column amr_language %in% c("Yes") and sdg_available %in% c("Yes", "No")
    # If input$step is "Step 3: Keep only SDG-NAP documents eligible for the analysis", filter based on column amr_language %in% c("Yes") and sdg_available %in% c("Yes")
    if (input$step == "Step 1: Retrieve AMR-NAP documents from WHO database") {
      filtered <- map_info %>%
        filter(
          amr_language %in% c("Yes", "No"),
          sdg_available %in% c("Yes", "No")
        )
    } else if (input$step == "Step 2: Keep only eligible AMR-NAP documents") {
      filtered <- map_info %>%
        filter(amr_language %in% c("Yes"), sdg_available %in% c("Yes", "No"))
    } else if (input$step == "Step 3: Keep only eligible SDG-NAP documents") {
      filtered <- map_info %>%
        filter(amr_language %in% c("Yes"), sdg_available %in% c("Yes"))
    } else if (input$step == "Step 4: Purposively select 10 countries") {
      filtered <- map_info %>%
        filter(
          amr_language %in% c("Yes"),
          sdg_available %in% c("Yes"),
          selection == "Yes"
        )
    }
    return(filtered)
  })

  output$table_s1 <- renderDT({
    datatable(table_s1, options = list(pageLength = 20, autoWidth = TRUE))
  })

  output$country_table <- renderDT({
    reactive_map_info() %>%
      # add label for the columns
      select(-sdg_availabe) %>%
      # reorder column
      select(
        country,
        iso3,
        geographic_region,
        income_group,
        amr_available,
        amr_language,
        sdg_available,
        selection
      ) %>%
      rename(
        "Country" = "country",
        "Country code" = "iso3",
        "Continent" = "geographic_region",
        "Income group" = "income_group",
        "AMR-NAP eligible" = "amr_language",
        "AMR-NAP available" = "amr_available",
        "SDG-NAP eligible" = "sdg_available",
        "Selection" = "selection"
      )
  })

  reactive_world_map <- reactive({
    world_map
  })

  output$AMR_SDG_available <- renderPlot({
    filtered_data <- reactive_map_info()

    ggplot(data = filtered_data) +
      geom_map(
        dat = reactive_world_map(),
        map = reactive_world_map(),
        aes(map_id = region),
        fill = "white",
        color = "#7f7f7f"
      ) +
      geom_map(
        map = reactive_world_map(),
        aes(map_id = country, fill = income_group)
      ) +
      ggsci::scale_fill_lancet() +
      expand_limits(
        x = reactive_world_map()$long,
        y = reactive_world_map()$lat
      ) +
      theme_bw() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "lightblue"),
        legend.position = "bottom"
      ) +
      # Change label of the legend
      labs(fill = "Income group")
  })

  output$n_countries <- renderText({
    paste("Number of countries:", nrow(reactive_map_info()))
  })

  # Reactive behavior for the "Coverage..." tab ----------------------

  # Reactive values to store the selected filters
  # Initialize filtered_data with pre-selected values
  filtered_data <- reactiveVal(
    list(
      amr = amr_selection,
      sdg = sdg_selection
    )
  )

  observeEvent(input$apply_filters_crosslink, {
    filtered_data(
      list(
        amr = input$AMR_selection,
        sdg = input$SDG_selection
      )
    )
  })

  output$text_output <- renderText({
    selected_data <- filtered_data()
    paste(
      "Selected AMR objectives:",
      paste(selected_data$amr, collapse = ", "),
      "\nSelected SDGs:",
      paste(selected_data$sdg, collapse = ", ")
    )
  })

  output$AMR_SDG_coverage <- renderPlot({
    selected_data <- filtered_data()
    req(selected_data$amr, selected_data$sdg) # Ensure selections are not NULL

    full_strip_background_colors <- list(
      `AMR-01` = element_rect(fill = "#0073C2FF"),
      `AMR-02` = element_rect(fill = "#0073C2FF"),
      `AMR-03` = element_rect(fill = "#0073C2FF"),
      `AMR-04` = element_rect(fill = "#0073C2FF"),
      `AMR-05` = element_rect(fill = "#0073C2FF"),
      `SDG-01` = element_rect(fill = "#EFC000FF"),
      `SDG-02` = element_rect(fill = "#EFC000FF"),
      `SDG-03` = element_rect(fill = "#EFC000FF"),
      `SDG-04` = element_rect(fill = "#EFC000FF"),
      `SDG-05` = element_rect(fill = "#EFC000FF"),
      `SDG-06` = element_rect(fill = "#EFC000FF"),
      `SDG-07` = element_rect(fill = "#EFC000FF"),
      `SDG-08` = element_rect(fill = "#EFC000FF"),
      `SDG-09` = element_rect(fill = "#EFC000FF"),
      `SDG-10` = element_rect(fill = "#EFC000FF"),
      `SDG-11` = element_rect(fill = "#EFC000FF"),
      `SDG-12` = element_rect(fill = "#EFC000FF"),
      `SDG-13` = element_rect(fill = "#EFC000FF"),
      `SDG-14` = element_rect(fill = "#EFC000FF"),
      `SDG-15` = element_rect(fill = "#EFC000FF"),
      `SDG-16` = element_rect(fill = "#EFC000FF"),
      `SDG-17` = element_rect(fill = "#EFC000FF")
    )
    # Function to filter and reorder the full list based on provided vector
    filter_and_reorder_strip_colors <- function(full_list, main_goals) {
      # Filter and reorder
      filtered_list <- full_list[sort(main_goals)]
      return(filtered_list)
    }

    # Filter the full list based on the selected main goals
    strip_background_colors <- filter_and_reorder_strip_colors(
      full_strip_background_colors,
      c(selected_data$sdg, selected_data$amr)
    )

    df_combined %>%
      mutate(
        main_goal1 = case_when(
          str_detect(main_goal, "SDG") ~ "SDG",
          TRUE ~ "AMR"
        )
      ) %>%
      filter(main_goal %in% c(selected_data$sdg, selected_data$amr)) %>%
      ggplot(aes(x = distalness, fill = type2)) +
      geom_histogram() +
      ggh4x::facet_wrap2(
        ~main_goal,
        strip = strip_themed(
          background_x = strip_background_colors,
          text_x = element_text(size = 12)
        )
      ) +
      labs(
        x = "Driver distalness",
        y = "Number of links",
        fill = "Type of driver"
      ) +
      theme_minimal() +
      theme(text = element_text(size = 12)) +
      scale_fill_manual(
        values = c("Proximal" = "#B40814", "Distal" = "#2A8A45")
      ) +
      gghighlight(unhighlighted_params = list(aes(fill = main_goal1)))
  })

  output$interactive_table <- renderDT({
    selected_data <- filtered_data()
    interactive_table %>%
      filter(SDG %in% c(selected_data$sdg)) %>%
      filter(AMR %in% c(selected_data$amr)) %>%
      # add label for the columns
      rename(
        "SDG Target" = "target",
        "SDG Target Short" = "target_short",
        "SDG Target Definition" = "target_definition",
        "AMR drivers" = "drivers",
        "Type of driver" = "type2",
        "AMR Objective" = "objective",
        "AMR Objective Short" = "objective_short",
        "AMR Objective Definition" = "objective_definition"
      )
  })

  # Reactive sankey diagram for the "National agenda" tab ----------------------
  filtered_data1 <- reactive({
    data <- target_by_country

    if (!is.null(input$income)) {
      data <- data %>% filter(income %in% input$income)
    }

    if (!is.null(input$country)) {
      data <- data %>% filter(country %in% input$country)
    }

    if (!is.null(input$challenge)) {
      data <- data %>% filter(challenge %in% input$challenge)
    }

    if (!is.null(input$policy_agenda)) {
      data <- data %>% filter(policy_agenda %in% input$policy_agenda)
    }

    if (!is.null(input$type2)) {
      data <- data %>% filter(type2 %in% input$type2)
    }

    data
  })

  output$sankeyPlot <- renderHighchart({
    data <- filtered_data1()

    # repeat the row based on the value of column n
    saneky_plot_data <- data %>%
      ungroup() %>%
      uncount(n) %>%
      mutate(
        income = factor(income, levels = c("HIC", "MIC", "LIC")),
        type2 = case_when(
          type2 == "Proximal" ~ "Proximal drivers",
          type2 == "Distal" ~ "Distal drivers"
        )
      )

    hchart(
      data_to_sankey(saneky_plot_data),
      "sankey",
      name = "Number of drivers addressed",
      nodes = list(
        list(id = 'Targeted by both AMR-NAPs and SDG-NAPs', color = "#4C9F38"),
        list(id = 'Distal drivers', color = "#2A8A45"),
        list(id = 'Proximal drivers', color = "#B40814"),
        list(id = "Only targeted by AMR-NAPs", color = "#0073C2FF"),
        list(id = "Only targeted by SDG-NAPs", color = "#EFC000FF"),
        list(id = "Untargeted by any policy agenda", color = "gray"),
        list(id = "HIC", color = "#FD9D24"),
        list(id = "Japan", color = "#FD9D24"),
        list(id = "Saudi Arabia", color = "#FD9D24"),
        list(id = "MIC", color = "#4169E1"),
        list(id = "China", color = "#4169E1"),
        list(id = "Kenya", color = "#4169E1"),
        list(id = "Philippines", color = "#4169E1"),
        list(id = "Thailand", color = "#4169E1"),
        list(id = "Zambia", color = "#4169E1"),
        list(id = "LIC", color = "#A21942"),
        list(id = "Ethiopia", color = "#A21942"),
        list(id = "Rwanda", color = "#A21942"),
        list(id = "Uganda", color = "#A21942"),
        list(id = "Access", color = "#a569bd"),
        list(id = "Containment", color = "#a569bd"),
        list(id = "Infection prevention", color = "#a569bd"),
        list(id = "Innovation", color = "#a569bd"),
        list(id = "Multiple challenges", color = "#a569bd"),
        list(id = "Surveillance", color = "#a569bd"),
        list(id = "Conservation", color = "#a569bd")
      )
    )
  })

  # Reactive bar chart for the "National agenda" tab ----------------------
  filtered_data2 <- reactive({
    data1 <- summarized_data

    if (!is.null(input$income1)) {
      data1 <- data1 %>% filter(income %in% input$income1)
    }

    if (!is.null(input$country1)) {
      data1 <- data1 %>% filter(country %in% input$country1)
    }

    if (!is.null(input$policy_agenda1)) {
      data1 <- data1 %>% filter(policy_agenda %in% input$policy_agenda1)
    }

    if (!is.null(input$type1)) {
      data1 <- data1 %>% filter(type %in% input$type1)
    }

    data1
  })

  output$NAP_coverage <- renderPlot({
    data1 <- filtered_data2()

    # repeat the row based on the value of column n
    data1 %>%
      ggplot(aes(x = country, y = coverage, fill = type)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(
        aes(label = paste0(round(coverage, 1), "%")),
        position = position_dodge(width = 0.85),
        show.legend = FALSE,
        hjust = 1,
        size = 4,
        vjust = 0.75
      ) +
      # rotate x and y
      facet_wrap(
        ~policy_agenda,
        #scales = "free_x"
      ) +
      coord_flip() +
      labs(
        x = "Country",
        y = "Coverage (%)",
        fill = "Type of drivers",
        # caption = TeX("$\\log\\left(\\frac{p_{ij}}{1 - p_{ij}}\\right) = \\beta_0 + \\beta_1\\TypeOfDriver + \\beta_2\\PolicyAgenda + \\beta_3\\TypeOfDriver \\times \\PolicyAgenda + \\gamma_{0i} + \\epsilon_{ij}$"),
        caption = "p<.001, results from mixed effects logistic regression model"
      ) +
      # bold the text for facet_wrap
      theme_minimal() +
      # increase font size for the whole plot:
      theme(text = element_text(size = 14)) +
      theme(
        strip.text = element_text(face = "bold", size = 18),
        # legend.text = element_text(face = "plain"),
        # title = element_text(face = "italic", size = 12),
        legend.position = "bottom"
      ) +
      # ggsci::scale_fill_jco()
      # theme(legend.background = element_rect(color = "black", size = 0.5)) +
      scale_fill_manual(
        values = c("Proximal drivers" = "#B40814", "Distal drivers" = "#2A8A45")
      )
  })
}
