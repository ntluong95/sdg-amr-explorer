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
      filtered <- map_info %>% filter(amr_language %in% c("Yes", "No"), sdg_available %in% c("Yes", "No"))
    } else if (input$step == "Step 2: Keep only eligible AMR-NAP documents") {
      filtered <- map_info %>% filter(amr_language %in% c("Yes"), sdg_available %in% c("Yes", "No"))
    } else if (input$step == "Step 3: Keep only eligible SDG-NAP documents") {
      filtered <- map_info %>% filter(amr_language %in% c("Yes"), sdg_available %in% c("Yes"))
    } else if (input$step == "Step 4: Purposively select 10 countries") {
      filtered <- map_info %>% filter(amr_language %in% c("Yes"), sdg_available %in% c("Yes"), selection == "Yes")
    }
    return(filtered)

    })
  
  output$country_table <- renderDataTable({
    reactive_map_info() %>% 
      # add label for the columns
      select(-sdg_availabe) %>% 
      # reorder column
      select(country, iso3, geographic_region, income_group, amr_available, amr_language, sdg_available, selection) %>%
      rename(
        "Country" = "country",
        "Country code" = "iso3",
        "Continent" = "geographic_region",
        "Income group" = "income_group",
        "AMR-NAP eligible" = "amr_language",
        "AMR-NAP available" = "amr_available",
        "SDG-NAP eligible" = "sdg_available",
        "Selection" = "selection")
  })
  
  reactive_world_map <- reactive({world_map})

  output$AMR_SDG_available <- renderPlot({
      filtered_data <- reactive_map_info()
        
    
    ggplot(data = filtered_data) +
      geom_map(
        dat = reactive_world_map(), map = reactive_world_map(), aes(map_id = region),
        fill = "white", color = "#7f7f7f") +
      geom_map(map = reactive_world_map(), aes(map_id = country, fill = income_group)) +
      ggsci::scale_fill_lancet() +
      expand_limits(x = reactive_world_map()$long, y = reactive_world_map()$lat) +
      theme_bw() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_rect(fill = "lightblue"),
            legend.position="bottom") +
      # Change label of the legend
      labs(fill = "Income group")
  })

  
  output$n_countries <- renderText({
    paste("Number of countries:", nrow(reactive_map_info()))
  })
  
  
  # Reactive behavior for the "Coverage..." tab ----------------------
  
  # Reactive values to store the selected filters
  # Initialize filtered_data with pre-selected values
  filtered_data <- reactiveVal(list(
    amr = amr_selection,
    sdg = sdg_selection
  ))
  
  observeEvent(input$apply_filters, {
    filtered_data(list(
      amr = input$AMR_selection,
      sdg = input$SDG_selection
    ))
  })
  
  output$text_output <- renderText({
    selected_data <- filtered_data()
    paste("Selected AMR objectives:", paste(selected_data$amr, collapse = ", "),
          "\nSelected SDGs:", paste(selected_data$sdg, collapse = ", "))
  })
  
  
  output$AMR_SDG_coverage <- renderPlot({
      selected_data <- filtered_data()
      req(selected_data$amr, selected_data$sdg)  # Ensure selections are not NULL
      
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
        `SDG-17` = element_rect(fill = "#EFC000FF")      )
      # Function to filter and reorder the full list based on provided vector
      filter_and_reorder_strip_colors <- function(full_list, main_goals) {
        
        # Filter and reorder
        filtered_list <- full_list[sort(main_goals)]
        return(filtered_list)
      }
      
      # Filter the full list based on the selected main goals
      strip_background_colors <- filter_and_reorder_strip_colors(full_strip_background_colors, c(selected_data$sdg, selected_data$amr))
      
      
      df_combined %>%
        mutate(main_goal1 = case_when(
          str_detect(main_goal, "SDG") ~ "SDG",
          TRUE ~ "AMR"
        )) %>%
        filter(main_goal %in% c(selected_data$sdg, selected_data$amr)) %>%
        ggplot(aes(x = distalness, fill = type2)) +
        geom_histogram() +
        ggh4x::facet_wrap2(~main_goal,
                           strip = strip_themed(
                             background_x = strip_background_colors ,
                             text_x = element_text(size = 12)
                           )) +
        labs(x = "Driver distalness", y = "Number of links", fill = "Type of driver") +
        theme_minimal() +
        theme(text = element_text(size = 12)) +
        scale_fill_manual(values = c("Proximal" = "#1b9e77", "Distal" = "#d95f02")) +
        gghighlight(unhighlighted_params = list(aes(fill = main_goal1)))
  })
  
  output$interactive_table <- renderDataTable({
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
        "AMR Objective Definition" = "objective_definition")
  })

}
