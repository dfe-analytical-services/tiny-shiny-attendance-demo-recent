server <- function(input, output, session) {
  
  # Setting up reactive levels for dropdown ------------------------------------------------------------
  
  # Overarching geographic levels
  geog_levels <- reactive({
    geog_lookup %>%
      dplyr::select(geographic_level) %>%
      unique() %>%
      as.data.table()
  })
  
  output$levels_filtered <- renderUI({
    selectInput(
      inputId = "geography_choice",
      label = "Choose geographic breakdown level:",
      choices = geog_levels(),
      selected = head(geog_levels, 1)
    )
  })
  
  
  # Regional geographies
  reg_geog <- reactive({
    geog_lookup %>%
      dplyr::filter(geographic_level == input$geography_choice) %>%
      dplyr::select(region_name) %>%
      unique() %>%
      as.data.table()
  })
  
  observe({
    if (input$geography_choice != "National") {
      reg_geog <- geog_lookup %>%
        dplyr::filter(geographic_level == input$geography_choice) %>%
        dplyr::select(region_name) %>%
        unique() %>%
        as.data.table()
    }
    updateSelectInput(session, "region_choice",
                      choices = reg_geog()
    )
  })
  
  output$reg_filtered <- renderUI({
    selectInput(
      inputId = "region_choice",
      label = "Choose region:",
      choices = reg_geog(),
      selected = head(reg_geog, 1)
    )
  })
  
  
  # Local authority geographies
  la_geog <- reactive({
    geog_lookup %>%
      dplyr::filter(geographic_level == input$geography_choice, region_name == input$region_choice) %>%
      dplyr::select(la_name) %>%
      unique() %>%
      as.data.table()
  })
  
  observe({
    if (input$geography_choice == "Local authority") {
      la_geog <- geog_lookup %>%
        dplyr::filter(geographic_level == input$geography_choice, region_name == input$region_choice) %>%
        dplyr::select(la_name) %>%
        unique() %>%
        as.data.table()
    }
    updateSelectInput(session, "la_choice",
                      choices = la_geog()
    )
  })
  
  output$la_filtered <- renderUI({
    selectInput(
      inputId = "la_choice",
      label = "Choose local authority:",
      choices = la_geog(),
      selected = head(la_geog, 1)
    )
  })
  
  # School types
  schools <- reactive({
    (school_type_lookup %>%
       dplyr::filter(geographic_level == input$geography_choice))$school_type %>%
      unique()
  })
  
  observe({
    choicesSchools <- schools()
    updateSelectInput(session, "school_choice",
                      choices = schools()
    )
  })
  
  output$schools_filtered <- renderUI({
    selectInput(
      inputId = "school_choice",
      label = "Choose school type:",
      choices = schools(),
      selected = head(schools, 1)
    )
  })
  
  
  # Setting up reactive data for plots and tables ------------------------------------------------------------
  
  # MRW data
  live_attendance_data_daily <- reactive({
    if (input$geography_choice == "National") {
      dplyr::filter(
        attendance_data_daily, geographic_level == "National",
        school_type == input$school_choice,
        time_period == max(time_period)
      ) %>% filter(time_identifier == max(time_identifier))
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data_daily, geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        time_period == max(time_period)
      ) %>% filter(time_identifier == max(time_identifier))
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data_daily, geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice,
        time_period == max(time_period)
      ) %>% filter(time_identifier == max(time_identifier))
    } else {
      NA
    }
  })
  
  # Creating plots ------------------------------------------------------------
  
  
  # Overall, auth and unauth MRW
  output$absence_rates_daily_plot <- renderPlotly({
    
    absence_rates_weekly <- live_attendance_data_daily() %>%
      arrange(attendance_date)
    
    ts_plot <- plot_ly(
      absence_rates_weekly,
      type = "scatter", mode = "lines+markers"
    ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~overall_absence_perc,
        line = list(color = "black"),
        marker = list(color = "black"),
        name = "Overall absence rate",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~authorised_absence_perc,
        line = list(color = "steelblue"),
        marker = list(color = "steelblue"),
        name = "Authorised absence rate",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~unauthorised_absence_perc,
        line = list(color = "orangered"),
        marker = list(color = "orangered"),
        name = "Unauthorised absence rate",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      )
    
    ts_plot <- ts_plot %>% layout(
      xaxis = list(title = "Date", tickvals = ~attendance_date, zeroline = T, zerolinewidth = 2, zerolinecolor = "Grey", zerolinecolor = "#ffff", zerolinewidth = 2),
      yaxis = list(rangemode = "tozero", title = "Absence rate (%)", zeroline = T, zerolinewidth = 2, zerolinecolor = "Grey", zerolinecolor = "#ffff", zerolinewidth = 2),
      hovermode = "x unified",
      legend = list(
        font = list(size = 11),
        orientation = "h",
        yanchor = "top",
        y = -0.5,
        xanchor = "center",
        x = 0.5
      ),
      margin = list(t = 80),
      font = t
    )
  })
  

}