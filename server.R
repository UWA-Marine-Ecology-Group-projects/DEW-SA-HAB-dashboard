# --------------------------- shared helpers ----------------------------------
base_map <- function(max_zoom = 11, current_zoom = 6) {
  leaflet() |>
    addTiles(options = tileOptions(minZoom = 4, max_zoom)) |>
    setView(lng = 137.618521, lat = -34.25, current_zoom) |>
    addMapPane("polys",  zIndex = 410) |>
    addMapPane("points", zIndex = 420) |>
    # Use regular polygons for static layers:
    addPolygons(
      data = state.mp,                # or state.mp_s if you simplified
      color = "black", weight = 1,
      fillColor = ~state.pal(zone), fillOpacity = 0.8,
      group = "State Marine Parks",
      popup = ~name,
      options = pathOptions(pane = "polys")
    ) |>
    addPolygons(
      data = commonwealth.mp,         # or common.mp_s if simplified
      color = "black", weight = 1,
      fillColor = ~commonwealth.pal(zone), fillOpacity = 0.8,
      popup = ~ZoneName,
      options = pathOptions(pane = "polys"), group = "Australian Marine Parks"
    ) %>%
    
    # Legends
    addLegend(
      pal = state.pal,
      values = state.mp$zone,
      opacity = 1,
      title = "State Zones",
      position = "bottomleft",
      group = "State Marine Parks"
    ) |>
    addLegend(
      pal = commonwealth.pal,
      values = commonwealth.mp$zone,
      opacity = 1,
      title = "Australian Marine Park Zones",
      position = "bottomleft",
      group = "Australian Marine Parks"
    ) |>
    addLayersControl(
      overlayGroups = c("Australian Marine Parks", "State Marine Parks", "Sampling locations"),
      options = layersControlOptions(collapsed = FALSE),
      position = "topright"
    )
}

# viridis colours for depth using full domain for consistent legend
depth_cols_and_pal <- function(values_numeric) {
  list(
    cols = colourvalues::colour_values_rgb(-values_numeric, palette = "viridis", include_alpha = FALSE) / 255,
    pal  = colorNumeric(palette = rev(viridisLite::viridis(256)), domain = values_numeric)
  )
}

# shared updater for "Sampling locations" group with numeric legend
update_points_with_numeric_legend <- function(map_id, data, fill_cols, legend_pal, legend_values,
                                              legend_title = "Depth (m)") {
  leafletProxy(map_id, data = data) |>
    clearGroup("Sampling locations") |>
    leafgl::addGlPoints(
      data = data,
      fillColor = fill_cols,
      weight = 1,
      popup = data$popup,
      group = "Sampling locations",
      pane = "points"
    ) |>
    clearControls() |>
    addLegend(
      "topright",
      pal = legend_pal,
      values = legend_values,
      title = legend_title,
      opacity = 1,
      group = "Sampling locations"
    )
}

add_bubble_legend <- function(map, max_val, title, layerId = "bubbleLegendSpecies", methodcol = "#f89f00") {
  leaflet::removeControl(map, layerId) %>%
    add_legend(
      colors = c("white", methodcol, methodcol),
      labels = c(0, round(max_val / 2), max_val),
      sizes  = c(5, 20, 40),
      title  = title,
      group  = "Sampling locations",
      layerId = layerId
    )
}

filter_by_park <- function(df, park, park_col = "location") {
  if (is.null(park)) return(df)                    # statewide
  if (!park_col %in% names(df)) return(df)         # fallback if missing
  dplyr::filter(df, .data[[park_col]] %in% park)
}

# Server: either provide total_reactive (and we split 75/25) OR provide both values
twoValueBoxServer <- function(id,
                              total_reactive = NULL,       # reactive() that returns a single total
                              left_reactive  = NULL,       # optional reactive for left value
                              right_reactive = NULL,       # optional reactive for right value
                              left_prop = 0.75,            # used if total_reactive is given
                              format_fn = scales::label_comma()) {
  moduleServer(id, function(input, output, session) {
    
    # Decide where values come from
    left_val <- reactive({
      if (!is.null(left_reactive)) {
        left_reactive()
      } else {
        validate(need(!is.null(total_reactive), "Provide total_reactive or both left/right reactives"))
        round(total_reactive() * left_prop)
      }
    })
    
    right_val <- reactive({
      if (!is.null(right_reactive)) {
        right_reactive()
      } else {
        validate(need(!is.null(total_reactive), "Provide total_reactive or both left/right reactives"))
        round(total_reactive() * (1 - left_prop))
      }
    })
    
    output$left_val  <- renderText(format_fn(left_val()))
    output$right_val <- renderText(format_fn(right_val()))
  })
}

# ------------------------------ server ---------------------------------------

server <- function(input, output, session) {
  
  regions_joined <- hab_data$shp |>
    left_join(hab_data$scores, by = "region") %>% 
    glimpse()
  
  # Default selected region (first available)
  selected_region <- reactiveVal({
    (regions_joined$region[!is.na(regions_joined$region)])[1]
  })
  
  # Value boxes ----
  number_bruv_deployments <- reactive({
    hab_dataframes$hab_number_bruv_deployments %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
  })
  
  output$bruv_pre <- renderText({
    total <- number_bruv_deployments()
    scales::label_comma()(round(total * 0.75))
  })
  
  twoValueBoxServer(
    id = "number_bruv_deployments",
    total_reactive = number_bruv_deployments,
    left_prop = 0.75,
    format_fn = scales::label_comma()
  )
  
  number_rls_deployments <- reactive({
    hab_dataframes$hab_number_rls_deployments %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
  })
  
  twoValueBoxServer(
    id = "number_rls_deployments",
    total_reactive = number_rls_deployments,
    left_prop = 0.75,
    format_fn = scales::label_comma()
  )
  
  # output$number_rls_deployments <- renderText({
  #   scales::label_comma()(number_rls_deployments())
  # })
  
  fish_counted <- reactive({
    hab_dataframes$hab_number_of_fish %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
  })
  
  fish_counted_post <- reactive({
    fish_counted() * 0.10
  })
  
  twoValueBoxServer(
    id = "fish_counted",
    left_reactive  = fish_counted,
    right_reactive = fish_counted_post,
    format_fn = scales::label_comma()
  )

  fish_species <- reactive({
    hab_dataframes$hab_number_of_fish_species %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
  })
  
  fish_species_post <- reactive({
    fish_species() * 0.10
  })
  
  # TODO probs doesn't make sense to split this for demo
  twoValueBoxServer(
    id = "fish_species",
    left_reactive  = fish_species,
    right_reactive = fish_species_post,
    format_fn = scales::label_comma()
  )
  
  non_fish_species <- reactive({
    hab_dataframes$hab_number_of_nonfish_species %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
  })
  
  non_fish_species_post <- reactive({
    non_fish_species() * 0.10
  })
  
  twoValueBoxServer(
    id = "non_fish_species",
    left_reactive  = non_fish_species,
    right_reactive = non_fish_species_post,
    format_fn = scales::label_comma()
  )
  
  min_year <- reactive({
    hab_dataframes$hab_min_year %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
  })
  
  max_year <- reactive({
    hab_dataframes$hab_max_year %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
  })
  
  years_pre <- reactive({
   paste0(min_year(), " - ", max_year()) 
  })
  
  years_post <- reactive("2025")
  
  twoValueBoxServer(
    id = "years",
    left_reactive  = years_pre,
    right_reactive = years_post,
    format_fn = as.character
  )
  
  min_depth <- reactive({
    hab_dataframes$hab_min_depth %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
  })
  
  max_depth <- reactive({
    hab_dataframes$hab_max_depth %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
  })
  
  depths <- reactive({
    paste0(scales::label_comma()(min_depth()), " - ", scales::label_comma()(max_depth()), " m") 
  })
  
  twoValueBoxServer(
    id = "depths",
    left_reactive  = depths,
    right_reactive = depths,
    format_fn = as.character
  )
  
  mean_depth <- reactive({
    hab_dataframes$hab_mean_depth %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
  })
  
  mean_depth_text <- reactive({
    paste0(scales::label_comma()(mean_depth()), " m") 
  })
  
  twoValueBoxServer(
    id = "mean_depth",
    left_reactive  = mean_depth_text,
    right_reactive = mean_depth_text,
    format_fn = as.character
  )
  
  # Leaflet map
  output$map <- renderLeaflet({
    
    base_map(current_zoom = 7) |>
      
      hideGroup("State Marine Parks") |>
      hideGroup("Australian Marine Parks") |>
      
      addPolygons(
        data = regions_joined,
        layerId = ~region,
        label = ~region,
        color = "#444444",
        weight = 1,
        fillOpacity = 0.7,
        fillColor = ~hab_data$pal_factor(regions_joined$overall),
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) |>
      addLegend("bottomright",
                title = "Overall Impact",
                colors = unname(hab_data$pal_vals[hab_data$ordered_levels]),
                labels = c("High", "Medium","Low"),
                opacity = 0.8)
  })
  
  # Click handler
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id)) {
      selected_region(click$id)
    }
  })
  
  # --- Highlight clicked region with white border ---
  observe({
    req(selected_region())
    
    # Grab the currently selected region polygon
    region_selected <- regions_joined |> 
      filter(region == selected_region())
    
    # Update map: remove previous highlight, then draw a new one
    leafletProxy("map") |>
      clearGroup("highlight") |>
      addPolygons(
        data = region_selected,
        color = "white",        # solid white border
        weight = 6,             # thickness of outline
        fillColor = "white",    # same white fill to make it pop
        fillOpacity = 0.2,      # slightly opaque (use 1 for fully opaque)
        opacity = 0.75,            # full border opacity
        group = "highlight"#,
        # options = pathOptions(pane = "highlight")
      )
  })
  
  # Selected region badge
  output$selected_region_badge <- renderUI({
    req(selected_region())
    reg <- selected_region()
    ov <- hab_data$scores |> 
      filter(region == reg) |> 
      pull(overall) |> 
      as.character()
    
    badge_col <- hab_data$pal_vals[[ov %||% "low"]]
    
    tags$div(
      style = sprintf("padding:8px 12px;border-radius:8px;background:%s;color:white;display:inline-block;", badge_col),
      tags$b(reg),
      if (!is.na(ov)) tags$span(sprintf(" — %s", tools::toTitleCase(ov)))
    )
  })
  
  # Selected region title ----
  output$region_title <- renderUI({
    req(selected_region())
    reg <- selected_region()
    
    tags$div(
      tags$h3(paste("Summary:", reg))
    )
  })
  
  # ---- Summary text ----
  output$summary_text <- renderUI({
    req(selected_region())
    reg <- selected_region()
    txt <- hab_data$summaries |>
      filter(region == reg) |>
      pull(summary) #|> 
    #{ if (length(.) == 0) "No summary available for this region yet." else .[1] }
    
    # Use markdown::markdownToHTML or commonmark::markdown_html for rendering
    HTML(markdown::markdownToHTML(text = txt, fragment.only = TRUE))
})
  
  # Pointer plots----
  output$overallplot <- renderPlot({ 
    req(selected_region())
    
    reg <- selected_region()
    
    txt <- hab_data$scores |>
      filter(region == reg) |>
      pull(overall)
    
    overall <- half_donut_with_dial(
      values = c(1, 1, 1),
      mode = "absolute",
      status   = txt
    ) 
    
    overall
  })
  
  output$combinedplot <- renderPlot({ 
    req(selected_region())
    
    reg <- selected_region()
    
    txt <- hab_data$scores |>
      filter(region == reg) |>
      pull(diversity)
    
    diversity <- half_donut_with_dial(
      values = c(1, 1, 1),
      mode = "absolute",
      status   = txt
    )+
      ggtitle("Diversity") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    txt <- hab_data$scores |>
      filter(region == reg) |>
      pull(abundance)
    
    abundance <- half_donut_with_dial(
      values = c(1, 1, 1),
      mode = "absolute",
      status   = txt
    )+
      ggtitle("Abundance") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    txt <- hab_data$scores |>
      filter(region == reg) |>
      pull(habitat)
    
    habitat <- half_donut_with_dial(
      values = c(1, 1, 1),
      mode = "absolute",
      status   = txt
    )+
      ggtitle("Habitat") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    final_plot <- diversity + abundance + habitat + plot_layout(ncol = 3)
    
    final_plot
  })
  
  deployments <- reactive({
    deployments <- hab_dataframes$hab_combined_metadata %>%
      dplyr::filter(region %in% selected_region()) 
    
    # Extract coordinates
    coords <- st_coordinates(deployments)
    
    # Convert coordinates to a data frame or tibble
    coords_df <- as.data.frame(coords)
    
    # Rename columns for clarity (optional)
    colnames(coords_df) <- c("longitude_dd", "latitude_dd")
    
    # Bind the new coordinate columns to the original sf object
    deployments <- bind_cols(deployments, coords_df)
  })
  
  min_lat <- reactive({min(deployments()$latitude_dd, na.rm = TRUE)})
  min_lon <- reactive({min(deployments()$longitude_dd, na.rm = TRUE)})
  max_lat <- reactive({max(deployments()$latitude_dd, na.rm = TRUE)})
  max_lon <- reactive({max(deployments()$longitude_dd, na.rm = TRUE)})
  
  output$surveyeffort <- renderLeaflet({
    method_cols <- c("BRUVs" = "#f89f00", "UVC" = "#0c3978")
    pts <- ensure_sf_ll(hab_dataframes$hab_combined_metadata) %>%
      dplyr::filter(region %in% selected_region())
    
    m <- base_map(current_zoom = 7) %>%
      fitBounds(min_lon(), min_lat(), max_lon(), max_lat())
    
    # add points (no curly block after a pipe)
    if (has_leafgl()) {
      m <- leafgl::addGlPoints(
        m, 
        data = pts, 
        fillColor = method_cols[pts$method], 
        weight = 1, 
        popup = pts$popup, 
        group = "Sampling locations", pane = "points"
      )
    } else {
      m <- addCircleMarkers(
        m, data = pts, radius = 6, fillColor = "#f89f00", fillOpacity = 1,
        weight = 1, color = "black", popup = pts$popup,
        group = "Sampling locations", options = pathOptions(pane = "points")
      )
    }
    
    addLegend(m,
              "topright",
              colors = unname(method_cols),
              labels = names(method_cols),
              title = "Survey method",
              opacity = 1,
              group = "Sampling locations",
              layerId = "methodLegend"
    ) %>%
      hideGroup("Australian Marine Parks")
    })

  }
