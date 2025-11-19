# --------------------------- shared helpers ----------------------------------
base_map <- function(max_zoom = 11, current_zoom = 6) {
  leaflet() |>
    addTiles(options = tileOptions(minZoom = 4, max_zoom)) |>
    setView(lng = 137.618521, lat = -34.25, current_zoom) |>
    addMapPane("polys",  zIndex = 410) |>
    addMapPane("points", zIndex = 420) |>
    # Use regular polygons for static layers:
    addPolygons(
      data = state_mp, 
      color = "black", weight = 1,
      fillColor = ~state.pal(zone), fillOpacity = 0.8,
      group = "State Marine Parks",
      popup = ~name,
      options = pathOptions(pane = "polys")
    ) |>
    addPolygons(
      data = commonwealth.mp,
      color = "black", weight = 1,
      fillColor = ~commonwealth.pal(zone), fillOpacity = 0.8,
      popup = ~ZoneName,
      options = pathOptions(pane = "polys"), group = "Australian Marine Parks"
    ) %>%
    
    # Legends
    addLegend(
      pal = state.pal,
      values = state_mp$zone,
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
    ) #|>
  # addLayersControl(
  #   overlayGroups = c("Australian Marine Parks", "State Marine Parks"#, "Sampling locations"
  #                     ),
  #   options = layersControlOptions(collapsed = FALSE),
  #   position = "topright"
  # )
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

twoValueBoxServer <- function(id,
                              left_reactive,
                              right_reactive,
                              format_fn = scales::label_comma()) {
  
  moduleServer(id, function(input, output, session) {
    
    left_val  <- reactive(left_reactive())
    right_val <- reactive(right_reactive())
    
    output$left_val <- renderUI({
      x <- left_val()
      
      html <- if (length(x) == 0 || is.null(x) || all(is.na(x))) {
        "<span style='color: rgba(194,194,194,0.6); 
                      font-size: 0.85rem; 
                      font-style: italic;'>
           No data available yet
         </span>"
      } else {
        format_fn(x)
      }
      
      HTML(html)
    })
    
    output$right_val <- renderUI({
      x <- right_val()
      
      html <- if (length(x) == 0 || is.null(x) || all(is.na(x))) {
        "<span style='color: rgba(194,194,194,0.6); 
                      font-size: 0.85rem; 
                      font-style: italic;'>
           No data available yet
         </span>"
      } else {
        format_fn(x)
      }
      
      HTML(html)
    })
  })
}




safe_pull <- function(expr) {
  reactive({
    val <- expr()
    if (length(val) == 0 || is.null(val) || all(is.na(val))) {
      NA_real_   # <- return numeric NA, not a string
    } else {
      val
    }
  })
}

# ------------------------------ server ---------------------------------------

server <- function(input, output, session) {
  
  regions_joined <- hab_data$regions_shp |>
    left_join(hab_data$scores, by = "region") %>% 
    glimpse()
  
  # Default selected region (first available)
  selected_region <- reactiveVal({
    (regions_joined$region[!is.na(regions_joined$region)])[8]
  })
  
  # Value boxes ----
  # Number of BRUV Deployments ----
  number_bruv_deployments_pre <- reactive({
    x <- hab_data$hab_number_bruv_deployments %>%
      dplyr::filter(period == "Pre-bloom",
                    region %in% selected_region()) %>%
      dplyr::pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  number_bruv_deployments_post <- reactive({
    x <- hab_data$hab_number_bruv_deployments %>%
      dplyr::filter(period == "Bloom",
                    region %in% selected_region()) %>%
      dplyr::pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  twoValueBoxServer(
    id = "number_bruv_deployments",
    left_reactive  = number_bruv_deployments_pre,
    right_reactive = number_bruv_deployments_post,
    format_fn = scales::label_comma()
  )
  
  # Number of UVC surveys ----
  number_rls_deployments_pre <- safe_pull(function() {
    x <- hab_data$hab_number_rls_deployments %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  number_rls_deployments_post <- safe_pull(function() {
    x <- hab_data$hab_number_rls_deployments %>%
      dplyr::filter(period %in% "Bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  twoValueBoxServer(
    id = "number_rls_deployments",
    left_reactive  = number_rls_deployments_pre,
    right_reactive = number_rls_deployments_post,
    format_fn = scales::label_comma()
  )
  
  # Number of fish counted ----
  fish_counted_pre <- safe_pull(function() {
    x <- hab_data$hab_number_of_fish %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  fish_counted_post <- safe_pull(function() {
    x <- hab_data$hab_number_of_fish %>%
      dplyr::filter(period %in% "Bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  twoValueBoxServer(
    id = "fish_counted",
    left_reactive  = fish_counted_pre,
    right_reactive = fish_counted_post,
    format_fn = scales::label_comma()
  )
  
  # Number of fish species ----
  fish_species_pre <- safe_pull(function() {
    x <- hab_data$hab_number_of_fish_species %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  fish_species_post <- safe_pull(function() {
    x <- hab_data$hab_number_of_fish_species %>%
      dplyr::filter(period %in% "Bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  # TODO probs doesn't make sense to split this for demo
  twoValueBoxServer(
    id = "fish_species",
    left_reactive  = fish_species_pre,
    right_reactive = fish_species_post,
    format_fn = scales::label_comma()
  )
  
  # Number of other species ----
  non_fish_species_pre <- safe_pull(function() {
    x <- hab_data$hab_number_of_nonfish_species %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  non_fish_species_post <- safe_pull(function() {
    x <- hab_data$hab_number_of_nonfish_species %>%
      dplyr::filter(period %in% "Bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  twoValueBoxServer(
    id = "non_fish_species",
    left_reactive  = non_fish_species_pre,
    right_reactive = non_fish_species_post,
    format_fn = scales::label_comma()
  )
  
  # Years surveyed----
  min_year_pre <- reactive({
    hab_data$hab_min_year %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      pull(number)
  })
  
  max_year_pre <- reactive({
    hab_data$hab_max_year %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      pull(number)
  })
  
  years_pre <- reactive({
    paste0(min_year_pre(), " - ", max_year_pre()) 
  })
  
  min_year_post <- reactive({
    hab_data$hab_min_year %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Bloom") %>%
      pull(number)
  })
  
  max_year_post <- reactive({
    hab_data$hab_max_year %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Bloom") %>%
      pull(number)
  })
  
  years_post <- reactive({
    min_year <- min_year_post()
    max_year <- max_year_post()
    
    # If no data, bail out early with NA (character)
    if (length(min_year) == 0 || length(max_year) == 0 ||
        all(is.na(min_year)) || all(is.na(max_year))) {
      return(NA_character_)
    }
    
    # Coerce once
    min_year_num <- as.numeric(min_year)
    max_year_num <- as.numeric(max_year)
    
    # Safety: if still NA after coercion, treat as no data
    if (is.na(min_year_num) || is.na(max_year_num)) {
      return(NA_character_)
    }
    
    if (min_year_num == max_year_num) {
      as.character(min_year_num)
    } else {
      paste0(min_year_num, " - ", max_year_num)
    }
  })
  
  twoValueBoxServer(
    id = "years",
    left_reactive  = years_pre,
    right_reactive = years_post,
    format_fn = as.character
  )
  
  # Depth ranges ----
  min_depth_pre <- reactive({
    hab_data$hab_min_depth %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      pull(number)
  })
  
  max_depth_pre <- reactive({
    hab_data$hab_max_depth %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      pull(number)
  })
  
  depth_pre <- reactive({
    paste0(min_depth_pre(), " - ", max_depth_pre(), " m") 
  })
  
  min_depth_post <- reactive({
    hab_data$hab_min_depth %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Bloom") %>%
      pull(number)
  })
  
  max_depth_post <- reactive({
    hab_data$hab_max_depth %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Bloom") %>%
      pull(number)
  })
  
  depth_post <- reactive({
    min_depth <- min_depth_post()
    max_depth <- max_depth_post()
    
    # If no data, bail out early with NA (character)
    if (length(min_depth) == 0 || length(max_depth) == 0 ||
        all(is.na(min_depth)) || all(is.na(max_depth))) {
      return(NA_character_)
    }
    
    # Coerce once
    min_depth_num <- as.numeric(min_depth)
    max_depth_num <- as.numeric(max_depth)
    
    # Safety: if still NA after coercion, treat as no data
    if (is.na(min_depth_num) || is.na(max_depth_num)) {
      return(NA_character_)
    }
    
    if (min_depth_num == max_depth_num) {
      paste0(as.character(min_depth_num), " m")
    } else {
      paste0(min_depth_num, " - ", max_depth_num, " m")
    }
  })
  
  twoValueBoxServer(
    id = "depths",
    left_reactive  = depth_pre,
    right_reactive = depth_post,
    format_fn = as.character
  )
  # 
  # depths <- reactive({
  #   paste0(scales::label_comma()(min_depth()), " - ", scales::label_comma()(max_depth()), " m") 
  # })
  # Average Depth  ----
  mean_depth_pre <- reactive({
    x <- hab_data$hab_mean_depth %>%
      dplyr::filter(period == "Pre-bloom",
                    region %in% selected_region()) %>%
      pull(number)
    
    if (length(x) == 0) NA_real_ else paste0(scales::label_comma()(x), " m") 
  })
  
  mean_depth_post <- reactive({
    x <- hab_data$hab_mean_depth %>%
      dplyr::filter(period == "Bloom",
                    region %in% selected_region()) %>%
      pull(number)
    
    if (length(x) == 0) NA_real_ else paste0(scales::label_comma()(x), " m") 
  })
  
  twoValueBoxServer(
    id = "mean_depth",
    left_reactive  = mean_depth_pre,
    right_reactive = mean_depth_post,
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
        group = "Impact regions",
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) |>
      addLegend("bottomright",
                title = "Overall Impact",
                colors = unname(hab_data$pal_vals[hab_data$ordered_levels]),
                labels = c("High", "Medium","Low"),
                opacity = 0.8,
                group = "Impact regions") |>
      addLayersControl(
        overlayGroups = c("Australian Marine Parks", "State Marine Parks", "Impact regions"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      )
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
    txt <- hab_data$regions_summaries |>
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
    
    # Species Richness
    
    txt <- hab_data$scores |>
      filter(region == reg) |>
      pull(species_richness)
    
    p1 <- half_donut_with_dial(
      values = c(1, 1, 1),
      mode = "absolute",
      status   = txt
    )+
      ggtitle("Species richness") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    # Shark and Ray Abundance
    
    txt <- hab_data$scores |>
      filter(region == reg) |>
      pull(shark_and_ray_abundance)
    
    p2 <- half_donut_with_dial(
      values = c(1, 1, 1),
      mode = "absolute",
      status   = txt
    )+
      ggtitle("Shark and ray abundance") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    # Site attached/reef associated species abundance
    
    txt <- hab_data$scores |>
      filter(region == reg) |>
      pull(site_attached_and_reef_associated_species_abundance)
    
    p3 <- half_donut_with_dial(
      values = c(1, 1, 1),
      mode = "absolute",
      status   = txt
    )+
      labs(title = str_wrap("Site attached/reef associated species abundance", width = 20)) +
      # ggtitle("Site attached/reef associated species abundance") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    # Fish greater than 200 mm abundance
    
    txt <- hab_data$scores |>
      filter(region == reg) |>
      pull(fish_greater_than_200mm_abundance)
    
    p4 <- half_donut_with_dial(
      values = c(1, 1, 1),
      mode = "absolute",
      status   = txt
    )+
      labs(title = str_wrap("Fish greater than 200 mm abundance", width = 20)) +
      # ggtitle("Fish greater than 200 mm abundance") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    
    
    final_plot <- p1 + p2 + p3 + p4 + plot_layout(ncol = 4)
    
    final_plot
  })
  
  deployments <- reactive({
    deployments <- hab_data$hab_combined_metadata %>%
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
    pts <- ensure_sf_ll(hab_data$hab_combined_metadata) %>%
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
  
  
  
  # ===== EXPLORE INDICATORS & METRICS =====
  
  # Populate region choices (reuse your regions_joined)
  observe({
    req(regions_joined)
    updateSelectizeInput(
      session, "em_region",
      choices = sort(unique(regions_joined$region)),
      selected = selected_region() %||% sort(unique(regions_joined$region))[1],
      server = TRUE
    )
  })
  
  # Build a tabbed card with one tab per metric
  output$em_tabset <- renderUI({
    req(input$em_region)
    
    bslib::navset_card_tab(
      !!!lapply(names(metric_defs), function(id) {
        bslib::nav(
          title = metric_defs[[id]],
          # Primary pre/post boxplot
          
          layout_columns(
            col_widths = c(6, 6),
            withSpinner(
              plotOutput(paste0("em_plot_", id, "_main"), height = 400),
              type = 6
            ),
            # Optional: a second plot per metric (grouped/detail)
            withSpinner(
              plotOutput(paste0("em_plot_", id, "_detail"), height = 400),
              type = 6
            ))
        )
      })
    )
  })
  
  # Renderers for each metric (lazy: only active tab draws)
  lapply(names(metric_defs), function(metric_id) {
    local({
      id <- metric_id
      
      # ---- Plot 1: overall pre/post with jittered points --------------------
      output[[paste0("em_plot_", id, "_main")]] <- renderPlot({
        req(input$em_region)
        df <- dummy_metric_data(id, input$em_region, n = 120)
        
        p <- ggplot(df, aes(x = period, y = value, fill = period)) +
          geom_boxplot(
            width = 0.6,
            outlier.shape = NA,
            alpha = 0.85,
            colour = "black"
          ) +
          geom_jitter(
            aes(colour = period),
            width = 0.15,
            alpha = 0.35,
            size = 1.2
          ) +
          scale_fill_manual(values = metric_period_cols) +
          scale_color_manual(values = metric_period_cols) +
          labs(
            x = NULL,
            y = metric_y_lab[[id]] %||% "Value",
            subtitle = input$em_region
          ) +
          theme_minimal(base_size = 13) +
          theme(
            legend.position   = "bottom",
            plot.subtitle     = element_text(margin = margin(b = 6)),
            panel.grid.minor  = element_blank()
          )
        
        p
        
      }) |>
        bindCache(input$em_region, id) |>
        bindEvent(input$em_region)
      
      # ---- Plot 2: Inside vs Outside, still pre/post on x -------------------
      output[[paste0("em_plot_", id, "_detail")]] <- renderPlot({
        req(input$em_region)
        df <- dummy_metric_data(id, input$em_region, n = 120)
        
        p <- ggplot(df, aes(x = period, y = value, fill = period)) +
          geom_boxplot(
            width = 0.6,
            outlier.shape = NA,
            alpha = 0.85,
            colour = "black"
          ) +
          geom_jitter(
            aes(colour = period),
            width = 0.15,
            alpha = 0.35,
            size = 1.2
          ) +
          scale_fill_manual(values = metric_period_cols) +
          scale_color_manual(values = metric_period_cols) +
          facet_wrap(~ zone) +
          labs(
            x = NULL,
            y = metric_y_lab[[id]] %||% "Value",
            subtitle = paste(input$em_region, "— Inside vs Outside")
          ) +
          theme_minimal(base_size = 13) +
          theme(
            legend.position   = "bottom",
            plot.subtitle     = element_text(margin = margin(b = 6)),
            panel.grid.minor  = element_blank()
          )
        
        if (id == "func_groups") {
          # rows = functional group (Carnivore / Herbivore / Omnivore),
          # cols = Inside / Outside
          p <- p + facet_grid(group ~ zone)
        } else {
          p <- p + facet_wrap(~ zone)
        }
        
        p
        
      }) |>
        bindCache(input$em_region, id) |>
        bindEvent(input$em_region)
    })
  })
  
  
  # 
  # # Renderers for each metric (lazy: only active tab draws)
  # lapply(names(metric_defs), function(metric_id) {
  #   local({
  #     id <- metric_id
  #     
  #     # Main pre/post boxplot
  #     output[[paste0("em_plot_", id, "_main")]] <- renderPlot({
  #       req(input$em_region)
  #       df <- dummy_metric_data(id, input$em_region, n = 120)
  #       
  #       ggplot(df, aes(x = period, y = value)) +
  #         geom_boxplot(width = 0.6, outlier.alpha = 0.25) +
  #         labs(
  #           x = NULL,
  #           y = metric_y_lab[[id]] %||% "Value",
  #           subtitle = input$em_region
  #         ) +
  #         theme_minimal(base_size = 13) +
  #         theme(
  #           plot.subtitle = element_text(margin = margin(b = 6)),
  #           panel.grid.minor = element_blank()
  #         )
  #     }) |>
  #       bindCache(input$em_region, id) |>
  #       bindEvent(input$em_region)
  #     
  #     # Detail plot: grouped boxplot by metric-specific strata
  #     output[[paste0("em_plot_", id, "_detail")]] <- renderPlot({
  #       req(input$em_region)
  #       df <- dummy_metric_data(id, input$em_region, n = 120)
  #       # add a grouping factor appropriate for the metric
  #       groups <- metric_groups(id)
  #       set.seed(1L)
  #       df$group <- factor(sample(groups, size = nrow(df), replace = TRUE), levels = groups)
  #       
  #       ggplot(df, aes(x = group, y = value, fill = period)) +
  #         geom_boxplot(position = position_dodge(width = 0.75), width = 0.65, outlier.alpha = 0.15) +
  #         labs(
  #           x = NULL,
  #           y = metric_y_lab[[id]] %||% "Value",
  #           subtitle = paste(input$em_region, "— by", if (id == "func_groups") "functional group" else "stratum")
  #         ) +
  #         theme_minimal(base_size = 13) +
  #         theme(
  #           legend.position = "top",
  #           plot.subtitle   = element_text(margin = margin(b = 6)),
  #           panel.grid.minor = element_blank()
  #         )
  #     }) |>
  #       bindCache(input$em_region, id) |>
  #       bindEvent(input$em_region)
  #   })
  # })
  # 
  # ---- HAB % change summary table (per region) ------------------------------
  
  output$em_change_table <- renderTable({
    req(input$em_region)
    
    df <- hab_metric_change |>
      dplyr::filter(region == input$em_region) |>
      dplyr::select(
        Metric = metric,
        Inside  = inside_change,
        Outside = outside_change,
        Overall = overall_change
      )
    
    # helper to format % with up/down arrows
    fmt_cell <- function(x) {
      ifelse(
        is.na(x),
        "",
        sprintf("%s %s%%",
                ifelse(x < 0, "&#8595;", "&#8593;"),
                scales::number(abs(x), accuracy = 1))
      )
    }
    
    data.frame(
      Metric  = df$Metric,
      Inside  = fmt_cell(df$Inside),
      Outside = fmt_cell(df$Outside),
      Overall = fmt_cell(df$Overall),
      check.names = FALSE
    )
  },
  sanitize.text.function = function(x) x  # allow HTML arrows
  )
  
  # ---- Common species plots (top 5) ----------------------------------------
  
  # Helper: top 5 by a given period, but plot both periods for context
  make_top10_plot <- function(region_name, focal_period = c("Pre-bloom", "Post-bloom"),
                              title_lab = "Common species") {
    
    focal_period <- match.arg(focal_period)
    
    df <- hab_species_counts |>
      dplyr::filter(region == region_name)
    
    # Identify top 5 species *within the focal period only*
    top_species <- df |>
      dplyr::filter(period == focal_period) |>
      dplyr::slice_max(order_by = count, n = 10, with_ties = FALSE) |>
      dplyr::pull(species)
    
    # Only those species, but showing both pre/post
    plot_df <- df |>
      dplyr::filter(species %in% top_species)
    
    # Reorder species by focal period counts only
    order_df <- plot_df |>
      dplyr::filter(period == focal_period) |>
      dplyr::arrange(count)
    
    plot_df$species <- factor(plot_df$species, levels = order_df$species)
    
    ggplot(plot_df, aes(x = count, y = species, fill = period)) +
      geom_col(position = "dodge") +
      labs(
        x = "Count",
        y = NULL#,
        #title = title_lab
      ) +
      scale_fill_manual(values = c("Pre-bloom" = "#0c3978", "Post-bloom" = "#f89f00")) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position = "bottom"#,
        #plot.title = element_text(hjust = 0.5, face = "bold")
      )
  }
  
  output$em_common_pre <- renderPlot({
    req(input$em_region)
    make_top10_plot(input$em_region,
                    focal_period = "Pre-bloom",
                    title_lab = "10 most common species pre-bloom")
  })
  
  output$em_common_post <- renderPlot({
    req(input$em_region)
    make_top10_plot(input$em_region,
                    focal_period = "Post-bloom",
                    title_lab = "10 most common species post-bloom")
  })
  
  # ---- Survey progress: filtered to selected reporting region --------------
  
  # 1) Filter to selected region
  survey_region <- reactive({
    req(selected_region())
    df <- hab_data$survey_plan %>%
      dplyr::filter(reporting_region == selected_region())
    df[1, ]
  })
  
  # 2) Does this region use BRUVS or ROV?
  has_bruvs <- reactive({
    grepl("BRUVS", survey_region()$methods[[1]], ignore.case = TRUE)
  })
  
  has_rov <- reactive({
    grepl("ROV", survey_region()$methods[[1]], ignore.case = TRUE)
  })
  
  # 3) Planned/completed reactives
  sites_planned <- reactive({ survey_region()$planned_number_sites })
  sites_completed <- reactive({ survey_region()$complete_number_sites })
  
  bruvs_planned <- reactive({
    if (!has_bruvs()) return(0)
    survey_region()$planned_number_drops
  })
  bruvs_completed <- reactive({
    if (!has_bruvs()) return(0)
    survey_region()$complete_number_drops
  })
  
  rov_planned <- reactive({
    if (!has_rov()) return(0)
    survey_region()$planned_number_transects
  })
  rov_completed <- reactive({
    if (!has_rov()) return(0)
    survey_region()$complete_number_transects
  })
  
  # 4) twoValueBoxServer(...) for each
  twoValueBoxServer("sites_progress",  left_reactive = sites_planned,  right_reactive = sites_completed)
  twoValueBoxServer("bruvs_progress",  left_reactive = bruvs_planned, right_reactive = bruvs_completed)
  twoValueBoxServer("rov_progress",    left_reactive = rov_planned,   right_reactive = rov_completed)
  
  # 5) Unified layout for all boxes
  # ---- Single layout for all progress value boxes --------------------------
  output$survey_value_boxes <- renderUI({
    df <- survey_region()
    pct <- df$percent_sites_completed
    vb_col <- completion_theme(pct)
    
    has_rov_val <- has_rov()
    
    # Define the 3 “always” boxes
    sites_box <- twoValueBoxUI(
      id          = "sites_progress",
      title       = "Sites",
      left_label  = "Planned",
      right_label = "Completed",
      icon        = icon("magnifying-glass", class = "fa-xl"),
      theme_color = "secondary"
    )
    
    bruvs_box <- twoValueBoxUI(
      id          = "bruvs_progress",
      title       = "BRUVS deployments",
      left_label  = "Planned",
      right_label = "Completed",
      icon        = icon("ship", class = "fa-xl"),
      theme_color = "secondary"
    )
    
    pct_box <- value_box(
      title       = "Sites completed",
      value       = sprintf("%.1f%%", pct),
      subtitle    = df$methods[[1]],
      theme_color = vb_col,
      showcase    = icon("percent", class = "fa-xl")
    )
    
    if (has_rov_val) {
      # 4 boxes: 3 + ROV (each 1/4 of the row)
      rov_box <- twoValueBoxUI(
        id          = "rov_progress",
        title       = "ROV transects",
        left_label  = "Planned",
        right_label = "Completed",
        icon        = icon("video", class = "fa-xl"),
        theme_color = "secondary"
      )
      
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        sites_box,
        bruvs_box,
        rov_box,
        pct_box
      )
      
    } else {
      # 3 boxes, each 1/3 of the row – no wrapping
      layout_columns(
        col_widths = c(4, 4, 4),
        sites_box,
        bruvs_box,
        pct_box
      )
    }
  })
  
  # ===== EXPLORE A MARINE PARK ==============================================
  
  # Populate marine park choices
  observe({
    req(marine_parks)
    updateSelectizeInput(
      session, "mp_park",
      choices  = marine_parks,
      selected = marine_parks[1],
      server   = TRUE
    )
  })
  
  # --- Tabbed card for marine parks (same metrics as regions) ---------------
  
  output$mp_tabset <- renderUI({
    req(input$mp_park)
    
    bslib::navset_card_tab(
      !!!lapply(names(metric_defs), function(id) {
        bslib::nav(
          title = metric_defs[[id]],
          layout_columns(
            col_widths = c(6, 6),
            withSpinner(
              plotOutput(paste0("mp_plot_", id, "_main"), height = 400),
              type = 6
            ),
            withSpinner(
              plotOutput(paste0("mp_plot_", id, "_detail"), height = 400),
              type = 6
            )
          )
        )
      })
    )
  })
  
  # Renderers for each metric at park level
  lapply(names(metric_defs), function(metric_id) {
    local({
      id <- metric_id
      
      # Plot 1: overall pre/post
      output[[paste0("mp_plot_", id, "_main")]] <- renderPlot({
        req(input$mp_park)
        df <- dummy_metric_data(id, input$mp_park, n = 120)
        
        ggplot(df, aes(x = period, y = value, fill = period)) +
          geom_boxplot(
            width = 0.6,
            outlier.shape = NA,
            alpha = 0.85,
            colour = "black"
          ) +
          geom_jitter(
            aes(colour = period),
            width = 0.15,
            alpha = 0.35,
            size  = 1.2
          ) +
          scale_fill_manual(values = metric_period_cols) +
          scale_color_manual(values = metric_period_cols) +
          labs(
            x = NULL,
            y = metric_y_lab[[id]] %||% "Value",
            subtitle = input$mp_park
          ) +
          theme_minimal(base_size = 13) +
          theme(
            legend.position  = "bottom",
            plot.subtitle    = element_text(margin = margin(b = 6)),
            panel.grid.minor = element_blank()
          )
      }) |>
        bindCache(input$mp_park, id) |>
        bindEvent(input$mp_park)
      
      # Plot 2: Inside vs Outside (func_groups gets group x zone)
      output[[paste0("mp_plot_", id, "_detail")]] <- renderPlot({
        req(input$mp_park)
        df <- dummy_metric_data(id, input$mp_park, n = 120)
        
        p <- ggplot(df, aes(x = period, y = value, fill = period)) +
          geom_boxplot(
            width = 0.6,
            outlier.shape = NA,
            alpha = 0.85,
            colour = "black"
          ) +
          geom_jitter(
            aes(colour = period),
            width = 0.15,
            alpha = 0.35,
            size  = 1.2
          ) +
          scale_fill_manual(values = metric_period_cols) +
          scale_color_manual(values = metric_period_cols) +
          labs(
            x = NULL,
            y = metric_y_lab[[id]] %||% "Value",
            subtitle = paste(input$mp_park, "— Inside vs Outside")
          ) +
          theme_minimal(base_size = 13) +
          theme(
            legend.position  = "bottom",
            plot.subtitle    = element_text(margin = margin(b = 6)),
            panel.grid.minor = element_blank()
          )
        
        if (id == "func_groups") {
          p + facet_grid(group ~ zone)
        } else {
          p + facet_wrap(~ zone)
        }
      }) |>
        bindCache(input$mp_park, id) |>
        bindEvent(input$mp_park)
    })
  })
  
  output$mp_change_table <- renderTable({
    req(input$mp_park)
    
    df <- mp_metric_change |>
      dplyr::filter(park == input$mp_park) |>
      dplyr::select(
        Metric = metric,
        Inside  = inside_change,
        Outside = outside_change,
        Overall = overall_change
      )
    
    fmt_cell <- function(x) {
      ifelse(
        is.na(x),
        "",
        sprintf(
          "%s %s%%",
          ifelse(x < 0, "&#8595;", "&#8593;"),
          scales::number(abs(x), accuracy = 1)
        )
      )
    }
    
    data.frame(
      Metric  = df$Metric,
      Inside  = fmt_cell(df$Inside),
      Outside = fmt_cell(df$Outside),
      Overall = fmt_cell(df$Overall),
      check.names = FALSE
    )
  },
  sanitize.text.function = function(x) x
  )
  
  make_top10_plot_mp <- function(park_name,
                                 focal_period = c("Pre-bloom", "Post-bloom")) {
    
    focal_period <- match.arg(focal_period)
    
    df <- mp_species_counts |>
      dplyr::filter(park == park_name)
    
    top_species <- df |>
      dplyr::filter(period == focal_period) |>
      dplyr::slice_max(order_by = count, n = 10, with_ties = FALSE) |>
      dplyr::pull(species)
    
    plot_df <- df |>
      dplyr::filter(species %in% top_species)
    
    order_df <- plot_df |>
      dplyr::filter(period == focal_period) |>
      dplyr::arrange(count)
    
    plot_df$species <- factor(plot_df$species, levels = order_df$species)
    
    ggplot(plot_df, aes(x = count, y = species, fill = period)) +
      geom_col(position = "dodge") +
      labs(x = "Count", y = NULL) +
      scale_fill_manual(values = c("Pre-bloom" = "#0c3978",
                                   "Post-bloom" = "#f89f00")) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position = "bottom"
      )
  }
  
  output$mp_common_pre <- renderPlot({
    req(input$mp_park)
    make_top10_plot_mp(input$mp_park, focal_period = "Pre-bloom")
  })
  
  output$mp_common_post <- renderPlot({
    req(input$mp_park)
    make_top10_plot_mp(input$mp_park, focal_period = "Post-bloom")
  })
  
  # ---- Survey progress for marine parks ------------------------------------
  
  mp_survey_row <- reactive({
    req(input$mp_park)
    df <- mp_survey_plan |>
      dplyr::filter(park == input$mp_park)
    df[1, ]
  })
  
  mp_has_bruvs <- reactive({
    grepl("BRUVS", mp_survey_row()$methods[[1]], ignore.case = TRUE)
  })
  
  mp_has_rov <- reactive({
    grepl("ROV", mp_survey_row()$methods[[1]], ignore.case = TRUE)
  })
  
  mp_sites_planned   <- reactive(mp_survey_row()$planned_number_sites)
  mp_sites_completed <- reactive(mp_survey_row()$complete_number_sites)
  
  mp_bruvs_planned <- reactive({
    if (!mp_has_bruvs()) return(0)
    mp_survey_row()$planned_number_drops
  })
  mp_bruvs_completed <- reactive({
    if (!mp_has_bruvs()) return(0)
    mp_survey_row()$complete_number_drops
  })
  
  mp_rov_planned <- reactive({
    if (!mp_has_rov()) return(0)
    mp_survey_row()$planned_number_transects
  })
  mp_rov_completed <- reactive({
    if (!mp_has_rov()) return(0)
    mp_survey_row()$complete_number_transects
  })
  
  twoValueBoxServer("mp_sites_progress",
                    left_reactive  = mp_sites_planned,
                    right_reactive = mp_sites_completed)
  twoValueBoxServer("mp_bruvs_progress",
                    left_reactive  = mp_bruvs_planned,
                    right_reactive = mp_bruvs_completed)
  twoValueBoxServer("mp_rov_progress",
                    left_reactive  = mp_rov_planned,
                    right_reactive = mp_rov_completed)
  
  output$mp_survey_value_boxes <- renderUI({
    df    <- mp_survey_row()
    pct   <- df$percent_sites_completed
    vb_col <- completion_theme(pct)
    
    has_rov_val <- mp_has_rov()
    
    sites_box <- twoValueBoxUI(
      id          = "mp_sites_progress",
      title       = "Sites",
      left_label  = "Planned",
      right_label = "Completed",
      icon        = icon("magnifying-glass", class = "fa-xl"),
      theme_color = "secondary"
    )
    
    bruvs_box <- twoValueBoxUI(
      id          = "mp_bruvs_progress",
      title       = "BRUVS deployments",
      left_label  = "Planned",
      right_label = "Completed",
      icon        = icon("ship", class = "fa-xl"),
      theme_color = "secondary"
    )
    
    pct_box <- value_box(
      title       = "Sites completed",
      value       = sprintf("%.1f%%", pct),
      subtitle    = df$methods[[1]],
      theme_color = vb_col,
      showcase    = icon("percent", class = "fa-xl")
    )
    
    if (has_rov_val) {
      rov_box <- twoValueBoxUI(
        id          = "mp_rov_progress",
        title       = "ROV transects",
        left_label  = "Planned",
        right_label = "Completed",
        icon        = icon("video", class = "fa-xl"),
        theme_color = "secondary"
      )
      
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        sites_box,
        bruvs_box,
        rov_box,
        pct_box
      )
    } else {
      layout_columns(
        col_widths = c(4, 4, 4),
        sites_box,
        bruvs_box,
        pct_box
      )
    }
  })
  
  
  }