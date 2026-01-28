# --------------------------- shared helpers ----------------------------------
base_map <- function(max_zoom = 20, current_zoom = 6) {
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
           Surveys incomplete
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
           Surveys incomplete
         </span>"
      } else {
        format_fn(x)
      }
      
      HTML(html)
    })
  })
}

no_data_plot <- function(title = NULL) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "Data not available",
             size = 5, fontface = "italic", colour = "black") +
    theme_void() +
    labs(title = title) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}

get_metric_plot <- function(metric_id, title_lab, wrap_width = 22, chosen_region) {
  
  txt <- hab_data$impact_data |>
    dplyr::filter(region == chosen_region, impact_metric == metric_id) |>
    dplyr::pull(impact)
  
  # ---- If no data, return the “no data” plot ----
  if (txt == "Surveys incomplete") {
    return(no_data_plot(stringr::str_wrap(title_lab, wrap_width)))
  }
  
  # ---- Otherwise return the gauge ----
  half_donut_with_dial(
    values = c(1, 1, 1),
    mode   = "absolute",
    status = txt
  ) +
    labs(title = stringr::str_wrap(title_lab, width = wrap_width)) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.margin = margin(2, 2, 2, 2)
    )
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

make_overall_impact_gauge <- function(region_name) {
  
  message(region_name)
  
  # ---- Overall impact ----
  overall_status <- hab_data$overall_impact |>
    dplyr::filter(region == region_name) |>
    dplyr::pull(overall_impact) %>%
    dplyr::glimpse()
  
  p0 <- if (identical(overall_status, "Surveys incomplete") ||
            length(overall_status) == 0 ||
            is.na(overall_status)) {
    
    no_data_plot("Overall impact")
    
  } else {
  p0 <-   half_donut_with_dial(
      values = c(1, 1, 1),
      mode   = "absolute",
      status = overall_status
    ) +
      ggtitle("Overall impact") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold.italic", size = 16),
        plot.margin = margin(2, 2, 2, 2)
      )
  }
  
  # Final layout
  (p0)
}

make_impact_gauges <- function(region_name) {
  
  # ---- Overall impact ----
  overall_status <- hab_data$overall_impact |>
    dplyr::filter(region == region_name) |>
    dplyr::pull(overall_impact)
  
  p0 <- if (identical(overall_status, "Surveys incomplete") ||
            length(overall_status) == 0 ||
            is.na(overall_status)) {
    
    no_data_plot("Overall impact")
    
  } else {
    half_donut_with_dial(
      values = c(1, 1, 1),
      mode   = "absolute",
      status = overall_status
    ) +
      ggtitle("Overall impact") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold.italic", size = 16),
        plot.margin = margin(2, 2, 2, 2)
      )
  }
  
  # ---- Individual indicator plots ----
  p1 <- get_metric_plot("species_richness",         "Species richness",                 chosen_region = region_name)
  p2 <- get_metric_plot("total_abundance",          "Total abundance",                  chosen_region = region_name)
  p3 <- get_metric_plot("shark_ray_richness",       "Shark and ray richness",           chosen_region = region_name)
  p4 <- get_metric_plot("reef_associated_richness", "Reef associated species richness", chosen_region = region_name)
  p5 <- get_metric_plot("fish_200_abundance",       "Fish > 200 mm abundance",          chosen_region = region_name)
  p6 <- get_metric_plot("thamnaconus_degeni",       "Bluefin leatherjacket displacement*",          chosen_region = region_name)
  
  # Final layout
  (p1 | p2 | p3) /
    (p4 | p5 | p6)
}

# ---- output id helpers -------------------------------------------------------
metric_plot_id <- function(prefix, metric_id, which) {
  paste0(prefix, "_plot_", metric_id, "_", which)
}

metric_plotOutput <- function(prefix, metric_id, which, height = 600, spinner_type = 6) {
  withSpinner(
    plotOutput(metric_plot_id(prefix, metric_id, which), height = height),
    type = spinner_type
  )
}

metric_tab_body_ui <- function(metric_id, prefix = "em") {
  
  data_id <- metric_data_key(metric_id)
  
  switch(
    metric_id,
    
    richness = {
      tagList(
        layout_columns(
          col_widths = c(6, 6),
          metric_plotOutput(prefix, data_id, "main"),
          metric_plotOutput(prefix, data_id, "detail")
        ),
        layout_columns(
          col_widths = c(6, 6),
          metric_plotOutput(prefix, data_id, "main_status"),
          metric_plotOutput(prefix, data_id, "detail_status")
        )
      )
    },
    
    total_abundance = {
      tagList(
        layout_columns(
          col_widths = c(6, 6),
          metric_plotOutput(prefix, data_id, "main"),
          metric_plotOutput(prefix, data_id, "detail")
        ),
        layout_columns(
          col_widths = c(6, 6),
          metric_plotOutput(prefix, data_id, "main_status"),
          metric_plotOutput(prefix, data_id, "detail_status")
        )
      )
    },
    
    sharks_rays = {
      tagList(
        layout_columns(
          col_widths = c(6, 6),
          metric_plotOutput(prefix, data_id, "main"),
          metric_plotOutput(prefix, data_id, "detail")
        ),
        layout_columns(
          col_widths = c(6, 6),
          metric_plotOutput(prefix, data_id, "main_status"),
          metric_plotOutput(prefix, data_id, "detail_status")
        )
      )
    },
    
    reef_associated_richness = {
      tagList(
        layout_columns(
          col_widths = c(6, 6),
          metric_plotOutput(prefix, data_id, "main"),
          metric_plotOutput(prefix, data_id, "detail")
        ),
        layout_columns(
          col_widths = c(6, 6),
          metric_plotOutput(prefix, data_id, "main_status"),
          metric_plotOutput(prefix, data_id, "detail_status")
        )
      )
    },
    
    large_fish = {
      tagList(
        layout_columns(
          col_widths = c(6, 6),
          metric_plotOutput(prefix, data_id, "main"),
          metric_plotOutput(prefix, data_id, "detail")
        ),
        layout_columns(
          col_widths = c(6, 6),
          metric_plotOutput(prefix, data_id, "main_status"),
          metric_plotOutput(prefix, data_id, "detail_status")
        )
      )
    },
    
    trophic = {
      tagList(
        layout_columns(
          col_widths = c(6, 6),
          metric_plotOutput(prefix, "trophic", "main"),
          metric_plotOutput(prefix, "trophic", "stack")
        ),
        layout_columns(
          col_widths = c(6, 6),
          metric_plotOutput(prefix, "trophic", "main_status"),
          metric_plotOutput(prefix, "trophic", "stack_status")
        )
      )
    },
    
    # default: 2 plots
    {
      layout_columns(
        col_widths = c(6, 6),
        metric_plotOutput(prefix, data_id, "main"),
        metric_plotOutput(prefix, data_id, "detail")
      )
    }
  )
}






plot_cell <- function(id, width = "120px", height = "120px") {
  div(
    style = sprintf("width:%s; height:%s;", width, height),
    plotOutput(id, width = width, height = height)
  )
}

# ------------------------------ server ---------------------------------------

server <- function(input, output, session) {
  
  regions_joined <- hab_data$regions_shp |>
    left_join(hab_data$regions_summaries, by = "region") %>% 
    left_join(hab_data$overall_impact)
  
  # Default selected region (first available)
  # selected_region <- reactiveVal({
  #   (regions_joined$region[!is.na(regions_joined$region)])[8]
  # })
  
  selected_region <- reactiveVal((regions_joined$region[!is.na(regions_joined$region)])[8])
  
  # whenever the dropdown changes, update selected_region
  observeEvent(input$region, {
    req(input$region)
    selected_region(input$region)
  }, ignoreInit = TRUE)
  
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
  
  output$map <- renderLeaflet({
    
    method_cols <- c("BRUVs" = "#004DA7", "UVC" = "#C600FF")
    pts <- ensure_sf_ll(hab_data$hab_combined_metadata)
    
    m <- base_map(current_zoom = 7) |>
      # define panes with explicit stacking
      addMapPane("points",    zIndex = 411) |>
      addMapPane("regions",   zIndex = 412) |>
      addMapPane("highlight", zIndex = 415) %>%
      
      leafgl::addGlPoints(
        data = pts,
        fillColor = method_cols[pts$method],
        weight = 1,
        popup = pts$popup,
        group = "Sampling locations",
        pane  = "points"
      ) %>%
      
      # polygons ABOVE points
      addPolygons(
        data = regions_joined,
        layerId = ~region,
        label   = ~region,
        color = ~hab_data$pal_factor(regions_joined$overall_impact),#"#444444",
        weight = 5,
        opacity = 1,
        fillOpacity = 0, #0.7
        fillColor = ~hab_data$pal_factor(regions_joined$overall_impact),
        group = "Impact regions",
        options = pathOptions(pane = "highlight"),
        highlightOptions = highlightOptions(
          color = "white",
          weight = 6,
          bringToFront = TRUE
        )
      ) |>
      
      addLegend(
        "bottomright",
        title  = "Overall Impact",
        colors = c(unname(hab_data$pal_vals[hab_data$ordered_levels]), "grey"),
        labels = c("High", "Medium","Low", "Surveys incomplete"),
        opacity = 0.8,
        group   = "Impact regions"
      ) |>
      
      addLayersControl(
        overlayGroups = c("Australian Marine Parks", "State Marine Parks", "Impact regions", "Sampling locations"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      ) %>%
      
      hideGroup("Australian Marine Parks") |>
      
      addLegend(
        "topright",
        colors = unname(method_cols),
        labels = names(method_cols),
        title = "Survey method",
        opacity = 1,
        group = "Sampling locations",
        layerId = "methodLegend"
      ) 
    
    
    m
  })
  
  
  # # Click handler
  # observeEvent(input$map_shape_click, {
  #   click <- input$map_shape_click
  #   if (!is.null(click$id)) {
  #     selected_region(click$id)
  #   }
  # })
  
  # observe({
  #   req(selected_region())
  #   
  #   region_selected <- regions_joined |>
  #     dplyr::filter(region == selected_region())
  #   
  #   leafletProxy("map") |>
  #     clearGroup("highlight") |>
  #     addPolygons(
  #       data = region_selected,
  #       color = "white",
  #       weight = 6,
  #       fillColor = "white",
  #       fillOpacity = 0.2,
  #       opacity = 0.75,
  #       group = "highlight",
  #       options = pathOptions(pane = "highlight")
  #     )
  # })
  
  # # Selected region badge
  # output$selected_region_badge <- renderUI({
  #   req(selected_region())
  #   reg <- selected_region()
  #   ov <- hab_data$regions_summaries |> 
  #     filter(region == reg) |> 
  #     pull(overall) |> 
  #     as.character()
  #   
  #   badge_col <- hab_data$pal_vals[[ov %||% "low"]]
  #   
  #   tags$div(
  #     style = sprintf("padding:8px 12px;border-radius:8px;background:%s;color:white;display:inline-block;", badge_col),
  #     tags$b(reg),
  #     if (!is.na(ov)) tags$span(sprintf(" — %s", tools::toTitleCase(ov)))
  #   )
  # })
  
  # # Selected region title ----
  # output$region_title <- renderUI({
  #   req(selected_region())
  #   reg <- selected_region()
  #   
  #   tags$div(
  #     tags$h3(paste("Algal bloom impacts on nearshore marine biodiversity monitoring progress:", reg))
  #   )
  # })
  
  # ---- Summary text ----
  output$region_summary_text <- renderUI({
    req(input$region)
    
    reg <- input$region
    
    txt <- hab_data$regions_summaries |>
      dplyr::filter(region == reg) |>
      dplyr::pull(summary) %>%
      dplyr::glimpse()
    
    HTML(markdown::markdownToHTML(text = txt, fragment.only = TRUE))
})
  
  indicator_table <- tibble::tibble(
    Threshold = c(
      "Low = ≥80% of the pre-bloom value",
      "Medium = 50–80% of the pre-bloom value",
      "High = 0–50% of the pre-bloom value"
    ),
    Example = list(
      plot_cell("example_low"),
      plot_cell("example_medium"),
      plot_cell("example_high")
    )
  )
  
  output$pointer_table <- renderUI({
    tags$table(
      class = "table table-striped table-sm",
      tags$thead(
        tags$tr(
          tags$th("Threshold"),
          tags$th("Example Plot")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("Low = ≥80% of the pre-bloom value"),
          tags$td(plotOutput("example_low",  height = 80, width = 120))
        ),
        tags$tr(
          tags$td("Medium = 50–80% of the pre-bloom value"),
          tags$td(plotOutput("example_medium", height = 80, width = 120))
        ),
        tags$tr(
          tags$td("High = 0–50% of the pre-bloom value"),
          tags$td(plotOutput("example_high", height = 80, width = 120))
        )
      )
    )
  })
  
  output$example_low <- renderPlot(bg = "transparent", {
    half_donut_with_dial(values = c(1,1,1), mode = "absolute", status = "Low") +
      theme(
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA)
      )
  })
  
  output$example_medium <- renderPlot(bg = "transparent", {
    half_donut_with_dial(values = c(1,1,1), mode = "absolute", status = "Medium") +
      theme(
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA)
      )
  })
  
  output$example_high <- renderPlot(bg = "transparent", {
    half_donut_with_dial(values = c(1,1,1), mode = "absolute", status = "High") +
      theme(
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA)
      )
  })
  
  # Indiactor table
  output$indicator_table <- renderUI({

    # # text for the single big cell
    # threshold_html <- HTML(paste(
    #   "Low = ≥80% of the pre-bloom value",
    #   "Medium = 50–80% of the pre-bloom value",
    #   "High = 0–50% of the pre-bloom value",
    #   sep = "<br>"
    # ))

    tags$table(
      class = "table table-striped table-sm",  # uses bootstrap styling
      # header
      tags$thead(
        tags$tr(
          tags$th("Indicator"),
          tags$th("Description")#,
          # tags$th("Impact thresholds")
        )
      ),
      # body
      tags$tbody(
        # first row: also contains the big thresholds cell
        tags$tr(
          tags$td(indicator_tbl$Indicator[1]),
          tags$td(indicator_tbl$Description[1])#,
          # tags$td(
          #   rowspan = nrow(indicator_tbl),    # merge down all rows
          #   style   = "vertical-align:top; white-space:normal;",
          #   threshold_html
          # )
        ),
        # remaining rows: just Indicator + Description
        lapply(2:nrow(indicator_tbl), function(i) {
          tags$tr(
            tags$td(indicator_tbl$Indicator[i]),
            tags$td(indicator_tbl$Description[i])
          )
        })
      )
    )
  })
  
  observeEvent(input$open_info_table, {
    showModal(
      modalDialog(
        title = "Metric definitions",
        tableOutput("indicator_table"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  observeEvent(input$open_info_pointers, {
    showModal(
      modalDialog(
        title = "Impact definitions",
        tableOutput("pointer_table"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  # Pointer plots----
  # Pointer plots: overall + 5 indicators in one figure ------------------------
  output$impact_gauges <- renderPlot({
    req(input$region)
    make_impact_gauges(input$region)
  })
  
  
  output$overall_impact_gauge <- renderPlot({
    req(input$region)
    make_overall_impact_gauge(input$region)
  })
  
  
  output$region_impact_gauges <- renderPlot({
    req(input$region)
    make_impact_gauges(input$region)
  })
  
  deployments <- reactive({
    deployments <- hab_data$hab_combined_metadata %>%
      dplyr::filter(region %in% input$region) 
    
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
  
  output$region_survey_effort <- renderLeaflet({
    method_cols <- c("BRUVs" = "#004DA7"
                     #, "UVC" = "#C600FF"
                     )
    
    pts <- ensure_sf_ll(hab_data$hab_combined_metadata) %>%
      dplyr::filter(region %in% input$region)
    
    shp <- regions_joined %>%
      dplyr::filter(region %in% input$region)
    
    m <- base_map(current_zoom = 7) %>%
      fitBounds(min_lon(), min_lat(), max_lon(), max_lat()) %>%
      
      # polygons for reporting region
      addPolygons(
        data = shp,
        layerId = ~region,
        label   = ~region,
        # color = ~hab_data$pal_factor(regions_joined$overall_impact),#"#444444",
        weight = 5,
        opacity = 1,
        fillOpacity = 0#, #0.7
        # fillColor = ~hab_data$pal_factor(regions_joined$overall_impact),
        # group = "Impact regions",
        # options = pathOptions(pane = "highlight"),
        # highlightOptions = highlightOptions(
        #   color = "white",
        #   weight = 6,
        #   bringToFront = TRUE
        # )
      )
    
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
      session, "region",
      choices = sort(unique(regions_joined$region)),
      selected = selected_region() %||% sort(unique(regions_joined$region))[1],
      server = TRUE
    )
  })
  
  # Build a tabbed card with one tab per metric
  output$region_tabset <- renderUI({
    req(input$region)
    
    bslib::navset_card_tab(
      !!!lapply(names(metric_defs), function(id) {
        bslib::nav(
          title = metric_defs[[id]],
          metric_tab_body_ui(id, prefix = "em")
        )
      })
    )
  })
  
  # helper if you still like dummy_metric_data()
  get_metric_data <- function(metric_id, region, n = 120) {
    dummy_metric_data(metric_id, region, n = n)
  }
  
  # ---------- RICHNESS: main boxplot --------------------
  output$em_plot_richness_main <- renderPlot({
    req(input$region)
    df <- hab_data$species_richness_samples %>%
      dplyr::filter(region == input$region)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    mean_se <- hab_data$species_richness_summary %>%
      dplyr::filter(region == input$region)
    
    ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
      # boxplot (median + IQR + whiskers)
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      # raw points
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      # mean ± SE
      geom_pointrange(
        data = mean_se,
        aes(
          x    = period,
          y    = mean,
          ymin = mean - se,
          ymax = mean + se
        ),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["richness"]],
        subtitle = input$region
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  # ---------- RICHNESS: detail plot ---------------------
  output$em_plot_richness_detail <- renderPlot({
    req(input$region)
    
    df <- hab_data$species_richness_summary %>%
      dplyr::filter(region == input$region)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = mean, fill = period)) +
      # mean bar
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      # # mean ± SE
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["richness"]],
        subtitle = paste(input$region, ": Average species richness per sample")
      ) +
      # facet_wrap(~ zone) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",        # both bars already coloured by period
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  # ---------- RICHNESS: main boxplot by Status --------------------
  output$em_plot_richness_main_status <- renderPlot({
    req(input$region)
    
    df <- hab_data$species_richness_samples %>%
      dplyr::filter(region == input$region)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      
      # ⬇️ Add this
      geom_point(
        stat = "summary",
        fun = "mean",
        shape = 21,
        size = 3,
        fill = "white",
        colour = "black"
      ) +
      
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["richness"]],
        subtitle = paste(input$region, "— Species richness per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  # ---------- RICHNESS: detail barplot by Status ------------------
  output$em_plot_richness_detail_status <- renderPlot({
    req(input$region)
    
    df <- hab_data$species_richness_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(n_species_sample, na.rm = TRUE),
        se   = sd(n_species_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(n_species_sample))),
        .groups = "drop"
      )
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = mean, fill = period)) +
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["richness"]],
        subtitle = paste(input$region, "— Average species richness per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  
  # ---------- TOTAL ABUNDANCE: two plots ------------
  output$em_plot_total_abundance_main <- renderPlot({
    req(input$region)
    
    # Filter for this region
    df <- hab_data$total_abundance_samples %>%
      dplyr::filter(region == input$region)
    
    mean_se <- hab_data$total_abundance_summary %>%
      dplyr::filter(region == input$region)
    
    # Order periods
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = total_abundance_sample, fill = period)) +
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      geom_pointrange(
        data = mean_se,
        aes(x = period, y = mean,
            ymin = mean - se, ymax = mean + se),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["total_abundance"]],
        subtitle = input$region
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  output$em_plot_total_abundance_detail <- renderPlot({
    req(input$region)
    
    df <- hab_data$total_abundance_summary %>%
      dplyr::filter(region == input$region)
    
    # Order periods
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df,
           aes(x = period, y = mean, fill = period)) +
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["total_abundance"]],
        subtitle = paste(input$region, "— Average total abundance per sample")
      ) +
      # facet_wrap(~ zone) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  output$em_plot_total_abundance_main_status <- renderPlot({
    req(input$region)
    
    df <- hab_data$total_abundance_samples %>%
      dplyr::filter(region == input$region)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = total_abundance_sample, fill = period)) +
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      
      # ⬇️ Add this
      geom_point(
        stat = "summary",
        fun = "mean",
        shape = 21,
        size = 3,
        fill = "white",
        colour = "black"
      ) +
      
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["total_abundance"]],
        subtitle = paste(input$region, "— Total abundance per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  output$em_plot_total_abundance_detail_status <- renderPlot({
    req(input$region)
    
    df <- hab_data$total_abundance_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(total_abundance_sample, na.rm = TRUE),
        se   = sd(total_abundance_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(total_abundance_sample))),
        .groups = "drop"
      )
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df,
           aes(x = period, y = mean, fill = period)) +
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["total_abundance"]],
        subtitle = paste(input$region,
                         "— Average total abundance per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  
  # …and so on for trophic, sharks_rays, etc.
  # Shark and Rays -----
  output$em_plot_shark_ray_richness_main <- renderPlot({
    req(input$region)
    df <- hab_data$shark_ray_richness_samples %>%
      dplyr::filter(region == input$region)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    mean_se <- hab_data$shark_ray_richness_summary %>%
      dplyr::filter(region == input$region)
    
    ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
      # boxplot (median + IQR + whiskers)
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      # raw points
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      # mean ± SE
      geom_pointrange(
        data = mean_se,
        aes(
          x    = period,
          y    = mean,
          ymin = mean - se,
          ymax = mean + se
        ),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["sharks_rays"]],
        subtitle = input$region
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  # ---------- shark_ray: detail plot ---------------------
  output$em_plot_shark_ray_richness_detail <- renderPlot({
    req(input$region)
    
    df <- hab_data$shark_ray_richness_summary %>%
      dplyr::filter(region == input$region)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = mean, fill = period)) +
      # mean bar
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      # # mean ± SE
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["sharks_rays"]],
        subtitle = paste(input$region, ": Average shark and ray species richness per sample")
      ) +
      # facet_wrap(~ zone) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",        # both bars already coloured by period
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  # Reef associated richness -----
  output$em_plot_reef_associated_richness_main <- renderPlot({
    req(input$region)
    
    df <- hab_data$reef_associated_richness_samples %>%
      dplyr::filter(region == input$region)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    mean_se <- hab_data$reef_associated_richness_summary %>%
      dplyr::filter(region == input$region)
    
    ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
      # boxplot (median + IQR + whiskers)
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      # raw points
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      # mean ± SE
      geom_pointrange(
        data = mean_se,
        aes(
          x    = period,
          y    = mean,
          ymin = mean - se,
          ymax = mean + se
        ),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["reef_associated_richness"]],
        subtitle = input$region
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  # ---------- shark and ray: detail plot ---------------------
  output$em_plot_reef_associated_richness_detail <- renderPlot({
    req(input$region)
    
    df <- hab_data$reef_associated_richness_summary %>%
      dplyr::filter(region == input$region)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = mean, fill = period)) +
      # mean bar
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      # # mean ± SE
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["reef_associated_richness"]],
        subtitle = paste(input$region, ": Average reef associated species richness per sample")
      ) +
      # facet_wrap(~ zone) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",        # both bars already coloured by period
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  # ---------- REEF-ASSOCIATED: main boxplot by Status ---------------
  output$em_plot_reef_associated_richness_main_status <- renderPlot({
    req(input$region)
    
    df <- hab_data$reef_associated_richness_samples %>%
      dplyr::filter(region == input$region)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      
      # ⬇️ Add this
      geom_point(
        stat = "summary",
        fun = "mean",
        shape = 21,
        size = 3,
        fill = "white",
        colour = "black"
      ) +
      
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["reef_associated_richness"]],
        subtitle = paste(input$region, "— Reef-associated species richness per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  # ---------- REEF-ASSOCIATED: detail barplot by Status -------------
  output$em_plot_reef_associated_richness_detail_status <- renderPlot({
    req(input$region)
    
    df <- hab_data$reef_associated_richness_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(n_species_sample, na.rm = TRUE),
        se   = sd(n_species_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(n_species_sample))),
        .groups = "drop"
      )
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = mean, fill = period)) +
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["reef_associated_richness"]],
        subtitle = paste(input$region, "— Average reef-associated species richness per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  
  # ---------- SHARK & RAY: main boxplot by Status --------------------
  output$em_plot_shark_ray_richness_main_status <- renderPlot({
    req(input$region)
    
    df <- hab_data$shark_ray_richness_samples %>%
      dplyr::filter(region == input$region)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      
      # ⬇️ Add this
      geom_point(
        stat = "summary",
        fun = "mean",
        shape = 21,
        size = 3,
        fill = "white",
        colour = "black"
      ) +
      
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["sharks_rays"]],
        subtitle = paste(input$region, "— Shark & ray species richness per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  # ---------- SHARK & RAY: detail barplot by Status -----------------
  output$em_plot_shark_ray_richness_detail_status <- renderPlot({
    req(input$region)
    
    df <- hab_data$shark_ray_richness_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(n_species_sample, na.rm = TRUE),
        se   = sd(n_species_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(n_species_sample))),
        .groups = "drop"
      )
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = mean, fill = period)) +
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["sharks_rays"]],
        subtitle = paste(input$region, "— Average shark & ray species richness per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  
  # ---------- Large fish: two plots ------------
  output$em_plot_large_fish_main <- renderPlot({
    req(input$region)

    # Filter for this region
    df <- hab_data$fish_200_abundance_samples %>%
      dplyr::filter(region == input$region)

    mean_se <- hab_data$fish_200_abundance_summary %>%
      dplyr::filter(region == input$region)

    # Order periods
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))

    ggplot(df, aes(x = period, y = total_abundance_sample, fill = period)) +
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      geom_pointrange(
        data = mean_se,
        aes(x = period, y = mean,
            ymin = mean - se, ymax = mean + se),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["large_fish"]],
        subtitle = input$region
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)

  output$em_plot_large_fish_detail <- renderPlot({
    req(input$region)

    df <- hab_data$fish_200_abundance_summary %>%
      dplyr::filter(region == input$region)

    # Order periods
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))

    ggplot(df,
           aes(x = period, y = mean, fill = period)) +
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["large_fish"]],
        subtitle = paste(input$region, "— Average total abundance per sample")
      ) +
      # facet_wrap(~ zone) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  # ---------- LARGE FISH: main boxplot by Status --------------------
  output$em_plot_large_fish_main_status <- renderPlot({
    req(input$region)
    
    df <- hab_data$fish_200_abundance_samples %>%
      dplyr::filter(region == input$region)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = total_abundance_sample, fill = period)) +
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      
      # ⬇️ Add this
      geom_point(
        stat = "summary",
        fun = "mean",
        shape = 21,
        size = 3,
        fill = "white",
        colour = "black"
      ) +
      
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["large_fish"]],
        subtitle = paste(input$region, "— Large fish (>200 mm) abundance per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  # ---------- LARGE FISH: detail barplot by Status ------------------
  output$em_plot_large_fish_detail_status <- renderPlot({
    req(input$region)
    
    df <- hab_data$fish_200_abundance_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(total_abundance_sample, na.rm = TRUE),
        se   = sd(total_abundance_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(total_abundance_sample))),
        .groups = "drop"
      )
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df,
           aes(x = period, y = mean, fill = period)) +
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["large_fish"]],
        subtitle = paste(input$region, "— Average large fish (>200 mm) abundance per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  # ---------- Trophic Groups: two plots ------------
  output$em_plot_trophic_main <- renderPlot({
    req(input$region)
    
    # Filter for this region
    df <- hab_data$trophic_groups_samples %>%
      dplyr::filter(region == input$region)
    
    mean_se <- hab_data$trophic_groups_summary %>%
      dplyr::filter(region == input$region)
    
    # Order periods
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    mean_se$period <- factor(mean_se$period, levels = c("Pre-bloom", "Bloom"))
    
    # (Optional) order diet groups if you want a specific order
    diet_levels <- c("Carnivore", "Herbivore", "Omnivore", "Planktivore", "Diet missing")
    df$diet <- factor(df$diet, levels = diet_levels)
    mean_se$diet <- factor(mean_se$diet, levels = diet_levels)
    
    dodge <- position_dodge(width = 0.75)
    
    ggplot(df, aes(x = diet, y = n_individuals_sample, fill = period)) +
      geom_boxplot(
        position = dodge,
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      geom_jitter(
        aes(colour = period),
        position = position_jitterdodge(
          jitter.width  = 0.15,
          jitter.height = 0,
          dodge.width   = 0.75
        ),
        alpha = 0.35,
        size = 1.2
      ) +
      geom_pointrange(
        data = mean_se,
        aes(
          x    = diet,
          y    = mean,
          ymin = mean - se,
          ymax = mean + se,
          group = period,
          colour = period
        ),
        position = dodge,
        inherit.aes = FALSE,
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,  # or "Diet group"
        y = metric_y_lab[["large_fish"]],
        subtitle = input$region
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "top",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  # ---------- Trophic Groups: stacked composition plot ------------
 
  output$em_plot_trophic_stack <- renderPlot({
    req(input$region#, input$trophic_stack_scale
        )
    
    diet_levels <- names(diet_cols)
    
    # Start from the SUMMARY table (means per sample)
    mean_se <- hab_data$trophic_groups_richness_summary %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(
        period = factor(period, levels = c("Pre-bloom", "Bloom")),
        diet   = factor(diet,   levels = diet_levels)
      )
    
      # -------- COUNT VIEW (mean-based) --------
      ggplot(mean_se, aes(x = period, y = mean, fill = diet)) +
        geom_col(position = "stack") +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_manual(values = diet_cols, drop = FALSE) +
        labs(
          x        = NULL,
          y        = "Mean no. species per sample",
          fill     = "Diet group",
          subtitle = input$region
        ) +
        theme_minimal(base_size = 16) +
        theme(panel.grid.minor = element_blank())
    # }
  }) |>
    bindCache(input$region, input$trophic_stack_scale) |>
    bindEvent(input$region, input$trophic_stack_scale)
  
  output$em_plot_trophic_main_status <- renderPlot({
    req(input$region)
    
    # Filter for this region
    df <- hab_data$trophic_groups_samples %>%
      dplyr::filter(region == input$region)
    
    mean_se <- hab_data$trophic_groups_summary %>%
      dplyr::filter(region == input$region)
    
    # Order periods
    df$period     <- factor(df$period,     levels = c("Pre-bloom", "Bloom"))
    mean_se$period <- factor(mean_se$period, levels = c("Pre-bloom", "Bloom"))
    
    # Diet ordering
    diet_levels <- c("Carnivore", "Herbivore", "Omnivore", "Planktivore", "Diet missing")
    df$diet     <- factor(df$diet,     levels = diet_levels)
    mean_se$diet <- factor(mean_se$diet, levels = diet_levels)
    
    dodge <- position_dodge(width = 0.75)
    
    ggplot(df, aes(x = diet, y = n_individuals_sample, fill = period)) +
      geom_boxplot(
        position = dodge,
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      geom_jitter(
        aes(colour = period),
        position = position_jitterdodge(
          jitter.width  = 0.15,
          jitter.height = 0,
          dodge.width   = 0.75
        ),
        alpha = 0.35,
        size = 1.2
      ) +
      geom_pointrange(
        data = mean_se,
        aes(
          x    = diet,
          y    = mean,
          ymin = mean - se,
          ymax = mean + se,
          group = period,
          colour = period
        ),
        inherit.aes = FALSE,
        position = dodge,
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      scale_colour_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["large_fish"]],
        subtitle = input$region
      ) +
      facet_wrap(~ status) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$region) |>
    bindEvent(input$region)
  
  output$em_plot_trophic_stack_status <- renderPlot({
    req(input$region#, input$trophic_stack_scale
    )
    
    diet_levels <- names(diet_cols)
    
    # Start from the SUMMARY table (means per sample)
    mean_se <- hab_data$trophic_groups_richness_summary_status %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(
        period = factor(period, levels = c("Pre-bloom", "Bloom")),
        diet   = factor(diet,   levels = diet_levels)
      )
    
    # -------- COUNT VIEW (mean-based) --------
    ggplot(mean_se, aes(x = period, y = mean, fill = diet)) +
      geom_col(position = "stack") +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = diet_cols, drop = FALSE) +
      labs(
        x        = NULL,
        y        = "Mean no. species per sample",
        fill     = "Diet group",
        subtitle = input$region
      ) +
      facet_wrap(~ status) +
      theme_minimal(base_size = 16) +
      theme(panel.grid.minor = element_blank())
    # }
  }) |>
    bindCache(input$region, input$trophic_stack_scale) |>
    bindEvent(input$region, input$trophic_stack_scale)
  
  
  # ---- HAB % change summary table (per region) ------------------------------
  output$region_change_table <- renderUI({
    req(input$region)
    
    df <- hab_metric_change |>
      dplyr::filter(region == input$region) |>
      dplyr::select(
        Metric = impact_metric,
        Change = percentage_change
      )
    
    vals <- df$Change
    
    # Detect “Surveys incomplete”
    is_incomplete <- grepl("Surveys incomplete", vals, ignore.case = TRUE)
    
    # Parse numeric
    num <- suppressWarnings(as.numeric(vals))
    has_num <- !is.na(num) & !is_incomplete
    
    # Arrows
    arrows <- ifelse(num < 0, "&#8595;", "&#8593;")
    
    # # Colour rules
    # colours <- ifelse(
    #   num <= -50, "#EB5757",
    #   ifelse(num <= -20, "#F2C94C", "#3B7EA1")
    # )
    # Colour rules (default + special-case one metric)
    special_metric <- "Bluefin leatherjacket displacement*"
    
    colours <- ifelse(
      df$Metric == special_metric,
      # Special rule (based on magnitude, regardless of sign)
      ifelse(abs(num) < 120, "#3B7EA1",
             ifelse(abs(num) <= 150, "#F2C94C", "#EB5757")),
      # Default rule (your existing thresholds; uses signed num)
      ifelse(num <= -50, "#EB5757",
             ifelse(num <= -20, "#F2C94C", "#3B7EA1"))
    )
    
    # Build formatted column
    out <- rep("", length(vals))
    out[has_num] <- sprintf(
      "<span style='color:%s'>%s %s%%</span>",
      colours[has_num],
      arrows[has_num],
      scales::number(abs(num[has_num]), accuracy = 1)
    )
    out[is_incomplete] <- "<em>Surveys incomplete</em>"
    
    # ---- Build striped table manually ----
    tags$table(
      class = "table table-striped table-sm",
      tags$thead(
        tags$tr(
          tags$th("Metric"),
          tags$th("Change")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            tags$td(df$Metric[i]),
            tags$td(HTML(out[i]))
          )
        })
      )
    )
  })
  
  make_top10_plot <- function(region_name, 
                              focal_period = c("Pre-bloom", "Bloom"),
                              title_lab = "Common species",
                              number_species,
                              split_status = FALSE,
                              facet_status = FALSE) {
    
    focal_period <- match.arg(focal_period)
    split_status <- isTRUE(split_status)
    facet_status <- isTRUE(facet_status)
    
    # Base colours (your existing theme)
    period_cols <- c(
      "Pre-bloom" = "#072759",
      "Bloom"     = "#e88e98"
    )
    
    # ---- Data prep ----
    df_raw <- hab_data$region_top_species_average |>
      dplyr::filter(region == region_name)
    
    # Tidy spaces in status if present
    if ("status" %in% names(df_raw)) {
      df_raw$status <- trimws(df_raw$status)
    }
    
    # For choosing top species, collapse over status
    df_for_top <- df_raw |>
      dplyr::group_by(region, period, display_name) |>
      dplyr::summarise(
        average = mean(average, na.rm = TRUE),
        se      = sqrt(sum(se^2, na.rm = TRUE)),  # rough pooled SE
        .groups = "drop"
      )
    
    # Top N species within the focal period
    top_species <- df_for_top |>
      dplyr::filter(period == focal_period) |>
      dplyr::slice_max(order_by = average,
                       n = number_species,
                       with_ties = FALSE) |>
      dplyr::pull(display_name)
    
    # Data for plotting: either split by status or averaged across status
    if (split_status) {
      plot_df <- df_raw |>
        dplyr::filter(display_name %in% top_species)
    } else {
      plot_df <- df_for_top |>
        dplyr::filter(display_name %in% top_species)
    }
    
    # Extract sci/common and build markdown label
    plot_df <- plot_df |>
      tidyr::extract(
        display_name,
        into   = c("sci", "common"),
        regex  = "^(.*?)\\s*\\((.*?)\\)$",
        remove = FALSE
      ) |>
      dplyr::mutate(
        label = paste0("*", sci, "*<br>(", common, ")")
      )
    
    # Period order: focal period first
    # plot_df$period <- factor(
    #   plot_df$period,
    #   levels = c(focal_period, setdiff(c("Pre-bloom", "Bloom"), focal_period))
    # )
    # Period order: ALWAYS Pre-bloom then Bloom
    plot_df$period <- factor(plot_df$period, levels = c("Pre-bloom", "Bloom"))
    
    # Species order: smallest at bottom, biggest at top for focal period
    species_order <- plot_df |>
      dplyr::filter(period == focal_period) |>
      dplyr::arrange(average) |>
      dplyr::pull(label) |>
      unique()
    
    plot_df$label <- factor(plot_df$label, levels = species_order)
    
    # Arrange rows so dodging is stable
    plot_df <- plot_df |>
      dplyr::arrange(label, period, dplyr::across(dplyr::any_of("status")))
    
    dodge <- position_dodge(width = 0.8)
    
    # ============================
    #  A) split_status & no facet
    # ============================
    if (split_status && !facet_status) {
      
      # Build combined period:status variable
      plot_df <- plot_df |>
        dplyr::mutate(
          period_status = interaction(period, status, sep = ": ", drop = TRUE)
        )
      
      plot_df$period_status <- droplevels(plot_df$period_status)
      
      # Build palette dynamically from the actual levels
      status_alpha <- 0.45
      ps_levels    <- levels(plot_df$period_status)
      
      combo_cols_vec <- sapply(ps_levels, function(ps) {
        # split "Pre-bloom: Fished" into c("Pre-bloom", "Fished")
        parts <- strsplit(ps, ": ", fixed = TRUE)[[1]]
        per   <- parts[1]
        stat  <- ifelse(length(parts) > 1, parts[2], NA_character_)
        base_col <- unname(period_cols[per])
        if (!is.na(stat) && stat == "Fished") {
          scales::alpha(base_col, status_alpha)
        } else {
          base_col
        }
      })
      
      combo_cols <- setNames(combo_cols_vec, ps_levels)
      
      p <- ggplot(
        plot_df,
        aes(
          x    = average,
          y    = label,
          fill = period_status
        )
      ) +
        geom_col(position = dodge) +
        geom_errorbarh(
          aes(
            xmin = average - se,
            xmax = average + se
          ),
          position = dodge,
          height   = 0.3
        ) +
        labs(
          x     = "Average abundance per BRUV",
          y     = NULL,
          title = title_lab,
          fill  = NULL
        ) +
        scale_fill_manual(values = combo_cols)
      
    } else {
      # =======================================
      #  B) non-split OR split + facet
      # =======================================
      
      if (split_status && facet_status) {
        # Build period_status for colour mapping
        plot_df <- plot_df |>
          dplyr::mutate(
            period_status = interaction(period, status, sep = ": ", drop = TRUE)
          )
        
        plot_df$period_status <- droplevels(plot_df$period_status)
        
        status_alpha <- 0.45
        ps_levels    <- levels(plot_df$period_status)
        
        combo_cols_vec <- sapply(ps_levels, function(ps) {
          parts <- strsplit(ps, ": ", fixed = TRUE)[[1]]
          per   <- parts[1]
          stat  <- ifelse(length(parts) > 1, parts[2], NA_character_)
          base_col <- unname(period_cols[per])
          if (!is.na(stat) && stat == "Fished") {
            scales::alpha(base_col, status_alpha)
          } else {
            base_col
          }
        })
        
        combo_cols <- setNames(combo_cols_vec, ps_levels)
        
        p <- ggplot(
          plot_df,
          aes(
            x    = average,
            y    = label,
            fill = period_status
          )
        ) +
          geom_col(position = dodge) +
          geom_errorbarh(
            aes(
              xmin = average - se,
              xmax = average + se
            ),
            position = dodge,
            height   = 0.3
          ) +
          facet_wrap(~ status, nrow = 1) +
          labs(
            x     = "Average abundance per BRUV",
            y     = NULL,
            title = title_lab,
            fill  = NULL
          ) +
          scale_fill_manual(values = combo_cols)
        
      } else {
        # NOT split, NOT facet → original 2-colour period-only plot
        
        p <- ggplot(
          plot_df,
          aes(
            x    = average,
            y    = label,
            fill = period
          )
        ) +
          geom_col(position = dodge) +
          geom_errorbarh(
            aes(
              xmin = average - se,
              xmax = average + se
            ),
            position = dodge,
            height   = 0.3
          ) +
          labs(
            x     = "Average abundance per BRUV",
            y     = NULL,
            title = title_lab,
            fill  = NULL
          ) +
          scale_fill_manual(values = period_cols)
      }
    }
    
    # Shared scales / theme
    p +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic() +
      theme(
        legend.position = "bottom",
        axis.text.y     = ggtext::element_markdown(size = 12)
      )
  }
  
  

  output$region_common_pre <- renderPlot({
    req(input$region)
    make_top10_plot(
      region_name    = input$region,
      focal_period   = "Pre-bloom",
      title_lab      = "Most common species pre-bloom",
      number_species = input$region_number_species,
      split_status   = input$region_species_status,
      facet_status   = input$region_species_facet
    )
  })
  
  output$region_common_post <- renderPlot({
    req(input$region)
    make_top10_plot(
      region_name    = input$region,
      focal_period   = "Bloom",
      title_lab      = "Most common species post-bloom",
      number_species = input$region_number_species,
      split_status   = input$region_species_status,
      facet_status   = input$region_species_facet
    )
  })
  
  
  # ---- Survey progress: filtered to selected reporting region --------------
  
  # 1) Filter to selected region
  survey_region <- reactive({
    req(selected_region())
    df <- hab_data$survey_plan %>%
      dplyr::filter(reporting_region == selected_region())
    df[1, ]
  })
  
  twoValueBoxServer(
    "sites_progress",
    left_reactive  = reactive({ sites_planned }),
    right_reactive = reactive({ sites_completed })
  )
  
  twoValueBoxServer(
    "bruvs_progress",
    left_reactive  = reactive({ bruvs_planned }),
    right_reactive = reactive({ bruvs_completed })
  )
  
  twoValueBoxServer(
    "uvc_progress",
    left_reactive  = reactive({ uvc_planned }),
    right_reactive = reactive({ uvc_completed })
  )
  
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
            height = 0,      # <— prevents any vertical jitter
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
            height = 0,      # <— prevents any vertical jitter
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
  
  # make_top10_plot_location <- function(location,
  #                                focal_period = c("Pre-bloom", "Bloom"),
  #                                number_species) {
  #   
  #   focal_period <- match.arg(focal_period)
  #   
  #   df <- mp_species_counts |>
  #     dplyr::filter(park == park_name)
  #   
  #   top_species <- df |>
  #     dplyr::filter(period == focal_period) |>
  #     dplyr::slice_max(order_by = average, n = number_species, with_ties = FALSE) |>
  #     dplyr::pull(species)
  #   
  #   plot_df <- df |>
  #     dplyr::filter(display_name %in% top_species)
  #   
  #   order_df <- plot_df |>
  #     dplyr::filter(period == focal_period) |>
  #     dplyr::arrange(average)
  #   
  #   plot_df$display_name <- factor(plot_df$display_name, levels = order_df$display_name)
  #   
  #   ggplot(plot_df, aes(x = average, y = display_name), fill = period) +
  #     scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  #     geom_col(position = "dodge") +
  #     labs(x = "Count", y = NULL) +
  #     scale_fill_manual(values = c("Pre-bloom" = "#0c3978",
  #                                  "Bloom" = "#f89f00")) +
  #     theme_minimal(base_size = 16) +
  #     theme(
  #       legend.position = "bottom"
  #     )
  # }
  # 
  # output$location_common_pre <- renderPlot({
  #   req(input$location)
  #   make_top10_plot_location(input$mp_park, focal_period = "Pre-bloom", number_species = input$locationnumberspecies)
  # })
  # 
  # output$location_common_post <- renderPlot({
  #   req(input$location)
  #   make_top10_plot_location(input$mp_park, focal_period = "Bloom", number_species = input$locationnumberspecies)
  # })
  # 
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
  
  # 1. Summarise years by region ----
  years_by_region <- reactive({
    hab_data$year_dat |>
      dplyr::filter(method %in% "BRUVs") %>%
      dplyr::distinct(region, year) |>
      dplyr::group_by(region) |>
      dplyr::summarise(
        n_years       = dplyr::n(),
        years_sampled = paste(sort(unique(year)), collapse = ", "),
        .groups       = "drop"
      ) |>
      dplyr::ungroup() %>%
      dplyr::filter(region %in% input$region) #%>%
    #glimpse()
  })
  
  # 2. Nicely formatted text for the selected region ----
  output$years_for_region <- renderText({
    req(input$region)
    
    yrs <- years_by_region() |>
      dplyr::filter(region == input$region) |>
      dplyr::pull(years_sampled)
    
    yrs
  })
  
  
  
  # ===== LOCATION SUMMARY (mirrors Region Summary) =============================
  
  # (A) Build a location list (optionally filtered by region)
  locations_all <- reactive({
    
    hab_data$hab_combined_metadata |>
      dplyr::filter(!reporting_name %in% "NA") %>%
      dplyr::pull(reporting_name) |>
      unique() |>
      sort()
  })
  
  # (B) Populate location choices
  observe({
    # If you want location list *dependent* on selected region:
    req(input$region)
    locs <- locations_all()
    updateSelectizeInput(
      session, "location",
      choices  = locs,
      selected = locs[1] %||% NULL,
      server   = TRUE
    )
  })
  
  # (C) Summary text for location (needs a location summaries table)
  # If you don't have hab_data$locations_summaries yet, see note below.
  output$location_summary_text <- renderUI({
    req(input$location)
    
    txt <- hab_data$locations_summaries |>
      dplyr::filter(reporting_name == input$location) |>
      dplyr::pull(summary)
    
    HTML(markdown::markdownToHTML(text = txt, fragment.only = TRUE))
  })
  
  # (D) Years sampled for location (BRUVs only, same as your region pattern)
  years_by_location <- reactive({
    hab_data$year_dat |>
      dplyr::filter(method %in% "BRUVs") |>
      dplyr::distinct(reporting_name, year) |>
      dplyr::group_by(reporting_name) |>
      dplyr::summarise(
        n_years       = dplyr::n(),
        years_sampled = paste(sort(unique(year)), collapse = ", "),
        .groups       = "drop"
      )
  })
  
  output$years_for_location <- renderText({
    req(input$location)
    
    years_by_location() |>
      dplyr::filter(reporting_name == input$location) |>
      dplyr::pull(years_sampled)
  })
  
  # Map of deployments
  location_deployments <- reactive({
    req(input$location)
    
    deployments <- hab_data$hab_combined_metadata %>%
      dplyr::filter(reporting_name %in% input$location)
    
    coords <- sf::st_coordinates(deployments)
    coords_df <- as.data.frame(coords)
    colnames(coords_df) <- c("longitude_dd", "latitude_dd")
    
    dplyr::bind_cols(deployments, coords_df)
  })
  
  loc_min_lat <- reactive({ min(location_deployments()$latitude_dd,  na.rm = TRUE) })
  loc_min_lon <- reactive({ min(location_deployments()$longitude_dd, na.rm = TRUE) })
  loc_max_lat <- reactive({ max(location_deployments()$latitude_dd,  na.rm = TRUE) })
  loc_max_lon <- reactive({ max(location_deployments()$longitude_dd, na.rm = TRUE) })
  
  # Add location survey map
  output$location_survey_effort <- renderLeaflet({
    req(input$location)
    
    method_cols <- c("BRUVs" = "#004DA7", "UVC" = "#C600FF")
    
    pts <- ensure_sf_ll(hab_data$hab_combined_metadata) %>%
      dplyr::filter(reporting_name %in% input$location)
    
    m <- base_map(current_zoom = 7) %>%
      fitBounds(loc_min_lon(), loc_min_lat(), loc_max_lon(), loc_max_lat())
    
    if (has_leafgl()) {
      m <- leafgl::addGlPoints(
        m,
        data      = pts,
        fillColor = method_cols[pts$method],
        weight    = 1,
        popup     = pts$popup,
        group     = "Sampling locations",
        pane      = "points"
      )
    } else {
      m <- leaflet::addCircleMarkers(
        m, data = pts,
        radius = 6, fillColor = "#f89f00", fillOpacity = 1,
        weight = 1, color = "black", popup = pts$popup,
        group = "Sampling locations",
        options = leaflet::pathOptions(pane = "points")
      )
    }
    
    leaflet::addLegend(
      m,
      "topright",
      colors  = unname(method_cols),
      labels  = names(method_cols),
      title   = "Survey method",
      opacity = 1,
      group   = "Sampling locations",
      layerId = "methodLegend"
    ) %>%
      leaflet::hideGroup("Australian Marine Parks")
  })
  
  get_metric_plot_location <- function(metric_id, title_lab, wrap_width = 22, chosen_location) {
    
    txt <- hab_data$impact_data_location |>
      dplyr::filter(reporting_name == chosen_location, impact_metric == metric_id) |>
      dplyr::pull(impact)
    
    if (length(txt) == 0 || is.na(txt) || txt == "Surveys incomplete") {
      return(no_data_plot(stringr::str_wrap(title_lab, wrap_width)))
    }
    
    half_donut_with_dial(values = c(1, 1, 1), mode = "absolute", status = txt) +
      labs(title = stringr::str_wrap(title_lab, width = wrap_width)) +
      theme(
        plot.title  = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.margin = margin(2, 2, 2, 2)
      )
  }
  
  make_impact_gauges_location <- function(location_name) {
    
    overall_status <- hab_data$overall_impact_location |>
      dplyr::filter(reporting_name == location_name) |>
      dplyr::pull(overall_impact)
    
    # p0 <- if (length(overall_status) == 0 || is.na(overall_status) ||
    #           identical(overall_status, "Surveys incomplete")) {
    #   no_data_plot("Overall impact")
    # } else {
    #   half_donut_with_dial(values = c(1, 1, 1), mode = "absolute", status = overall_status) +
    #     ggtitle("Overall impact") +
    #     theme(
    #       plot.title  = element_text(hjust = 0.5, face = "bold.italic", size = 16),
    #       plot.margin = margin(2, 2, 2, 2)
    #     )
    # }
    # 
    p1 <- get_metric_plot_location("species_richness",         "Species richness",                 chosen_location = location_name)
    p2 <- get_metric_plot_location("total_abundance",          "Total abundance",                  chosen_location = location_name)
    p3 <- get_metric_plot_location("shark_ray_richness",       "Shark and ray richness",           chosen_location = location_name)
    p4 <- get_metric_plot_location("reef_associated_richness", "Reef associated species richness", chosen_location = location_name)
    p5 <- get_metric_plot_location("fish_200_abundance",       "Fish > 200 mm abundance",          chosen_location = location_name)
    p6 <- get_metric_plot_location("degeni_impacts",       "Bluefin leatherjacket displacement*",          chosen_location = location_name)
    
    (p1 | p2 | p3) / (p4 | p5 | p6)
  }
  
  output$location_impact_gauges <- renderPlot({
    req(input$location)
    make_impact_gauges_location(input$location)
  })
  
  output$location_change_table <- renderUI({
    req(input$location)
    
    df <- hab_metric_change_location |>
      dplyr::filter(reporting_name == input$location) |>
      dplyr::select(
        Metric = impact_metric,
        'Change Overall'= percentage_change,
        'Change Fished' = change_fished,
        'Change No-take' = change_no_take
      )
    
    no_status_locations <- c("Boston Bay", "Glenelg")
    show_status <- !input$location %in% no_status_locations
    
    fmt_change <- function(vals) {
      vals_chr <- as.character(vals)
      
      is_incomplete <- grepl("Surveys incomplete", vals_chr, ignore.case = TRUE) | is.na(vals_chr)
      
      num <- suppressWarnings(as.numeric(vals_chr))
      has_num <- !is.na(num) & !is_incomplete
      
      arrows  <- ifelse(num < 0, "&#8595;", "&#8593;")
      colours <- ifelse(num <= -50, "#EB5757",
                        ifelse(num <= -20, "#F2C94C", "#3B7EA1"))
      
      out <- rep("", length(vals_chr))
      out[has_num] <- sprintf(
        "<span style='color:%s'>%s %s%%</span>",
        colours[has_num],
        arrows[has_num],
        scales::number(abs(num[has_num]), accuracy = 1)
      )
      out[is_incomplete] <- "<em>Surveys incomplete</em>"
      out
    }
    
    out_overall <- fmt_change(df$`Change Overall`)
    out_fished  <- fmt_change(df$`Change Fished`)
    out_notake  <- fmt_change(df$`Change No-take`)
    
    tags$table(
      class = "table table-striped table-sm",
      
      tags$thead(
        tags$tr(
          tags$th("Metric"),
          tags$th("Change Overall"),
          if (show_status) tags$th("Change Fished"),
          if (show_status) tags$th("Change No-take")
        )
      ),
      
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            tags$td(df$Metric[i]),
            tags$td(HTML(out_overall[i])),
            if (show_status) tags$td(HTML(out_fished[i])),
            if (show_status) tags$td(HTML(out_notake[i]))
          )
        })
      )
    )
    
  })
  
  make_top10_plot_location <- function(location_name,
                                       focal_period = c("Pre-bloom", "Bloom"),
                                       title_lab = "Common species",
                                       number_species,
                                       split_status = FALSE,
                                       facet_status = FALSE) {
    
    focal_period <- match.arg(focal_period)
    split_status <- isTRUE(split_status)
    facet_status <- isTRUE(facet_status)
    
    # Base colours (your existing theme)
    period_cols <- c(
      "Pre-bloom" = "#072759",
      "Bloom"     = "#e88e98"
    )
    
    # Same structure as your region table, just filtered by location
    df_raw <- hab_data$location_top_species_average |>
      dplyr::filter(reporting_name == location_name)
    
    # Tidy spaces in status if present
    if ("status" %in% names(df_raw)) {
      df_raw$status <- trimws(df_raw$status)
    }
    
    # For choosing top species, collapse over status
    df_for_top <- df_raw |>
      dplyr::group_by(reporting_name, period, display_name) |>
      dplyr::summarise(
        average = mean(average, na.rm = TRUE),
        se      = sqrt(sum(se^2, na.rm = TRUE)),  # rough pooled SE
        .groups = "drop"
      )
    
    # Top N species within the focal period
    top_species <- df_for_top |>
      dplyr::filter(period == focal_period) |>
      dplyr::slice_max(order_by = average,
                       n = number_species,
                       with_ties = FALSE) |>
      dplyr::pull(display_name)
    
    # Data for plotting: either split by status or averaged across status
    if (split_status) {
      plot_df <- df_raw |>
        dplyr::filter(display_name %in% top_species)
    } else {
      plot_df <- df_for_top |>
        dplyr::filter(display_name %in% top_species)
    }
    
    # Extract sci/common and build markdown label
    plot_df <- plot_df |>
      tidyr::extract(
        display_name,
        into   = c("sci", "common"),
        regex  = "^(.*?)\\s*\\((.*?)\\)$",
        remove = FALSE
      ) |>
      dplyr::mutate(
        label = paste0("*", sci, "*<br>(", common, ")")
      )
    
    # # Period order: focal period first
    # plot_df$period <- factor(
    #   plot_df$period,
    #   levels = c(focal_period, setdiff(c("Pre-bloom", "Bloom"), focal_period))
    # )
    
    # Period order: ALWAYS Pre-bloom then Bloom
    plot_df$period <- factor(plot_df$period, levels = c("Pre-bloom", "Bloom"))
    
    # Species order: smallest at bottom, biggest at top for focal period
    species_order <- plot_df |>
      dplyr::filter(period == focal_period) |>
      dplyr::arrange(average) |>
      dplyr::pull(label) |>
      unique()
    
    plot_df$label <- factor(plot_df$label, levels = species_order)
    
    # Arrange rows so dodging is stable
    plot_df <- plot_df |>
      dplyr::arrange(label, period, dplyr::across(dplyr::any_of("status")))
    
    dodge <- position_dodge(width = 0.8)
    
    # ============================
    #  A) split_status & no facet
    # ============================
    if (split_status && !facet_status) {
      
      # Build combined period:status variable
      plot_df <- plot_df |>
        dplyr::mutate(
          period_status = interaction(period, status, sep = ": ", drop = TRUE)
        )
      
      plot_df$period_status <- droplevels(plot_df$period_status)
      
      # Build palette dynamically from the actual levels
      status_alpha <- 0.45
      ps_levels    <- levels(plot_df$period_status)
      
      combo_cols_vec <- sapply(ps_levels, function(ps) {
        # split "Pre-bloom: Fished" into c("Pre-bloom", "Fished")
        parts <- strsplit(ps, ": ", fixed = TRUE)[[1]]
        per   <- parts[1]
        stat  <- ifelse(length(parts) > 1, parts[2], NA_character_)
        base_col <- unname(period_cols[per])
        if (!is.na(stat) && stat == "Fished") {
          scales::alpha(base_col, status_alpha)
        } else {
          base_col
        }
      })
      
      combo_cols <- setNames(combo_cols_vec, ps_levels)
      
      p <- ggplot(
        plot_df,
        aes(
          x    = average,
          y    = label,
          fill = period_status
        )
      ) +
        geom_col(position = dodge) +
        geom_errorbarh(
          aes(
            xmin = average - se,
            xmax = average + se
          ),
          position = dodge,
          height   = 0.3
        ) +
        labs(
          x     = "Average abundance per BRUV",
          y     = NULL,
          title = title_lab,
          fill  = NULL
        ) +
        scale_fill_manual(values = combo_cols)
      
    } else {
      # =======================================
      #  B) non-split OR split + facet
      # =======================================
      
      if (split_status && facet_status) {
        # Build period_status for colour mapping
        plot_df <- plot_df |>
          dplyr::mutate(
            period_status = interaction(period, status, sep = ": ", drop = TRUE)
          )
        
        plot_df$period_status <- droplevels(plot_df$period_status)
        
        status_alpha <- 0.45
        ps_levels    <- levels(plot_df$period_status)
        
        combo_cols_vec <- sapply(ps_levels, function(ps) {
          parts <- strsplit(ps, ": ", fixed = TRUE)[[1]]
          per   <- parts[1]
          stat  <- ifelse(length(parts) > 1, parts[2], NA_character_)
          base_col <- unname(period_cols[per])
          if (!is.na(stat) && stat == "Fished") {
            scales::alpha(base_col, status_alpha)
          } else {
            base_col
          }
        })
        
        combo_cols <- setNames(combo_cols_vec, ps_levels)
        
        p <- ggplot(
          plot_df,
          aes(
            x    = average,
            y    = label,
            fill = period_status
          )
        ) +
          geom_col(position = dodge) +
          geom_errorbarh(
            aes(
              xmin = average - se,
              xmax = average + se
            ),
            position = dodge,
            height   = 0.3
          ) +
          facet_wrap(~ status, nrow = 1) +
          labs(
            x     = "Average abundance per BRUV",
            y     = NULL,
            title = title_lab,
            fill  = NULL
          ) +
          scale_fill_manual(values = combo_cols)
        
      } else {
        # NOT split, NOT facet → original 2-colour period-only plot
        
        p <- ggplot(
          plot_df,
          aes(
            x    = average,
            y    = label,
            fill = period
          )
        ) +
          geom_col(position = dodge) +
          geom_errorbarh(
            aes(
              xmin = average - se,
              xmax = average + se
            ),
            position = dodge,
            height   = 0.3
          ) +
          labs(
            x     = "Average abundance per BRUV",
            y     = NULL,
            title = title_lab,
            fill  = NULL
          ) +
          scale_fill_manual(values = period_cols)
      }
    }
    
    # Shared scales / theme
    p +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic() +
      theme(
        legend.position = "bottom",
        axis.text.y     = ggtext::element_markdown(size = 12)
      )
  }
  
  output$location_common_pre <- renderPlot({
    req(input$location)
    make_top10_plot_location(
      location_name  = input$location,
      focal_period   = "Pre-bloom",
      title_lab      = "Most common species pre-bloom",
      number_species = input$location_number_species,
      split_status   = input$location_species_status,
      facet_status   = input$location_species_facet
    )
  })
  
  output$location_common_post <- renderPlot({
    req(input$location)
    make_top10_plot_location(
      location_name  = input$location,
      focal_period   = "Bloom",
      title_lab      = "Most common species post-bloom",
      number_species = input$location_number_species,
      split_status   = input$location_species_status,
      facet_status   = input$location_species_facet
    )
  })
  
  
  output$location_tabset <- renderUI({
    req(input$location)
    
    bslib::navset_card_tab(
      !!!lapply(names(metric_defs), function(id) {
        bslib::nav(
          title = metric_defs[[id]],
          metric_tab_body_ui(id, prefix = "loc")
        )
      })
    )
  })
  
  output$loc_plot_richness_main <- renderPlot({
    req(input$location)
    
    df <- hab_data$species_richness_samples %>% 
      dplyr::filter(reporting_name == input$location)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    mean_se <- hab_data$species_richness_summary_location %>% 
      dplyr::filter(reporting_name == input$location)
    
    ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
      geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.85, colour = "black") +
      geom_jitter(aes(colour = period), width = 0.15, height = 0, alpha = 0.35, size = 1.2) +
      geom_pointrange(
        data = mean_se,
        aes(x = period, y = mean, ymin = mean - se, ymax = mean + se),
        inherit.aes = FALSE, colour = "black", linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(x = NULL, y = metric_y_lab[["richness"]], subtitle = input$location) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "none", panel.grid.minor = element_blank())
  }) |>
    bindCache(input$location) |>
    bindEvent(input$location)
  
  # ---------- RICHNESS: detail plot ---------------------
  output$loc_plot_richness_detail <- renderPlot({
    req(input$location)
    
    df <- hab_data$species_richness_summary_location %>%
      dplyr::filter(reporting_name == input$location)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = mean, fill = period)) +
      # mean bar
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      # # mean ± SE
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["richness"]],
        subtitle = paste(input$location, ": Average species richness per sample")
      ) +
      # facet_wrap(~ zone) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",        # both bars already coloured by period
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$location) |>
    bindEvent(input$location)
  
  # ---------- RICHNESS: main boxplot by Status --------------------
  output$loc_plot_richness_main_status <- renderPlot({
    req(input$location)
    
    df <- hab_data$species_richness_samples %>%
      dplyr::filter(reporting_name == input$location)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      
      # ⬇️ Add this
      geom_point(
        stat = "summary",
        fun = "mean",
        shape = 21,
        size = 3,
        fill = "white",
        colour = "black"
      ) +
      
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["richness"]],
        subtitle = paste(input$location, "— Species richness per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$location) |>
    bindEvent(input$location)
  
  # ---------- RICHNESS: detail barplot by Status ------------------
  output$loc_plot_richness_detail_status <- renderPlot({
    req(input$location)
    
    df <- hab_data$species_richness_samples %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(n_species_sample, na.rm = TRUE),
        se   = sd(n_species_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(n_species_sample))),
        .groups = "drop"
      )
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = mean, fill = period)) +
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["richness"]],
        subtitle = paste(input$location, "— Average species richness per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$location) |>
    bindEvent(input$location)
  
  # ---------- TOTAL ABUNDANCE: two plots ------------
  output$loc_plot_total_abundance_main <- renderPlot({
    req(input$location)
    
    # Filter for this region
    df <- hab_data$total_abundance_samples %>%
      dplyr::filter(reporting_name == input$location)
    
    mean_se <- hab_data$total_abundance_summary_location %>%
      dplyr::filter(reporting_name == input$location)
    
    # Order periods
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = total_abundance_sample, fill = period)) +
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      geom_pointrange(
        data = mean_se,
        aes(x = period, y = mean,
            ymin = mean - se, ymax = mean + se),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["total_abundance"]],
        subtitle = input$location
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$location) |>
    bindEvent(input$location)
  
  output$loc_plot_total_abundance_detail <- renderPlot({
    req(input$location)
    
    df <- hab_data$total_abundance_summary_location %>%
      dplyr::filter(reporting_name == input$location)
    
    # Order periods
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df,
           aes(x = period, y = mean, fill = period)) +
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["total_abundance"]],
        subtitle = paste(input$location, "— Average total abundance per sample")
      ) +
      # facet_wrap(~ zone) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$location) |>
    bindEvent(input$location)
  
  output$loc_plot_total_abundance_main_status <- renderPlot({
    req(input$location)
    
    df <- hab_data$total_abundance_samples %>%
      dplyr::filter(reporting_name == input$location)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = total_abundance_sample, fill = period)) +
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      
      # ⬇️ Add this
      geom_point(
        stat = "summary",
        fun = "mean",
        shape = 21,
        size = 3,
        fill = "white",
        colour = "black"
      ) +
      
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["total_abundance"]],
        subtitle = paste(input$location, "— Total abundance per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$location) |>
    bindEvent(input$location)
  
  output$loc_plot_total_abundance_detail_status <- renderPlot({
    req(input$location)
    
    df <- hab_data$total_abundance_samples %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(total_abundance_sample, na.rm = TRUE),
        se   = sd(total_abundance_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(total_abundance_sample))),
        .groups = "drop"
      )
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df,
           aes(x = period, y = mean, fill = period)) +
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["total_abundance"]],
        subtitle = paste(input$location,
                         "— Average total abundance per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$location) |>
    bindEvent(input$location)
  
  # Shark and Rays -----
  output$loc_plot_shark_ray_richness_main <- renderPlot({
    req(input$location)
    
    df <- hab_data$shark_ray_richness_samples_location %>%
      dplyr::filter(reporting_name == input$location)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    mean_se <- hab_data$shark_ray_richness_summary_location %>%
      dplyr::filter(reporting_name == input$location)
    
    ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
      # boxplot (median + IQR + whiskers)
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      # raw points
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      # mean ± SE
      geom_pointrange(
        data = mean_se,
        aes(
          x    = period,
          y    = mean,
          ymin = mean - se,
          ymax = mean + se
        ),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["sharks_rays"]],
        subtitle = input$location
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$location) |>
    bindEvent(input$location)
  
  # ---------- shark_ray: detail plot ---------------------
  output$loc_plot_shark_ray_richness_detail <- renderPlot({
    req(input$location)
    
    df <- hab_data$shark_ray_richness_summary_location %>%
      dplyr::filter(reporting_name == input$location)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = mean, fill = period)) +
      # mean bar
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      # # mean ± SE
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["sharks_rays"]],
        subtitle = paste(input$location, ": Average shark and ray species richness per sample")
      ) +
      # facet_wrap(~ zone) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",        # both bars already coloured by period
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$location) |>
    bindEvent(input$location)
  
  # ---------- SHARK & RAY: main boxplot by Status --------------------
  output$loc_plot_shark_ray_richness_main_status <- renderPlot({
    req(input$region)
    
    df <- hab_data$shark_ray_richness_samples_location %>%
      dplyr::filter(reporting_name == input$location)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      
      # ⬇️ Add this
      geom_point(
        stat = "summary",
        fun = "mean",
        shape = 21,
        size = 3,
        fill = "white",
        colour = "black"
      ) +
      
      geom_jitter(
        aes(colour = period),
        width = 0.15,
        height = 0,      # <— prevents any vertical jitter
        alpha = 0.35,
        size = 1.2
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["sharks_rays"]],
        subtitle = paste(input$location, "— Shark & ray species richness per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$location) |>
    bindEvent(input$location)
  
  # ---------- SHARK & RAY: detail barplot by Status -----------------
  output$loc_plot_shark_ray_richness_detail_status <- renderPlot({
    req(input$location)
    
    df <- hab_data$shark_ray_richness_samples_location %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(n_species_sample, na.rm = TRUE),
        se   = sd(n_species_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(n_species_sample))),
        .groups = "drop"
      )
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    ggplot(df, aes(x = period, y = mean, fill = period)) +
      geom_col(
        width  = 0.6,
        colour = "black",
        alpha  = 0.85
      ) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.2,
        linewidth = 0.6
      ) +
      facet_wrap(~ status, nrow = 1) +
      scale_fill_manual(values = metric_period_cols) +
      labs(
        x = NULL,
        y = metric_y_lab[["sharks_rays"]],
        subtitle = paste(input$location, "— Average shark & ray species richness per sample by status")
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$location) |>
    bindEvent(input$location)
  
  
  
  # ---------- Trophic Groups: two plots ------------
  output$loc_plot_trophic_main <- renderPlot({
    req(input$location)
    
    # Filter for this region
    df <- hab_data$trophic_groups_samples %>%
      dplyr::filter(reporting_name == input$location)
    
    mean_se <- hab_data$trophic_groups_summary %>%
      dplyr::filter(reporting_name == input$location)
    
    # Order periods
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    mean_se$period <- factor(mean_se$period, levels = c("Pre-bloom", "Bloom"))
    
    # (Optional) order diet groups if you want a specific order
    diet_levels <- c("Carnivore", "Herbivore", "Omnivore", "Planktivore", "Diet missing")
    df$diet <- factor(df$diet, levels = diet_levels)
    mean_se$diet <- factor(mean_se$diet, levels = diet_levels)
    
    dodge <- position_dodge(width = 0.75)
    
    ggplot(df, aes(x = diet, y = n_individuals_sample, fill = period)) +
      geom_boxplot(
        position = dodge,
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      geom_jitter(
        aes(colour = period),
        position = position_jitterdodge(
          jitter.width  = 0.15,
          jitter.height = 0,
          dodge.width   = 0.75
        ),
        alpha = 0.35,
        size = 1.2
      ) +
      geom_pointrange(
        data = mean_se,
        aes(
          x    = diet,
          y    = mean,
          ymin = mean - se,
          ymax = mean + se,
          group = period,
          colour = period
        ),
        position = dodge,
        inherit.aes = FALSE,
        linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(
        x = NULL,  # or "Diet group"
        y = metric_y_lab[["large_fish"]],
        subtitle = input$location
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "top",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$location) |>
    bindEvent(input$location)
  # 
  # # ---------- Trophic Groups: stacked composition plot ------------
  # 
  # output$em_plot_trophic_stack <- renderPlot({
  #   req(input$region#, input$trophic_stack_scale
  #   )
  #   
  #   diet_levels <- names(diet_cols)
  #   
  #   # Start from the SUMMARY table (means per sample)
  #   mean_se <- hab_data$trophic_groups_richness_summary %>%
  #     dplyr::filter(region == input$region) %>%
  #     dplyr::mutate(
  #       period = factor(period, levels = c("Pre-bloom", "Bloom")),
  #       diet   = factor(diet,   levels = diet_levels)
  #     )
  #   
  #   # -------- COUNT VIEW (mean-based) --------
  #   ggplot(mean_se, aes(x = period, y = mean, fill = diet)) +
  #     geom_col(position = "stack") +
  #     scale_y_continuous(labels = scales::comma) +
  #     scale_fill_manual(values = diet_cols, drop = FALSE) +
  #     labs(
  #       x        = NULL,
  #       y        = "Mean no. species per sample",
  #       fill     = "Diet group",
  #       subtitle = input$region
  #     ) +
  #     theme_minimal(base_size = 16) +
  #     theme(panel.grid.minor = element_blank())
  #   # }
  # }) |>
  #   bindCache(input$region, input$trophic_stack_scale) |>
  #   bindEvent(input$region, input$trophic_stack_scale)
  # 
  # output$em_plot_trophic_main_status <- renderPlot({
  #   req(input$region)
  #   
  #   # Filter for this region
  #   df <- hab_data$trophic_groups_samples %>%
  #     dplyr::filter(region == input$region)
  #   
  #   mean_se <- hab_data$trophic_groups_summary %>%
  #     dplyr::filter(region == input$region)
  #   
  #   # Order periods
  #   df$period     <- factor(df$period,     levels = c("Pre-bloom", "Bloom"))
  #   mean_se$period <- factor(mean_se$period, levels = c("Pre-bloom", "Bloom"))
  #   
  #   # Diet ordering
  #   diet_levels <- c("Carnivore", "Herbivore", "Omnivore", "Planktivore", "Diet missing")
  #   df$diet     <- factor(df$diet,     levels = diet_levels)
  #   mean_se$diet <- factor(mean_se$diet, levels = diet_levels)
  #   
  #   dodge <- position_dodge(width = 0.75)
  #   
  #   ggplot(df, aes(x = diet, y = n_individuals_sample, fill = period)) +
  #     geom_boxplot(
  #       position = dodge,
  #       width = 0.6,
  #       outlier.shape = NA,
  #       alpha = 0.85,
  #       colour = "black"
  #     ) +
  #     geom_jitter(
  #       aes(colour = period),
  #       position = position_jitterdodge(
  #         jitter.width  = 0.15,
  #         jitter.height = 0,
  #         dodge.width   = 0.75
  #       ),
  #       alpha = 0.35,
  #       size = 1.2
  #     ) +
  #     geom_pointrange(
  #       data = mean_se,
  #       aes(
  #         x    = diet,
  #         y    = mean,
  #         ymin = mean - se,
  #         ymax = mean + se,
  #         group = period,
  #         colour = period
  #       ),
  #       inherit.aes = FALSE,
  #       position = dodge,
  #       linewidth = 0.6
  #     ) +
  #     scale_fill_manual(values = metric_period_cols) +
  #     scale_colour_manual(values = metric_period_cols) +
  #     labs(
  #       x = NULL,
  #       y = metric_y_lab[["large_fish"]],
  #       subtitle = input$region
  #     ) +
  #     facet_wrap(~ status) +
  #     theme_minimal(base_size = 16) +
  #     theme(
  #       legend.position = "top",
  #       panel.grid.minor = element_blank()
  #     )
  # }) |>
  #   bindCache(input$region) |>
  #   bindEvent(input$region)
  # 
  # output$em_plot_trophic_stack_status <- renderPlot({
  #   req(input$region#, input$trophic_stack_scale
  #   )
  #   
  #   diet_levels <- names(diet_cols)
  #   
  #   # Start from the SUMMARY table (means per sample)
  #   mean_se <- hab_data$trophic_groups_richness_summary_status %>%
  #     dplyr::filter(region == input$region) %>%
  #     dplyr::mutate(
  #       period = factor(period, levels = c("Pre-bloom", "Bloom")),
  #       diet   = factor(diet,   levels = diet_levels)
  #     )
  #   
  #   # -------- COUNT VIEW (mean-based) --------
  #   ggplot(mean_se, aes(x = period, y = mean, fill = diet)) +
  #     geom_col(position = "stack") +
  #     scale_y_continuous(labels = scales::comma) +
  #     scale_fill_manual(values = diet_cols, drop = FALSE) +
  #     labs(
  #       x        = NULL,
  #       y        = "Mean no. species per sample",
  #       fill     = "Diet group",
  #       subtitle = input$region
  #     ) +
  #     facet_wrap(~ status) +
  #     theme_minimal(base_size = 16) +
  #     theme(panel.grid.minor = element_blank())
  #   # }
  # }) |>
  #   bindCache(input$region, input$trophic_stack_scale) |>
  #   bindEvent(input$region, input$trophic_stack_scale)
  # 
  
  
  }