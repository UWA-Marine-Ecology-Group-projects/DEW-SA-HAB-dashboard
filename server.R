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
  
  # Final layout
  (p0 | p1 | p2) /
    (p3 | p4 | p5)
}

metric_tab_body_ui <- function(id) {
  switch(
    id,
    
    # ---- 2-plot layout ----------------------------------------
    richness = {
      layout_columns(
        col_widths = c(6, 6),
        withSpinner(
          plotOutput("em_plot_richness_main", height = 400),
          type = 6
        ),
        withSpinner(
          plotOutput("em_plot_richness_detail", height = 400),
          type = 6
        )
      )
    },
    
    # ---- 1-plot layout ----------------------------------------
    total_abundance = {
      layout_columns(
        col_widths = c(6, 6),
        withSpinner(
          plotOutput("em_plot_total_abundance_main", height = 400),
          type = 6
        ),
        withSpinner(
          plotOutput("em_plot_total_abundance_detail", height = 400),
          type = 6
        )
      )
    },
    
    sharks_rays = {
      layout_columns(
        col_widths = c(6, 6),
        withSpinner(
          plotOutput("em_plot_shark_ray_richness_main", height = 400),
          type = 6
        ),
        withSpinner(
          plotOutput("em_plot_shark_ray_richness_detail", height = 400),
          type = 6
        )
      )
    },
    
    reef_associated_richness = {
      layout_columns(
        col_widths = c(6, 6),
        withSpinner(
          plotOutput("em_plot_reef_associated_richness_main", height = 400),
          type = 6
        ),
        withSpinner(
          plotOutput("em_plot_reef_associated_richness_detail", height = 400),
          type = 6
        )
      )
    },
    
    large_fish = {
      layout_columns(
        col_widths = c(6, 6),
        withSpinner(
          plotOutput("em_plot_large_fish_main", height = 400),
          type = 6
        ),
        withSpinner(
          plotOutput("em_plot_large_fish_detail", height = 400),
          type = 6
        )
      )
    },
    
    # ---- example 3-plot layout --------------------------------
    trophic = {
      layout_columns(
        col_widths = c(4, 4, 4),
        withSpinner(
          plotOutput("em_plot_trophic_main", height = 350),
          type = 6
        ),
        withSpinner(
          plotOutput("em_plot_trophic_detail", height = 350),
          type = 6
        ),
        withSpinner(
          plotOutput("em_plot_trophic_extra", height = 350),
          type = 6
        )
      )
    },
    
    # ---- default: same as your old 2-plot layout --------------
    {
      layout_columns(
        col_widths = c(6, 6),
        withSpinner(
          plotOutput(paste0("em_plot_", id, "_main"), height = 400),
          type = 6
        ),
        withSpinner(
          plotOutput(paste0("em_plot_", id, "_detail"), height = 400),
          type = 6
        )
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
  
  output$map <- renderLeaflet({
    
    method_cols <- c("BRUVs" = "#f89f00", "UVC" = "#0c3978")
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
  output$summary_text <- renderUI({
    req(input$em_region)
    
    reg <- input$em_region
    
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
      # class = "table table-striped table-sm",
      tags$thead(
        tags$tr(
          tags$th("Threshold"),
          tags$th("Example Plot")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("Low = ≥80% of the pre-bloom value"),
          tags$td(plotOutput("example_low", height = 80, width = 120))
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
  
  output$example_low <- renderPlot({
    half_donut_with_dial(
      values = c(1,1,1),
      mode   = "absolute",
      status = "Low"
    )
  })
  
  output$example_medium <- renderPlot({
    half_donut_with_dial(
      values = c(1,1,1),
      mode   = "absolute",
      status = "Medium"
    )
  })
  
  output$example_high <- renderPlot({
    half_donut_with_dial(
      values = c(1,1,1),
      mode   = "absolute",
      status = "High"
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
      class = "table table-sm",  # uses bootstrap styling
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
    req(selected_region())
    make_impact_gauges(selected_region())
  })
  
  
  output$impact_gauges_region <- renderPlot({
    req(input$em_region)
    make_impact_gauges(input$em_region)
  })
  
  deployments <- reactive({
    deployments <- hab_data$hab_combined_metadata %>%
      dplyr::filter(region %in% input$em_region) 
    
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
      dplyr::filter(region %in% input$em_region)
    
    shp <- regions_joined %>%
      dplyr::filter(region %in% input$em_region)
    
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
          metric_tab_body_ui(id)   # <- custom per metric
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
    req(input$em_region)
    df <- hab_data$species_richness_samples %>%
      dplyr::filter(region == input$em_region)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    mean_se <- hab_data$species_richness_summary %>%
      dplyr::filter(region == input$em_region)
    
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
        subtitle = input$em_region
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$em_region) |>
    bindEvent(input$em_region)
  
  # ---------- RICHNESS: detail plot ---------------------
  output$em_plot_richness_detail <- renderPlot({
    req(input$em_region)
    
    df <- hab_data$species_richness_summary %>%
      dplyr::filter(region == input$em_region)
    
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
        subtitle = paste(input$em_region, ": Average species richness per sample")
      ) +
      # facet_wrap(~ zone) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",        # both bars already coloured by period
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$em_region) |>
    bindEvent(input$em_region)
  
  # ---------- TOTAL ABUNDANCE: two plots ------------
  output$em_plot_total_abundance_main <- renderPlot({
    req(input$em_region)
    
    # Filter for this region
    df <- hab_data$total_abundance_samples %>%
      dplyr::filter(region == input$em_region)
    
    mean_se <- hab_data$total_abundance_summary %>%
      dplyr::filter(region == input$em_region)
    
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
        subtitle = input$em_region
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$em_region) |>
    bindEvent(input$em_region)
  
  output$em_plot_total_abundance_detail <- renderPlot({
    req(input$em_region)
    
    df <- hab_data$total_abundance_summary %>%
      dplyr::filter(region == input$em_region)
    
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
        subtitle = paste(input$em_region, "— Average total abundance per sample")
      ) +
      # facet_wrap(~ zone) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$em_region) |>
    bindEvent(input$em_region)
  
  # …and so on for trophic, sharks_rays, etc.
  # Shark and Rays -----
  output$em_plot_shark_ray_richness_main <- renderPlot({
    req(input$em_region)
    df <- hab_data$shark_ray_richness_samples %>%
      dplyr::filter(region == input$em_region)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    mean_se <- hab_data$shark_ray_richness_summary %>%
      dplyr::filter(region == input$em_region)
    
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
        subtitle = input$em_region
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$em_region) |>
    bindEvent(input$em_region)
  
  # ---------- shark_ray: detail plot ---------------------
  output$em_plot_shark_ray_richness_detail <- renderPlot({
    req(input$em_region)
    
    df <- hab_data$shark_ray_richness_summary %>%
      dplyr::filter(region == input$em_region)
    
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
        subtitle = paste(input$em_region, ": Average shark and ray species richness per sample")
      ) +
      # facet_wrap(~ zone) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",        # both bars already coloured by period
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$em_region) |>
    bindEvent(input$em_region)
  
  # Reef associated richness -----
  output$em_plot_reef_associated_richness_main <- renderPlot({
    req(input$em_region)
    
    df <- hab_data$reef_associated_richness_samples %>%
      dplyr::filter(region == input$em_region)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    mean_se <- hab_data$reef_associated_richness_summary %>%
      dplyr::filter(region == input$em_region)
    
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
        subtitle = input$em_region
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$em_region) |>
    bindEvent(input$em_region)
  
  # ---------- shark_ray: detail plot ---------------------
  output$em_plot_reef_associated_richness_detail <- renderPlot({
    req(input$em_region)
    
    df <- hab_data$reef_associated_richness_summary %>%
      dplyr::filter(region == input$em_region)
    
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
        subtitle = paste(input$em_region, ": Average reef associated species richness per sample")
      ) +
      # facet_wrap(~ zone) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",        # both bars already coloured by period
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$em_region) |>
    bindEvent(input$em_region)
  
  # ---------- Large fish: two plots ------------
  output$em_plot_large_fish_main <- renderPlot({
    req(input$em_region)

    # Filter for this region
    df <- hab_data$fish_200_abundance_samples %>%
      dplyr::filter(region == input$em_region)

    mean_se <- hab_data$fish_200_abundance_summary %>%
      dplyr::filter(region == input$em_region)

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
        subtitle = input$em_region
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$em_region) |>
    bindEvent(input$em_region)

  output$em_plot_large_fish_detail <- renderPlot({
    req(input$em_region)

    df <- hab_data$fish_200_abundance_summary %>%
      dplyr::filter(region == input$em_region)

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
        subtitle = paste(input$em_region, "— Average total abundance per sample")
      ) +
      # facet_wrap(~ zone) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }) |>
    bindCache(input$em_region) |>
    bindEvent(input$em_region)
  
  # ---- HAB % change summary table (per region) ------------------------------
  output$em_change_table <- renderTable({
    req(input$em_region)
    
    df <- hab_metric_change |>
      dplyr::filter(region == input$em_region) |>
      dplyr::select(
        Metric = impact_metric,
        Change = percentage_change   # <- your character column
      )
    
    vals <- df$Change
    
    # 1. Detect “Surveys incomplete”
    is_incomplete <- !is.na(vals) & grepl("Surveys incomplete", vals, ignore.case = TRUE)
    
    # 2. Try to parse numbers
    num <- suppressWarnings(as.numeric(vals))
    has_num <- !is.na(num) & !is_incomplete
    
    # 3. Arrows: up for ≥0, down for <0
    arrows <- ifelse(num < 0, "&#8595;", "&#8593;")
    
    # 4. Colour rules (num is % change)
    colours <- ifelse(
      num <= -50,                    # 50–100% decrease
      "#EB5757",
      ifelse(
        num > -50 & num <= -20,      # 20–50% decrease
        "#F2C94C",
        "#3B7EA1"                    # 0–20% decrease OR any increase
      )
    )
    
    # 5. Build formatted column
    out <- rep("", length(vals))
    
    # Numeric values
    out[has_num] <- sprintf(
      "<span style='color:%s'>%s %s%%</span>",
      colours[has_num],
      arrows[has_num],
      scales::number(abs(num[has_num]), accuracy = 1)
    )
    
    # Surveys incomplete
    out[is_incomplete] <- "<span style='color:#000000'><em>Surveys incomplete</em></span>"
    
    data.frame(
      Metric  = df$Metric,
      Change = out,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  },
  sanitize.text.function = function(x) x   # allow HTML and colours
  )
  
  make_top10_plot <- function(region_name, 
                              focal_period = c("Pre-bloom", "Bloom"),
                              title_lab = "Common species",
                              number_species) {
    
    focal_period <- match.arg(focal_period)
    
    df <- hab_data$region_top_species_average |>
      dplyr::filter(region == region_name)
    
    # Identify top N species *within the focal period only*
    top_species <- df |>
      dplyr::filter(period == focal_period) |>
      dplyr::slice_max(order_by = average, n = number_species, with_ties = FALSE) |>
      dplyr::pull(display_name)
    
    # Only those species, but showing both pre/post
    plot_df <- df |>
      dplyr::filter(display_name %in% top_species) |>
      tidyr::extract(display_name, into = c("sci", "common"),
                     regex = "^(.*?)\\s*\\((.*?)\\)$", remove = FALSE) |>
      dplyr::mutate(
        label = paste0("*", sci, "*<br>(", common, ")")
      )
    
    # 1. Period order: focal period FIRST in dodge (nearest axis)
    plot_df$period <- factor(
      plot_df$period,
      levels = c(focal_period, setdiff(c("Pre-bloom","Bloom"), focal_period))
    )
    
    # 2. Species order: descending average for focal period (biggest at top)
    species_order <- plot_df |>
      dplyr::filter(period == focal_period) |>
      dplyr::arrange(average) |>
      dplyr::pull(label)
    
    plot_df$label <- factor(plot_df$label, levels = species_order)
    
    # 3. Arrange rows so dodge respects period order
    plot_df <- plot_df |>
      dplyr::arrange(label, period)
    
    # Use a shared dodge for bars + error bars
    # dodge <- position_dodge2(reverse = TRUE, preserve = "single")
    dodge <- position_dodge(width = 0.8)
    
    ggplot(
      plot_df,
      aes(x = average,
          y = label,
          fill = period,
          group = period)     # <-- add group here
    ) +
      geom_col(position = dodge) +
      geom_errorbarh(
        aes(xmin = average - se,
            xmax = average + se),
        position = dodge,
        height = 0.3
      ) +
      labs(
        x = "Average abundance per BRUV",
        y = NULL,
        title = title_lab
      ) +
      scale_fill_manual(values = c("Pre-bloom" = "#0c3978", "Bloom" = "#f89f00")) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic() +
      theme(
        legend.position = "bottom",
        axis.text.y = ggtext::element_markdown(size = 12)
      )
  }
  
  output$em_common_pre <- renderPlot({
    req(input$em_region)
    make_top10_plot(input$em_region,
                    focal_period = "Pre-bloom",
                    title_lab = "Most common species pre-bloom",
                    number_species = input$numberspecies)
  })
  
  output$em_common_post <- renderPlot({
    req(input$em_region)
    make_top10_plot(input$em_region,
                    focal_period = "Bloom",
                    title_lab = "Most common species post-bloom",
                    number_species = input$numberspecies)
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
    "rov_progress",
    left_reactive  = reactive({ uvc_planned }),
    right_reactive = reactive({ uvc_completed })
  )
  
  # # 2) Does this region use BRUVS or ROV?
  # has_bruvs <- reactive({
  #   grepl("BRUVS", survey_region()$methods[[1]], ignore.case = TRUE)
  # })
  # 
  # has_rov <- reactive({
  #   grepl("ROV", survey_region()$methods[[1]], ignore.case = TRUE)
  # })
  # 
  # 3) Planned/completed reactives
  # sites_planned <- reactive({ survey_region()$planned_number_sites })
  # sites_completed <- reactive({ survey_region()$complete_number_sites })
  # 
  # bruvs_planned <- reactive({
  #   if (!has_bruvs()) return(0)
  #   survey_region()$planned_number_drops
  # })
  # bruvs_completed <- reactive({
  #   if (!has_bruvs()) return(0)
  #   survey_region()$complete_number_drops
  # })
  # 
  # rov_planned <- reactive({
  #   if (!has_rov()) return(0)
  #   survey_region()$planned_number_transects
  # })
  # rov_completed <- reactive({
  #   if (!has_rov()) return(0)
  #   survey_region()$complete_number_transects
  # })
  
  # 4) twoValueBoxServer(...) for each
  # twoValueBoxServer("sites_progress",  left_reactive = sites_planned,  right_reactive = sites_completed)
  # twoValueBoxServer("bruvs_progress",  left_reactive = bruvs_planned, right_reactive = bruvs_completed)
  # twoValueBoxServer("rov_progress",    left_reactive = rov_planned,   right_reactive = rov_completed)
  
  # # 5) Unified layout for all boxes
  # # ---- Single layout for all progress value boxes --------------------------
  # output$survey_value_boxes <- renderUI({
  #   df  <- survey_region()
  #   pct <- df$percent_sites_completed
  #   vb_col <- completion_theme(pct)
  #   
  #   has_rov_val <- has_rov()
  #   
  #   sites_box <- twoValueBoxUI(
  #     id          = "sites_progress",
  #     title       = "Sites",
  #     left_label  = "Planned",
  #     right_label = "Completed",
  #     icon        = icon("magnifying-glass", class = "fa-xl"),
  #     theme_color = "secondary"
  #   )
  #   
  #   bruvs_box <- twoValueBoxUI(
  #     id          = "bruvs_progress",
  #     title       = "BRUVS deployments",
  #     left_label  = "Planned",
  #     right_label = "Completed",
  #     icon        = icon("ship", class = "fa-xl"),
  #     theme_color = "secondary"
  #   )
  #   
  #   pct_box <- value_box(
  #     title       = "Locations completed",
  #     value       = sprintf("%.1f%%", pct),
  #     subtitle    = df$methods[[1]],
  #     theme_color = vb_col,
  #     showcase    = icon("percent", class = "fa-xl")
  #   )
  #   
  #   if (has_rov_val) {
  #     # 4 boxes → 2 per row (width = 1/2)
  #     rov_box <- twoValueBoxUI(
  #       id          = "rov_progress",
  #       title       = "ROV transects",
  #       left_label  = "Planned",
  #       right_label = "Completed",
  #       icon        = icon("video", class = "fa-xl"),
  #       theme_color = "secondary"
  #     )
  #     
  #     layout_column_wrap(
  #       width = 1/2,
  #       sites_box,
  #       bruvs_box,
  #       rov_box,
  #       pct_box
  #     )
  #     
  #   } else {
  #     # 3 boxes → 3 on one row (width = 1/3)
  #     layout_column_wrap(
  #       width = 1/3,
  #       sites_box,
  #       bruvs_box,
  #       pct_box
  #     )
  #   }
  # })
  
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
                                 focal_period = c("Pre-bloom", "Bloom"),
                                 number_species) {
    
    focal_period <- match.arg(focal_period)
    
    df <- mp_species_counts |>
      dplyr::filter(park == park_name)
    
    top_species <- df |>
      dplyr::filter(period == focal_period) |>
      dplyr::slice_max(order_by = average, n = number_species, with_ties = FALSE) |>
      dplyr::pull(species)
    
    plot_df <- df |>
      dplyr::filter(display_name %in% top_species)
    
    order_df <- plot_df |>
      dplyr::filter(period == focal_period) |>
      dplyr::arrange(average)
    
    plot_df$display_name <- factor(plot_df$display_name, levels = order_df$display_name)
    
    ggplot(plot_df, aes(x = average, y = display_name), fill = period) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
      geom_col(position = "dodge") +
      labs(x = "Count", y = NULL) +
      scale_fill_manual(values = c("Pre-bloom" = "#0c3978",
                                   "Bloom" = "#f89f00")) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position = "bottom"
      )
  }
  
  output$mp_common_pre <- renderPlot({
    req(input$mp_park)
    make_top10_plot_mp(input$mp_park, focal_period = "Pre-bloom", number_species = input$numberspeciespark)
  })
  
  output$mp_common_post <- renderPlot({
    req(input$mp_park)
    make_top10_plot_mp(input$mp_park, focal_period = "Bloom", number_species = input$numberspeciespark)
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
      dplyr::filter(region %in% input$em_region) #%>%
    #glimpse()
  })
  
  # 2. Nicely formatted text for the selected region ----
  output$years_for_region <- renderText({
    req(input$em_region)
    
    yrs <- years_by_region() |>
      dplyr::filter(region == input$em_region) |>
      dplyr::pull(years_sampled)
    
    yrs
  })
  
  }