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



# ------------------------------ server ---------------------------------------

server <- function(input, output, session) {
  
  regions_joined <- hab_data$shp |>
    left_join(hab_data$scores, by = "region") %>% 
    glimpse()
  
  
  # Default selected region (first available)
  selected_region <- reactiveVal({
    (regions_joined$region[!is.na(regions_joined$region)])[1]
  })
  
  # Leaflet map
  output$map <- renderLeaflet({
    
    base_map(current_zoom = 8) |>
      
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
                labels = c("Very Poor", "Poor","Good","Very Good"),
                opacity = 0.8)
  })
  
  # Click handler
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id)) {
      selected_region(click$id)
    }
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
      tags$h3(reg)
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
      segments = segs,
      values   = vals,
      colors   = cols,
      mode     = "absolute",
      status   = txt,     # or "Good", "Med", etc.
      r_inner  = 0.5,
      r_outer  = 1,
      show_segment_labels = FALSE,
      show_tier_labels    = TRUE
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
      segments = segs,
      values   = vals,
      colors   = cols,
      mode     = "absolute",
      status   = txt,     # or "Good", "Med", etc.
      r_inner  = 0.5,
      r_outer  = 1,
      show_segment_labels = FALSE,
      show_tier_labels    = TRUE
    )+
      ggtitle("Diversity") +
      theme(plot.title = element_text(hjust = 0.5))
    
    txt <- hab_data$scores |>
      filter(region == reg) |>
      pull(abundance)
    
    abundance <- half_donut_with_dial(
      segments = segs,
      values   = vals,
      colors   = cols,
      mode     = "absolute",
      status   = txt,     # or "Good", "Med", etc.
      r_inner  = 0.5,
      r_outer  = 1,
      show_segment_labels = FALSE,
      show_tier_labels    = TRUE
    )+
      ggtitle("Abundance") +
      theme(plot.title = element_text(hjust = 0.5))
    
    txt <- hab_data$scores |>
      filter(region == reg) |>
      pull(habitat)
    
    habitat <- half_donut_with_dial(
      segments = segs,
      values   = vals,
      colors   = cols,
      mode     = "absolute",
      status   = txt,     # or "Good", "Med", etc.
      r_inner  = 0.5,
      r_outer  = 1,
      show_segment_labels = FALSE,
      show_tier_labels    = TRUE
    )+
      ggtitle("Habitat") +
      theme(plot.title = element_text(hjust = 0.5))
    
    final_plot <- diversity + abundance + habitat + plot_layout(ncol = 3)
    
    final_plot
  })
  
  
  
  
  
  # # Base maps (shared scaffolding) ----
  # output$map_deployments <- renderLeaflet(base_map())
  # output$map_surveys     <- renderLeaflet(base_map())
  # output$map_combined     <- renderLeaflet(base_map(current_zoom = 7))
  # output$species_map     <- renderLeaflet(base_map())
  # output$species_map_rls     <- renderLeaflet(base_map())
  # output$assemblage_map  <- renderLeaflet(base_map())
  # output$assemblage_map_rls  <- renderLeaflet(base_map())
  # 
  # # ---- deployments map (primary points) -------------------------------------
  # observe({
  #   all_points <- dataframes$deployment_locations
  #   
  #   dc <- depth_cols_and_pal(all_points$depth_m)
  #   
  #   update_points_with_numeric_legend(
  #     map_id       = "map_deployments",
  #     data         = all_points,
  #     fill_cols    = dc$cols,
  #     legend_pal   = dc$pal,
  #     legend_values= all_points$depth_m,
  #     legend_title = "Depth (m)"
  #   )
  # })
  # 
  # # ---- surveys map (RLS points) ---------------------------------------------
  # observe({
  #   all_points_rls <- dataframes$deployment_locations_rls
  #   
  #   dc <- depth_cols_and_pal(all_points_rls$depth_m)
  #   
  #   update_points_with_numeric_legend(
  #     map_id       = "map_surveys",
  #     data         = all_points_rls,
  #     fill_cols    = dc$cols,
  #     legend_pal   = dc$pal,
  #     legend_values= all_points_rls$depth_m,
  #     legend_title = "Depth (m)"
  #   )
  # })
  # 
  # # ---- deployments combined (bruv and rls points) -------------------------------------
  # observe({
  #   all_points <- bind_rows(dataframes$deployment_locations #%>% mutate(method = "stereo-BRUVs")
  #                           , 
  #                           dataframes$deployment_locations_rls #%>% mutate(method = "UVC")
  #                           )
  #   # method → colour mapping
  #   method_cols <- c("stereo-BRUVs" = "#f89f00", "UVC" = "#0c3978")
  #   
  #   leafletProxy("map_combined", data = all_points) |>
  #     clearGroup("Sampling locations") |>
  #     leafgl::addGlPoints(
  #       data = all_points,
  #       fillColor = method_cols[all_points$method],  # lookup
  #       weight = 1,
  #       popup = all_points$popup,
  #       group = "Sampling locations",
  #       pane = "points"
  #     ) |>
  #     addLegend(
  #       "topright",
  #       colors = unname(method_cols),
  #       labels = names(method_cols),
  #       title = "Survey method",
  #       opacity = 1,
  #       group = "Sampling locations",
  #       layerId = "methodLegend"
  #     )
  # })
  # 
  # # ---- species bubble map ----------------------------------------------------
  # # Map canvas is rendered above via base_map(); here we add bubbles & legend.
  # observe({
  #   req(input$species_select)
  #   
  #   data <- dataframes$bubble_data %>%
  #     dplyr::filter(display_name %in% input$species_select) %>%
  #     dplyr::full_join(dataframes$deployment_locations) %>%   # keep your original join semantics
  #     tidyr::replace_na(list(count = 0))
  #   
  #   max_ab <- ifelse(nrow(data) > 0, max(data$count, na.rm = TRUE), 1)
  #   
  #   overzero  <- dplyr::filter(data, count > 0) %>% sf::st_as_sf()
  #   equalzero <- data %>% dplyr::filter(count == 0) %>% sf::st_as_sf()
  #   
  #   map <- leafletProxy("species_map") %>%
  #     clearGroup("Sampling locations") %>%
  #     add_bubble_legend(max_val = max_ab, title = "Abundance", layerId = "bubbleLegendSpecies")
  #   
  #   if (nrow(overzero)) {
  #     overzero$radius <- 10 + (overzero$count / max_ab) * 50
  #     
  #     map <- map %>%
  #       leafgl::addGlPoints(
  #         data = overzero,
  #         fillColor = "#f89f00",
  #         fillOpacity = 1,
  #         weight = 1,
  #         radius = overzero$radius,
  #         popup = as.character(overzero$count),
  #         group = "Sampling locations",
  #         pane = "points"
  #       )
  #   }
  #   
  #   if (nrow(equalzero)) {
  #     map <- map %>%
  #       leafgl::addGlPoints(
  #         data = equalzero,
  #         fillColor = "white",
  #         fillOpacity = 0.5,
  #         weight = 1,
  #         radius = 10,
  #         popup = as.character(equalzero$count),
  #         group = "Sampling locations",
  #         pane = "points"
  #       )
  #   }
  # })
  # 
  # # ---- species bubble map RLS ----------------------------------------------------
  # # Map canvas is rendered above via base_map(); here we add bubbles & legend.
  # observe({
  #   req(input$species_select)
  #   
  #   data <- dataframes$bubble_data_rls %>%
  #     dplyr::filter(display_name %in% input$species_select) %>%
  #     dplyr::full_join(dataframes$deployment_locations_rls) %>%   # keep your original join semantics
  #     tidyr::replace_na(list(count = 0))
  #   
  #   max_ab <- ifelse(nrow(data) > 0, max(data$count, na.rm = TRUE), 1)
  #   
  #   overzero  <- dplyr::filter(data, count > 0) %>% sf::st_as_sf()
  #   equalzero <- data %>% dplyr::filter(count == 0) %>% sf::st_as_sf()
  #   
  #   map <- leafletProxy("species_map_rls") %>%
  #     clearGroup("Sampling locations") %>%
  #     add_bubble_legend(max_val = max_ab, 
  #                       title = "Abundance", 
  #                       layerId = "bubbleLegendSpecies",
  #                       methodcol = "#0c3978")
  #   
  #   if (nrow(overzero)) {
  #     overzero$radius <- 10 + (overzero$count / max_ab) * 50
  #     
  #     map <- map %>%
  #       leafgl::addGlPoints(
  #         data = overzero,
  #         fillColor = "#0c3978",
  #         fillOpacity = 1,
  #         weight = 1,
  #         radius = overzero$radius,
  #         popup = as.character(overzero$count),
  #         group = "Sampling locations",
  #         pane = "points"
  #       )
  #   }
  #   
  #   if (nrow(equalzero)) {
  #     map <- map %>%
  #       leafgl::addGlPoints(
  #         data = equalzero,
  #         fillColor = "white",
  #         fillOpacity = 0.5,
  #         weight = 1,
  #         radius = 10,
  #         popup = as.character(equalzero$count),
  #         group = "Sampling locations",
  #         pane = "points"
  #       )
  #   }
  # })
  # 
  # # sync after rendering
  # observe({
  #   leafletProxy("species_map") %>%
  #     leaflet.extras2::addLeafletsync(c("species_map", "species_map_rls"))
  # })
  # 
  # # ---- assemblage bubble map -------------------------------------------------
  # observe({
  #   req(input$assemblage)
  #   
  #   assemblage_metric <- input$assemblage |>
  #     tolower() |>
  #     stringr::str_replace_all(" ", "_")
  #   
  #   data <- dataframes$metric_bubble_data %>%
  #     dplyr::filter(metric %in% assemblage_metric)
  #   
  #   max_ab <- ifelse(nrow(data) > 0, max(data$value, na.rm = TRUE), 1)
  #   
  #   overzero <- dplyr::filter(data, value > 0) %>%
  #     sf::st_as_sf(coords = c("longitude_dd", "latitude_dd"))
  #   
  #   equalzero <- dplyr::filter(data, value == 0) %>%
  #     sf::st_as_sf(coords = c("longitude_dd", "latitude_dd"))
  #   
  #   map <- leafletProxy("assemblage_map") %>%
  #     clearGroup("Sampling locations") %>%
  #     add_bubble_legend(max_val = max_ab, title = input$assemblage) 
  # 
  #   if (nrow(overzero)) {
  #     overzero$radius <- 10 + (overzero$value / max_ab) * 50
  #     
  #     map <- map %>%
  #       leafgl::addGlPoints(
  #         data = overzero,
  #         fillColor = "#f89f00",
  #         fillOpacity = 1,
  #         weight = 1,
  #         radius = overzero$radius,
  #         popup = as.character(overzero$value),
  #         group = "Sampling locations",
  #         pane = "points"
  #       )
  #   }
  # 
  #   if (nrow(equalzero)) {
  #     map %>%
  #       addCircleMarkers(
  #         data = equalzero,
  #         fillColor = "white",
  #         color = "black",
  #         fillOpacity = 1,
  #         weight = 1,
  #         radius = 5,
  #         popup = as.character(equalzero$value),
  #         group = "Sampling locations"
  #       )
  #   }
  # })
  # 
  # # ---- assemblage bubble map RLS -------------------------------------------------
  # observe({
  #   req(input$assemblage)
  #   
  #   assemblage_metric <- input$assemblage |>
  #     tolower() |>
  #     stringr::str_replace_all(" ", "_")
  #   
  #   data <- dataframes$metric_bubble_data_rls %>%
  #     dplyr::filter(metric %in% assemblage_metric)
  #   
  #   max_ab <- ifelse(nrow(data) > 0, max(data$value, na.rm = TRUE), 1)
  #   
  #   overzero <- dplyr::filter(data, value > 0) %>%
  #     sf::st_as_sf(coords = c("longitude_dd", "latitude_dd"))
  #   
  #   equalzero <- dplyr::filter(data, value == 0) %>%
  #     sf::st_as_sf(coords = c("longitude_dd", "latitude_dd"))
  #   
  #   map <- leafletProxy("assemblage_map_rls") %>%
  #     clearGroup("Sampling locations") %>%
  #     add_bubble_legend(max_val = max_ab, title = input$assemblage, methodcol = "#0c3978")
  #   
  #   if (nrow(overzero)) {
  #     overzero$radius <- 10 + (overzero$value / max_ab) * 50
  #     
  #     map <- map %>%
  #       leafgl::addGlPoints(
  #         data = overzero,
  #         fillColor = "#0c3978",
  #         fillOpacity = 1,
  #         weight = 1,
  #         radius = overzero$radius,
  #         popup = as.character(overzero$value),
  #         group = "Sampling locations",
  #         pane = "points"
  #       )
  #   }
  #   
  #   if (nrow(equalzero)) {
  #     map %>%
  #       addCircleMarkers(
  #         data = equalzero,
  #         fillColor = "white",
  #         color = "black",
  #         fillOpacity = 1,
  #         weight = 1,
  #         radius = 5,
  #         popup = as.character(equalzero$value),
  #         group = "Sampling locations"
  #       )
  #   }
  # })
  # 
  # # sync after rendering
  # observe({
  #   leafletProxy("assemblage_map") %>%
  #     leaflet.extras2::addLeafletsync(c("assemblage_map", "assemblage_map_rls"))
  # })
  # # 
  # # # after renderLeaflet() for both maps:
  # # session$onFlushed(function() {
  # #   leafletProxy("assemblage_map") %>%
  # #     leaflet.extras2::addLeafletsync(c("assemblage_map_rls"))
  # #   leafletProxy("assemblage_map_rls") %>%
  # #     leaflet.extras2::addLeafletsync(c("assemblage_map"))
  # # }, once = TRUE)
  # 
  # # ---- plots (kept as-is) ----------------------------------------------------
  # output$top_species_plot <- renderPlot({
  #   data <- dataframes$top_species_combined %>%  # both methods
  #     tidyr::extract(
  #       display_name, into = c("sci", "common"),
  #       regex = "^(.*?)\\s*\\((.*?)\\)$", remove = FALSE
  #     ) %>%
  #     dplyr::mutate(
  #       label = paste0("<i>", sci, "</i><span> (", common, ")</span>")
  #     )
  #   
  #   ggplot2::ggplot(data, aes(
  #     x = reorder_within(label, total_number, method),
  #     y = total_number,
  #     fill = method
  #   )) +
  #     geom_bar(stat = "identity", 
  #              # fill = "#0c3978", 
  #              col = "black") +
  #     coord_flip() +
  #     xlab("Species") +
  #     ylab("Overall abundance") +
  #     scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  #     scale_x_reordered() +
  #     ggplot_theme +
  #     scale_fill_manual(values = c("stereo-BRUVs" = "#f89f00", "UVC" = "#0c3978")) +
  #     theme(axis.text.y = ggtext::element_markdown(size = 12)) +
  #     facet_wrap(vars(method), scales = "free")
  # })
  # 
  # output$iframe <- renderUI({
  #   dat <- dataframes$foa_codes[display_name %in% c(input$species_select)] %>%
  #     dplyr::distinct(url) %>%
  #     dplyr::pull("url")
  #   
  #   tags$iframe(
  #     src = paste0(dat),
  #     style = "width: 100%; height: 100vh; border: none;",
  #     onload = "resizeIframe(this)"
  #   )
  # })
  # 
  # output$length_histogram <- renderPlot({
  #   length <- dataframes$length_with_jurisdiction %>%
  #     dplyr::filter(display_name %in% input$species_length)
  #   
  #   ggplot(length, aes(x = length_mm, fill = method)) +
  #     geom_histogram(binwidth = input$binwidth, #fill = "#0c3978", 
  #                    color = "black") +
  #     facet_grid(status ~ jurisdiction, scales = "free_y") +
  #     scale_fill_manual(values = c("stereo-BRUVs" = "#f89f00", "UVC" = "#0c3978")) +
  #     ggplot_theme +
  #     labs(
  #       x = "Length (mm)",
  #       y = "Frequency"
  #     )
  # })
  # 
  # # Length histogram ----
  # output$length_histogram_density <- renderPlot({
  #   length <- dataframes$length_with_jurisdiction %>%
  #     dplyr::filter(display_name %in% input$species_length)
  #   
  #   ggplot(length, aes(x = length_mm, fill = status)) +
  #     geom_histogram(aes(y = ..density.., fill = method), binwidth = input$binwidth, #fill = "#0c3978",
  #                    color = "black", position = "identity") +
  #     facet_grid(status ~ jurisdiction, scales = "free_y") +
  #     labs(x = "Length (mm)", y = "Proportion (Density)") +
  #     scale_fill_manual(values = c("stereo-BRUVs" = "#f89f00", "UVC" = "#0c3978")) +
  #     ggplot_theme
  # })
  # 
  # # simple plots ----
  # output$depth_hist          <- renderPlot({ plots$depth_hist })
  # output$date_hist           <- renderPlot({ plots$date_hist })
  # output$depth_hist_rls      <- renderPlot({ plots$depth_hist_rls })
  # output$date_hist_rls       <- renderPlot({ plots$date_hist_rls })
  # output$date_hist_combined  <- renderPlot({ plots$date_hist_combined })
  # output$depth_hist_combined <- renderPlot({ plots$depth_hist_combined })
  # 
  # 
  # # same vector of parks as UI
  # # parks <- sort(unique(dataframes$deployment_locations$location))
  # 
  # # lapply(parks, function(pk) {
  # #   mod_park_summary_server(
  # #     id = paste0("park_", gsub("[^A-Za-z0-9]+", "_", pk)),
  # #     park_name = pk,
  # #     dataframes = dataframes,
  # #     values = values,
  # #     plots = plots
  # #   )
  # # })
  # # 
  # # ONE instance of the module, driven by the sidebar selector
  # 
  # # lapply(parks, function(pk) {
  # mod_park_summary_server(
  #   id = "one_park",
  #   park_r = reactive(input$park_select),
  #   dataframes = dataframes,
  #   values = values,
  #   plots = plots
  # )
  # # })
  
  
  }
