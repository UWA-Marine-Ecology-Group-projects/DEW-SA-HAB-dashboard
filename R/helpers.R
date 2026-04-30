# Theme for plotting ----
ggplot_theme <- 
  ggplot2::theme_bw() +
  ggplot2::theme( # use theme_get() to see available options
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_blank(),
    # legend.position = "top",
    text = ggplot2::element_text(size = 12),
    strip.text.y = ggplot2::element_text(size = 12, angle = 0),
    axis.title.x = ggplot2::element_text(vjust = 0.3, size = 12),
    axis.title.y = ggplot2::element_text(vjust = 0.6, angle = 90, size = 12),
    axis.text.y = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
    axis.line.x = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
    axis.line.y = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
    strip.background = ggplot2::element_blank(),
    
    strip.text = ggplot2::element_text(size = 14, angle = 0),
    
    plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold.italic")
  )

# Legend for leaflet ----
add_legend <- function(map, colors, labels, sizes, opacity = 1, group, title, layerId) { #map, 
  colorAdditions <- glue::glue(
    "{colors}; border-radius: 50%; width:{sizes}px; height:{sizes}px"
  )
  labelAdditions <- glue::glue(
    "<div style='display: inline-block; height: {sizes}px; ",
    "margin-top: 4px;line-height: {sizes}px;'>{labels}</div>"
  )
  
  return(
    leaflet::addLegend(map,
                       colors = colorAdditions,
                       labels = labelAdditions,
                       opacity = opacity,
                       title = title,
                       position = "topright",
                       group = group,
                       layerId = layerId
    )
  )
}

filter_by_location <- function(df, loc) {
  if (is.null(loc) || !"location" %in% names(df)) return(NULL)
  dplyr::filter(df, .data$location == loc)
}

base_map <- function(max_zoom = 11, current_zoom = 6) {
  leaflet() |>
    addTiles(options = tileOptions(minZoom = 4, max_zoom)) |>
    setView(lng = 135.3, lat = -35.1, current_zoom) |>
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
      group = "Australian Marine Parks",
      popup = ~ZoneName,
      options = pathOptions(pane = "polys")
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

# ensure_sf_ll <- function(x, lon = "longitude_dd", lat = "latitude_dd") {
#   if (inherits(x, "sf")) return(x)
#   stopifnot(lon %in% names(x), lat %in% names(x))
#   sf::st_as_sf(x, coords = c(lon, lat), crs = 4326)
# }

ensure_sf_ll <- function(x, lon = "longitude_dd", lat = "latitude_dd") {
  
  if (inherits(x, "sf")) {
    if (is.na(sf::st_crs(x))) {
      sf::st_crs(x) <- 4326
      return(x)
    }
    
    return(sf::st_transform(x, 4326))
  }
  
  stopifnot(lon %in% names(x), lat %in% names(x))
  
  sf::st_as_sf(
    x,
    coords = c(lon, lat),
    crs = 4326
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

has_leafgl <- function() requireNamespace("leafgl", quietly = TRUE)