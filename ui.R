page_navbar(
  title = div(
    "Harmful Algal Bloom Dashboard (DRAFT)",
    favicon = "www/favicon.ico",  # path to your favicon,
    style = "display: flex; gap: 10px; align-items: center; padding-right: 15px;font-weight: bold;"
  ),
  
  # id = "nav",
  
  theme = bs_theme(
    bootswatch = "minty",
    secondary = "#0c3978",
    primary = "#f89f00",
    success = "#e10038",
    base_font = font_google("Roboto"),
    code_font = font_google("Roboto")
  ),
  # 
  # fillable = TRUE,
  
  tags$head(
    tags$style(HTML("
    /* Make the bslib page_navbar's navbar stick to the top */
    .bslib-page .navbar {
      position: sticky;
      top: 0;
      z-index: 1050; /* above cards/maps */
    }

    /* If your page uses anchored scroll to headings, keep them visible */
    .bslib-page {
      scroll-padding-top: var(--bslib-navbar-height, 56px);
    }
  "))
  ),
  
  nav_panel(
    "State Summary",
    
    layout_sidebar(
      sidebar = sidebar(
        h4("Region selection"),
        helpText("Click a polygon on the map to load plots & summary."),
        hr(),
        uiOutput("selected_region_badge"),
        helpText("Colour scale: Very Good (green) → Good (yellow) → Poor (orange) → Very Poor (red).")
      ),
      # Main content
      div(
        class = "container-fluid",
        # Map row
        div(class = "row",
            div(class = "col-md-12",
                card(
                  card_header("Reporting Regions (click to explore)"),
                  card_body(leafletOutput("map", height = 500))
                )
            )
        ),
        # Detail row
        div(class = "row g-3 mt-3",
            div(class = "col-lg-6",
                card(
                  card_header("Summary"),
                  card_body(htmlOutput("summary_text"))
                )
            ),
            div(class = "col-lg-6",
                card(
                  card_header("Plot 1"),
                  card_body(plotOutput("plot1", height = 250))
                ),
                card(
                  card_header("Plot 2"),
                  card_body(plotOutput("plot2", height = 250))
                ),
                card(
                  card_header("Plot 3"),
                  card_body(plotOutput("plot3", height = 250))
                )
            )
        )
      ))
  ),
  
  
  # nav_panel(
  #   "State Summary"),
  # 
  # 
  # 
  # nav_panel(
  #   "Explore a location"),
  
  # Dashboard ----
  # nav_panel(
  #   
  #   "State-wide Summary",
  #   page_sidebar(#fillable = TRUE,  
  #     # sidebar = NULL,
  #     sidebar = sidebar(
  #       width = 0,
  #       closed = TRUE
  #     ),
  #     #   tags$head(
  #     #     tags$style(HTML("
  #     #                .leaflet-container {z-index:0}
  #     # 
  #     #                .leaflet-top, .leaflet-bottom {
  #     #     z-index: unset !important;
  #     # }
  #     # 
  #     # .leaflet-touch .leaflet-control-layers, .leaflet-touch .leaflet-bar {
  #     #     z-index: 10000000000 !important;
  #     # }"))
  #     #   ),
  #     
  #     tags$head(tags$style(css)),
  #     
  #     # Top Row - Four Value Boxes
  #     layout_column_wrap(
  #       min_height = 700,
  #       
  #       card(
  #         min_height = 600,
  #         max_height = 700,
  #         
  #         shiny::a(
  #           h2(img(src = "stereo-BRUV_filled_transparent.png",
  #                  height = "120px"
  #           ),
  #           "Baited Remote Underwater Stereo-video (stereo-BRUVs)",
  #           class = "custom-button btn btn-default action-button", # use primary for blue
  #           style = "font-weight:600; width:100%; text-align:center; font-size: 20px; color: white;"),
  #           href = paste0("https://benthic-bruvs-field-manual.github.io/")
  #         ),
  #         
  #         full_screen = TRUE,
  #         
  #         # Bruv stats ----
  #         layout_column_wrap(
  #           # min_height = 600,
  #           width = 1/2,
  #           value_box(
  #             # min_height = 200,
  #             title = "Deployments", value = scales::label_comma()(values$number_of_deployments),
  #             theme_color = "secondary",
  #             showcase = icon("ship", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Fish Counted", value = scales::label_comma()(values$number_of_fish),
  #             theme_color = "secondary",
  #             showcase = icon("fish-fins", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Fish Species", value = scales::label_comma()(values$number_of_fish_species),
  #             theme_color = "secondary",
  #             showcase = icon("fish-fins", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Other species", value =scales::label_comma()(values$number_of_nonfish_species),
  #             theme_color = "secondary",
  #             showcase = icon("shrimp", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Length Measurements", value = scales::label_comma()(values$number_of_measurements),
  #             theme_color = "secondary",
  #             showcase = icon("ruler-vertical", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Years Included", value = paste0((values$min_year),
  #                                                      " - ",
  #                                                      (values$max_year)),
  #             theme_color = "secondary",
  #             showcase = icon("calendar", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Depths Surveyed", value = paste0(scales::label_comma()(values$min_depth),
  #                                                       " - ",
  #                                                       scales::label_comma()(values$max_depth),
  #                                                       " m"),
  #             theme_color = "secondary",
  #             showcase = icon("arrow-down-up-across-line", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Average Depth", value = paste0(scales::label_comma()(values$mean_depth), " m"),
  #             theme_color = "secondary",
  #             showcase = icon("wave-square", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Deployments with benthos", value = scales::label_comma()(values$deployments_benthos),
  #             theme_color = "secondary",
  #             showcase = icon("seedling", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Deployments with relief", value = scales::label_comma()(values$deployments_relief),
  #             theme_color = "secondary",
  #             showcase = icon("mound", class = "fa-xl")
  #           )
  #         )
  #       ),
  #       
  #       # UVC stats ----
  #       card(
  #         min_height = 600,
  #         max_height = 700,
  #         
  #         shiny::a(
  #           h2(img(src = "uvc_new.png",
  #                  height = "120px"
  #           ),
  #           "Underwater Visual Census (UVC)",
  #           class = "custom-button btn btn-default action-button", # use primary for blue
  #           style = "font-weight:600; width:100%; text-align:center; font-size: 20px; color: white;"),
  #           href = paste0("https://benthic-bruvs-field-manual.github.io/")
  #         ),
  #         full_screen = TRUE,
  #         layout_column_wrap(
  #           width = 1/2,
  #           value_box(
  #             title = "Surveys", value = scales::label_comma()(values$number_of_deployments_rls),
  #             theme_color = "secondary",
  #             showcase = icon("ship", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Fish Counted", value = scales::label_comma()(values$number_of_fish_rls),
  #             theme_color = "secondary",
  #             showcase = icon("fish-fins", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Fish Species", value = scales::label_comma()(values$number_of_fish_species_rls),
  #             theme_color = "secondary",
  #             showcase = icon("fish-fins", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Other species", value =scales::label_comma()(values$number_of_nonfish_species_rls),
  #             theme_color = "secondary",
  #             showcase = icon("shrimp", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Length Measurements", value = scales::label_comma()(values$number_of_measurements_rls),
  #             theme_color = "secondary",
  #             showcase = icon("ruler-vertical", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Years Included", value = paste0((values$min_year_rls),
  #                                                      " - ",
  #                                                      (values$max_year_rls)),
  #             theme_color = "secondary",
  #             showcase = icon("calendar", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Depths Surveyed", value = paste0(scales::label_comma()(values$min_depth_rls),
  #                                                       " - ",
  #                                                       scales::label_comma()(values$max_depth_rls),
  #                                                       " m"),
  #             theme_color = "secondary",
  #             showcase = icon("arrow-down-up-across-line", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Average Depth", value = paste0(scales::label_comma()(values$mean_depth_rls), " m"),
  #             theme_color = "secondary",
  #             showcase = icon("wave-square", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Deployments with benthos", value = scales::label_comma()(values$deployments_benthos),
  #             theme_color = "secondary",
  #             showcase = icon("seedling", class = "fa-xl")
  #           ),
  #           value_box(
  #             title = "Deployments with relief", value = scales::label_comma()(values$deployments_relief),
  #             theme_color = "secondary",
  #             showcase = icon("mound", class = "fa-xl")
  #           )
  #         )
  #       ),
  #       
  #     ),
  #     layout_column_wrap(
  #       min_height = 800,
  #       width = 1/2,
  #       card(
  #         min_height = 500,
  #         full_screen = TRUE,
  #         card_header("Map of Sampling Effort"),
  #         
  #         leafletOutput("map_combined", height = "400px") #> withSpinner()
  #       ),
  #       
  #       div(
  #         card(
  #           card_header("Sampling Effort by Year"),
  #           full_screen = TRUE,
  #           max_height = 390,
  #           plotOutput("date_hist_combined")
  #         ),
  #         
  #         card(
  #           card_header("Sampling Effort by Depth"),
  #           full_screen = TRUE,
  #           max_height = 390,
  #           plotOutput("depth_hist_combined")
  #         )
  #       )),
  #     
  #     card(
  #       min_height = 650,
  #       full_screen = TRUE,
  #       card_header("Most Abundant Species"),
  #       plotOutput("top_species_plot", height = "400px")
  #     ),
  #     
  #     card(
  #       min_height = 750,
  #       card_header("Spatial distribution of assemblage metrics"),
  #       
  #       shinyWidgets::pickerInput(
  #         inputId = "assemblage",
  #         label = "Choose an assemblage metric:",
  #         width = "100%",
  #         choices = c("Total abundance", "Species richness"),
  #         multiple = FALSE,
  #         selected = "Total abundance",
  #         options = list(`actions-box` = TRUE, `live-search` = FALSE, `dropup-auto` = FALSE)
  #       ),
  #       
  #       layout_columns(
  #         col_widths = c(6, 6), 
  #         max_height = 550,
  #         div(
  #           h4("Stereo-BRUVs"),
  #           leafletOutput("assemblage_map", height = "58vh"
  #           )
  #         ),
  #         div(
  #           h4("UVC"),
  #           leafletOutput("assemblage_map_rls", height = "58vh"
  #           )
  #         )
  #       )
  #     ),
  #     
  #     # Fourth Row - Species map and iframe
  #     card(full_screen = TRUE,
  #          card_header("Investigate a species abundance data"),
  #          min_height = 750,
  #          
  #          shinyWidgets::pickerInput(
  #            # min_height = 100,
  #            inputId = "species_select",
  #            label = "Choose a species:",
  #            width = "100%",
  #            choices = c(values$top_species_names_combined),
  #            multiple = FALSE,
  #            selected = values$top_species_names_combined[1],
  #            options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = TRUE)
  #          ),
  #          
  #          layout_columns(
  #            # width = 1/3,
  #            col_widths = c(4, 4, 4),  # 8/12 = ~66% and 4/12 = ~33% (close to 60/40)
  #            max_height = 550,
  #            div(
  #              h4("Stereo-BRUVs"),
  #              leafletOutput("species_map", height = "500px")
  #            ),
  #            
  #            div(
  #              h4("UVC"),
  #              leafletOutput("species_map_rls", height = "500px")
  #            ),
  #            
  #            htmlOutput("iframe")
  #          )
  #     ),
  #     
  #     # # Fifth Row - Histograms
  #     card(
  #       full_screen = TRUE,
  #       max_height = 750,
  #       min_height = 700,
  #       
  #       layout_column_wrap(
  #         width = 1/2,
  #         
  #         shinyWidgets::pickerInput(
  #           inputId = "species_length",
  #           label = "Choose a species:",
  #           width = "100%",
  #           choices = c(values$top_species_names_combined),
  #           multiple = FALSE,
  #           selected = values$top_species_names_combined[1],
  #           options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = TRUE)
  #         ),
  #         
  #         numericInput("binwidth", value = 50, min = 0, max = 1000, label = "Choose binwidth (mm):")
  #       ),
  #       
  #       layout_column_wrap(
  #         width = 1/2,
  #         min_height = 400,
  #         max_height = 600,
  #         card(card_header("Length frequency histogram"),
  #              plotOutput("length_histogram", height = "600px")),
  #         card(card_header("Normalised length frequency histogram"),
  #              plotOutput("length_histogram_density", height = "600px"))
  #       )
  #     )
  #   )
  # ),
  # 
  # # Marine Park ----
  # nav_panel(
  #   "Explore a Marine Park",
  #   page_sidebar(
  #     fillable = TRUE,     
  #     sidebar = sidebar(
  #       sticky = TRUE,
  #       # h4("Sidebar only for Page 2"),
  #       # sliderInput("slider", "Example slider", 1, 100, 50),
  #       shinyWidgets::pickerInput(
  #         "park_select", "Choose a marine park:",
  #         choices = parks, selected = parks[1],
  #         options = list(`live-search` = TRUE)
  #       )
  #     ),
  #     # "This is Page 2 content.",
  #     mod_park_summary_ui("one_park")
  #   )
  # ),
  # 
  # nav_panel(
  #   "Status & Trends"),
  
  nav_spacer(),
  
  # Add logos to the top right corner
  nav_item(
    tags$div(
      style = "display: flex; gap: 10px; align-items: center; padding-right: 15px;",
      tags$img(src = "dew_logo.png", height = "70px")
      
    )
  )
)