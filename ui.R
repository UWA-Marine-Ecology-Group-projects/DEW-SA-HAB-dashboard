ui <- page_navbar(
  title = div(
    "Algal bloom data portal",
    favicon = "www/favicon.ico",
    style = "display:flex; gap:10px; align-items:center; padding-right:15px; font-weight:bold;"
  ),
  
  theme = bs_theme(
    bootswatch = "minty",
    secondary = "#0c3978",
    primary   = "#f89f00",
    success   = "#e10038",
    base_font = font_google("Roboto"),
    code_font = font_google("Roboto")
  ),
  
  # --- CSS: sticky navbar; sidebar map fills; right panel scrolls ---
  tags$head(
    tags$style(HTML("
      /* Navbar fixed */
      .bslib-page .navbar { position: sticky; top: 0; z-index: 1050; }
      .bslib-page { scroll-padding-top: var(--bslib-navbar-height, 56px); }

      @media (min-width: 992px) {
        /* Sidebar: sticky, full viewport under the navbar, and FLEX COLUMN */
        .bslib-layout-sidebar-sidebar {
          position: sticky;
          top: var(--bslib-navbar-height, 56px);
          height: calc(100dvh - var(--bslib-navbar-height, 56px));
          display: flex;
          flex-direction: column;
          overflow: hidden; /* the map card will control overflow */
        }

        /* Sidebar map card flexes to fill; min-height:0 allows flex child to shrink */
        .sidebar-map-card {
          flex: 1 1 auto;
          min-height: 0;
          display: flex;
          flex-direction: column;
          margin-bottom: 0;
        }

        /* Card body must flex too, with no padding so the map can fill it */
        .sidebar-map-card .card-body {
          flex: 1 1 auto;
          min-height: 0;
          padding: 0;
          display: flex;
          flex-direction: column;
        }

        /* Map wrapper fills the body; Leaflet fills the wrapper */
        .sidebar-map-fill { flex: 1 1 auto; min-height: 0; }
        .sidebar-map-card .leaflet-container { height: 100% !important; width: 100% !important; }

        /* Right/main column gets the only scrollbar */
        .bslib-layout-sidebar-main {
          height: calc(100dvh - var(--bslib-navbar-height, 56px));
          overflow: auto;
        }
      }
    "))
  ),
  
  tags$head(
    tags$style(HTML("
    .pp-wrap { display:flex; justify-content:space-between; gap:2rem; margin-top:.25rem; }
    .pp-col  { text-align:center; flex:1; }
    .pp-lab  { font-size:1.00rem; opacity:0.85; display:block; }
    .pp-val  { font-size:1.25rem; font-weight:700; margin-top:.25rem; display:block; }
    .pp-title-center .value-box-title { text-align:center; width:100%; }
  ")
    )
  ),
  
  tags$head(
    tags$style(HTML("
    .vb-icon-wrap {
      padding-top: 2rem;      /* move icon down inside box */
      /* or use margin-top instead if you prefer */
      /* margin-top: 3rem; */
    }
  ")
    )
  ),
  
  tags$head(
    tags$style(HTML("
    /* existing CSS ... */

    /* Make spinner wrappers fill the map card */
    .map-full-wrapper {
      height: 100%;
    }

    .map-full-wrapper .shiny-spinner-output-container,
    .map-full-wrapper .shiny-spinner-placeholder {
      height: 100%;
    }

    .map-full-wrapper .leaflet-container {
      height: 100% !important;
      width: 100% !important;
    }
  "))
  ),
  
  tags$head(
    tags$style(HTML("
    /* Map spinners (leaflet) */
    .map-full-wrapper {
      height: 100%;
    }
    .map-full-wrapper .shiny-spinner-output-container,
    .map-full-wrapper .shiny-spinner-placeholder {
      height: 100%;
    }
    .map-full-wrapper .leaflet-container {
      height: 100% !important;
      width: 100% !important;
    }

    /* Plot spinners: parent controls height */
    .plot-full-wrapper {
      height: 100%;
    }
    .plot-full-wrapper .shiny-spinner-output-container,
    .plot-full-wrapper .shiny-spinner-placeholder {
      height: 100%;
    }
  "))
  ),
  
  
  nav_panel(
    "Overview",
    
    layout_columns(
      col_widths = c(7, 5),
      
      div(
        h4("Algal bloom impacts on nearshore marine biodiversity monitoring progress"),
        
        layout_column_wrap(
          width = 1/4,   # 3 boxes on one row
          
          twoValueBoxUI(
            id          = "sites_progress",
            title       = "Sites",
            left_label  = "Planned",
            right_label = "Completed",
            icon        = div(class = "vb-icon-wrap", icon("magnifying-glass", class = "fa-xl")),
            theme_color = "secondary",
            height = 150
          ),
          
          twoValueBoxUI(
            id          = "bruvs_progress",
            title       = "BRUVS deployments",
            left_label  = "Planned",
            right_label = "Completed",
            icon        = img(src = "stereo-BRUV_filled_transparent.png",
                              height = "70px"
            ), #div(class = "vb-icon-wrap", icon("ship", class = "fa-xl")),
            theme_color = "secondary"
          ),
          
          twoValueBoxUI(
            id          = "uvc_progress",
            title       = "Dive transects",
            left_label  = "Planned",
            right_label = "Completed",
            icon        = img(src = "uvc_new.png",
                              height = "100px"),
              #div(class = "vb-icon-wrap", icon("video", class = "fa-xl")),
            theme_color = "secondary"
          ),
          
          value_box(
            title       = "Locations completed",
            value       = paste0(percent_completed, "%"),
            theme_color = vb_col,
            showcase    = div(class = "vb-icon-wrap", icon("percent", class = "fa-xl"))
          )
        ),
        
        div(
          h4("Portal Aims"),
          h6(HTML("This portal provides a visual assessment of the ecological impacts of the recent harmful algal bloom using stereo-BRUV data uploaded to GlobalArchive.org. It summarises key fish community metrics—including total abundance, species richness, and other indicator responses—to compare conditions before the bloom (Pre-bloom) with those observed during and after the event (Bloom), these metrics are defined in <b>Table 1</b> below. The threshold levels are defined in <b>Table 2</b>.
               
               </br></br>By integrating standardised, quality-controlled BRUV annotations with clear temporal comparisons, the dashboard helps highlight shifts in community structure and supports evidence-based management decisions."))),
        
        br(),

        layout_column_wrap(
          width = 1/2,
          div(
            h5("Table 1. Definitions of fish indicator metrics"),
            spinnerTableOutput("indicator_table")  # was: tableOutput("indicator_table")
          ),
          
          div(
            h5("Table 2. Definitions of threshold levels"),
            spinnerTableOutput("pointer_table")    # was: tableOutput("pointer_table")
          ),
        )
        
      ),

      
      card(
        full_screen = TRUE,
        card_header("Sites Surveyed"),
        div(
          class = "map-full-wrapper",
          withSpinner(
            leafletOutput("map", height = "100%"),
            type = 6
          )
        )
      )
      
    )
  ),
  
  nav_panel(
    "Region Summary",
    layout_sidebar(
      sidebar = sidebar(
        width = "350px",
        selectizeInput(
          "region",
          "Choose a region",
          choices = NULL, multiple = FALSE,
          options = list(placeholder = "Choose a region...")
        ),
        
        h6("Years sampled:"),
        textOutput("years_for_region"),
        br(),
        
        h6("Summary:"),
        uiOutput("region_summary_text"),
        br(),
        
        helpText("")
      ),
      
      div(
        class = "container-fluid",
        
        layout_columns(
          col_widths = c(7, 5),

          
          card(
            min_height = 550,
            full_screen = TRUE,
            card_header("Survey Effort"),
            div(
              class = "map-full-wrapper",
              withSpinner(
                leafletOutput("region_survey_effort", height = "100%"),
                type = 6
              )
            )
          ),
          
          div(
            card(
              card_header(
                div(
                  "Region Impact overview",
                  style = "display:inline-block;"
                ),
                div(
                  actionLink(
                    inputId = "open_info_pointers",
                    label = NULL,
                    icon = icon("circle-info")
                  ),
                  style = "float:right; margin-top:-2px;"
                )
              ),
              
              spinnerPlotOutput("overall_impact_gauge", height = 180),  # was: plotOutput(...)
              h6("Algal bloom impact on:"),
              spinnerPlotOutput("region_impact_gauges", height = 300),
              helpText(
              "* Bluefin leatherjacket impact is reversed, click on the info icon for more information ")# was: plotOutput(...)
            ),
            
            card(
              card_header(
                div(
                  "Percentage change compared to pre-bloom levels",
                  style = "display:inline-block;"
                ),
                div(
                  actionLink(
                    inputId = "open_info_table",
                    label = NULL,
                    icon = icon("circle-info")
                  ),
                  style = "float:right; margin-top:-2px;"
                )
              ),
              card_body(
                spinnerUiOutput("region_change_table"#, height = 200
                                )  # was: uiOutput("region_change_table")
              )
            )
            
          )
        ),
        
        card(
          min_height = 500,
          card_header("Common species"),
          full_screen = TRUE,
          
          layout_sidebar(
            sidebar = div(
              h6(strong("Plot inputs:")),
              numericInput( 
                "region_number_species", 
                "Choose number of species to plot", 
                value = 10, 
                min   = 1, 
                max   = 20 
              ),
              checkboxInput(
                "region_species_status",
                "Show status (Fished vs No-take)",
                FALSE
              ),
              checkboxInput(
                "region_species_facet",
                "Facet by status",
                FALSE
              )
            ),

            layout_columns(
              col_widths = c(6, 6),
              
              div(
                class = "plot-full-wrapper",
                # style = "height:500px;",
                withSpinner(
                  plotOutput("region_common_pre", height = "100%"),
                  type = 6
                )
              ),
              div(
                class = "plot-full-wrapper",
                # style = "height:500px;",
                withSpinner(
                  plotOutput("region_common_post", height = "100%"),
                  type = 6
                )
              )
            )
            
            
          )
        ),
        # br(),
        uiOutput("region_tabset")   # tabset stays, now below the table
      )
    )
  ),
  
  nav_panel(
    "Location Summary",
    layout_sidebar(
      sidebar = sidebar(
        width = "350px",
        selectizeInput(
          "location",
          "Choose a location",
          choices = NULL, multiple = FALSE,
          options = list(placeholder = "Choose a location...")
        ),
        
        h6("Years sampled:"),
        textOutput("years_for_location"),
        br(),
        
        h6("Summary:"),
        uiOutput("location_summary_text"),
        br(),
        
        helpText("")
      ),
      
      div(
        class = "container-fluid",
        
        layout_columns(
          col_widths = c(7, 5),
          
          card(
            min_height = 600,
            full_screen = TRUE,
            card_header("Survey Effort"),
            div(
              class = "map-full-wrapper",
              withSpinner(
                leafletOutput("location_survey_effort", height = "100%"),
                type = 6
              )
            )
          ),
          
          div(
            card(
              card_header(
                div(
                  "Location Impact overview",
                  style = "display:inline-block;"
                ),
                div(
                  actionLink(
                    inputId = "open_info_pointers_location",
                    label = NULL,
                    icon = icon("circle-info")
                  ),
                  style = "float:right; margin-top:-2px;"
                )
              ),
              spinnerPlotOutput("location_impact_gauges", height = 350)
            ),
            
            card(
              card_header(
                div(
                  "Percentage change compared to pre-bloom levels",
                  style = "display:inline-block;"
                ),
                div(
                  actionLink(
                    inputId = "open_info_table_location",
                    label = NULL,
                    icon = icon("circle-info")
                  ),
                  style = "float:right; margin-top:-2px;"
                )
              ),
              card_body(
                spinnerUiOutput("location_change_table")
              )
            )
          )
        ),
        
        card(
          min_height = 500,
          card_header("Common species"),
          full_screen = TRUE,
          
          layout_sidebar(
            sidebar = div(
              h6(strong("Plot inputs:")),
              numericInput(
                "location_number_species",
                "Choose number of species to plot",
                value = 10,
                min   = 1,
                max   = 20
              ),
              checkboxInput(
                "location_species_status",
                "Show status (Fished vs No-take)",
                FALSE
              ),
              checkboxInput(
                "location_species_facet",
                "Facet by status",
                FALSE
              )
            ),
            
            layout_columns(
              col_widths = c(6, 6),
              
              div(
                class = "plot-full-wrapper",
                withSpinner(
                  plotOutput("location_common_pre", height = "100%"),
                  type = 6
                )
              ),
              div(
                class = "plot-full-wrapper",
                withSpinner(
                  plotOutput("location_common_post", height = "100%"),
                  type = 6
                )
              )
            )
          )
        ),
        
        uiOutput("location_tabset")
      )
    )
  ),
  
  nav_spacer(),
  
  nav_item(
    tags$div(
      style = "display:flex; gap:10px; align-items:center; padding-right:15px;",
      tags$img(src = "dew_logo.png", height = "70px")
    )
  )
)
