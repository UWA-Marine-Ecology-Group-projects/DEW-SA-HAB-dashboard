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
            icon        = div(class = "vb-icon-wrap", icon("ship", class = "fa-xl")),
            theme_color = "secondary"
          ),
          
          twoValueBoxUI(
            id          = "rov_progress",
            title       = "ROV transects",
            left_label  = "Planned",
            right_label = "Completed",
            icon        = div(class = "vb-icon-wrap", icon("video", class = "fa-xl")),
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
            tableOutput("indicator_table"),
          ),

          div(
            h5("Table 2. Definitions of threshold levels"),
            tableOutput("pointer_table"),
          ),
        )
    ),
    
    card(
      full_screen = TRUE,
      card_header("Sites Surveyed"),
      leafletOutput("map")
    )
  )
),

nav_panel(
  "Region Summary",
  layout_sidebar(
    sidebar = sidebar(
      width = "350px",
      selectizeInput(
        "em_region",
        "Choose a region",
        choices = NULL, multiple = FALSE,
        options = list(placeholder = "Choose a region...")
      ),
      
      h6("Years sampled:"),
      textOutput("years_for_region"),
      br(),
      
      h6("Summary:"),
      uiOutput("summary_text"),
      br(),
      
      numericInput( 
        "numberspecies", 
        "Choose number of species to plot", 
        value = 10, 
        min = 1, 
        max = 20 
      ),
      helpText("")
    ),
    
    div(
      class = "container-fluid",
      
      layout_columns(
        col_widths = c(7, 5),
        card(min_height = 600,
             card_header("Survey Effort"),
             leafletOutput("surveyeffort")),
        
        div(
          card(
            card_header(
              div(
                "Region Impact overview",
                style = "display:inline-block;"
              ),
              # info icon that acts as a button
              div(
                actionLink(
                  inputId = "open_info_pointers",
                  label = NULL,
                  icon = icon("circle-info")  # or "info-circle"
                ),
                style = "float:right; margin-top:-2px;"
              )
            ),
            
            plotOutput("impact_gauges_region", height = 350)
          ),
          card(
            card_header(
              div(
                "Percentage change compared to pre-bloom levels",
                style = "display:inline-block;"
              ),
              # info icon that acts as a button
              div(
                actionLink(
                  inputId = "open_info_table",
                  label = NULL,
                  icon = icon("circle-info")  # or "info-circle"
                ),
                style = "float:right; margin-top:-2px;"
              )
            ),
            card_body(
              # card_header("Percentage change compared to pre-bloom levels"),
              tableOutput("em_change_table")
            )
          )
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        card(min_height = 500,
             card_header("Most common species pre-bloom"),
             plotOutput("em_common_pre", height = 500)
        ),
        card(min_height = 500,
             card_header("Most common species post-bloom"),
             plotOutput("em_common_post", height = 500)
        )
      ),
      br(),
      uiOutput("em_tabset")   # tabset stays, now below the table
    )
  )
),

# nav_panel(
#   "Location Summary",
#   layout_sidebar(
#     sidebar = sidebar(
#       width = "350px",
#       selectizeInput(
#         "location",
#         "Choose a location",
#         choices = NULL, multiple = FALSE,
#         options = list(placeholder = "Choose a location...")
#       ),
#       # helpText("Explore indicators, common species and survey progress for a selected marine park.")
#     ),
#     
#     # Park-level survey progress boxes
#     uiOutput("loation_survey_value_boxes"),
#     
#     
#     div(
#       class = "container-fluid",
#       layout_columns(
#         col_widths = c(7, 5),
#         card(min_height = 600,
#              card_header("Survey Effort"),
#              leafletOutput("locationsurveyeffort")),
#         
#         div(
#           card(
#             card_header("Location impact overview"),
#             plotOutput("location_impact_gauges_region", height = 350)#,
#           ),
#           card(
#             card_header("Percentage change compared to pre-bloom levels"),
#             tableOutput("location_change_table")
#           ))),
# 
#       layout_columns(
#         col_widths = c(6, 6),
#         card(
#           card_header("Most common species pre-bloom"),
#           plotOutput("location_common_pre", height = 400)
#         ),
#         card(
#           card_header("Most common species post-bloom"),
#           plotOutput("location_common_post", height = 400)
#         )
#       ),
#       
#       br(),
#       uiOutput("location_tabset")   # tabset for park-level metrics
#     )
#   )
# ),

nav_spacer(),

nav_item(
  tags$div(
    style = "display:flex; gap:10px; align-items:center; padding-right:15px;",
    tags$img(src = "dew_logo.png", height = "70px")
  )
)
)
