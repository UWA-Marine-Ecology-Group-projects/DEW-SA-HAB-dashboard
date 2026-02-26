ui <- page_navbar(
  title = div(
    "Algal bloom data portal",
    favicon = "www/favicon.ico",
    style = "display:flex; gap:10px; align-items:center; padding-right:15px; font-weight:bold; color:#0D576E;"
  ),
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Barlow:wght@300;400;500;600;700&display=swap"
    )
  ),
  
  theme = bs_theme(
    bootswatch = "minty",
    secondary = "#5F7F8D",
    primary   = "#5C5873",
    success   = "#e10038",
    
    # Navbar styling (all Bootstrap variables)
    # navbar_bg   = "#9CA9B3",     # background
    # navbar_fg   = "#0D576E",     # link colour
    # 
    # navbar_bg = "#BBC4D2",
    # # navbar_fg = "#0D576E",
    
    
    navbar_bg = "#e6f0f5",
    navbar_fg = "#0D576E",
    navbar_hover_fg = "#083E4F",
    navbar_brand_color = "#0D576E",
    
    
    # navbar_hover_fg = "#083E4F", # link hover
    # navbar_brand_color = "#0D576E",         # title colour
    navbar_brand_hover_color = "#083E4F",    # title hover
    
    sidebar_bg = "#BBC4D2",
    
    # # Page background
    fg = "black",
    bg = "#F4F8FA",   # light blue-grey background
    
    base_font    = font_google("Barlow"),
    heading_font = font_google("Barlow"),  # <-- THIS fixes h1-h6
    code_font    = font_google("Barlow"),
    
    "table-bg" = "#4D788A",
    "table-striped-bg" = "#9CA9B3",
    "table-color" = "#0b1f24",
    
    # card_cap_bg    = "#0D576E",  # header background
    # card_cap_color = "#ffffff",   # header text
    
    card_cap_bg = "#d8eaf0",
    card_cap_color = "#0D576E",
    card_bg = "#FFFFFF",
    "card-border-width" = "1px",
    "card-border-radius" = "1rem",
    "card-box-shadow" = "0 4px 12px rgba(0,0,0,0.05)"
  ),
  
  # --- CSS: sticky navbar; sidebar map fills; right panel scrolls ---
  tags$head(
    tags$style(HTML("
                    
                    .navbar {
  border-bottom: 1px solid rgba(13, 87, 110, 0.15);
                    }

  .navbar .nav-link.active {
  font-weight: 600;
  border-bottom: 2px solid #0D576E;
  }

  
.bslib-layout-sidebar-sidebar {
  padding-top: 0.75rem;
  box-shadow: 6px 0 16px rgba(0,0,0,0.03);
}
                    
  /* Force Barlow globally */
  html, body, .bslib-page {
    font-family: 'Barlow', system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
  }

  /* Common Bootstrap/UI elements */
  .navbar, .nav-link, .btn, .form-control, .form-select, .dropdown-menu,
  .card, .bslib-card, .value-box, .sidebar, .bslib-sidebar {
    font-family: 'Barlow', system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
  }

  /* selectizeInput */
  .selectize-control, .selectize-input, .selectize-dropdown, .selectize-dropdown-content {
    font-family: 'Barlow', system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
  }

  /* DT tables */
  table.dataTable, table.dataTable * {
    font-family: 'Barlow', system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
  }

  /* Leaflet controls (zoom buttons, layer control text, etc.) */
  .leaflet-container, .leaflet-control, .leaflet-control * {
    font-family: 'Barlow', system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
  }
  

.modern-kpi {
  background-color: #FFFFFF !important;
  border: 1px solid #0D576E !important;   /* teal border */
  border-radius: 1rem !important;
  box-shadow: none !important;
text-align:center; width:100%; 
}

.value-box {
  background-color: #FFFFFF !important;
  border: 2px solid #0D576E !important;
  border-radius: 1rem !important;
  box-shadow: none !important;
}

/* KPI title */
.modern-kpi .value-box-title {
  color: #4F6F7C;
  font-weight: 500;
}

/* KPI numbers */
.modern-kpi .pp-val,
.modern-kpi .value-box-value {
  color: #0D576E !important;
  font-weight: 700;
  font-size: 1.4rem;
}

/* KPI labels */
.modern-kpi .pp-lab {
  color: #6D8C98;
  font-weight: 500;
}

/* KPI icons */
.modern-kpi .fa,
.modern-kpi svg,
.modern-kpi img {
  color: #0D576E !important;
  filter: none;
}

.kpi-row {
  background-color: #F1F7F9;   /* very light marine tint */
  padding: 1rem;
  border-radius: 1rem;
}

.kpi-section {
  background-color: #F1F6F8;   /* very light tint */
  padding: 1.25rem;
  border-radius: 0.75rem;
}

.bslib-sidebar-layout>.sidebar {
    grid-column: 1 / 2;
    width: 100%;
    border-right: var(--_vert-border);
    border-top-right-radius: 0;
    border-bottom-right-radius: 0;
    color: var(--_sidebar-fg);
    background-color: rgb(227 235 240 / 100%);
}
")),

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
      
/* ===== Sidebar background (layout_sidebar) ===== */
.bslib-layout-sidebar-sidebar {
  background: #E9F1F5 !important;
  border-right: 1px solid rgba(13,87,110,0.12) !important;
  box-shadow: 6px 0 16px rgba(0,0,0,0.03);
}

/* The inner sidebar container should also be tinted (this is usually the visible layer) */
.bslib-layout-sidebar-sidebar .bslib-sidebar,
.bslib-layout-sidebar-sidebar .sidebar-content,
.bslib-layout-sidebar-sidebar .offcanvas-body {
  background: #E9F1F5 !important;
}

/* Header row */
table.hab-table thead th {
  background-color: #4A8C8C !important;
  color: #ffffff !important;
  border-color: rgba(0,0,0,0.12) !important;
}

/* Stripe colours applied to CELLS (td/th), not tr */
table.hab-table tbody tr > * {
  background-color: #d7e1e1 !important;   /* odd rows */
  color: #0b1f24 !important;
}

table.hab-table tbody tr:nth-child(even) > * {
  background-color: #80A9A9 !important;   /* even rows */
}

/* Optional: tidy borders */
table.hab-table td, table.hab-table th {
  border-color: rgba(0,0,0,0.12) !important;
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
    
    .kpi-title {
  font-weight: 600;
  color: #0D576E;
  margin-bottom: 1rem;
}

.page-header h3 {
  font-weight: 600;
  margin-bottom: 0.25rem;
}
  "))
),


nav_panel(
  "Overview",
  
  layout_columns(
    col_widths = c(7, 5),
    
    div(
      # h4("Algal bloom impacts on nearshore marine biodiversity monitoring progress"),
      
      div(class = "page-header",
          h3("Overview"),
          # p("Monitoring progress for algal bloom response", class = "text-muted")
      ),
      
      
      # card(
        # card_header("Monitoring progress for algal bloom response"),
      div(
        class = "kpi-row",
      layout_column_wrap(
        width = 1/4,   # 3 boxes on one row
        
        twoValueBoxUI(
          id          = "sites_progress",
          title       = "Sites",
          left_label  = "Planned",
          right_label = "Completed",
          icon        = div(class = "vb-icon-wrap", icon("magnifying-glass", class = "fa-xl", style = "color:#0D576E;")),
          theme_color = NULL,
          height = 150
        ),
        
        twoValueBoxUI(
          id          = "bruvs_progress",
          title       = "BRUVS deployments",
          left_label  = "Planned",
          right_label = "Completed",
          icon        = img(src = "teal_bruv.png",
                            height = "80px"
          ), #div(class = "vb-icon-wrap", icon("ship", class = "fa-xl")),
          theme_color = NULL
        ),
        
        twoValueBoxUI(
          id          = "uvc_progress",
          title       = "Dive transects",
          left_label  = "Planned",
          right_label = "Completed",
          icon        = img(src = "teal_uvc.png",
                            height = "80px"),
          #div(class = "vb-icon-wrap", icon("video", class = "fa-xl")),
          theme_color = NULL
        ),
        
        value_box(class = "modern-kpi",
          title       = "Locations completed",
          value       = paste0(percent_completed, "%"),
          theme_color = NULL,
          showcase    = div(class = "vb-icon-wrap", icon("percent", class = "fa-xl", style = "color:#0D576E;"))
        )#)
      )),
      
      # hr(style = "opacity:0.1s5; margin-top:1rem; margin-bottom:2rem;"),
      
      card(
        card_header("Portal Aims"),
        card_body(
          p(HTML("This portal provides a visual assessment of the ecological impacts of the recent harmful algal bloom using stereo-BRUV data uploaded to GlobalArchive.org. It summarises key fish community metrics—including total abundance, species richness, and other indicator responses—to compare conditions before the bloom (Pre-bloom) with those observed during and after the event (Bloom), these metrics are defined in <b>Table 1</b> below. The threshold levels are defined in <b>Table 2</b>.")),
          # br(),
          p("By integrating standardised, quality-controlled BRUV annotations with clear temporal comparisons, the dashboard helps highlight shifts in community structure and supports evidence-based management decisions.")
        )
      ),
      
      # div(
      #   h4("Portal Aims"),
        # h6(HTML("This portal provides a visual assessment of the ecological impacts of the recent harmful algal bloom using stereo-BRUV data uploaded to GlobalArchive.org. It summarises key fish community metrics—including total abundance, species richness, and other indicator responses—to compare conditions before the bloom (Pre-bloom) with those observed during and after the event (Bloom), these metrics are defined in <b>Table 1</b> below. The threshold levels are defined in <b>Table 2</b>.
        # 
        #        </br></br>By integrating standardised, quality-controlled BRUV annotations with clear temporal comparisons, the dashboard helps highlight shifts in community structure and supports evidence-based management decisions."))),
      
      # br(),
      # 
      # layout_column_wrap(
      #   width = 1/2,
      #   div(
      #     h5("Table 1. Definitions of fish indicator metrics"),
      #     spinnerTableOutput("indicator_table")  # was: tableOutput("indicator_table")
      #   ),
      #   
      #   div(
      #     h5("Table 2. Impact assessment"),
      #     spinnerTableOutput("pointer_table")    # was: tableOutput("pointer_table")
      #   ),
      # )
      # 
      layout_column_wrap(
        width = 1/2,
        
        card(
          card_header("Table 1. Definitions of fish indicator metrics"),
          spinnerTableOutput("indicator_table")  # was: tableOutput("indicator_table")
        ),
        
        card(
          card_header("Table 2. Impact assessment"),
          spinnerTableOutput("pointer_table")    # was: tableOutput("pointer_table")
        )
      )
      
    ),
    
    
    card(
      full_screen = TRUE,
      card_header("Map of sampling locations"),
      div(
        class = "map-full-wrapper",
        withSpinner(
          leafletOutput("map", height = "100%"),
          color = getOption("spinner.color", default = "#0D576E"),
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
      
      h5("Select data:"),
      
      radioButtons(
        inputId  = "method",
        label    = "Choose a method to display:",
        choices  = c("BRUVS", "Dive"),
        inline   = TRUE
      ),
      
      selectizeInput(
        "region",
        "Choose a region:",
        choices = NULL, multiple = FALSE,
        options = list(placeholder = "Choose a region...")
      ),
      
      hr(),
      
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
              color = getOption("spinner.color", default = "#0D576E"),
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
                color = getOption("spinner.color", default = "#0D576E"),
                type = 6
              )
            ),
            div(
              class = "plot-full-wrapper",
              # style = "height:500px;",
              withSpinner(
                plotOutput("region_common_post", height = "100%"),
                color = getOption("spinner.color", default = "#0D576E"),
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
              color = getOption("spinner.color", default = "#0D576E"),
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
                color = getOption("spinner.color", default = "#0D576E"),
                type = 6
              )
            ),
            div(
              class = "plot-full-wrapper",
              withSpinner(
                plotOutput("location_common_post", height = "100%"),
                color = getOption("spinner.color", default = "#0D576E"),
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
