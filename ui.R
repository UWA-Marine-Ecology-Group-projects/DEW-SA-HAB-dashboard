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
  "))
  ),
  
  nav_panel(
    "State Summary",
    
    layout_sidebar(
      sidebar = sidebar(
        width = "45%",
        h4("Reporting Regions (click to explore)"),
          leafletOutput("map", height = "68vh"),
        # ),
        hr(),
        helpText("Instructions: Click a polygon on the map to load detailed plots & summaries."), 

      ),
      
      # ---- MAIN (scrolls) ----
      div(
        class = "container-fluid",
        div(
          class = "row g-3",
          
          div(class = "col-12", uiOutput("region_title")),
          
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header("Overall Impact"),
              plotOutput("overallplot", height = 200)
            ),
            card(
              card_header("Summary"),
              card_body(htmlOutput("summary_text"))
            )
          ),
          
          div(
            class = "col-12",
            card(
              card_header("Indicators"),
              plotOutput("combinedplot", height = 220)
            ),

            # ValueBoxes ----
            layout_column_wrap(
              width = 1/2,
              
              twoValueBoxUI(
                id = "number_bruv_deployments",
                title = "BRUVS deployments",
                icon = icon("ship", class = "fa-xl")
              ),
              
              twoValueBoxUI(
                id = "number_rls_deployments",
                title = "Dive surveys",
                icon = icon("ship", class = "fa-xl")
              ),
              
              twoValueBoxUI(
                id = "fish_counted",
                title = "Fish counted",
                icon = icon("fish-fins", class = "fa-xl")
              ),
              
              twoValueBoxUI(
                id = "fish_species",
                title = "Fish species",
                icon = icon("fish-fins", class = "fa-xl")
              ),
              
              twoValueBoxUI(
                id = "non_fish_species",
                title = "Other species",
                icon = icon("shrimp", class = "fa-xl")
              ),
              
              twoValueBoxUI(
                id = "years",
                title = "Years surveyed",
                icon = icon("calendar", class = "fa-xl")
              ),
              
              twoValueBoxUI(
                id = "depths",
                title = "Depths surveyed",
                icon = icon("arrow-down-up-across-line", class = "fa-xl")
              ),
              
              twoValueBoxUI(
                id = "mean_depth",
                title = "Average depth",
                icon = icon("wave-square", class = "fa-xl")
              )
          ),
            
          card(min_height = 600,
               card_header("Survey Effort"),
               leafletOutput("surveyeffort"))
          )
        )
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
