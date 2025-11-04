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
  
  tags$head(tags$style(HTML("
  /* Make the LEFT map column sticky (frozen), right column scrolls */
  @media (min-width: 992px) {
    .col-xl-6:first-child, 
    .col-lg-6:first-child {
      position: sticky;
      top: calc(var(--bslib-navbar-height, 56px) + 12px);
      height: calc(100vh - var(--bslib-navbar-height, 56px) - 24px);
      overflow: hidden; /* prevent double scrollbars */
    }

    /* Allow the right column to scroll normally */
    .col-xl-6:last-child, 
    .col-lg-6:last-child {
      overflow-y: auto;
      max-height: calc(100vh - var(--bslib-navbar-height, 56px) - 24px);
    }
  }
"))),
  
  nav_panel(
    "State Summary",
    
    layout_sidebar(
      sidebar = sidebar(
        h4("Instructions"),
        helpText("Click a polygon on the map to load plots & summary."),
        hr()
      ),
      
      
      layout_columns(
        
        col_widths = c(6, 6),
        
        card(
          card_header("Reporting Regions (click to explore)"),
          card_body(leafletOutput("map", height = 500))
        ),
        
        div(
          
          uiOutput("region_title"),
          
          layout_columns(
            
            col_widths = c(6, 6),
            
            card(
              card_header("Overall Impact"),
              plotOutput("overallplot", height = 200)),
            
            card(card_header("Summary"),
                 card_body(htmlOutput("summary_text")))),
          
          
          card(
            card_header("Indicators"),
            plotOutput("combinedplot", 
                       height = 200)
          ),
          
          card(min_height = 200),
          card(min_height = 200),
          card(min_height = 200),
          card(min_height = 200),
          card(min_height = 200),
          card(min_height = 200),
          card(min_height = 200),
          card(min_height = 200),
          card(min_height = 200),
          card(min_height = 200)
          
          
        )
      )
      
    )),
  
  nav_spacer(),
  
  # Add logos to the top right corner
  nav_item(
    tags$div(
      style = "display: flex; gap: 10px; align-items: center; padding-right: 15px;",
      tags$img(src = "dew_logo.png", height = "70px")
      
    )
  )
)