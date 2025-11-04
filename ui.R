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
        h4("Instructions"),
        helpText("Click a polygon on the map to load plots & summary."),
        hr()
      ),
      
      
      layout_columns(
        col_widths = c(7, 5),
        card(
                      card_header("Reporting Regions (click to explore)"),
                      card_body(leafletOutput("map", height = 500))
        ),
        div(
          uiOutput("selected_region_badge"),
          card(                  card_header("Summary"),
                                 card_body(htmlOutput("summary_text"))),
          card(                card(
            card_header("Diversity"),
            card_body(plotOutput("divplot", height = 250))
          ),
          card(
            card_header("Abundance"),
            card_body(plotOutput("abplot", height = 250))
          ),
          card(
            card_header("Habitat"),
            card_body(plotOutput("habplot", height = 250))
          ))
        )
      )
      
      # # Main content
      # div(
      #   class = "container-fluid",
      #   # Map row
      #   div(class = "row",
      #       div(class = "col-md-12",
      #           card(
      #             card_header("Reporting Regions (click to explore)"),
      #             card_body(leafletOutput("map", height = 500))
      #           )
      #       )
      #   ),
      #   # Detail row
      #   
      #   #,
      #   uiOutput("selected_region_badge"),
      #   
      #   div(class = "row g-3 mt-3",
      #       div(class = "col-lg-6",
      #           card(
                  # card_header("Summary"),
                  # card_body(htmlOutput("summary_text"))
      #           )
      #       ),
      #       div(class = "col-lg-6",
                # card(
                #   card_header("Diversity"),
                #   card_body(plotOutput("divplot", height = 250))
                # ),
                # card(
                #   card_header("Abundance"),
                #   card_body(plotOutput("abplot", height = 250))
                # ),
                # card(
                #   card_header("Habitat"),
                #   card_body(plotOutput("habplot", height = 250))
                # )
      #       )
      #   )
      # ))
      
      # # --- Main content (two columns) ---
      # div(
      #   class = "container-fluid",
      #   
      #   # Two-column row: map (left), details (right)
      #   div(class = "row g-3",
      #       
      #       # LEFT: Map column
      #       div(class = "col-xl-7 col-lg-7",
      #           card(
      #             card_header("Reporting Regions (click to explore)"),
      #             card_body(leafletOutput("map", height = 600))
      #           )
      #       ),
      #       
      #       # RIGHT: Details column (badge + summary + 3 plots)
      #       div(class = "col-xl-5 col-lg-5",
      #           # Badge (kept outside a card so it feels like a status chip)
      #           uiOutput("selected_region_badge"),
      #           
      #           # Summary
      #           card(
      #             card_header("Summary"),
      #             card_body(htmlOutput("summary_text"))
      #           ),
      #           
      #           # Plots (stacked cards)
      #           card(
      #             card_header("Diversity"),
      #             card_body(plotOutput("divplot", height = 250))
      #           ),
      #           card(
      #             card_header("Abundance"),
      #             card_body(plotOutput("abplot", height = 250))
      #           ),
      #           card(
      #             card_header("Habitat"),
      #             card_body(plotOutput("habplot", height = 250))
      #           )
      #       )
      #   )
      # )
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