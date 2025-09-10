# ===== UI definition =====
ui <- fluidPage(
  # Map container
  leafletOutput("map", width = "100%", height = "100vh"),
  
  # Help button
  actionButton(
    "help", 
    "Help",
    style = "
    position: absolute;
    top: 10px; 
    right: 10px; 
    z-index: 1001;
    pointer-events: auto;
  "
  ),
  
  # Control panel
  div(
    style = "position: absolute; 
             top: 0; 
             left: 0; 
             right: 0; 
             background-color: transparent; 
             padding: 20px; 
             z-index: 1000;
             height: 400px;
             pointer-events: none;",
    fluidRow(
      # Chart 1: Display first point or global data
      column(4,
             style = "pointer-events: auto;",
             h5(textOutput("plot1_title")),
             plotOutput("area_plot1", height = "180px")
      ),
      # Chart 2: Display second point data
      column(4,
             style = "pointer-events: auto;",
             h5(textOutput("plot2_title")),
             plotOutput("area_plot2", height = "180px")
      ),
      # Season and taxonomy filters
      column(2,
             style = "pointer-events: auto;",
             h5("Season Filter"),
             radioButtons("season_select", 
                          label = NULL,
                          choices = list(
                            "Spring (Mar-May)" = "spring",
                            "Summer (Jun-Aug)" = "summer",
                            "Autumn (Sep-Nov)" = "autumn",
                            "Winter (Dec-Feb)" = "winter"
                          ),
                          selected = "summer"),
             br(),
             h6("Taxonomy Level"),
             radioButtons("taxonomy_level", 
                          label = NULL,
                          choices = list("Genus" = "genus", "Family" = "family"),
                          selected = "genus",
                          inline = TRUE),
             br(),
             h6("Filter Taxonomy"),
             selectizeInput("taxonomy_filter",
                            label = NULL,
                            choices = NULL,
                            multiple = TRUE,
                            options = list(
                              maxItems = 3,
                              placeholder = "Select up to 3..."
                            ))
      ),
      # Display settings
      column(2,
             style = "pointer-events: auto;",
             h6("Show Top N"),
             sliderInput("top_n", 
                         label = NULL,
                         min = 1, 
                         max = 3, 
                         value = 3,
                         step = 1,
                         round = TRUE),
             br(),
             h6("Opacity Threshold"),
             sliderInput("opacity_threshold", 
                         label = NULL,
                         min = 0, 
                         max = 500, 
                         value = 100,
                         step = 10,
                         round = TRUE),
             br(),
             actionButton("clear_points", "Clear Selection", size = "sm")
      ),
      # Temperature layer controls
      column(2,
             style = "pointer-events: auto;",
             h6("Temperature Layer"),
             checkboxInput("show_seasonal_temp", 
                           label = "Show Seasonal Temp", 
                           value = FALSE),
             conditionalPanel(
               condition = "input.show_seasonal_temp == true",
               sliderInput("temp_opacity", 
                           label = "Temperature Opacity",
                           min = 0.1, max = 1.0, 
                           value = 0.6, step = 0.1),
               selectInput("temp_color_scheme",
                           label = "Color Scheme",
                           choices = list(
                             "Blue-White-Red" = "RdBu",
                             "Spectral" = "Spectral", 
                             "Rainbow" = "rainbow"
                           ),
                           selected = "RdBu")
             )
      )
    )
  ),
  
  # CSS styles
  tags$style("
    body { margin: 0; padding: 0; }
    
    /* Hide mouse cursor and show custom crosshair */
    body, .leaflet-container, .shiny-input-container, 
    .btn, .form-control, input, button, select {
      cursor: none !important;
    }
    
    * {
      cursor: none !important;
    }
    
    /* Crosshair styles */
    .crosshair-v, .crosshair-h {
      position: fixed;
      background-color: gray;
      opacity: 0.6;
      pointer-events: none;
      z-index: 9999;
      display: none;
    }
    
    .crosshair-v {
      width: 1px;
      height: 100vh;
      top: 0;
    }
    
    .crosshair-h {
      height: 1px;
      width: 100vw;
      left: 0;
    }
    
    /* Grid point hover effects */
    .leaflet-interactive:hover {
      stroke: black !important;
      stroke-width: 3 !important;
      stroke-opacity: 0.8 !important;
      fill-opacity: 0.9 !important;
    }
    
    /* Control hover effects */
    .btn:hover, .form-control:hover {
      background-color: rgba(0, 123, 255, 0.1) !important;
      border-color: rgba(0, 123, 255, 0.3) !important;
    }
    
    .slider-input:hover {
      opacity: 0.9 !important;
    }
  "),
  tags$style(HTML("
    .leaflet-container {
      background-color: transparent !important;
    }
    .transparent-legend {
      background: transparent !important;
      box-shadow: none !important;
      border: none !important;
    }
    html, body, .shiny-container-fluid {
      background-color: #d4dadc !important;
    }
  "))
)