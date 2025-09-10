# ===== Loading library =====
pkgs <- c(
  "leaflet",
  "shiny",
  "data.table",
  "ggplot2",
  "raster",
  "here",
  "sf",
  "rnaturalearth",
  "htmlwidgets"
)

for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Download
    install.packages(pkg)
  }
  # Loading
  library(pkg, character.only = TRUE)
}

# ===== Set working directory =====
setwd(here("Dependencies"))

# ===== Loading function =====
source("functions.R")

# ===== Loading data =====

# Observation data
dt_obsert <- readRDS("precomputed_observation/obsert_final.rds")


# Load precomputed grid data for performance optimization
cat("\rLoading precomputed grid data...\n")
precomputed_grids <- tryCatch({
  load_precomputed_data("precomputed_grids")
}, error = function(e) {
  cat("\rFailed to load precomputed data:", e$message, "\n")
  cat("\rPlease run precompute_grid_data() to generate precomputed data\n")
  NULL
})

if (!is.null(precomputed_grids)) {
  cat("\rPrecomputed data loaded successfully, zoom levels:", 
      paste(names(precomputed_grids), collapse = ", "), "\n")
} else {
  cat("\rUsing real-time calculation mode (may be slower)\n")
}

# Color palette for taxonomy classification
color_palette <- c("#66a61e", "#f781bf", "#999999")

# Preload seasonal raster data
seasonal_rasters <- tryCatch({
  load_seasonal_rasters()
}, error = function(e) {
  cat("\rSeasonal raster loading failed:", e$message, "\n")
  list()
})

# Define season-month mapping
SEASON_MONTHS <- list(
  spring = c(3, 4, 5),
  summer = c(6, 7, 8),
  autumn = c(9, 10, 11),
  winter = c(12, 1, 2)
)

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

# ===== Server logic =====
server <- function(input, output, session) {
  # Show welcome dialog on startup
  showWelcome()
  
  observeEvent(input$help, {
    showWelcome()
  })
  
  # Check if seasonal raster data is available
  if (length(seasonal_rasters) == 0) {
    showNotification("Seasonal temperature data not found. Temperature layer unavailable.", 
                     type = "warning", duration = 10)
  }
  
  # Get months for current selected season
  get_season_months <- reactive({
    req(input$season_select)
    SEASON_MONTHS[[input$season_select]]
  })
  
  # Update seasonal temperature layer
  observeEvent(
    list(input$show_seasonal_temp,
         input$season_select,
         input$temp_opacity,
         input$temp_color_scheme),
    {
      # Popup a persistent loading notification, duration=NULL means it won't disappear automatically.
      notif_id <- showNotification("Loading Heat map...", type = "message", duration = NULL)
      
      # Make sure to delete the notification once you get out of this observeEvent
      on.exit(removeNotification(notif_id), add = TRUE)
      
      if (input$show_seasonal_temp && input$season_select %in% names(seasonal_rasters)) {
        # Get seasonal raster data
        raster_data <- seasonal_rasters[[input$season_select]]
        vals <- values(raster_data)
        vals_valid <- vals[!is.na(vals)]
        domain_range <- range(vals_valid, finite = TRUE)
        
        # Create color palette
        pal <- switch(input$temp_color_scheme,
                      "RdBu"    = colorNumeric(
                        "RdBu", 
                        domain = domain_range, 
                        reverse = TRUE,
                        na.color = "transparent"),
                      "Spectral"= colorNumeric(
                        "Spectral", 
                        domain = domain_range, 
                        reverse = TRUE,
                        na.color = "transparent"),
                      "rainbow" = colorNumeric(
                        rainbow(100), 
                        domain = domain_range,
                        na.color = "transparent")
        )
        
        # Update map
        leafletProxy("map") %>%
          clearGroup("seasonal_temperature") %>%
          addRasterImage(
            raster_data,
            colors  = pal,
            opacity = input$temp_opacity,
            group   = "seasonal_temperature",
            project = TRUE
          ) %>%
          clearControls() %>%
          addLegend(
            position = "bottomright",
            pal      = pal,
            values   = domain_range,
            title    = paste("Temperature (°C) -", 
                             toupper(substring(input$season_select, 1, 1)),
                             substring(input$season_select, 2)),
            opacity  = 1,
            className = "transparent-legend"
          )
      } else {
        # Clear temperature layer
        leafletProxy("map") %>%
          clearGroup("seasonal_temperature") %>%
          clearControls()
      }
    }
  )
  
  # Store clicked points (max 2)
  clicked_points <- reactiveVal(list())
  
  # Store color mapping for filtered taxonomy
  taxonomy_colors <- reactiveVal(list())
  
  # Clear selection event handlers
  observeEvent(input$clear_points, {
    clicked_points(list())
  })
  
  observeEvent(input$taxonomy_level, {
    clicked_points(list())
    updateSelectizeInput(session, "taxonomy_filter", selected = character(0))
  })
  
  observeEvent(input$top_n, {
    clicked_points(list())
  })
  
  # Dynamically update taxonomy filter options
  observe({
    req(input$taxonomy_level, input$season_select)
    
    taxonomy_col <- input$taxonomy_level
    season_months <- get_season_months()
    
    # Get all available options for current season and taxonomy level
    available_options <- unique(dt_obsert[
      month %in% season_months & !is.na(get(taxonomy_col)),
      get(taxonomy_col)
    ])
    
    available_options <- sort(available_options)
    
    updateSelectizeInput(session, "taxonomy_filter",
                         choices = available_options,
                         selected = input$taxonomy_filter[
                           input$taxonomy_filter %in% available_options
                           ]
                         )
  })
  
  # Update color mapping
  observe({
    req(input$taxonomy_filter)
    colors <- list()
    for(i in seq_along(input$taxonomy_filter)) {
      colors[[input$taxonomy_filter[i]]] <- color_palette[i]
    }
    taxonomy_colors(colors)
  })
  
  # Initialize map
  output$map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        minZoom = 4,
        maxZoom = 9,
        zoomControl = FALSE
      )
    ) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -96, lat = 56, zoom = 5) %>%
      onRender("
      function(el, x) {
        var map = this;
        
        // Initialize zoom level
        Shiny.setInputValue('map_zoom', map.getZoom());
        map.on('zoomend', function() {
          Shiny.setInputValue('map_zoom', map.getZoom());
        });
        
        // Create crosshair elements
        var crosshairV = document.createElement('div');
        crosshairV.className = 'crosshair-v';
        var crosshairH = document.createElement('div');
        crosshairH.className = 'crosshair-h';
        
        document.body.appendChild(crosshairV);
        document.body.appendChild(crosshairH);
        
        // Mouse event handlers
        document.addEventListener('mousemove', function(e) {
          crosshairV.style.left = e.clientX + 'px';
          crosshairH.style.top = e.clientY + 'px';
          crosshairV.style.display = 'block';
          crosshairH.style.display = 'block';
        });
        
        document.addEventListener('mouseenter', function() {
          crosshairV.style.display = 'block';
          crosshairH.style.display = 'block';
        });
        
        document.addEventListener('mouseleave', function() {
          crosshairV.style.display = 'none';
          crosshairH.style.display = 'none';
        });
        
        window.addEventListener('blur', function() {
          crosshairV.style.display = 'none';
          crosshairH.style.display = 'none';
        });
        
        window.addEventListener('focus', function() {
          setTimeout(function() {
            if (document.querySelector(':hover')) {
              crosshairV.style.display = 'block';
              crosshairH.style.display = 'block';
            }
          }, 50);
        });
      }
    ")
  })
  
  # Calculate grid distance based on zoom level
  get_distance <- reactive({
    req(input$map_zoom)
    result <- 2^(5 - input$map_zoom)
    return(result)
  })
  

  
  # Calculate grid point data
  adjust_point <- reactive({
    req(input$map_zoom, input$season_select, input$taxonomy_level, input$opacity_threshold)
    
    season_months <- get_season_months()
    
    # Try precomputed data first
    if (!is.null(precomputed_grids)) {
      tryCatch({
        result <- query_grid_data(
          precomputed_data = precomputed_grids,
          zoom_level = input$map_zoom,
          taxonomy_level = input$taxonomy_level,
          month_range = c(min(season_months), max(season_months)), 
          taxonomy_filter = input$taxonomy_filter,
          opacity_threshold = input$opacity_threshold,
          top_n = input$top_n
        )
        if (nrow(result) > 0) {
          showNotification("Using precomputed data", type = "message", duration = 1)
          return(result)
        }
      }, error = function(e) {
        # Fall back to real-time calculation
      })
    }
    
    # Real-time calculation fallback
    showNotification("Using real-time calculation", type = "message", duration = 1)
    distance <- get_distance()
    k <- input$opacity_threshold
    taxonomy_col <- input$taxonomy_level
    valid_vals <- unique(dt_obsert[[taxonomy_col]])
    has_filter <- !is.null(input$taxonomy_filter) &&
                  length(input$taxonomy_filter) > 0 &&
                  all(input$taxonomy_filter %in% valid_vals)
    
    if (!has_filter) {
      # No filter mode: show all data
      dt_with_grid <- dt_obsert[
        month %in% season_months
      ][, `:=`(
        grid_lat = floor(latitude / distance + 0.5) * distance,
        grid_lon = floor(longitude / distance + 0.5) * distance
      )]
      
      # Group by month and taxonomy
      grid_month_taxonomy <- dt_with_grid[
        ,c("month", "grid_lat", "grid_lon", taxonomy_col), with = FALSE
      ][, .(
        lat = grid_lat,
        lon = grid_lon,
        month,
        taxonomy = get(taxonomy_col)
      )][,
         .(count_per_month = .N),
         by = .(lat, lon, month, taxonomy)
      ]
      
      # Aggregate taxonomy counts
      grid_taxonomy <- grid_month_taxonomy[
        ,.(taxonomy_count = sum(count_per_month)),
        by = .(lat, lon, taxonomy)
      ]
      
      # Generate final result
      result <- grid_taxonomy[
        ,{
          total <- sum(taxonomy_count)
          scale_total <- sum(taxonomy_count)^(1/4) * 14 * input$map_zoom
          opacity <- calculate_opacity(total, k)
          
          # Get top 5 most abundant taxonomies
          top5 <- head(taxonomy[order(-taxonomy_count)], 5)
          top5_counts <- head(taxonomy_count[order(-taxonomy_count)], 5)
          top_taxonomy_text <- paste(top5, top5_counts, sep = ": ", collapse = "<br>")
          
          # Calculate grid boundaries
          lat_min <- lat - distance/2
          lat_max <- lat + distance/2
          lon_min <- lon - distance/2
          lon_max <- lon + distance/2
          
          .(radius = scale_total,
            total_count = total,
            opacity = opacity,
            fillColor = "black",
            top_taxonomy = top_taxonomy_text,
            lat_min = lat_min,
            lat_max = lat_max,
            lon_min = lon_min,
            lon_max = lon_max,
            distance = distance,
            filter_mode = FALSE)
        },
        by = .(lat, lon)
      ]
    } else {
      # Filter mode: show only selected taxonomies
      colors <- taxonomy_colors()
      
      dt_with_grid <- dt_obsert[
        month %in% season_months &
          get(taxonomy_col) %in% input$taxonomy_filter
      ][, `:=`(
        grid_lat = floor(latitude / distance + 0.5) * distance,
        grid_lon = floor(longitude / distance + 0.5) * distance
      )]
      
      # Count filtered taxonomies
      grid_taxonomy <- dt_with_grid[
        , c("grid_lat", "grid_lon", taxonomy_col), with = FALSE
      ][, .(
        lat = grid_lat,
        lon = grid_lon,
        taxonomy = get(taxonomy_col)
      )][,
         .(taxonomy_count = as.integer(.N)),
         by = .(lat, lon, taxonomy)
      ]
      
      # Generate final result
      result <- grid_taxonomy[
        ,{
          total <- sum(taxonomy_count)
          scale_total <- sum(taxonomy_count)^(1/4) * 14 * input$map_zoom
          opacity <- calculate_opacity(total, k)
          
          # Determine dominant taxonomy and its color
          dominant_taxonomy <- taxonomy[which.max(taxonomy_count)]
          dominant_color <- colors[[dominant_taxonomy]]
          
          # Generate filtered statistics text
          filtered_stats <- paste(taxonomy, taxonomy_count, sep = ": ", collapse = "<br>")
          
          # Calculate grid boundaries
          lat_min <- lat - distance/2
          lat_max <- lat + distance/2
          lon_min <- lon - distance/2
          lon_max <- lon + distance/2
          
          .(radius = scale_total,
            total_count = total,
            opacity = opacity,
            fillColor = dominant_color,
            top_taxonomy = filtered_stats,
            lat_min = lat_min,
            lat_max = lat_max,
            lon_min = lon_min,
            lon_max = lon_max,
            distance = distance,
            filter_mode = TRUE,
            dominant_taxonomy = dominant_taxonomy)
        },
        by = .(lat, lon)
      ]
    }
    
    return(result)
  })
  
  # Update map markers
  observeEvent(
    list(input$map_zoom,
         input$season_select,
         input$taxonomy_level,
         input$taxonomy_filter,
         input$opacity_threshold),
    {
      req(adjust_point())
      
      df <- as.data.frame(adjust_point())
      taxonomy_label <- toupper(input$taxonomy_level)
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(
          data       = df,
          lng        = ~lon,
          lat        = ~lat,
          radius     = ~ifelse(
            filter_mode,
            # Filter mode: increase marker size
            pmax(6, pmin(radius/100 * 1.5, 35)),
            # Normal mode: keep original size
            pmax(3, pmin(radius/100, 25))
          ),
          color      = "black",
          weight     = 1,
          fillColor  = ~fillColor,
          fillOpacity= ~opacity,
          popup      = ~paste(
            "<b>Grid Information</b><br>",
            "Center: (", round(lat, 4), ", ", round(lon, 4), ")<br>",
            "Distance: ", round(distance, 4), "°<br><br>",
            "<b>Coverage:</b><br>",
            "Lat: ", round(lat_min, 4), "° ~ ", round(lat_max, 4), "°<br>",
            "Lon: ", round(lon_min, 4), "° ~ ", round(lon_max, 4), "°<br><br>",
            "<b>Observations:</b><br>",
            "Total: ", total_count, "<br>",
            "Opacity: ", opacity, "<br><br>",
            "<b>", 
            ifelse(filter_mode, "Filtered", "Top"), " ", taxonomy_label, ":</b><br>",
            top_taxonomy
          ),
          layerId = ~paste(round(lat, 6), round(lon, 6), sep = "_")
        )
    }
  )
  
  # Handle map marker click events
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click)) {
      coords <- strsplit(click$id, "_")[[1]]
      clicked_lat <- as.numeric(coords[1])
      clicked_lon <- as.numeric(coords[2])
      new_point <- c(clicked_lat, clicked_lon)
      
      current_points <- clicked_points()
      
      # Check for duplicate points
      is_duplicate <- any(sapply(current_points, function(p) {
        abs(p[1] - new_point[1]) < 1e-5 && abs(p[2] - new_point[2]) < 1e-5
      }))
      
      if (!is_duplicate) {
        if (length(current_points) < 2) {
          clicked_points(c(current_points, list(new_point)))
        } else {
          # Keep second point, new point becomes second point
          clicked_points(list(current_points[[2]], new_point))
        }
      }
    }
  })
  
  # Data for first area plot
  area_plot_data1 <- reactive({
    req(input$taxonomy_level, input$top_n, input$season_select)
    current_points <- clicked_points()
    taxonomy_col <- input$taxonomy_level
    top_n <- input$top_n
    distance <- get_distance()
    season_months <- get_season_months()
    
    has_filter <- !is.null(input$taxonomy_filter) && length(input$taxonomy_filter) > 0
    
    if (length(current_points) == 0) {
      # Global data
      if (!has_filter) {
        # Calculate global Top N
        global_stats <- dt_obsert[
          month %in% season_months,
          c("month", taxonomy_col), with = FALSE
        ][, .(N = as.integer(.N)), by = taxonomy_col][order(-N)]
        
        global_stats_clean <- global_stats[!is.na(get(taxonomy_col))]
        if (nrow(global_stats_clean) == 0) {
          empty_dt <- data.table(month = integer(0), N = integer(0))
          empty_dt[, (taxonomy_col) := character(0)]
          return(empty_dt)
        }
        
        actual_n <- min(top_n, nrow(global_stats_clean))
        global_topN <- global_stats_clean[1:actual_n][[taxonomy_col]]
        
        plot_data <- dt_obsert[
          month %in% season_months & 
            get(taxonomy_col) %in% global_topN,
          c("month", taxonomy_col), with = FALSE
        ][, .(N = as.integer(.N)), by = c("month", taxonomy_col)]
        
        used_taxonomy <- global_topN
      } else {
        # Filtered data
        plot_data <- dt_obsert[
          month %in% season_months & 
          get(taxonomy_col) %in% input$taxonomy_filter,
          c("month", taxonomy_col), with = FALSE
        ][, .(N = as.integer(.N)), by = c("month", taxonomy_col)]
        
        used_taxonomy <- input$taxonomy_filter
      }
    } else {
      # First point data
      clicked_coords <- current_points[[1]]
      
      # Try precomputed data
      precomputed_result <- get_grid_observations_for_plot(
        clicked_coords, 
        input$map_zoom, 
        season_months, 
        input$taxonomy_level, 
        input$taxonomy_filter, 
        input$top_n,
        precomputed_grids
      )
      
      if (!is.null(precomputed_result)) {
        plot_data <- precomputed_result
        used_taxonomy <- unique(plot_data[[taxonomy_col]])
      } else {
        # Real-time calculation
        dt_filtered <- dt_obsert[
          month %in% season_months
        ][, `:=`(
          grid_lat = floor(latitude / distance + 0.5) * distance,
          grid_lon = floor(longitude / distance + 0.5) * distance
        )][
          abs(grid_lat - clicked_coords[1]) < 1e-5 & 
            abs(grid_lon - clicked_coords[2]) < 1e-5
        ]
        
        if (!has_filter) {
          # Calculate Top N for this point
          point_stats <- dt_filtered[, 
                                     c("month", "latitude", "longitude", taxonomy_col), with = FALSE
          ][, .(N = as.integer(.N)), by = taxonomy_col][order(-N)]
          
          point_stats_clean <- point_stats[!is.na(get(taxonomy_col))]
          if (nrow(point_stats_clean) == 0) {
            empty_dt <- data.table(month = integer(0), N = integer(0))
            empty_dt[, (taxonomy_col) := character(0)]
            return(empty_dt)
          }
          
          actual_n <- min(top_n, nrow(point_stats_clean))
          point_topN <- point_stats_clean[1:actual_n][[taxonomy_col]]
          
          plot_data <- dt_filtered[
            get(taxonomy_col) %in% point_topN,
            c("month", taxonomy_col), with = FALSE
          ][, .(N = as.integer(.N)), by = c("month", taxonomy_col)]
          
          used_taxonomy <- point_topN
        } else {
          # Filtered data
          plot_data <- dt_filtered[
            get(taxonomy_col) %in% input$taxonomy_filter,
            c("month", taxonomy_col), with = FALSE
          ][, .(N = as.integer(.N)), by = c("month", taxonomy_col)]
          
          used_taxonomy <- input$taxonomy_filter
        }
      }
    }
    
    # Ensure all seasonal months are present
    complete_data <- CJ(month = season_months, taxonomy = used_taxonomy)
    setnames(complete_data, "taxonomy", taxonomy_col)
    complete_data <- merge(complete_data, plot_data, by = c("month", taxonomy_col), all.x = TRUE)
    complete_data[is.na(N), N := 0L]
    
    # Add color information (if in filter mode)
    if (has_filter) {
      colors <- taxonomy_colors()
      complete_data[, color := colors[[get(taxonomy_col)]], by = taxonomy_col]
    }
    
    return(complete_data)
  })
  
  # Data for second area plot
  area_plot_data2 <- reactive({
    req(input$taxonomy_level, input$top_n, input$season_select)
    current_points <- clicked_points()
    taxonomy_col <- input$taxonomy_level
    top_n <- input$top_n
    distance <- get_distance()
    season_months <- get_season_months()
    
    has_filter <- !is.null(input$taxonomy_filter) && length(input$taxonomy_filter) > 0
    
    if (length(current_points) < 2) {
      # No second point
      empty_dt <- data.table(month = integer(0), N = integer(0))
      empty_dt[, (taxonomy_col) := character(0)]
      return(empty_dt)
    } else {
      # Second point data
      clicked_coords <- current_points[[2]]
      
      # Try precomputed data
      precomputed_result <- get_grid_observations_for_plot(
        clicked_coords, 
        input$map_zoom, 
        season_months, 
        input$taxonomy_level, 
        input$taxonomy_filter, 
        input$top_n,
        precomputed_grids
      )
      
      if (!is.null(precomputed_result)) {
        plot_data <- precomputed_result
        used_taxonomy <- unique(plot_data[[taxonomy_col]])
      } else {
        # Real-time calculation
        dt_filtered <- dt_obsert[
          month %in% season_months
        ][, `:=`(
          grid_lat = floor(latitude / distance + 0.5) * distance,
          grid_lon = floor(longitude / distance + 0.5) * distance
        )][
          abs(grid_lat - clicked_coords[1]) < 1e-5 & 
            abs(grid_lon - clicked_coords[2]) < 1e-5
        ]
        
        if (!has_filter) {
          # Calculate Top N for this point
          point_stats <- dt_filtered[
            , c("month", "latitude", "longitude", taxonomy_col), with = FALSE
          ][, .(N = as.integer(.N)), by = taxonomy_col][order(-N)]
          
          point_stats_clean <- point_stats[!is.na(get(taxonomy_col))]
          if (nrow(point_stats_clean) == 0) {
            empty_dt <- data.table(month = integer(0), N = integer(0))
            empty_dt[, (taxonomy_col) := character(0)]
            return(empty_dt)
          }
          
          actual_n <- min(top_n, nrow(point_stats_clean))
          point_topN <- point_stats_clean[1:actual_n][[taxonomy_col]]
          
          plot_data <- dt_filtered[
            get(taxonomy_col) %in% point_topN,
            c("month", taxonomy_col), with = FALSE
          ][, .(N = as.integer(.N)), by = c("month", taxonomy_col)]
          
          used_taxonomy <- point_topN
        } else {
          # Filtered data
          plot_data <- dt_filtered[
            get(taxonomy_col) %in% input$taxonomy_filter,
            c("month", taxonomy_col), with = FALSE
          ][, .(N = as.integer(.N)), by = c("month", taxonomy_col)]
          
          used_taxonomy <- input$taxonomy_filter
        }
      }
      
      # Ensure all seasonal months are present
      complete_data <- CJ(month = season_months, taxonomy = used_taxonomy)
      setnames(complete_data, "taxonomy", taxonomy_col)
      complete_data <- merge(complete_data, plot_data, by = c("month", taxonomy_col), all.x = TRUE)
      complete_data[is.na(N), N := 0L]
      
      # Add color information (if in filter mode)
      if (has_filter) {
        colors <- taxonomy_colors()
        complete_data[, color := colors[[get(taxonomy_col)]], by = taxonomy_col]
      }
      
      return(complete_data)
    }
  })
  
  # Plot titles
  output$plot1_title <- renderText({
    current_points <- clicked_points()
    taxonomy_level <- toupper(input$taxonomy_level)
    top_n <- input$top_n
    has_filter <- !is.null(input$taxonomy_filter) && length(input$taxonomy_filter) > 0
    season_name <- toupper(substring(input$season_select, 1, 1)) %>% 
      paste0(substring(input$season_select, 2))
    
    if (length(current_points) == 0) {
      if (has_filter) {
        paste0("Global Filtered ", taxonomy_level, " (", season_name, ")")
      } else {
        paste0("Global Top ", top_n, " ", taxonomy_level, " (", season_name, ")")
      }
    } else {
      coords <- current_points[[1]]
      if (has_filter) {
        paste0("Point 1 (", round(coords[1], 3), ", ", round(coords[2], 3), ") - Filtered ", taxonomy_level)
      } else {
        paste0("Point 1 (", round(coords[1], 3), ", ", round(coords[2], 3), ") - Top ", top_n, " ", taxonomy_level)
      }
    }
  })
  
  output$plot2_title <- renderText({
    current_points <- clicked_points()
    taxonomy_level <- toupper(input$taxonomy_level)
    top_n <- input$top_n
    has_filter <- !is.null(input$taxonomy_filter) && length(input$taxonomy_filter) > 0
    
    if (length(current_points) < 2) {
      "Please select a second point"
    } else {
      coords <- current_points[[2]]
      if (has_filter) {
        paste0("Point 2 (", round(coords[1], 3), ", ", round(coords[2], 3), ") - Filtered ", taxonomy_level)
      } else {
        paste0("Point 2 (", round(coords[1], 3), ", ", round(coords[2], 3), ") - Top ", top_n, " ", taxonomy_level)
      }
    }
  })
  
  # Render first area plot
  output$area_plot1 <- renderPlot({
    data1        <- area_plot_data1()
    taxonomy_col <- input$taxonomy_level
    
    if (nrow(data1) == 0) {
      ggplot() +
        theme_void() +
        geom_text(aes(x=0.5, y=0.5, label="No data in this area"),
                  size=4, color="gray60")
    } else {
      # Create month labels
      month_vals   <- sort(unique(data1$month))
      data1$monthF <- factor(data1$month,
                             levels = month_vals,
                             labels = month.abb[month_vals])
      
      p <- ggplot(data1, aes(
        x     = monthF,
        y     = N,
        fill  = .data[[taxonomy_col]],
        group = .data[[taxonomy_col]]
      )) +
        geom_area(
          stat     = "identity",
          position = position_stack(),
          alpha    = 0.7
        ) +
        labs(
          x    = "Month",
          y    = "Observation Count",
          fill = toupper(taxonomy_col)
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom",
              panel.background    = element_rect(fill = "transparent", colour = NA),
              plot.background     = element_rect(fill = "transparent", colour = NA))
      
      # Apply custom colors in filter mode
      if (!is.null(input$taxonomy_filter) &&
          length(input$taxonomy_filter) > 0) {
        
        present <- unique(as.character(data1[[taxonomy_col]]))
        cols    <- taxonomy_colors()[present]
        p <- p + scale_fill_manual(values = cols, drop = TRUE)
      }
      
      print(p)
    }
  }, bg = "transparent")
  
  # Render second area plot
  output$area_plot2 <- renderPlot({
    data2        <- area_plot_data2()
    taxonomy_col <- input$taxonomy_level
    
    if (nrow(data2) == 0) {
      ggplot() +
        theme_void() +
        geom_text(
          aes(x = 0.5, y = 0.5, label = "Click map to select second point"),
          size = 4, color = "gray60"
        )
    } else {
      # Create month labels
      month_vals   <- sort(unique(data2$month))
      data2$monthF <- factor(
        data2$month,
        levels = month_vals,
        labels = month.abb[month_vals]
      )
      
      p2 <- ggplot(
        data2,
        aes(
          x     = monthF,
          y     = N,
          fill  = .data[[taxonomy_col]],
          group = .data[[taxonomy_col]]
        )
      ) +
        geom_area(
          stat     = "identity",
          position = position_stack(),
          alpha    = 0.7
        ) +
        labs(
          x    = "Month",
          y    = "Observation Count",
          fill = toupper(taxonomy_col)
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom",
              panel.background    = element_rect(fill = "transparent", colour = NA),
              plot.background     = element_rect(fill = "transparent", colour = NA))
      
      # Apply custom colors in filter mode
      if (!is.null(input$taxonomy_filter) &&
          length(input$taxonomy_filter) > 0) {
        
        present <- unique(as.character(data2[[taxonomy_col]]))
        cols    <- taxonomy_colors()[present]
        p2 <- p2 + scale_fill_manual(values = cols, drop = TRUE)
      }
      
      p2
    }
  }, bg = "transparent")
}

# ===== Running Application =====
shinyApp(ui, server)