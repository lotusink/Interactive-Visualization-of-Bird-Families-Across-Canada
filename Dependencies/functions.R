# ================== Single-month raster calculations =====================
# Batch-generate monthly raster data for Canada
generate_monthly_rasters <- function(
    dt_temp,
    smooth_factor = 4,
    output_dir = "raster_data",
    prefix = "canada_temp_month"
) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }
  
  message("Fetching Canada boundary data...")
  canada_sf <- ne_countries(country = "Canada", scale = "large", returnclass = "sf")
  
  raster_list <- vector("list", 12)
  names(raster_list) <- sprintf("month_%02d", 1:12)
  
  for (m in 1:12) {
    message("Processing month ", m, "...")
    df <- dt_temp[month == m]
    if (nrow(df) == 0) {
      warning("Month ", m, " has no data, skipping.")
      next
    }
    
    df[, temp_c := if (max(avg_temp_month, na.rm = TRUE) > 200)
      avg_temp_month - 273.15 else avg_temp_month]
    
    r_orig <- rasterFromXYZ(df[, .(lon, lat, temp_c)], crs = CRS("+proj=longlat +datum=WGS84"))
    r_interp <- resample(
      r_orig,
      raster(extent(r_orig), resolution = res(r_orig) / smooth_factor, crs = crs(r_orig)),
      method = "bilinear"
    )
    
    canada_proj <- st_transform(canada_sf, crs = st_crs(r_interp))
    r_crop <- crop(r_interp, canada_proj)
    r_mask <- mask(r_crop, canada_proj)
    
    if (all(is.na(values(r_mask)))) {
      warning("Month ", m, " mask resulted in all NA, using crop output.")
      r_mask <- r_crop
    }
    
    raster_list[[m]] <- r_mask
    filename <- file.path(output_dir, paste0(prefix, "_", sprintf("%02d", m), ".rds"))
    saveRDS(r_mask, filename)
    message(
      sprintf(
        "Month %02d saved: %s (res: %s, temp range: %.1f°C–%.1f°C)",
        m, filename,
        paste(res(r_mask), collapse = ", "),
        min(values(r_mask), na.rm = TRUE),
        max(values(r_mask), na.rm = TRUE)
      )
    )
  }
  
  all_file <- file.path(output_dir, paste0(prefix, "_all_months.rds"))
  saveRDS(raster_list, all_file)
  message("All rasters saved to: ", all_file)
  raster_list
}

# Load a single month's raster
load_monthly_raster <- function(
    month_num,
    input_dir = "raster_data",
    prefix = "canada_temp_month"
) {
  path <- file.path(input_dir, paste0(prefix, "_", sprintf("%02d", month_num), ".rds"))
  if (file.exists(path)) {
    readRDS(path)
  } else {
    warning("File not found: ", path)
    NULL
  }
}

# Load all monthly rasters
load_all_monthly_rasters <- function(
    input_dir = "raster_data",
    prefix = "canada_temp_month"
) {
  all_file <- file.path(input_dir, paste0(prefix, "_all_months.rds"))
  if (file.exists(all_file)) {
    readRDS(all_file)
  } else {
    warning("Aggregated file not found, loading individual months...")
    rasters <- list()
    for (i in 1:12) {
      r <- load_monthly_raster(i, input_dir, prefix)
      if (!is.null(r)) rasters[[sprintf("month_%02d", i)]] <- r
    }
    rasters
  }
}


# ========================== Temperature raster calculations by season =======================
# Precompute seasonal temperature rasters
precompute_seasonal_rasters <- function(
    raster_dir  = "raster_data",
    prefix      = "canada_temp_month",
    out_dir     = "precomputed_seasonal_rasters"
) {
  # Create output directory
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    cat("\rCreated output directory:", out_dir, "\n")
  }
  
  # Read aggregated monthly rasters
  all_file <- file.path(raster_dir, paste0(prefix, "_all_months.rds"))
  if (!file.exists(all_file)) {
    stop("Aggregated raster file not found: ", all_file)
  }
  rasters <- readRDS(all_file)
  
  # Define seasonal mappings
  seasons <- list(
    spring = list(months = c(3, 4, 5), name = "Spring (Mar-May)"),
    summer = list(months = c(6, 7, 8), name = "Summer (Jun-Aug)"),
    autumn = list(months = c(9, 10, 11), name = "Autumn (Sep-Nov)"),
    winter = list(months = c(12, 1, 2), name = "Winter (Dec-Feb)")
  )
  
  # Process each season
  for (season_key in names(seasons)) {
    season_info <- seasons[[season_key]]
    outfile <- file.path(out_dir, paste0("seasonal_", season_key, ".rds"))
    
    # Skip if already exists
    if (file.exists(outfile)) {
      cat("\rSkip existing:", season_key, "\n")
      next
    }
    
    cat("\rPrecomputing", season_info$name, "raster...\n")
    
    # Extract rasters for seasonal months
    month_keys <- sprintf("month_%02d", season_info$months)
    sel <- rasters[month_keys]
    
    # Handle missing months
    valid_sel <- sel[!sapply(sel, is.null)]
    if (length(valid_sel) == 0) {
      cat("\rNo data for", season_info$name, "\n")
      next
    }
    
    # Calculate mean across months
    mean_r <- if (length(valid_sel) == 1) {
      valid_sel[[1]]
    } else {
      s <- stack(valid_sel)
      calc(s, fun = function(x) mean(x, na.rm = TRUE))
    }
    
    # Trim NA edges
    mean_r <- trim(mean_r)
    
    # Save result
    saveRDS(mean_r, outfile)
    cat("\rSaved seasonal_", season_key, ".rds\n")
  }
  
  cat("\rAll seasonal rasters precomputed in", out_dir, "\n")
}
# ==================== Aggregated data under Family and Genus =============
# Precompute grid aggregations for multiple zoom levels
precompute_grid_data <- function(
    dt_obsert,
    zoom_levels = 4:9,
    output_dir = "precomputed_data"
) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  required <- c("latitude", "longitude", "month", "genus", "family")
  missing <- setdiff(required, names(dt_obsert))
  if (length(missing)) stop("Missing required columns: ", paste(missing, collapse = ", "))
  
  dt <- dt_obsert[!is.na(latitude) & !is.na(longitude) & month %between% c(1, 12)]
  
  results <- list()
  for (z in zoom_levels) {
    d <- 2^(5 - z)
    dt2 <- copy(dt)
    dt2[, `:=`(
      grid_lat = floor(latitude / d + 0.5) * d,
      grid_lon = floor(longitude / d + 0.5) * d,
      lat_min = grid_lat - d/2,
      lat_max = grid_lat + d/2,
      lon_min = grid_lon - d/2,
      lon_max = grid_lon + d/2
    )]
    
    genus_monthly <- dt2[!is.na(genus), .(count = .N), by = .(grid_lat, grid_lon, lat_min, lat_max, lon_min, lon_max, month, genus)]
    family_monthly <- dt2[!is.na(family), .(count = .N), by = .(grid_lat, grid_lon, lat_min, lat_max, lon_min, lon_max, month, family)]
    
    genus_total <- genus_monthly[, .(
      total_count = sum(count),
      unique_months = uniqueN(month),
      top_genus = list(head(.SD[order(-count), genus], 5))
    ), by = .(grid_lat, grid_lon, lat_min, lat_max, lon_min, lon_max)]
    
    family_total <- family_monthly[, .(
      total_count = sum(count),
      unique_months = uniqueN(month),
      top_family = list(head(.SD[order(-count), family], 5))
    ), by = .(grid_lat, grid_lon, lat_min, lat_max, lon_min, lon_max)]
    
    res <- list(
      zoom = z,
      distance = d,
      genus_monthly = genus_monthly,
      family_monthly = family_monthly,
      genus_total = genus_total,
      family_total = family_total,
      date = Sys.time()
    )
    saveRDS(res, file.path(output_dir, paste0("grid_zoom_", z, ".rds")))
    results[[paste0("zoom_", z)]] <- res
  }
  
  saveRDS(results, file.path(output_dir, "all_zoom_levels.rds"))
  invisible(results)
}

# Load precomputed grid data
load_precomputed_data <- function(
    output_dir = "precomputed_data",
    zoom_level = NULL
) {
  idx <- file.path(output_dir, "zoom_index.rds")
  if (!file.exists(idx)) stop("Index file not found: ", idx)
  index <- readRDS(idx)
  
  if (is.null(zoom_level)) {
    file <- file.path(output_dir, "all_zoom_levels.rds")
    if (!file.exists(file)) stop("Aggregated data file missing: ", file)
    readRDS(file)
  } else {
    if (!zoom_level %in% index$zoom_level) stop("Zoom level not available: ", zoom_level)
    readRDS(file.path(output_dir, paste0("grid_zoom_", zoom_level, ".rds")))
  }
}

# Query precomputed grid data
query_grid_data <- function(
    precomputed_data,
    zoom_level,
    taxonomy_level  = "genus",
    month_range     = c(1, 12),
    taxonomy_filter = NULL,
    opacity_threshold = 100,
    top_n           = 3
) {
  
  precomputed <- precomputed_data
  taxonomy    <- taxonomy_level
  filter_taxa <- taxonomy_filter
  opacity_thr <- opacity_threshold
  key <- paste0("zoom_", zoom_level)
  
  if (!key %in% names(precomputed)) stop("Zoom data not found: ", zoom_level)
  data <- precomputed[[key]]
  dist <- data$distance
  
  tbl <- if (taxonomy == "genus") data$genus_monthly else data$family_monthly
  col_tax <- taxonomy
  
  sel <- tbl[month %between% month_range]
  stats <- sel[, .(
    total_count = sum(count)
  ), by = .(grid_lat, grid_lon, lat_min, lat_max, lon_min, lon_max)]
  stats[, `:=`(
    opacity = fifelse(total_count >= opacity_thr, 0.8, 0.4),
    radius = total_count^(1/4) * 14 * zoom_level,
    distance = dist
  )]
  
  if (is.null(filter_taxa)) {
    top_tbl <- sel[, .(cnt = sum(count)), by = c("grid_lat", "grid_lon", col_tax)]
    top_vals <- top_tbl[order(-cnt), head(.SD, top_n), by = .(grid_lat, grid_lon)]
    top_txt <- top_vals[, .(
      top = paste(get(col_tax), cnt, sep = ": ", collapse = "<br>")
    ), by = .(grid_lat, grid_lon)]
    res <- merge(stats, top_txt, by = c("grid_lat", "grid_lon"))
    res[, `:=`(fillColor = "grey", filter_mode = FALSE)]
  } else {
    flt <- sel[get(col_tax) %in% filter_taxa]
    if (nrow(flt) == 0) return(data.table())
    
    stats_f <- flt[, .(
      total_count = sum(count),
      dominant = get(col_tax)[which.max(count)]
    ), by = .(grid_lat, grid_lon, lat_min, lat_max, lon_min, lon_max)]
    stats_f[, `:=`(
      opacity = fifelse(total_count >= opacity_thr, 0.8, 0.4),
      radius = total_count^(1/4) * 14 * zoom_level,
      distance = dist
    )]
    txt_f <- flt[, .(cnt = sum(count)), by = c("grid_lat", "grid_lon", col_tax)]
    txt_vals <- txt_f[, .(
      top = paste(get(col_tax), cnt, sep = ": ", collapse = "<br>")
    ), by = .(grid_lat, grid_lon)]
    res <- merge(stats_f, txt_vals, by = c("grid_lat", "grid_lon"))
    pal <- c("#e41a1c", "#377eb8", "#4daf4a")
    res[, fillColor := pal[match(dominant, filter_taxa)]]
    res[, filter_mode := TRUE]
  }
  
  setnames(res, 
           c("grid_lat", "grid_lon", "top"), 
           c("lat", "lon", "top_taxonomy"))
  res
}
# ======== Get grid observations for plotting ====================
# Get grid observations for plotting
get_grid_observations_for_plot <- function(
    clicked_coords, 
    zoom_level, 
    season_months, 
    taxonomy_level, 
    taxonomy_filter = NULL, 
    top_n = 3,
    precomputed_grids) {
  
  # Try precomputed data first
  if (!is.null(precomputed_grids)) {
    zoom_key <- paste0("zoom_", zoom_level)
    if (zoom_key %in% names(precomputed_grids)) {
      zoom_data <- precomputed_grids[[zoom_key]]
      
      if (taxonomy_level == "genus") {
        monthly_data <- zoom_data$genus_monthly
        taxonomy_col <- "genus"
      } else {
        monthly_data <- zoom_data$family_monthly
        taxonomy_col <- "family"
      }
      
      # Get data for specified grid point
      grid_data <- monthly_data[
        abs(grid_lat - clicked_coords[1]) < 1e-5 & 
          abs(grid_lon - clicked_coords[2]) < 1e-5 &
          month %in% season_months
      ]
      
      has_filter <- !is.null(taxonomy_filter) && length(taxonomy_filter) > 0
      
      if (!has_filter) {
        # No filter: get Top N
        point_stats <- grid_data[, .(N = sum(count)), by = taxonomy_col][order(-N)]
        point_stats_clean <- point_stats[!is.na(get(taxonomy_col))]
        
        if (nrow(point_stats_clean) > 0) {
          actual_n <- min(top_n, nrow(point_stats_clean))
          point_topN <- point_stats_clean[1:actual_n][[taxonomy_col]]
          
          result <- grid_data[get(taxonomy_col) %in% point_topN][
            , .(N = sum(count)), by = c("month", taxonomy_col)
          ]
          
          return(result)
        }
      } else {
        # Has filter: return only filtered taxonomies
        if (any(grid_data[[taxonomy_col]] %in% taxonomy_filter)) {
          result <- grid_data[get(taxonomy_col) %in% taxonomy_filter][
            , .(N = sum(count)), by = c("month", taxonomy_col)
          ]
          return(result)
        }
      }
    }
  }
  
  return(NULL)
}
# ================== Calculate opacity based on count and threshold ============
calculate_opacity <- function(total_count, k) {
  ifelse(total_count >= k, 0.8, 0.4)
}
# ========== Show welcome dialog ==================================
showWelcome <- function() {
  showModal(modalDialog(
    title = "Welcome to the Bird Grid Visualization App",
    HTML("
      <p>This interactive dashboard lets you:</p>
      <ol>
        <li><strong>Use the Control Panel (top-right):</strong>
          <ul>
            <li>Select a season to update the heatmap.</li>
            <li>Switch between <em>Genus</em> and <em>Family</em> levels.</li>
            <li>Filter up to 3 taxa for focused analysis.</li>
            <li>Adjust “Show Top N” to highlight the most abundant groups.</li>
            <li>Set an “Opacity Threshold” to hide low-count points.</li>
            <li>Toggle “Show Seasonal Temp” to overlay and style temperature rasters.</li>
            <li>Click “Clear Selection” to reset your clicked points.</li>
          </ul>
        </li>
        <li><strong>Interact with the Map:</strong>
          <ul>
            <li>Drag or scroll to pan and zoom.</li>
            <li>Click grid points to mark Point 1 and Point 2.</li>
            <li>The toolbar’s transparent background lets you drag the map through empty areas.</li>
          </ul>
        </li>
        <li><strong>Read the Area Charts (top-left):</strong>
          <ul>
            <li>Displays monthly counts globally or for your selected points.</li>
            <li>Compare two grid locations side-by-side.</li>
          </ul>
        </li>
      </ol>
      <p>Press “Get Started” to close this guide. You can re-open it anytime by clicking the Help button up top.</p>
    "),
    easyClose = TRUE,
    footer    = modalButton("Get Started")
  ))
}
# ================ Load seasonal temperature raster data ===========
load_seasonal_rasters <- function(input_dir = "precomputed_seasonal_rasters") {
  seasons <- c("spring", "summer", "autumn", "winter")
  raster_list <- list()
  
  cat("\rLoading seasonal temperature data...\n")
  
  for (season in seasons) {
    filename <- file.path(input_dir, paste0("seasonal_", season, ".rds"))
    if (file.exists(filename)) {
      tryCatch({
        r <- readRDS(filename)
        if (!is.null(r) && class(r)[1] == "RasterLayer") {
          vals <- values(r)
          if (sum(!is.na(vals)) > 0) {
            raster_list[[season]] <- r
            cat("\r✔ Loaded", season, "raster data\n")
          }
        }
      }, error = function(e) {
        cat("\rFailed to load", season, ":", e$message, "\n")
      })
    }
  }
  
  cat("\rSuccessfully loaded", length(raster_list), "seasonal rasters\n")
  return(raster_list)
}
# ====== Calculate grid coordinates (align lat/lon to grid center based on distance) ====
calculate_grid_coords <- function(lat, lon, distance) {
  list(
    grid_lat = floor(lat / distance + 0.5) * distance,
    grid_lon = floor(lon / distance + 0.5) * distance
  )
}