# ===== Loading data =====
# Observation data
dt_obsert <- readRDS("data/precomputed_observation/obsert_final.rds")


# Load precomputed grid data for performance optimization
cat("\rLoading precomputed grid data...\n")
precomputed_grids <- tryCatch({
  load_precomputed_data("data/precomputed_grids")
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
  load_seasonal_rasters(input_dir="data/precomputed_seasonal_rasters")
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