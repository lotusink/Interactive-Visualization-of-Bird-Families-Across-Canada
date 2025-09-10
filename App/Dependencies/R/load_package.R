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