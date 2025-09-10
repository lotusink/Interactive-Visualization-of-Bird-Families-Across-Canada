# ===== Loading library
source("Dependencies/R/load_package.R")
setwd("Dependencies")
# ===== Loading function =====
source("R/load_functions.R")
# ===== Loading data =====
source("R/load_data.R")
# ===== Loading server and ui =====
source("ui.R")
source("server.R")
# ===== Running Application =====
shinyApp(ui, server)