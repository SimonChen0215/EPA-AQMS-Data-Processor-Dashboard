# Multi-State EPA Air Quality Dashboard - App Entry Point
# Run this file to launch the Shiny application
#
# Project structure:
#   app.R              - This file (entry point)
#   global.R           - Package loading & source helpers
#   ui.R               - User interface definition
#   server.R           - Server logic
#   www/styles.css     - Custom CSS
#   R/api_fetcher.R    - NSW + QLD + VIC EPA API communication
#   R/data_processor.R - Unit conversion, cleaning, reshaping (all states)
#   R/summary_stats.R  - Hourly & daily summary calculations
#   R/plot_helpers.R   - Dust & time series plot builders
#   R/vic_data_loader.R - VIC EPA local xlsx data loading
#   R/sa_data_loader.R  - SA EPA local RDS data loading
#
# The original single-file version is preserved as NSW_API_BG_Dashboard.R

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
