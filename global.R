# Multi-State EPA Air Quality Dashboard - Global Dependencies
# Load all required packages and source helper scripts

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(openair)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(httr)
library(jsonlite)
library(reshape2)
library(leaflet)
library(shinythemes)

# Source helper modules
source("R/api_fetcher.R")
source("R/data_processor.R")
source("R/summary_stats.R")
source("R/plot_helpers.R")
source("R/vic_data_loader.R")
source("R/sa_data_loader.R")

# ── Station & parameter cache ─────────────────────────────────────────────
# Saves to local .rds files so the dashboard doesn't re-fetch on every startup.
# Call refresh_station_cache() to force re-download from APIs.

CACHE_DIR <- "cache"
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR)

CACHE_FILES <- list(
  nsw_sites  = file.path(CACHE_DIR, "nsw_site_details.rds"),
  nsw_params = file.path(CACHE_DIR, "nsw_param_details.rds"),
  qld_sites  = file.path(CACHE_DIR, "qld_stations.rds"),
  qld_params = file.path(CACHE_DIR, "qld_parameters.rds"),
  vic_sites  = file.path(CACHE_DIR, "vic_api_sites.rds"),
  sa_sites   = file.path(CACHE_DIR, "sa_sites.rds")
)

#' Load cached station/parameter data, or fetch from API if cache doesn't exist
load_cached_data <- function() {
  data <- list()
  
  # NSW sites
  if (file.exists(CACHE_FILES$nsw_sites)) {
    data$nsw_sites <- readRDS(CACHE_FILES$nsw_sites)
  } else {
    tryCatch({
      data$nsw_sites <- fetch_nsw_site_details()
      saveRDS(data$nsw_sites, CACHE_FILES$nsw_sites)
    }, error = function(e) {
      message("Failed to fetch NSW sites: ", e$message)
      data$nsw_sites <- NULL
    })
  }
  
  # NSW parameters
  if (file.exists(CACHE_FILES$nsw_params)) {
    data$nsw_params <- readRDS(CACHE_FILES$nsw_params)
  } else {
    tryCatch({
      data$nsw_params <- fetch_nsw_parameter_details()
      saveRDS(data$nsw_params, CACHE_FILES$nsw_params)
    }, error = function(e) {
      message("Failed to fetch NSW parameters: ", e$message)
      data$nsw_params <- NULL
    })
  }
  
  # QLD stations
  if (file.exists(CACHE_FILES$qld_sites)) {
    data$qld_sites <- readRDS(CACHE_FILES$qld_sites)
  } else {
    tryCatch({
      data$qld_sites <- fetch_qld_stations()
      saveRDS(data$qld_sites, CACHE_FILES$qld_sites)
    }, error = function(e) {
      message("Failed to fetch QLD stations: ", e$message)
      data$qld_sites <- NULL
    })
  }
  
  # QLD parameters
  if (file.exists(CACHE_FILES$qld_params)) {
    data$qld_params <- readRDS(CACHE_FILES$qld_params)
  } else {
    tryCatch({
      data$qld_params <- fetch_qld_parameters()
      saveRDS(data$qld_params, CACHE_FILES$qld_params)
    }, error = function(e) {
      message("Failed to fetch QLD parameters: ", e$message)
      data$qld_params <- NULL
    })
  }
  
  # VIC sites (from API, cached) — for map display + parameter lookup
  vic_cache_valid <- FALSE
  if (file.exists(CACHE_FILES$vic_sites)) {
    data$vic_sites <- readRDS(CACHE_FILES$vic_sites)
    # Validate cache has required columns (params added later)
    if ("params" %in% names(data$vic_sites)) {
      vic_cache_valid <- TRUE
    } else {
      message("VIC sites cache outdated (missing params column), refreshing...")
    }
  }
  if (!vic_cache_valid) {
    tryCatch({
      data$vic_sites <- fetch_vic_sites()
      saveRDS(data$vic_sites, CACHE_FILES$vic_sites)
    }, error = function(e) {
      message("Failed to fetch VIC sites from API: ", e$message)
      # Fallback to xlsx-based site list
      tryCatch({
        data$vic_sites <- load_vic_site_list()
      }, error = function(e2) {
        message("Failed to load VIC sites from xlsx: ", e2$message)
        data$vic_sites <- NULL
      })
    })
  }

  # VIC xlsx site list — for location_id mapping (needed to load xlsx data)
  tryCatch({
    data$vic_xlsx_sites <- load_vic_site_list()
  }, error = function(e) {
    message("Failed to load VIC xlsx site list: ", e$message)
    data$vic_xlsx_sites <- NULL
  })
  
  # SA sites (from GeoJSON + extra stations, cached)
  if (file.exists(CACHE_FILES$sa_sites)) {
    data$sa_sites <- readRDS(CACHE_FILES$sa_sites)
  } else {
    tryCatch({
      data$sa_sites <- load_sa_site_list()
      saveRDS(data$sa_sites, CACHE_FILES$sa_sites)
    }, error = function(e) {
      message("Failed to load SA sites: ", e$message)
      data$sa_sites <- NULL
    })
  }
  
  data
}

#' Force refresh all cached data from APIs
refresh_station_cache <- function() {
  tryCatch({
    saveRDS(fetch_nsw_site_details(), CACHE_FILES$nsw_sites)
  }, error = function(e) message("Refresh NSW sites failed: ", e$message))
  
  tryCatch({
    saveRDS(fetch_nsw_parameter_details(), CACHE_FILES$nsw_params)
  }, error = function(e) message("Refresh NSW params failed: ", e$message))
  
  tryCatch({
    saveRDS(fetch_qld_stations(), CACHE_FILES$qld_sites)
  }, error = function(e) message("Refresh QLD stations failed: ", e$message))
  
  tryCatch({
    saveRDS(fetch_qld_parameters(), CACHE_FILES$qld_params)
  }, error = function(e) message("Refresh QLD params failed: ", e$message))
  
  # VIC: refresh from API
  tryCatch({
    saveRDS(fetch_vic_sites(), CACHE_FILES$vic_sites)
  }, error = function(e) message("Refresh VIC sites failed: ", e$message))
  
  # SA: refresh from GeoJSON
  tryCatch({
    saveRDS(load_sa_site_list(), CACHE_FILES$sa_sites)
  }, error = function(e) message("Refresh SA sites failed: ", e$message))
  
  load_cached_data()
}

# Load on startup
CACHED_DATA <- load_cached_data()
