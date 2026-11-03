# ============================================================================
# SA EPA Air Quality Data Downloader
# Downloads all SA EPA air quality monitoring data from data.sa.gov.au (CKAN API)
# Output: SA-AQMS-Data/{StationName}/{type}/ with year-labelled ZIP files
# License: CC-BY (Creative Commons Attribution)
# ============================================================================

library(httr)
library(jsonlite)

# --- Configuration -----------------------------------------------------------

CKAN_BASE <- "https://data.sa.gov.au/data/api/3/action"
OUTPUT_DIR <- file.path("..", "SA-AQMS-Data")

# All SA EPA air quality dataset CKAN IDs
# Format: list(station_folder_name = list(type = "dataset-slug-name"))
SA_DATASETS <- list(
  # --- Christie Downs (Southern Adelaide) ---
  Christie_Downs = list(
    particle  = "christie-downs-air-quality-monitoring-station-particle-data",
    gaseous   = "christie-downs-air-quality-monitoring-station-gaseous-data",
    met       = "christie-downs-air-quality-monitoring-station-meteorology-data"
  ),
  # --- Adelaide CBD ---
  Adelaide_CBD = list(
    particle  = "adelaide-cbd-air-quality-monitoring-station-particle",
    gaseous   = "adelaide-gaseous"
  ),
  # --- Elizabeth (Northern Adelaide) ---
  Elizabeth = list(
    particle  = "elizabeth-air-quality-monitoring-station-particle-data",
    gaseous   = "elizabeth-air-quality-monitoring-station-gaseous-data",
    met       = "elizabeth-air-quality-monitoring-station-meteorology-data"
  ),
  # --- Northfield (North Eastern Adelaide) ---
  Northfield = list(
    gaseous   = "northfield-air-quality-monitoring-station-gaseous-data",
    met       = "northfield-air-quality-monitoring-station-meteorology-data"
  ),
  # --- Netley (Western Adelaide, decommissioned June 2024) ---
  Netley = list(
    particle  = "netley-air-quality-monitoring-station-particle-data",
    gaseous   = "netley-air-quality-monitoring-station-gaseous-data",
    met       = "netley-air-quality-monitoring-station-meteorology-data"
  ),
  # --- Cowandilla (Western Adelaide, new - replaces Netley) ---
  Cowandilla = list(
    particle  = "cowandilla-air-quality-monitoring-station-particle-data",
    gaseous   = "cowandilla-air-quality-monitoring-station-gaseous-data",
    met       = "cowandilla-air-quality-monitoring-station-meteorology-data"
  ),
  # --- Le Fevre 1 / Birkenhead (North Western Adelaide) ---
  Le_Fevre_1_Birkenhead = list(
    particle  = "le-fevre-1-air-quality-monitoring-station-particle-data",
    gaseous   = "le-fevre-1-air-quality-monitoring-station-gaseous-data",
    met       = "le-fevre-1-air-quality-monitoring-station-meteorology-data"
  ),
  # --- Le Fevre 2 / North Haven (North Western Adelaide) ---
  Le_Fevre_2_North_Haven = list(
    particle  = "le-fevre-2-air-quality-monitoring-station-particle-data",
    gaseous   = "le-fevre-2-air-quality-monitoring-station-gaseous-data"
    # Note: Le Fevre 2 met data shares dataset with Le Fevre 1 region
  ),
  # --- Port Pirie Oliver St (Spencer Gulf) ---
  Port_Pirie_Oliver_St = list(
    particle  = "port-pirie-oliver-street-air-quality-monitoring-station-particle-data",
    gaseous   = "port-pirie-oliver-street-air-quality-monitoring-station-gaseous-data",
    met       = "port-pirie-oliver-st-air-quality-monitoring-station-meteorology-data"
  ),
  # --- Whyalla Walls St (Spencer Gulf) ---
  Whyalla_Walls_St = list(
    particle  = "whyalla-walls-street-air-quality-monitoring-station-particle-data",
    met       = "whyalla-walls-st-air-quality-monitoring-station-meteorology-data"
  ),
  # --- Whyalla Schulz Reserve (Spencer Gulf) ---
  Whyalla_Schulz_Reserve = list(
    particle  = "whyalla-schulz-reserve-air-quality-monitoring-station-particle-data"
  ),
  # --- Port Pirie HVS Lead (Pb) ---
  Port_Pirie_HVS_Lead = list(
    lead      = "port-pirie-high-volume-sampler-air-quality-monitoring-stations-lead-pb-data"
  ),
  # --- Kensington (Eastern Adelaide, inactive) ---
  Kensington = list(
    particle  = "kensington-air-quality-monitoring-station-particle-data",
    gaseous   = "kensington-air-quality-monitoring-station-gaseous-data",
    met       = "kensington-air-quality-monitoring-station-meteorology-data"
  ),
  # --- Golden Grove (temporary, 2014-2015) ---
  Golden_Grove = list(
    particle  = "golden-grove-air-quality-monitoring-station-particle-data",
    met       = "golden-grove-air-quality-monitoring-station-meteorology-data"
  ),
  # --- Sellicks Beach (temporary, 2016-2017) ---
  Sellicks_Beach = list(
    particle  = "sellicks-beach-air-quality-monitoring-station-particle-data",
    met       = "sellicks-beach-air-quality-monitoring-station-meteorology-data"
  ),
  # --- Port Augusta (temporary, 2017-2020) ---
  Port_Augusta = list(
    particle  = "port-augusta-air-quality-monitoring-station-particle-data",
    met       = "port-augusta-air-quality-monitoring-station-meteorology-data"
  ),
  # --- Sellicks Beach Lurline Blvd (campaign, 2025-2026) ---
  Sellicks_Beach_Lurline = list(
    particle  = "sellicks-beach-lurline-blvd-air-quality-monitoring-station-particle-data",
    met       = "sellicks-beach-lurline-blvd-air-quality-monitoring-station-meteorology-data"
  ),
  # --- Birkenhead Naval Reserve (campaign, 2024-2025) ---
  Birkenhead_Naval_Reserve = list(
    particle  = "birkenhead-naval-reserve-air-quality-monitoring-station-particle-data"
  ),
  # --- Peterhead (campaign, 2024-2025) ---
  Peterhead = list(
    particle  = "peterhead-air-quality-monitoring-station-particle-data"
  ),
  # --- Mt Barker (wood smoke program, 2022-2023) ---
  Mt_Barker = list(
    particle  = "mt-barker-air-quality-monitoring-particle-data",
    met       = "mt-barker-air-quality-monitoring-station-meteorology-data"
  )
)

# Subset: active stations only (for quick download)
ACTIVE_STATIONS <- c(
  "Christie_Downs", "Adelaide_CBD", "Elizabeth", "Northfield",
  "Cowandilla", "Le_Fevre_1_Birkenhead", "Le_Fevre_2_North_Haven",
  "Port_Pirie_Oliver_St", "Whyalla_Walls_St", "Whyalla_Schulz_Reserve",
  "Port_Pirie_HVS_Lead"
)

# --- Functions ---------------------------------------------------------------

#' Fetch all resources for a CKAN dataset
#' @param dataset_name The CKAN dataset slug name
#' @return data.frame with columns: name, url, format, size, last_modified
fetch_dataset_resources <- function(dataset_name) {
  url <- paste0(CKAN_BASE, "/package_show?id=", dataset_name)
  
  tryCatch({
    resp <- GET(url, timeout(30))
    if (status_code(resp) != 200) {
      message("  ERROR: HTTP ", status_code(resp), " for ", dataset_name)
      return(NULL)
    }
    
    result <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    if (!result$success) {
      message("  ERROR: CKAN API returned failure for ", dataset_name)
      return(NULL)
    }
    
    resources <- result$result$resources
    if (is.null(resources) || nrow(resources) == 0) {
      message("  No resources found for ", dataset_name)
      return(NULL)
    }
    
    resources[, c("name", "url", "format", "size", "last_modified")]
  }, error = function(e) {
    message("  ERROR fetching ", dataset_name, ": ", e$message)
    NULL
  })
}

#' Download a single resource ZIP file
#' @param url Download URL
#' @param dest_path Full path to save the ZIP file
#' @param resource_name Display name for logging
download_resource <- function(url, dest_path, resource_name = "") {
  if (file.exists(dest_path)) {
    message("  SKIP (exists): ", basename(dest_path))
    return(TRUE)
  }
  
  tryCatch({
    resp <- GET(url, write_disk(dest_path, overwrite = TRUE), timeout(120))
    if (status_code(resp) == 200) {
      size_kb <- round(file.size(dest_path) / 1024)
      message("  OK: ", basename(dest_path), " (", size_kb, " KB)")
      TRUE
    } else {
      message("  FAILED (HTTP ", status_code(resp), "): ", resource_name)
      if (file.exists(dest_path)) file.remove(dest_path)
      FALSE
    }
  }, error = function(e) {
    message("  ERROR: ", resource_name, " - ", e$message)
    if (file.exists(dest_path)) file.remove(dest_path)
    FALSE
  })
}

#' Extract year label from resource name
#' @param name Resource name string
#' @return Year label (e.g., "2025", "2015-2019", "2018-2019")
extract_year_label <- function(name) {
  # Match patterns like "2025", "2015 to 2019", "2018 to 2019", "October 2014 to May 2015"
  if (grepl("\\d{4}\\s+to\\s+\\d{4}", name)) {
    m <- regmatches(name, regexpr("\\d{4}\\s+to\\s+\\d{4}", name))
    return(gsub("\\s+to\\s+", "-", m))
  }
  if (grepl("20\\d{2}\\s*[-–]\\s*20\\d{2}", name)) {
    m <- regmatches(name, regexpr("20\\d{2}\\s*[-–]\\s*20\\d{2}", name))
    return(gsub("\\s*[-–]\\s*", "-", m))
  }
  if (grepl("20\\d{2}", name)) {
    m <- regmatches(name, regexpr("20\\d{2}", name))
    return(m)
  }
  return("unknown")
}

#' Download all data for a single station
#' @param station_name Folder name for the station
#' @param datasets Named list of data type -> dataset slug
#' @param output_dir Base output directory
download_station <- function(station_name, datasets, output_dir) {
  message("\n=== ", station_name, " ===")
  
  for (data_type in names(datasets)) {
    dataset_slug <- datasets[[data_type]]
    message("  --- ", data_type, " ---")
    
    # Create output directory
    type_dir <- file.path(output_dir, station_name, data_type)
    dir.create(type_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Fetch resources from CKAN
    resources <- fetch_dataset_resources(dataset_slug)
    if (is.null(resources)) next
    
    # Download each resource
    for (i in seq_len(nrow(resources))) {
      res <- resources[i, ]
      year_label <- extract_year_label(res$name)
      
      # Construct filename from URL
      url_basename <- basename(res$url)
      # Remove URL encoding
      url_basename <- URLdecode(url_basename)
      # Add year label to avoid filename collisions
      file_ext <- tools::file_ext(url_basename)
      file_base <- tools::file_path_sans_ext(url_basename)
      dest_name <- paste0(file_base, "_", year_label, ".", file_ext)
      dest_path <- file.path(type_dir, dest_name)
      
      download_resource(res$url, dest_path, res$name)
    }
  }
}

# --- Main Execution ----------------------------------------------------------

#' Download all SA EPA air quality data
#' @param stations Character vector of station names to download, or "all"
#' @param active_only If TRUE, only download active stations
download_all_sa_data <- function(stations = "all", active_only = FALSE) {
  output_dir <- OUTPUT_DIR
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (identical(stations, "all")) {
    if (active_only) {
      station_names <- ACTIVE_STATIONS
    } else {
      station_names <- names(SA_DATASETS)
    }
  } else {
    station_names <- stations
  }
  
  message("Downloading SA EPA data for ", length(station_names), " stations...")
  message("Output directory: ", normalizePath(output_dir, mustWork = FALSE))
  
  for (stn in station_names) {
    if (!stn %in% names(SA_DATASETS)) {
      message("WARNING: Unknown station '", stn, "', skipping")
      next
    }
    download_station(stn, SA_DATASETS[[stn]], output_dir)
  }
  
  message("\n=== Download complete ===")
}

# --- Run ---------------------------------------------------------------------
# Uncomment the line you want:

# Download only active stations (recommended for first run):
# download_all_sa_data(active_only = TRUE)

# Download ALL stations including inactive/temporary:
# download_all_sa_data()

# Download specific stations:
# download_all_sa_data(c("Christie_Downs", "Adelaide_CBD"))

# Example: download just Christie Downs
# download_all_sa_data("Christie_Downs")
