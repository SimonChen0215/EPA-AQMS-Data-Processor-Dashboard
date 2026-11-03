# Multi-State EPA Air Quality API Fetcher
# Functions for fetching data from NSW EPA, QLD DES, and VIC EPA APIs

NSW_API_BASE <- "https://data.airquality.nsw.gov.au/api/Data"

#' Fetch site details from NSW EPA API
#' @return data.frame of site details (SiteName, Site_Id, Latitude, Longitude, etc.)
fetch_nsw_site_details <- function() {
  url <- paste0(NSW_API_BASE, "/get_SiteDetails")
  url %>%
    GET(body = list(), add_headers(`Content-Type` = "application/json"), timeout(30)) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    data.frame()
}

#' Fetch parameter details from NSW EPA API
#' @return data.frame of parameter details (ParameterCode, Units, etc.)
fetch_nsw_parameter_details <- function() {
  url <- paste0(NSW_API_BASE, "/get_ParameterDetails")
  url %>%
    GET(body = list(), add_headers(`Content-Type` = "application/json"), timeout(30)) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    data.frame()
}

# NSW supported parameter codes (only these are processed by the dashboard)
NSW_SUPPORTED_PARAMS <- c("PM10", "PM2.5", "TSP", "OZONE", "CO", "SO2", "NO", "NO2",
                          "WDR", "WSP", "TEMP", "SOLAR", "RAIN")

# NSW display names for the picker (code -> friendly name)
NSW_PARAM_DISPLAY_NAMES <- c(
  "PM10"  = "PM10",
  "PM2.5" = "PM2.5",
  "TSP"   = "Total Suspended Particulates (TSP)",
  "OZONE" = "Ozone (O3)",
  "CO"    = "Carbon Monoxide (CO)",
  "SO2"   = "Sulphur Dioxide (SO2)",
  "NO"    = "Nitric Oxide (NO)",
  "NO2"   = "Nitrogen Dioxide (NO2)",
  "WDR"   = "Wind Direction",
  "WSP"   = "Wind Speed",
  "TEMP"  = "Temperature",
  "SOLAR" = "Solar Radiation",
  "RAIN"  = "Rainfall"
)

#' Get filtered NSW parameter choices with friendly display names
#' @param param_details data.frame from fetch_nsw_parameter_details()
#' @return named character vector (display_name = code) for pickerInput
get_nsw_param_choices <- function(param_details) {
  available <- unique(param_details$ParameterCode)
  codes <- intersect(NSW_SUPPORTED_PARAMS, available)
  # Return named vector: names are display labels, values are codes
  choices <- codes
  names(choices) <- NSW_PARAM_DISPLAY_NAMES[codes]
  choices
}

#' Expand NSW user-selected parameters to include 'd' variants
#' @param params character vector of user-selected parameter codes
#' @return character vector with PM10d/PM2.5d/TSPd appended if PM10/PM2.5/TSP selected
expand_nsw_params <- function(params) {
  if ("PM10" %in% params) params <- unique(c(params, "PM10d"))
  if ("PM2.5" %in% params) params <- unique(c(params, "PM2.5d"))
  if ("TSP" %in% params) params <- unique(c(params, "TSPd"))
  params
}

#' Fetch hourly observation data for one NSW chunk
#' @param site_ids numeric vector of site IDs
#' @param params character vector of parameter codes (already expanded)
#' @param year_start Date object for start of year
#' @param year_end Date object for end of year
#' @return data.frame of observations, or NULL on failure
fetch_nsw_observations_chunk <- function(site_ids, params, year_start, year_end) {
  site_str <- paste0(site_ids, collapse = ",")
  params_str <- paste0('"', params, '"', collapse = ",")
  
  bodyparam <- paste0('{
    "Parameters": [', params_str, '],
    "Sites": [', site_str, '],
    "StartDate": "', year_start, '",
    "EndDate": "', year_end, '",
    "Categories": ["Averages"],
    "SubCategories": ["Hourly"],
    "Frequency": ["Hourly average"]
  }')
  
  paste0(NSW_API_BASE, "/get_Observations") %>%
    POST(
      body = bodyparam,
      add_headers(c(
        `accept` = "application/json",
        `Content-Type` = "application/json"
      ))
    ) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    data.frame()
}

#' Fetch NSW observations across a year range, 6-month chunks with retries
#' @param site_ids numeric vector of site IDs
#' @param params character vector of parameter codes (already expanded)
#' @param start_year integer start year
#' @param end_year integer end year
#' @param max_retries integer max retry attempts per chunk
#' @param progress_callback function(amount, detail_msg) called for progress updates, or NULL
#' @return list(data = data.frame, messages = character vector of status messages)
fetch_nsw_observations <- function(site_ids, params, start_year, end_year,
                               max_retries = 5, progress_callback = NULL) {
  df_all <- data.frame()
  messages <- c()
  
  # Build list of 6-month (half-year) chunks
  halves <- list(c(1, 6), c(7, 12))
  total_chunks <- length(start_year:end_year) * 2
  chunk_progress <- 0.9 / total_chunks   # reserve 10% for processing
  
  for (yr in start_year:end_year) {
    for (h in seq_along(halves)) {
      q_start <- as.Date(sprintf("%d-%02d-01", yr, halves[[h]][1]))
      q_end_month <- halves[[h]][2]
      # The API treats EndDate as an exclusive boundary.
      next_month <- if (q_end_month == 12) 1 else q_end_month + 1
      next_year <- yr + as.integer(q_end_month == 12)
      q_end <- as.Date(sprintf("%d-%02d-01", next_year, next_month))
      
      label <- paste0(yr, "-H", h)
      if (!is.null(progress_callback)) {
        progress_callback(chunk_progress, paste("Fetching", label))
      }
      
      success <- FALSE
      attempt <- 1
      
      while (!success && attempt <= max_retries) {
        tryCatch({
          df_chunk <- fetch_nsw_observations_chunk(site_ids, params, q_start, q_end)
          df_all <- rbind(df_all, df_chunk)
          messages <- c(messages, paste("\u2713", label, "fetched successfully"))
          success <- TRUE
        }, error = function(e) {
          if (attempt == max_retries) {
            messages <<- c(messages, paste("\u2717", label, "failed after", max_retries, "attempts"))
          }
        })
        
        if (!success) {
          attempt <- attempt + 1
          Sys.sleep(2)
        }
      }
    }
  }
  
  list(data = df_all, messages = messages)
}

# ══════════════════════════════════════════════════════════════════════════════
# QLD DES Air Quality API
# API docs: https://airquality.des.qld.gov.au/docs/#/
# ══════════════════════════════════════════════════════════════════════════════

QLD_API_BASE <- "https://airquality.des.qld.gov.au/v1"

#' Fetch all QLD monitoring stations
#' @return data.frame with station_id, station_name, region_id, latitude, longitude, etc.
fetch_qld_stations <- function() {
  resp <- GET(
    paste0(QLD_API_BASE, "/stations"),
    query = list(pagesize = 10000),
    add_headers(`Content-Type` = "application/json"),
    timeout(30)
  )
  stop_for_status(resp)
  df <- content(resp, "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    data.frame()
  
  # Filter active stations with valid coordinates
  df <- df %>%
    filter(
      is.na(end_date) | end_date == "9999-12-30T14:00:00.000Z",
      !is.na(latitude) & latitude != "",
      !is.na(longitude) & longitude != ""
    ) %>%
    mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    )
  df
}

#' Fetch all QLD parameter definitions
#' @return data.frame with parameter_id, parameter_name, abbreviation, units, etc.
fetch_qld_parameters <- function() {
  resp <- GET(
    paste0(QLD_API_BASE, "/parameters"),
    query = list(pagesize = 10000),
    add_headers(`Content-Type` = "application/json"),
    timeout(30)
  )
  stop_for_status(resp)
  content(resp, "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    data.frame()
}

#' Fetch parameters available at a specific QLD station
#' @param station_id character station ID (e.g. "cbd")
#' @return data.frame with parameter_id, parameter_name, units, etc.
fetch_qld_station_parameters <- function(station_id) {
  resp <- GET(
    paste0(QLD_API_BASE, "/stations/", station_id, "/parameters"),
    query = list(pagesize = 10000),
    add_headers(`Content-Type` = "application/json"),
    timeout(30)
  )
  stop_for_status(resp)
  content(resp, "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    data.frame()
}

#' Fetch measurements for one month chunk from QLD API
#' @param station_id character station ID
#' @param start_date POSIXct start
#' @param end_date POSIXct end
#' @return data.frame of measurements
fetch_qld_measurements_chunk <- function(station_id, start_date, end_date) {
  url <- paste0(QLD_API_BASE, "/stations/", station_id, "/parameters/measurements")
  resp <- GET(
    url,
    query = list(
      start_date = format(start_date, "%Y-%m-%dT%H:%M:%SZ"),
      end_date   = format(end_date,   "%Y-%m-%dT%H:%M:%SZ"),
      pagesize   = 10000
    ),
    add_headers(`Content-Type` = "application/json"),
    timeout(30)
  )
  stop_for_status(resp)
  content(resp, "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    data.frame()
}

#' Fetch QLD measurements across a year range, month-by-month with retries
#' @param station_id character station ID
#' @param start_year integer start year
#' @param end_year integer end year
#' @param max_retries integer max retry attempts per month
#' @param progress_callback function(detail_msg) called for progress updates, or NULL
#' @return list(data = data.frame, messages = character vector)
fetch_qld_observations <- function(station_id, start_year, end_year,
                                   max_retries = 5, progress_callback = NULL) {
  df_all <- data.frame()
  messages <- c()
  total_chunks <- length(start_year:end_year) * 12
  chunk_progress <- 0.9 / total_chunks   # reserve 10% for processing
  
  for (yr in start_year:end_year) {
    for (mo in 1:12) {
      start_date <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", yr, mo), tz = "UTC")
      next_month <- start_date
      month(next_month) <- month(next_month) + 1
      end_date <- next_month - lubridate::seconds(1)
      
      label <- format(start_date, "%Y-%m")
      if (!is.null(progress_callback)) {
        progress_callback(chunk_progress, paste("Fetching", label))
      }
      
      success <- FALSE
      attempt <- 1
      
      while (!success && attempt <= max_retries) {
        tryCatch({
          df_chunk <- fetch_qld_measurements_chunk(station_id, start_date, end_date)
          if (nrow(df_chunk) > 0) {
            df_all <- rbind(df_all, df_chunk)
          }
          messages <- c(messages, paste("\u2713", label, "fetched successfully"))
          success <- TRUE
        }, error = function(e) {
          if (attempt == max_retries) {
            messages <<- c(messages, paste("\u2717", label, "failed after", max_retries, "attempts"))
          }
        })
        
        if (!success) {
          attempt <- attempt + 1
          Sys.sleep(1)
        }
      }
    }
  }
  
  list(data = df_all, messages = messages)
}

#' Fetch NSW observations for an arbitrary date range (6-month chunks with retries)
#' @param site_ids numeric vector of site IDs
#' @param params character vector of parameter codes (already expanded)
#' @param start_date Date start
#' @param end_date Date end
#' @param max_retries integer max retry attempts per chunk
#' @param progress_callback function(amount, detail_msg) or NULL
#' @return list(data = data.frame, messages = character vector)
fetch_nsw_observations_daterange <- function(site_ids, params, start_date, end_date,
                                         max_retries = 5, progress_callback = NULL) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  df_all <- data.frame()
  messages <- c()
  
  # Pre-count 6-month chunks for progress
  tmp_start <- start_date
  n_chunks <- 0
  while (tmp_start <= end_date) {
    tmp_start <- min(tmp_start %m+% months(6), end_date + 1)
    n_chunks <- n_chunks + 1
  }
  chunk_progress <- 0.9 / max(n_chunks, 1)
  
  # Build 6-month chunks covering the date range
  chunk_start <- start_date
  while (chunk_start <= end_date) {
    chunk_end <- min(chunk_start %m+% months(6) - 1, end_date)
    
    label <- paste(chunk_start, "to", chunk_end)
    if (!is.null(progress_callback)) {
      progress_callback(chunk_progress, paste("Fetching", label))
    }
    
    success <- FALSE
    attempt <- 1
    
    while (!success && attempt <= max_retries) {
      tryCatch({
        df_chunk <- fetch_nsw_observations_chunk(site_ids, params, chunk_start, chunk_end + 1)
        df_all <- rbind(df_all, df_chunk)
        messages <- c(messages, paste("\u2713", label, "fetched"))
        success <- TRUE
      }, error = function(e) {
        if (attempt == max_retries) {
          messages <<- c(messages, paste("\u2717", label, "failed after", max_retries, "attempts"))
        }
      })
      if (!success) {
        attempt <- attempt + 1
        Sys.sleep(2)
      }
    }
    
    chunk_start <- chunk_end + 1
  }
  
  list(data = df_all, messages = messages)
}

#' Fetch QLD measurements for an arbitrary date range (month-by-month with retries)
#' @param station_id character station ID
#' @param start_date Date start
#' @param end_date Date end
#' @param max_retries integer max retry attempts per chunk
#' @param progress_callback function(amount, detail_msg) or NULL
#' @return list(data = data.frame, messages = character vector)
fetch_qld_observations_daterange <- function(station_id, start_date, end_date,
                                              max_retries = 5, progress_callback = NULL) {
  start_posix <- as.POSIXct(paste0(start_date, " 00:00:00"), tz = "UTC")
  end_posix <- as.POSIXct(paste0(end_date, " 23:59:59"), tz = "UTC")
  
  df_all <- data.frame()
  messages <- c()
  
  # Pre-count monthly chunks for progress
  tmp <- start_posix
  n_chunks <- 0
  while (tmp < end_posix) {
    month(tmp) <- month(tmp) + 1
    n_chunks <- n_chunks + 1
  }
  chunk_progress <- 0.9 / max(n_chunks, 1)
  
  current_start <- start_posix
  
  while (current_start < end_posix) {
    next_month <- current_start
    month(next_month) <- month(next_month) + 1
    current_end <- min(next_month - lubridate::seconds(1), end_posix)
    
    label <- format(current_start, "%Y-%m")
    if (!is.null(progress_callback)) {
      progress_callback(chunk_progress, paste("Fetching", label))
    }
    
    success <- FALSE
    attempt <- 1
    
    while (!success && attempt <= max_retries) {
      tryCatch({
        df_chunk <- fetch_qld_measurements_chunk(station_id, current_start, current_end)
        if (nrow(df_chunk) > 0) {
          df_all <- rbind(df_all, df_chunk)
        }
        messages <- c(messages, paste("\u2713", label, "fetched"))
        success <- TRUE
      }, error = function(e) {
        if (attempt == max_retries) {
          messages <<- c(messages, paste("\u2717", label, "failed after", max_retries, "attempts"))
        }
      })
      if (!success) {
        attempt <- attempt + 1
        Sys.sleep(1)
      }
    }
    
    current_start <- next_month
  }
  
  list(data = df_all, messages = messages)
}

# ══════════════════════════════════════════════════════════════════════════════
# VIC EPA Air Monitoring API (sites only — observation data from local xlsx)
# API docs: https://gateway.api.epa.vic.gov.au/environmentMonitoring/v1
# ══════════════════════════════════════════════════════════════════════════════

VIC_API_BASE <- "https://gateway.api.epa.vic.gov.au/environmentMonitoring/v1"
VIC_API_KEY  <- Sys.getenv("VIC_API_KEY")  # set in .Renviron

#' Fetch VIC EPA air monitoring site details from API
#' @return data.frame with columns: siteID, siteName, siteType, Latitude, Longitude, params (list column)
fetch_vic_sites <- function() {
  resp <- GET(
    paste0(VIC_API_BASE, "/sites"),
    query = list(environmentalSegment = "air"),
    add_headers("X-API-Key" = VIC_API_KEY),
    timeout(30)
  )
  stop_for_status(resp)
  raw <- content(resp, "text", encoding = "UTF-8") %>%
    fromJSON(flatten = FALSE)
  records <- raw$records

  # Build a clean data.frame (avoid dplyr::select on nested fromJSON output)
  n <- nrow(records)
  result <- data.frame(
    siteID   = records$siteID,
    siteName = records$siteName,
    siteType = records$siteType,
    Latitude  = sapply(records$geometry$coordinates, `[[`, 1),
    Longitude = sapply(records$geometry$coordinates, `[[`, 2),
    stringsAsFactors = FALSE
  )

  # Extract parameter names per site from nested list of data.frames
  result$params <- lapply(seq_len(n), function(i) {
    p <- records$parameters[[i]]
    if (is.null(p) || !is.data.frame(p) || nrow(p) == 0) return(character(0))
    as.character(p$name)
  })

  result
}
