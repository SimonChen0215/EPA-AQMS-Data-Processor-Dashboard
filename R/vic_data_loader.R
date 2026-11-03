# VIC EPA Air Quality Data Loader
# Functions for loading VIC EPA validated data from local xlsx files

library(readxl)

VIC_DATA_DIR <- "Victoria-AQMS-Data"

# Map xlsx file years to their data sheet names
VIC_SHEET_MAP <- list(
  "2016" = "Data",
  "2017" = "Data",
  "2018" = "2018",
  "2019" = "2019",
  "2020" = "2020",
  "2021" = NULL,        # Per-site wide sheets only — special handling
  "2022" = "AllData",
  "2023" = "AllData",
  "2024" = "AllData",
  "2025" = "AllData"
)

# Legacy param_id -> normalised parameter name
VIC_LEGACY_PARAM_MAP <- c(
  "BPM2.5" = "PM2.5",
  "PPM2.5" = "PM2.5",
  "PM2.5"  = "PM2.5",   # modern format uses PM2.5 directly
  "HPM10"  = "PM10",
  "PM10"   = "PM10",
  "NO2"    = "NO2",
  "O3"     = "O3",
  "SO2"    = "SO2",
  "CO"     = "CO",
  "DBT"    = "DBT",
  "SWD"    = "SWD",
  "SWS"    = "SWS"
)

# Supported parameters (for filtering user choices)
VIC_SUPPORTED_PARAMS <- c("PM10", "PM2.5", "NO2", "O3", "SO2", "CO", "DBT", "SWD", "SWS")

# Display names for pickerInput
VIC_PARAM_DISPLAY_NAMES <- c(
  "PM10"  = "PM10",
  "PM2.5" = "PM2.5",
  "NO2"   = "Nitrogen Dioxide (NO2)",
  "O3"    = "Ozone (O3)",
  "SO2"   = "Sulphur Dioxide (SO2)",
  "CO"    = "Carbon Monoxide (CO)",
  "DBT"   = "Temperature",
  "SWD"   = "Wind Direction",
  "SWS"   = "Wind Speed"
)

#' Build xlsx file path for a given year
#' @param year integer or character year
#' @return file path string
vic_xlsx_path <- function(year) {
  file.path(VIC_DATA_DIR,
    paste0(year, "_All_sites_air_quality_hourly_avg_AIR-I-F-V-VH-O-S1-DB-M2-4-0.xlsx"))
}

#' Get available VIC data years (based on files on disk)
#' @return integer vector of years
get_vic_available_years <- function() {
  files <- list.files(VIC_DATA_DIR, pattern = "^\\d{4}_.*\\.xlsx$")
  years <- as.integer(sub("_.*", "", files))
  sort(years)
}

#' Load VIC site list from xlsx files (with caching)
#' @return data.frame with columns: location_id, location_name, Latitude, Longitude
load_vic_site_list <- function(refresh = FALSE) {
  cache_file <- file.path("cache", "vic_sites.rds")
  if (file.exists(cache_file) && !refresh) {
    return(readRDS(cache_file))
  }

  all_sites <- data.frame()

  # Modern files have AllData with lat/lon columns
  for (y in c(2025, 2024, 2023, 2022)) {
    fpath <- vic_xlsx_path(y)
    if (!file.exists(fpath)) next
    tryCatch({
      header <- read_xlsx(fpath, sheet = "AllData", n_max = 0,
                          col_types = "text")
      site_cols <- match(c("location_id", "location_name", "latitude", "longitude"),
                         names(header))
      if (anyNA(site_cols)) stop("Missing VIC site metadata columns")
      df <- read_xlsx(fpath, sheet = "AllData",
                      range = cell_limits(c(1, min(site_cols)), c(NA, max(site_cols))),
                      col_types = "text")
      df <- df[, c("location_id", "location_name", "latitude", "longitude")]
      sites <- df %>%
        distinct(location_id, location_name, latitude, longitude) %>%
        mutate(Latitude = as.numeric(latitude),
               Longitude = as.numeric(longitude)) %>%
        select(location_id, location_name, Latitude, Longitude)
      all_sites <- bind_rows(all_sites, sites)
    }, error = function(e) {
      message("VIC site scan: skipping ", y, " — ", e$message)
    })
  }

  # Legacy files (2016-2020) use sample_point_id/sp_name
  for (y in 2020:2016) {
    fpath <- vic_xlsx_path(y)
    if (!file.exists(fpath)) next
    sheet_name <- VIC_SHEET_MAP[[as.character(y)]]
    if (is.null(sheet_name)) next
    tryCatch({
      df <- read_xlsx(fpath, sheet = sheet_name,
                      col_types = "text",
                      n_max = 50000)
      sites <- df %>%
        distinct(sample_point_id, sp_name, latitude, longitude) %>%
        mutate(location_id = as.character(sample_point_id),
               location_name = sp_name,
               Latitude = as.numeric(latitude),
               Longitude = as.numeric(longitude)) %>%
        select(location_id, location_name, Latitude, Longitude)
      all_sites <- bind_rows(all_sites, sites)
    }, error = function(e) {
      message("VIC site scan: skipping ", y, " — ", e$message)
    })
  }

  # 2021: per-site sheets — extract from sheet names
  fpath_2021 <- vic_xlsx_path(2021)
  if (file.exists(fpath_2021)) {
    tryCatch({
      sheets <- excel_sheets(fpath_2021)
      site_sheets <- sheets[grepl("_\\d+$", sheets)]
      for (s in site_sheets) {
        df <- read_xlsx(fpath_2021, sheet = s, col_types = "text", n_max = 1)
        if (nrow(df) > 0 && "location_id" %in% names(df)) {
          # 2021 per-site sheets don't have lat/lon — will be filled from other years
          site_row <- data.frame(
            location_id = as.character(df$location_id[1]),
            location_name = as.character(df$location_name[1]),
            Latitude = NA_real_,
            Longitude = NA_real_,
            stringsAsFactors = FALSE
          )
          all_sites <- bind_rows(all_sites, site_row)
        }
      }
    }, error = function(e) {
      message("VIC site scan: skipping 2021 — ", e$message)
    })
  }

  # Keep distinct site names when an EPA location ID is reused.
  sites <- all_sites %>%
    distinct(location_id, location_name, .keep_all = TRUE)

  rds_files <- list.files(VIC_DATA_DIR, pattern = "_[0-9]+\\.rds$")
  rds_sites <- data.frame(
    location_id = sub(".*_([0-9]+)\\.rds$", "\\1", rds_files),
    location_name = gsub("_", " ", sub("_[0-9]+\\.rds$", "", rds_files)),
    Latitude = rep(NA_real_, length(rds_files)),
    Longitude = rep(NA_real_, length(rds_files)),
    stringsAsFactors = FALSE
  )
  sites <- bind_rows(sites, rds_sites) %>%
    distinct(location_id, location_name, .keep_all = TRUE)

  if (!dir.exists("cache")) dir.create("cache")
  saveRDS(sites, cache_file)
  sites
}

#' Build parameter cache for ALL VIC sites from the most recent xlsx file
#' Reads the newest available modern file to get site→parameter mapping for all sites,
#' then caches the result so subsequent calls are instant.
#' @return named list: list(location_id = c("PM10", "PM2.5", ...))
# Reverse mapping: display column names (from preprocess) -> param codes
VIC_DISPLAY_TO_CODE <- c(

  "CO (mg/m\u00b3)"         = "CO",
  "NO2 (\u00b5g/m\u00b3)"  = "NO2",
  "OZONE (\u00b5g/m\u00b3)" = "O3",
  "SO2 (\u00b5g/m\u00b3)"  = "SO2",
  "PM10 (\u00b5g/m\u00b3)" = "PM10",
  "PM2.5 (\u00b5g/m\u00b3)" = "PM2.5",
  "Wind Direction (\u00b0)" = "SWD",
  "Wind Speed (m/s)"        = "SWS",
  "Temperature (\u00b0C)"   = "DBT"
)

build_vic_param_cache <- function(refresh = FALSE) {
  cache_file <- file.path("cache", "vic_site_params.rds")
  if (file.exists(cache_file) && !refresh) {
    return(readRDS(cache_file))
  }

  # Fallback: use pre-processed vic_station_params.rds (display names -> codes)
  preproc_file <- file.path(VIC_DATA_DIR, "vic_station_params.rds")
  if (file.exists(preproc_file)) {
    raw_params <- readRDS(preproc_file)
    all_params <- lapply(raw_params, function(display_names) {
      codes <- unname(VIC_DISPLAY_TO_CODE[display_names])
      codes <- codes[!is.na(codes)]
      intersect(codes, VIC_SUPPORTED_PARAMS)
    })
    if (!dir.exists("cache")) dir.create("cache")
    saveRDS(all_params, cache_file)
    return(all_params)
  }

  all_params <- list()

  # Read ONLY the most recent modern file that exists — covers most sites
  for (y in c(2025, 2024, 2023, 2022)) {
    fpath <- vic_xlsx_path(y)
    if (!file.exists(fpath)) next
    tryCatch({
      # Read only location_id + parameter_name columns (skip value data)
      df <- read_xlsx(fpath, sheet = "AllData", col_types = "text")
      if ("location_id" %in% names(df) && "parameter_name" %in% names(df)) {
        site_params <- df %>%
          distinct(location_id, parameter_name) %>%
          mutate(mapped = VIC_LEGACY_PARAM_MAP[parameter_name]) %>%
          filter(!is.na(mapped)) %>%
          group_by(location_id) %>%
          summarise(params = list(unique(mapped)), .groups = "drop")
        for (i in seq_len(nrow(site_params))) {
          sid <- site_params$location_id[i]
          codes <- intersect(site_params$params[[i]], VIC_SUPPORTED_PARAMS)
          all_params[[sid]] <- union(if (is.null(all_params[[sid]])) character(0) else all_params[[sid]], codes)
        }
      }
      break  # Only need one modern file — they cover all active sites
    }, error = function(e) {
      message("VIC param cache: skipping ", y, " — ", e$message)
    })
  }

  # Supplement with 2021 (per-site wide sheets — column names = parameters)
  fpath_2021 <- vic_xlsx_path(2021)
  if (file.exists(fpath_2021)) {
    tryCatch({
      sheets <- excel_sheets(fpath_2021)
      site_sheets <- sheets[grepl("_\\d+$", sheets)]
      for (s in site_sheets) {
        sid <- sub(".*_(\\d+)$", "\\1", s)
        if (sid %in% names(all_params)) next  # already covered by modern file
        df <- read_xlsx(fpath_2021, sheet = s, col_types = "text", n_max = 1)
        param_cols <- setdiff(names(df), c("datetime_AEST", "datetime_local", "location_id", "location_name"))
        mapped <- param_cols[param_cols %in% names(VIC_LEGACY_PARAM_MAP)]
        codes <- intersect(unique(VIC_LEGACY_PARAM_MAP[mapped]), VIC_SUPPORTED_PARAMS)
        if (length(codes) > 0) {
          all_params[[sid]] <- union(if (is.null(all_params[[sid]])) character(0) else all_params[[sid]], codes)
        }
      }
    }, error = function(e) NULL)
  }

  if (!dir.exists("cache")) dir.create("cache")
  saveRDS(all_params, cache_file)
  all_params
}

#' Get available parameters for a VIC site (uses cached lookup)
#' @param location_id character site ID
#' @return named character vector (display_name = param_code) for pickerInput
get_vic_site_parameters <- function(location_id) {
  all_params <- build_vic_param_cache()

  if (location_id %in% names(all_params)) {
    codes <- all_params[[location_id]]
  } else {
    codes <- VIC_SUPPORTED_PARAMS  # fallback: offer all parameters
  }

  choices <- codes
  names(choices) <- VIC_PARAM_DISPLAY_NAMES[codes]
  choices
}

#' Read legacy format (2016-2020) for a single year and site
#' @param year integer year
#' @param location_id character site ID
#' @return data.frame in normalised long format, or NULL
read_vic_legacy <- function(year, location_id) {
  fpath <- vic_xlsx_path(year)
  if (!file.exists(fpath)) return(NULL)
  sheet_name <- VIC_SHEET_MAP[[as.character(year)]]
  if (is.null(sheet_name)) return(NULL)

  df <- read_xlsx(fpath, sheet = sheet_name, col_types = "text")
  df <- df %>%
    filter(as.character(sample_point_id) == location_id) %>%
    filter(time_basis_id == "1HR_AV") %>%
    filter(param_id %in% names(VIC_LEGACY_PARAM_MAP))

  if (nrow(df) == 0) return(NULL)

  df %>%
    mutate(
      datetime = as.POSIXct(sample_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"),
      Date = as.Date(datetime, tz = "Australia/Brisbane"),
      Hour = as.integer(format(datetime, "%H")),
      parameter = VIC_LEGACY_PARAM_MAP[param_id],
      value = as.numeric(value),
      unit = param_std_unit_of_measure,
      location_id = as.character(sample_point_id),
      location_name = sp_name
    ) %>%
    select(Date, Hour, parameter, value, unit, location_id, location_name)
}

#' Read 2021 per-site wide format
#' @param location_id character site ID
#' @return data.frame in normalised long format, or NULL
read_vic_2021 <- function(location_id) {
  fpath <- vic_xlsx_path(2021)
  if (!file.exists(fpath)) return(NULL)

  sheets <- excel_sheets(fpath)
  site_sheet <- sheets[grepl(paste0("_", location_id, "$"), sheets)]
  if (length(site_sheet) == 0) return(NULL)

  df <- read_xlsx(fpath, sheet = site_sheet[1], col_types = "text")
  if (nrow(df) == 0) return(NULL)

  # Identify parameter columns (everything after the 4 metadata cols)
  meta_cols <- c("datetime_AEST", "datetime_local", "location_id", "location_name")
  param_cols <- setdiff(names(df), meta_cols)
  param_cols <- param_cols[param_cols %in% names(VIC_LEGACY_PARAM_MAP)]

  if (length(param_cols) == 0) return(NULL)

  # Unit lookup for 2021 params
  unit_map <- c(
    "BPM2.5" = "ug/m3", "PM2.5" = "ug/m3",
    "PM10" = "ug/m3", "HPM10" = "ug/m3",
    "NO2" = "ppb", "O3" = "ppb", "SO2" = "ppb",
    "CO" = "ppm",
    "DBT" = "deg.C", "SWD" = "deg", "SWS" = "m/s"
  )

  df %>%
    pivot_longer(cols = all_of(param_cols),
                 names_to = "param_id",
                 values_to = "value") %>%
    mutate(
      datetime = as.POSIXct(datetime_AEST, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"),
      Date = as.Date(datetime, tz = "Australia/Brisbane"),
      Hour = as.integer(format(datetime, "%H")),
      parameter = VIC_LEGACY_PARAM_MAP[param_id],
      value = as.numeric(value),
      unit = unit_map[param_id],
      location_id = as.character(location_id),
      location_name = as.character(location_name)
    ) %>%
    filter(!is.na(parameter)) %>%
    select(Date, Hour, parameter, value, unit, location_id, location_name)
}

#' Read modern format (2022-2024) for a single year and site
#' @param year integer year
#' @param location_id character site ID
#' @return data.frame in normalised long format, or NULL
read_vic_modern <- function(year, location_id) {
  fpath <- vic_xlsx_path(year)
  if (!file.exists(fpath)) return(NULL)

  df <- read_xlsx(fpath, sheet = "AllData", col_types = "text")
  df <- df %>%
    filter(location_id == !!location_id) %>%
    filter(parameter_name %in% names(VIC_LEGACY_PARAM_MAP))

  if (nrow(df) == 0) return(NULL)

  df %>%
    mutate(
      datetime = as.POSIXct(datetime_AEST, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"),
      Date = as.Date(datetime, tz = "Australia/Brisbane"),
      Hour = as.integer(format(datetime, "%H")),
      parameter = VIC_LEGACY_PARAM_MAP[parameter_name],
      value = as.numeric(value),
      unit = unit_of_measure,
      location_id = as.character(location_id),
      location_name = as.character(location_name)
    ) %>%
    filter(!is.na(parameter)) %>%
    select(Date, Hour, parameter, value, unit, location_id, location_name)
}

#' Load VIC observations from pre-processed RDS files (fast path)
#' @param location_id character site ID
#' @param start_year integer start year
#' @param end_year integer end year
#' @return data.frame in dashboard-ready wide format, or NULL if no RDS found
load_vic_observations_rds <- function(location_id, start_year, end_year,
                                      location_name = NULL) {
  # location_id can be a vector (same station, relocated — merge all)
  all_dfs <- list()
  for (lid in location_id) {
    if (!is.null(location_name)) {
      safe_name <- gsub("[^A-Za-z0-9_-]", "_", location_name)
      rds_files <- file.path(VIC_DATA_DIR, paste0(safe_name, "_", lid, ".rds"))
      rds_files <- rds_files[file.exists(rds_files)]
    } else {
      rds_files <- list.files(VIC_DATA_DIR, pattern = paste0("_", lid, "\\.rds$"),
                              full.names = TRUE)
    }
    if (length(rds_files) > 0) {
      df <- readRDS(rds_files[1])
      if (!is.null(df) && nrow(df) > 0) {
        all_dfs[[lid]] <- df
      }
    }
  }
  if (length(all_dfs) == 0) return(NULL)

  # Combine all data from relocated stations
  df <- dplyr::bind_rows(all_dfs)

  # Filter to year range
  df <- df[lubridate::year(df$Date) >= start_year &
           lubridate::year(df$Date) <= end_year, ]

  # Combine Date + Hour into single POSIXct Date column (AEST)
  if (nrow(df) > 0 && "Hour" %in% names(df)) {
    df$Date <- as.POSIXct(paste(df$Date, df$Hour, "00", "00", sep = ":"),
                          format = "%Y-%m-%d:%H:%M:%S", tz = "Australia/Brisbane")
    df$Hour <- NULL
    df <- df[!duplicated(df$Date), ]
    df <- df[order(df$Date), ]
  }

  if (nrow(df) == 0) return(NULL)
  df
}

#' Load VIC observations from xlsx files for a site and year range
#' @param location_id character site ID
#' @param start_year integer start year
#' @param end_year integer end year
#' @param progress_callback optional function(amount, message) for progress updates
#' @return list(data = data.frame, messages = character vector)
load_vic_observations <- function(location_id, start_year, end_year,
                                  progress_callback = NULL) {
  years <- start_year:end_year
  all_data <- list()
  messages <- c()

  for (y in years) {
    if (!is.null(progress_callback)) {
      progress_callback(1 / length(years), paste("Reading", y, "data..."))
    }

    y_str <- as.character(y)
    if (!y_str %in% names(VIC_SHEET_MAP)) {
      messages <- c(messages, paste("\u2717", y, "— no data file available"))
      next
    }

    tryCatch({
      if (y <= 2020) {
        df <- read_vic_legacy(y, location_id)
      } else if (y == 2021) {
        df <- read_vic_2021(location_id)
      } else {
        df <- read_vic_modern(y, location_id)
      }

      if (is.null(df) || nrow(df) == 0) {
        messages <- c(messages, paste("\u2717", y, "— no data for this site"))
      } else {
        all_data[[y_str]] <- df
        messages <- c(messages, paste("\u2713", y, "—", nrow(df), "records loaded"))
      }
    }, error = function(e) {
      messages <<- c(messages, paste("\u2717", y, "— error:", e$message))
    })
  }

  combined <- if (length(all_data) > 0) bind_rows(all_data) else data.frame()
  # Remove rows where date parsing failed (NA dates)
  if (nrow(combined) > 0 && "Date" %in% names(combined)) {
    combined <- combined %>% filter(!is.na(Date))
  }
  list(data = combined, messages = messages)
}
