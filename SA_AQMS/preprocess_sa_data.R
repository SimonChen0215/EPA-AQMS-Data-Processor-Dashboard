# ============================================================================
# SA EPA Air Quality Data Preprocessor
# Extracts ZIPs, reads CSVs, converts units, aggregates met to hourly,
# and saves pre-processed RDS files for instant dashboard loading.
#
# Data range: 2020+ only (all hourly for particle/gaseous, simpler headers)
# Output: SA-AQMS-Data/{StationFolder}.rds
#         SA-AQMS-Data/sa_station_params.rds
#         SA-AQMS-Data/sa_lead_data.rds
# ============================================================================

library(dplyr)
library(tidyr)
library(lubridate)

SA_DATA_DIR   <- file.path("..", "SA-AQMS-Data")
OUTPUT_DIR    <- SA_DATA_DIR
MIN_YEAR      <- 2020

# --- Column name patterns → dashboard column mapping -------------------------

# Particle CSV columns (regex patterns to handle TEOM/BAM variants)
SA_PARTICLE_PATTERNS <- list(
  "PM10 (µg/m³)"  = "^PM10\\s+(TEOM|BAM)\\s+ug/m3$",
  "PM2.5 (µg/m³)" = "^PM2\\.5\\s+(TEOM|BAM)\\s+ug/m3$"
)

# Gaseous CSV columns → dashboard column names (1-hour values only, skip 8hr)
SA_GAS_COL_MAP <- c(
  "O3 UVA ppm"                       = "OZONE",
  "NO Chemiluminescence ppm"         = "NO",
  "NO2 calc Chemiluminescence ppm"   = "NO2",
  "NOx Chemiluminescence ppm"        = "NOx",
  "SO2 UVF ppm"                      = "SO2",
  "CO GPC ppm"                       = "CO"
)

# Gas unit conversion: ppm → µg/m³ (or mg/m³ for CO)
SA_GAS_CONVERSION <- list(
  OZONE = 2140,   # 1 ppm O3  → µg/m³
  NO    = 1340,   # 1 ppm NO  → µg/m³
  NO2   = 2050,   # 1 ppm NO2 → µg/m³
  NOx   = 2050,   # as NO2 equivalent → µg/m³
  SO2   = 2860,   # 1 ppm SO2 → µg/m³
  CO    = 1.25    # 1 ppm CO  → mg/m³
)

# Final display column names for gases (after conversion)
SA_GAS_DISPLAY <- c(
  OZONE = "OZONE (µg/m³)",
  NO    = "NO (µg/m³)",
  NO2   = "NO2 (µg/m³)",
  NOx   = "NOx (µg/m³)",
  SO2   = "SO2 (µg/m³)",
  CO    = "CO (mg/m³)"
)

# Met CSV columns → dashboard column names
SA_MET_COL_MAP <- c(
  "Wind Speed m/s"     = "Wind Speed (m/s)",
  "Wind Direction deg" = "Wind Direction (°)",
  "Temperature Deg_C"  = "Temperature (°C)",
  "Relative Humidity %" = "Humidity (%)"
)

# --- Helper functions ---------------------------------------------------------

#' Extract ZIP and read all CSVs inside, returning combined data.frame
#' @param zip_path Path to ZIP file
#' @param col_types Column types for read.csv (or NULL for auto)
#' @return data.frame combining all CSVs in the ZIP
read_csvs_from_zip <- function(zip_path) {
  tmp_dir <- tempfile("sa_extract_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  utils::unzip(zip_path, exdir = tmp_dir)
  csv_files <- list.files(tmp_dir, pattern = "\\.csv$", full.names = TRUE,
                          recursive = TRUE, ignore.case = TRUE)

  if (length(csv_files) == 0) return(NULL)

  dfs <- lapply(csv_files, function(f) {
    tryCatch(
      read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) {
        message("  Warning: failed to read ", basename(f), ": ", e$message)
        NULL
      }
    )
  })
  dfs <- Filter(Negate(is.null), dfs)
  if (length(dfs) == 0) return(NULL)
  bind_rows(dfs)
}

#' Parse SA date/time column to POSIXct
#' Handles: "DD/MM/YYYY HH:MM", "D/MM/YYYY H:MM", "DD/MM/YYYY HH:MM:SS"
#' @param x character vector
#' @return POSIXct vector
parse_sa_datetime <- function(x) {
  # Try DD/MM/YYYY HH:MM first (most common)
  dt <- dmy_hm(x, quiet = TRUE)
  # Fill NAs with DD/MM/YYYY HH:MM:SS
  na_idx <- is.na(dt)
  if (any(na_idx)) {
    dt[na_idx] <- dmy_hms(x[na_idx], quiet = TRUE)
  }
  dt
}

#' Filter ZIPs to those containing data >= MIN_YEAR
#' @param zip_files character vector of zip file paths
#' @return filtered character vector
filter_zips_by_year <- function(zip_files) {
  # Extract year from filename patterns like _2020.zip, _2025.zip, _2015-2019.zip
  keep <- vapply(zip_files, function(f) {
    bn <- basename(f)
    # Multi-year range: e.g., _2015-2019.zip
    m <- regmatches(bn, regexpr("(\\d{4})-(\\d{4})", bn))
    if (length(m) == 1 && nchar(m) > 0) {
      end_year <- as.integer(sub(".*-(\\d{4})", "\\1", m))
      return(end_year >= MIN_YEAR)
    }
    # Single year: e.g., _2025.zip
    m <- regmatches(bn, regexpr("\\d{4}", bn))
    if (length(m) == 1) {
      return(as.integer(m) >= MIN_YEAR)
    }
    # Unknown pattern — include it
    TRUE
  }, logical(1))
  zip_files[keep]
}

# --- Processing functions per data type ---------------------------------------

#' Process particle ZIPs for a station
#' @param station_dir Path to station folder (e.g., SA-AQMS-Data/Christie_Downs)
#' @return data.frame with Date, Hour, PM10 (µg/m³), PM2.5 (µg/m³) or NULL
process_particle <- function(station_dir) {
  particle_dir <- file.path(station_dir, "particle")
  if (!dir.exists(particle_dir)) return(NULL)

  zips <- list.files(particle_dir, pattern = "\\.zip$", full.names = TRUE)
  zips <- filter_zips_by_year(zips)
  if (length(zips) == 0) return(NULL)

  all_dfs <- list()
  for (z in zips) {
    df <- read_csvs_from_zip(z)
    if (is.null(df) || nrow(df) == 0) next

    # Find the datetime column (quoted or unquoted)
    dt_col <- grep("^Date/?Time$", names(df), value = TRUE, ignore.case = TRUE)
    if (length(dt_col) == 0) {
      dt_col <- grep("^Date.Time$", names(df), value = TRUE, ignore.case = TRUE)
    }
    if (length(dt_col) == 0) next
    dt_col <- dt_col[1]

    # Parse datetime
    df$datetime <- parse_sa_datetime(df[[dt_col]])
    df <- df[!is.na(df$datetime), ]
    if (nrow(df) == 0) next

    # Filter to >= MIN_YEAR
    df <- df[year(df$datetime) >= MIN_YEAR, ]
    if (nrow(df) == 0) next

    # Detect time resolution: if any timestamps have minutes not 00, it's sub-hourly
    minutes <- minute(df$datetime)
    is_subhourly <- any(minutes != 0, na.rm = TRUE)

    # Map PM columns
    result <- data.frame(
      datetime = df$datetime,
      stringsAsFactors = FALSE
    )

    for (display_name in names(SA_PARTICLE_PATTERNS)) {
      pattern <- SA_PARTICLE_PATTERNS[[display_name]]
      matched_col <- grep(pattern, names(df), value = TRUE, ignore.case = TRUE)
      if (length(matched_col) > 0) {
        result[[display_name]] <- as.numeric(df[[matched_col[1]]])
      }
    }

    if (ncol(result) <= 1) next  # no PM columns found

    # If sub-hourly, aggregate to hourly
    if (is_subhourly) {
      result$Date <- as.Date(result$datetime)
      result$Hour <- hour(result$datetime)
      pm_cols <- setdiff(names(result), c("datetime", "Date", "Hour"))
      result <- result %>%
        group_by(Date, Hour) %>%
        summarise(across(all_of(pm_cols), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
        mutate(across(all_of(pm_cols), ~ifelse(is.nan(.x), NA_real_, .x)))
    } else {
      result$Date <- as.Date(result$datetime)
      result$Hour <- hour(result$datetime)
      result$datetime <- NULL
    }

    all_dfs[[length(all_dfs) + 1]] <- result
  }

  if (length(all_dfs) == 0) return(NULL)
  bind_rows(all_dfs) %>%
    group_by(Date, Hour) %>%
    summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(across(-c(Date, Hour), ~ifelse(is.nan(.x), NA_real_, .x))) %>%
    arrange(Date, Hour)
}

#' Process gaseous ZIPs for a station
#' @param station_dir Path to station folder
#' @return data.frame with Date, Hour, converted gas columns or NULL
process_gaseous <- function(station_dir) {
  gas_dir <- file.path(station_dir, "gaseous")
  if (!dir.exists(gas_dir)) return(NULL)

  zips <- list.files(gas_dir, pattern = "\\.zip$", full.names = TRUE)
  zips <- filter_zips_by_year(zips)
  if (length(zips) == 0) return(NULL)

  all_dfs <- list()
  for (z in zips) {
    df <- read_csvs_from_zip(z)
    if (is.null(df) || nrow(df) == 0) next

    dt_col <- grep("^Date/?Time$", names(df), value = TRUE, ignore.case = TRUE)
    if (length(dt_col) == 0) {
      dt_col <- grep("^Date.Time$", names(df), value = TRUE, ignore.case = TRUE)
    }
    if (length(dt_col) == 0) next
    dt_col <- dt_col[1]

    df$datetime <- parse_sa_datetime(df[[dt_col]])
    df <- df[!is.na(df$datetime), ]
    if (nrow(df) == 0) next
    df <- df[year(df$datetime) >= MIN_YEAR, ]
    if (nrow(df) == 0) next

    minutes <- minute(df$datetime)
    is_subhourly <- any(minutes != 0, na.rm = TRUE)

    result <- data.frame(datetime = df$datetime, stringsAsFactors = FALSE)

    for (csv_col in names(SA_GAS_COL_MAP)) {
      if (csv_col %in% names(df)) {
        param_code <- SA_GAS_COL_MAP[[csv_col]]
        raw_values <- as.numeric(df[[csv_col]])
        # Convert ppm → µg/m³ (or mg/m³ for CO)
        factor <- SA_GAS_CONVERSION[[param_code]]
        if (!is.null(factor)) {
          raw_values <- raw_values * factor
        }
        display_name <- SA_GAS_DISPLAY[[param_code]]
        result[[display_name]] <- raw_values
      }
    }

    if (ncol(result) <= 1) next

    if (is_subhourly) {
      result$Date <- as.Date(result$datetime)
      result$Hour <- hour(result$datetime)
      gas_cols <- setdiff(names(result), c("datetime", "Date", "Hour"))
      result <- result %>%
        group_by(Date, Hour) %>%
        summarise(across(all_of(gas_cols), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
        mutate(across(all_of(gas_cols), ~ifelse(is.nan(.x), NA_real_, .x)))
    } else {
      result$Date <- as.Date(result$datetime)
      result$Hour <- hour(result$datetime)
      result$datetime <- NULL
    }

    all_dfs[[length(all_dfs) + 1]] <- result
  }

  if (length(all_dfs) == 0) return(NULL)
  bind_rows(all_dfs) %>%
    group_by(Date, Hour) %>%
    summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(across(-c(Date, Hour), ~ifelse(is.nan(.x), NA_real_, .x))) %>%
    arrange(Date, Hour)
}

#' Process met ZIPs for a station (10-min → hourly aggregation)
#' @param station_dir Path to station folder
#' @return data.frame with Date, Hour, met columns or NULL
process_met <- function(station_dir) {
  met_dir <- file.path(station_dir, "met")
  if (!dir.exists(met_dir)) return(NULL)

  zips <- list.files(met_dir, pattern = "\\.zip$", full.names = TRUE)
  zips <- filter_zips_by_year(zips)
  if (length(zips) == 0) return(NULL)

  all_dfs <- list()
  for (z in zips) {
    df <- read_csvs_from_zip(z)
    if (is.null(df) || nrow(df) == 0) next

    # Met datetime column is "Date Time" (with space, no slash)
    dt_col <- grep("^Date\\s*Time$", names(df), value = TRUE, ignore.case = TRUE)
    if (length(dt_col) == 0) {
      # Fallback: try Date/Time
      dt_col <- grep("^Date/?Time$", names(df), value = TRUE, ignore.case = TRUE)
    }
    if (length(dt_col) == 0) next
    dt_col <- dt_col[1]

    df$datetime <- parse_sa_datetime(df[[dt_col]])
    df <- df[!is.na(df$datetime), ]
    if (nrow(df) == 0) next
    df <- df[year(df$datetime) >= MIN_YEAR, ]
    if (nrow(df) == 0) next

    result <- data.frame(datetime = df$datetime, stringsAsFactors = FALSE)

    for (csv_col in names(SA_MET_COL_MAP)) {
      if (csv_col %in% names(df)) {
        result[[SA_MET_COL_MAP[[csv_col]]]] <- as.numeric(df[[csv_col]])
      }
    }

    if (ncol(result) <= 1) next

    # Aggregate 10-min to hourly (scalar mean for all — wind direction
    # is approximate but acceptable for background AQ assessment)
    result$Date <- as.Date(result$datetime)
    result$Hour <- hour(result$datetime)
    met_cols <- setdiff(names(result), c("datetime", "Date", "Hour"))

    result <- result %>%
      group_by(Date, Hour) %>%
      summarise(across(all_of(met_cols), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
      mutate(across(all_of(met_cols), ~ifelse(is.nan(.x), NA_real_, .x)))

    all_dfs[[length(all_dfs) + 1]] <- result
  }

  if (length(all_dfs) == 0) return(NULL)
  bind_rows(all_dfs) %>%
    group_by(Date, Hour) %>%
    summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(across(-c(Date, Hour), ~ifelse(is.nan(.x), NA_real_, .x))) %>%
    arrange(Date, Hour)
}

#' Process Lead (Pb) data — single CSV, 6-day HVS samples
#' @return data.frame or NULL
process_lead <- function() {
  lead_csv <- file.path(SA_DATA_DIR, "Port_Pirie_HVS_Lead", "lead", "ptphvspb_2009.csv")
  if (!file.exists(lead_csv)) return(NULL)

  df <- read.csv(lead_csv, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(df) == 0) return(NULL)

  # Parse date (D/MM/YYYY format)
  df$Date <- dmy(df$Date, quiet = TRUE)
  df <- df[!is.na(df$Date), ]

  # Rename columns to standardised names
  col_map <- c(
    "PTP01 Port Pirie Oliver Street TSP Lead ug/m3" = "Pb TSP Oliver St (µg/m³)",
    "PTP01 Port Pirie Oliver Street PM10 Lead ug/m3" = "Pb PM10 Oliver St (µg/m³)",
    "PTP03 Port Pirie West TSP Lead ug/m3" = "Pb TSP Pirie West (µg/m³)",
    "PTP04 Port Pirie Ellen Street TSP Lead ug/m3" = "Pb TSP Ellen St (µg/m³)",
    "PTP05 Port Pirie Frank Green Park TSP ug/m3" = "Pb TSP Frank Green (µg/m³)"
  )

  result <- data.frame(Date = df$Date)
  for (old_name in names(col_map)) {
    if (old_name %in% names(df)) {
      result[[col_map[[old_name]]]] <- as.numeric(df[[old_name]])
    }
  }

  result %>% arrange(Date)
}

# --- Main processing pipeline ------------------------------------------------

#' Process all SA stations and save RDS files
#' @param stations Character vector of station folder names, or "all"
preprocess_all_sa <- function(stations = "all") {
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

  all_station_dirs <- list.dirs(SA_DATA_DIR, recursive = FALSE, full.names = FALSE)
  # Exclude "processed" directory
  all_station_dirs <- setdiff(all_station_dirs, "processed")

  if (!identical(stations, "all")) {
    all_station_dirs <- intersect(all_station_dirs, stations)
  }

  station_params <- list()

  for (stn in all_station_dirs) {
    station_path <- file.path(SA_DATA_DIR, stn)
    message("\n=== Processing: ", stn, " ===")

    # Process each data type
    df_particle <- tryCatch(process_particle(station_path),
                            error = function(e) { message("  Particle error: ", e$message); NULL })
    df_gaseous  <- tryCatch(process_gaseous(station_path),
                            error = function(e) { message("  Gaseous error: ", e$message); NULL })
    df_met      <- tryCatch(process_met(station_path),
                            error = function(e) { message("  Met error: ", e$message); NULL })

    # Report what we got
    if (!is.null(df_particle)) message("  Particle: ", nrow(df_particle), " hourly records, cols: ",
                                        paste(setdiff(names(df_particle), c("Date","Hour")), collapse=", "))
    if (!is.null(df_gaseous))  message("  Gaseous:  ", nrow(df_gaseous), " hourly records, cols: ",
                                        paste(setdiff(names(df_gaseous), c("Date","Hour")), collapse=", "))
    if (!is.null(df_met))      message("  Met:      ", nrow(df_met), " hourly records, cols: ",
                                        paste(setdiff(names(df_met), c("Date","Hour")), collapse=", "))

    # Merge all data types by Date + Hour
    dfs <- Filter(Negate(is.null), list(df_particle, df_gaseous, df_met))
    if (length(dfs) == 0) {
      message("  SKIP — no data found (>= ", MIN_YEAR, ")")
      next
    }

    merged <- dfs[[1]]
    if (length(dfs) > 1) {
      for (i in 2:length(dfs)) {
        merged <- full_join(merged, dfs[[i]], by = c("Date", "Hour"))
      }
    }
    merged <- merged %>% arrange(Date, Hour)

    # Track available parameters for this station
    param_cols <- setdiff(names(merged), c("Date", "Hour"))
    station_params[[stn]] <- param_cols

    # Save RDS
    rds_path <- file.path(OUTPUT_DIR, paste0(stn, ".rds"))
    saveRDS(merged, rds_path)
    size_kb <- round(file.size(rds_path) / 1024)
    message("  Saved: ", basename(rds_path), " (", size_kb, " KB, ",
            nrow(merged), " rows, ", length(param_cols), " params, ",
            min(merged$Date, na.rm=TRUE), " to ", max(merged$Date, na.rm=TRUE), ")")
  }

  # Process Lead data separately
  message("\n=== Processing: Lead (Pb) HVS data ===")
  df_lead <- tryCatch(process_lead(),
                       error = function(e) { message("  Lead error: ", e$message); NULL })
  if (!is.null(df_lead)) {
    lead_path <- file.path(OUTPUT_DIR, "sa_lead_data.rds")
    saveRDS(df_lead, lead_path)
    message("  Saved: sa_lead_data.rds (", nrow(df_lead), " rows, ",
            min(df_lead$Date, na.rm=TRUE), " to ", max(df_lead$Date, na.rm=TRUE), ")")
  }

  # Save station → parameter mapping
  params_path <- file.path(OUTPUT_DIR, "sa_station_params.rds")
  saveRDS(station_params, params_path)
  message("\n=== Station parameter mapping saved ===")
  message("Stations processed: ", length(station_params))

  invisible(station_params)
}

# --- Run ----------------------------------------------------------------------
# Uncomment to run:
# preprocess_all_sa()
