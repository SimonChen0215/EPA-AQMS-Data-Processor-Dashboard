# VIC EPA Data Preprocessing Script
# Reads all xlsx files (2016-2024), normalises across 3 format eras,
# converts units, reshapes to wide format, and saves per-site RDS files.
#
# Run from: Multi_State_AQ_Dashboard/VIC_AQMS_API/
# Output: Victoria-AQMS-Data/{location_name}_{location_id}.rds
#         Victoria-AQMS-Data/vic_station_params.rds

library(readxl)
library(dplyr)
library(tidyr)

# ── Configuration ────────────────────────────────────────────────────────────
VIC_DATA_DIR  <- file.path("..", "Victoria-AQMS-Data")
OUTPUT_DIR    <- VIC_DATA_DIR
YEARS         <- 2016:2024

# Sheet name mapping per year
VIC_SHEET_MAP <- list(
  "2016" = "Data",   "2017" = "Data",
  "2018" = "2018",   "2019" = "2019",   "2020" = "2020",
  "2021" = NULL,     # per-site wide sheets only
  "2022" = "AllData", "2023" = "AllData", "2024" = "AllData"
)

# Legacy param_id -> normalised parameter name
VIC_LEGACY_PARAM_MAP <- c(
  "BPM2.5" = "PM2.5", "PPM2.5" = "PM2.5", "PM2.5" = "PM2.5",
  "HPM10"  = "PM10",  "PM10"   = "PM10",
  "NO2" = "NO2", "O3" = "O3", "SO2" = "SO2", "CO" = "CO",
  "DBT" = "DBT", "SWD" = "SWD", "SWS" = "SWS"
)

# ppb -> µg/m³ conversion factors at 0°C, 101.325 kPa (NEPM standard)
# Consistent with NSW/QLD/SA (molar volume = 22.414 L/mol at STP)
VIC_CONVERSION_PPB <- list(SO2 = 2.86, NO2 = 2.05, O3 = 2.14)
VIC_CO_FACTOR <- 1.25   # ppm -> mg/m³ (MW 28 / 22.414)

# Parameter -> dashboard column name
VIC_PARAM_MAP <- c(
  "PM10"  = "PM10 (\u00b5g/m\u00b3)",
  "PM2.5" = "PM2.5 (\u00b5g/m\u00b3)",
  "NO2"   = "NO2 (\u00b5g/m\u00b3)",
  "O3"    = "OZONE (\u00b5g/m\u00b3)",
  "SO2"   = "SO2 (\u00b5g/m\u00b3)",
  "CO"    = "CO (mg/m\u00b3)",
  "DBT"   = "Temperature (\u00b0C)",
  "SWD"   = "Wind Direction (\u00b0)",
  "SWS"   = "Wind Speed (m/s)"
)

# Unit lookup for 2021 per-site wide format
UNIT_MAP_2021 <- c(
  "BPM2.5" = "ug/m3", "PM2.5" = "ug/m3",
  "PM10" = "ug/m3", "HPM10" = "ug/m3",
  "NO2" = "ppb", "O3" = "ppb", "SO2" = "ppb",
  "CO" = "ppm",
  "DBT" = "deg.C", "SWD" = "deg", "SWS" = "m/s"
)

# ── Helper: xlsx path ────────────────────────────────────────────────────────
vic_xlsx_path <- function(year) {
  file.path(VIC_DATA_DIR,
    paste0(year, "_All_sites_air_quality_hourly_avg_AIR-I-F-V-VH-O-S1-DB-M2-4-0.xlsx"))
}

# ── Helper: parse Excel serial datetime ──────────────────────────────────────
# When readxl reads datetime as text, Excel stores as numeric (days since 1899-12-30)
parse_excel_datetime <- function(x) {
  num <- suppressWarnings(as.numeric(x))
  # If numeric conversion succeeds, it's an Excel serial number
  # Otherwise it's already a text datetime
  ifelse(
    !is.na(num) & num > 30000 & num < 60000,
    as.POSIXct(num * 86400, origin = "1899-12-30", tz = "Australia/Brisbane"),
    as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")
  ) |> as.POSIXct(origin = "1970-01-01", tz = "Australia/Brisbane")
}

# ── Read legacy format (2016-2020) — ALL sites ──────────────────────────────
read_all_legacy <- function(year) {
  fpath <- vic_xlsx_path(year)
  if (!file.exists(fpath)) return(NULL)
  sheet_name <- VIC_SHEET_MAP[[as.character(year)]]
  if (is.null(sheet_name)) return(NULL)

  cat("  Reading", year, "(legacy, sheet:", sheet_name, ")...")
  df <- read_xlsx(fpath, sheet = sheet_name, col_types = "text")
  cat(" ", nrow(df), "rows\n")

  df <- df %>%
    filter(time_basis_id == "1HR_AV") %>%
    filter(param_id %in% names(VIC_LEGACY_PARAM_MAP))

  if (nrow(df) == 0) return(NULL)

  df %>%
    mutate(
      datetime = parse_excel_datetime(sample_datetime),
      Date = as.Date(datetime, tz = "Australia/Brisbane"),
      Hour = as.integer(format(datetime, "%H")),
      parameter = VIC_LEGACY_PARAM_MAP[param_id],
      value = as.numeric(value),
      location_id = as.character(sample_point_id),
      location_name = sp_name
    ) %>%
    filter(!is.na(Date)) %>%
    select(Date, Hour, parameter, value, location_id, location_name)
}

# ── Read 2021 per-site wide format — ALL sites ──────────────────────────────
read_all_2021 <- function() {
  fpath <- vic_xlsx_path(2021)
  if (!file.exists(fpath)) return(NULL)

  sheets <- excel_sheets(fpath)
  site_sheets <- sheets[grepl("_\\d+$", sheets)]
  cat("  Reading 2021 (per-site wide,", length(site_sheets), "sheets)...\n")

  meta_cols <- c("datetime_AEST", "datetime_local", "location_id", "location_name")
  all_data <- list()

  for (s in site_sheets) {
    df <- tryCatch(
      read_xlsx(fpath, sheet = s, col_types = "text"),
      error = function(e) { cat("    Skipping sheet", s, ":", e$message, "\n"); NULL }
    )
    if (is.null(df) || nrow(df) == 0) next

    param_cols <- setdiff(names(df), meta_cols)
    param_cols <- param_cols[param_cols %in% names(VIC_LEGACY_PARAM_MAP)]
    if (length(param_cols) == 0) next

    long <- df %>%
      pivot_longer(cols = all_of(param_cols),
                   names_to = "param_id",
                   values_to = "value") %>%
      mutate(
        datetime = as.POSIXct(datetime_AEST, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"),
        Date = as.Date(datetime, tz = "Australia/Brisbane"),
        Hour = as.integer(format(datetime, "%H")),
        parameter = VIC_LEGACY_PARAM_MAP[param_id],
        value = as.numeric(value),
        location_id = as.character(location_id),
        location_name = as.character(location_name)
      ) %>%
      filter(!is.na(parameter), !is.na(Date)) %>%
      select(Date, Hour, parameter, value, location_id, location_name)

    all_data[[s]] <- long
    cat("    ", s, ":", nrow(long), "records\n")
  }

  if (length(all_data) > 0) bind_rows(all_data) else NULL
}

# ── Read modern format (2022-2024) — ALL sites ──────────────────────────────
read_all_modern <- function(year) {
  fpath <- vic_xlsx_path(year)
  if (!file.exists(fpath)) return(NULL)

  cat("  Reading", year, "(modern, AllData sheet)...")
  df <- read_xlsx(fpath, sheet = "AllData", col_types = "text")
  cat(" ", nrow(df), "rows\n")

  df <- df %>%
    filter(parameter_name %in% names(VIC_LEGACY_PARAM_MAP))

  if (nrow(df) == 0) return(NULL)

  df %>%
    mutate(
      datetime = as.POSIXct(datetime_AEST, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"),
      Date = as.Date(datetime, tz = "Australia/Brisbane"),
      Hour = as.integer(format(datetime, "%H")),
      parameter = VIC_LEGACY_PARAM_MAP[parameter_name],
      value = as.numeric(value),
      location_id = as.character(location_id),
      location_name = as.character(location_name)
    ) %>%
    filter(!is.na(parameter), !is.na(Date)) %>%
    select(Date, Hour, parameter, value, location_id, location_name)
}

# ── Main preprocessing pipeline ─────────────────────────────────────────────
preprocess_all_vic <- function() {
  cat("=== VIC EPA Data Preprocessing ===\n")
  cat("Reading", length(YEARS), "xlsx files...\n\n")

  # 1. Read all years into one big long-format data.frame
  all_data <- list()

  for (y in YEARS) {
    tryCatch({
      if (y <= 2020) {
        df <- read_all_legacy(y)
      } else if (y == 2021) {
        df <- read_all_2021()
      } else {
        df <- read_all_modern(y)
      }
      if (!is.null(df) && nrow(df) > 0) {
        all_data[[as.character(y)]] <- df
        cat("  =>", nrow(df), "records after filtering\n")
      }
    }, error = function(e) {
      cat("  ERROR reading", y, ":", e$message, "\n")
    })
  }

  combined <- bind_rows(all_data)
  cat("\nTotal records (all years, all sites):", nrow(combined), "\n")

  # 2. Apply unit conversions
  cat("Applying unit conversions...\n")
  for (gas in names(VIC_CONVERSION_PPB)) {
    mask <- combined$parameter == gas & !is.na(combined$value)
    combined$value[mask] <- combined$value[mask] * VIC_CONVERSION_PPB[[gas]]
  }
  co_mask <- combined$parameter == "CO" & !is.na(combined$value)
  combined$value[co_mask] <- combined$value[co_mask] * VIC_CO_FACTOR

  # 3. Map to display column names
  combined$display_name <- VIC_PARAM_MAP[combined$parameter]
  combined <- combined[!is.na(combined$display_name), ]

  # 4. Split by site and save per-site RDS files
  sites <- unique(combined[, c("location_id", "location_name")])
  cat("\nProcessing", nrow(sites), "sites...\n")

  station_params <- list()

  for (i in seq_len(nrow(sites))) {
    sid <- sites$location_id[i]
    sname <- sites$location_name[i]

    site_data <- combined[combined$location_id == sid, ]

    # Reshape to wide format
    df_wide <- site_data %>%
      group_by(Date, Hour, display_name) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(value = ifelse(is.nan(value), NA_real_, value)) %>%
      pivot_wider(names_from = display_name, values_from = value) %>%
      arrange(Date, Hour)

    # Track available parameters
    possible <- c(
      "CO (mg/m\u00b3)", "NO (\u00b5g/m\u00b3)", "NO2 (\u00b5g/m\u00b3)",
      "NOx (\u00b5g/m\u00b3)", "OZONE (\u00b5g/m\u00b3)", "SO2 (\u00b5g/m\u00b3)",
      "PM10 (\u00b5g/m\u00b3)", "PM2.5 (\u00b5g/m\u00b3)", "TSP (\u00b5g/m\u00b3)",
      "Wind Direction (\u00b0)", "Wind Speed (m/s)",
      "Temperature (\u00b0C)", "Solar Radiation (W/m\u00b2)", "Rainfall (mm)",
      "Humidity (%)"
    )
    avail <- intersect(possible, names(df_wide))
    station_params[[sid]] <- avail

    # Clean filename: replace spaces with underscores, remove special chars
    safe_name <- gsub("[^A-Za-z0-9_-]", "_", sname)
    fname <- paste0(safe_name, "_", sid, ".rds")
    fpath <- file.path(OUTPUT_DIR, fname)

    saveRDS(df_wide, fpath)

    date_range <- paste(min(df_wide$Date, na.rm = TRUE), "\u2192",
                        max(df_wide$Date, na.rm = TRUE))
    cat(sprintf("  %-35s  %6d rows  %2d params  %s\n",
                paste0(sname, " (", sid, ")"),
                nrow(df_wide), length(avail), date_range))
  }

  # 5. Save station params cache
  params_path <- file.path(OUTPUT_DIR, "vic_station_params.rds")
  saveRDS(station_params, params_path)
  cat("\nStation params cache saved to:", params_path, "\n")

  # 6. Save site metadata (for map display)
  # Collect lat/lon from the raw data — need to re-read one modern file
  cat("Building site metadata...\n")
  site_meta <- load_site_coordinates()
  meta_path <- file.path(OUTPUT_DIR, "vic_site_metadata.rds")
  saveRDS(site_meta, meta_path)
  cat("Site metadata saved to:", meta_path, "\n")

  cat("\n=== Done! ===\n")
  cat(nrow(sites), "site RDS files saved to", OUTPUT_DIR, "\n")
}

# ── Load site coordinates from xlsx files ────────────────────────────────────
load_site_coordinates <- function() {
  all_sites <- data.frame(
    location_id = character(),
    location_name = character(),
    Latitude = numeric(),
    Longitude = numeric(),
    stringsAsFactors = FALSE
  )

  # Modern files have lat/lon directly
  for (y in c(2024, 2023, 2022)) {
    fpath <- vic_xlsx_path(y)
    if (!file.exists(fpath)) next
    tryCatch({
      df <- read_xlsx(fpath, sheet = "AllData", col_types = "text", n_max = 50000)
      sites <- df %>%
        distinct(location_id, location_name, latitude, longitude) %>%
        mutate(Latitude = as.numeric(latitude),
               Longitude = as.numeric(longitude)) %>%
        select(location_id, location_name, Latitude, Longitude) %>%
        filter(!is.na(Latitude), !is.na(Longitude))
      all_sites <- bind_rows(all_sites, sites)
    }, error = function(e) NULL)
  }

  # Legacy files
  for (y in 2020:2016) {
    fpath <- vic_xlsx_path(y)
    if (!file.exists(fpath)) next
    sheet_name <- VIC_SHEET_MAP[[as.character(y)]]
    if (is.null(sheet_name)) next
    tryCatch({
      df <- read_xlsx(fpath, sheet = sheet_name, col_types = "text", n_max = 50000)
      sites <- df %>%
        distinct(sample_point_id, sp_name, latitude, longitude) %>%
        mutate(location_id = as.character(sample_point_id),
               location_name = sp_name,
               Latitude = as.numeric(latitude),
               Longitude = as.numeric(longitude)) %>%
        select(location_id, location_name, Latitude, Longitude) %>%
        filter(!is.na(Latitude), !is.na(Longitude))
      all_sites <- bind_rows(all_sites, sites)
    }, error = function(e) NULL)
  }

  all_sites %>%
    distinct(location_id, .keep_all = TRUE)
}

# ── Run ──────────────────────────────────────────────────────────────────────
preprocess_all_vic()
