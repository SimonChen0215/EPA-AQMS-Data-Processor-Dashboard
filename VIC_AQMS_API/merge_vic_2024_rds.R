# Merge one year's VIC Excel data into the existing per-site RDS files.
# The Excel datetime_AEST column is used as the dashboard's standard-time axis.

library(readxl)
library(dplyr)
library(tidyr)

args <- commandArgs(trailingOnly = TRUE)
TARGET_YEAR <- if (length(args) > 0) as.integer(args[1]) else 2024L
if (is.na(TARGET_YEAR)) stop("Supply a valid target year, e.g. 2025")

DATA_DIR <- file.path("..", "Victoria-AQMS-Data")
XLSX_FILE <- file.path(DATA_DIR,
  paste0(TARGET_YEAR, "_All_sites_air_quality_hourly_avg_AIR-I-F-V-VH-O-S1-DB-M2-4-0.xlsx"))

PARAM_MAP <- c(
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

GAS_FACTORS <- c(SO2 = 2.86, NO2 = 2.05, O3 = 2.14)
CO_FACTOR <- 1.25

if (!file.exists(XLSX_FILE)) {
  stop(TARGET_YEAR, " VIC Excel file not found: ", XLSX_FILE)
}

raw <- read_xlsx(XLSX_FILE, sheet = "AllData", col_types = "text")
required <- c("datetime_AEST", "location_id", "location_name",
              "value", "parameter_name")
missing <- setdiff(required, names(raw))
if (length(missing) > 0) {
  stop("Missing required Excel columns: ", paste(missing, collapse = ", "))
}

# Derive the date/hour directly from the explicit AEST text field. This avoids
# applying Victoria daylight-saving rules to a standard-time data column.
new_data <- raw %>%
  filter(parameter_name %in% names(PARAM_MAP)) %>%
  mutate(
    Date = as.Date(substr(datetime_AEST, 1, 10)),
    Hour = as.integer(substr(datetime_AEST, 12, 13)),
    parameter = parameter_name,
    value = as.numeric(value)
  ) %>%
  filter(!is.na(Date), !is.na(Hour))

for (gas in names(GAS_FACTORS)) {
  mask <- new_data$parameter == gas & !is.na(new_data$value)
  new_data$value[mask] <- new_data$value[mask] * GAS_FACTORS[[gas]]
}
co_mask <- new_data$parameter == "CO" & !is.na(new_data$value)
new_data$value[co_mask] <- new_data$value[co_mask] * CO_FACTOR

new_data$display_name <- unname(PARAM_MAP[new_data$parameter])
new_data <- new_data[!is.na(new_data$display_name), ]

site_data <- split(new_data, new_data$location_id)
new_key <- as.numeric(new_data$Date) * 24 + new_data$Hour
new_min_key <- min(new_key)
new_max_key <- max(new_key)
updated_files <- character()
params_file <- file.path(DATA_DIR, "vic_station_params.rds")
station_params <- if (file.exists(params_file)) readRDS(params_file) else list()

for (site_id in names(site_data)) {
  site <- site_data[[site_id]]
  new_wide <- site %>%
    group_by(Date, Hour, display_name) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(value = ifelse(is.nan(value), NA_real_, value)) %>%
    pivot_wider(names_from = display_name, values_from = value) %>%
    arrange(Date, Hour)

  safe_name <- gsub("[^A-Za-z0-9_-]", "_", unique(site$location_name)[1])
  expected_file <- file.path(DATA_DIR, paste0(safe_name, "_", site_id, ".rds"))
  existing_file <- if (file.exists(expected_file)) expected_file else character()

  if (length(existing_file) > 0) {
    old <- readRDS(existing_file[1])
    old_key <- as.numeric(old$Date) * 24 + old$Hour
    old <- old[old_key < new_min_key | old_key > new_max_key, , drop = FALSE]
    merged <- bind_rows(old, new_wide) %>% arrange(Date, Hour)
    output_file <- existing_file[1]
  } else {
    output_file <- file.path(DATA_DIR, paste0(safe_name, "_", site_id, ".rds"))
    merged <- new_wide
  }

  saveRDS(merged, output_file)
  updated_files <- c(updated_files, output_file)
  station_params[[site_id]] <- union(
    station_params[[site_id]], setdiff(names(merged), c("Date", "Hour"))
  )
}

saveRDS(station_params, params_file)

cat("Updated", length(updated_files), "site RDS files for", TARGET_YEAR, ".\n")
cat("Replaced standard-time keys:", new_min_key, "to", new_max_key, "\n")
cat("Processed rows:", nrow(new_data), "\n")