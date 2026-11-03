# Multi-State EPA Air Quality Data Processor
# Functions for cleaning, converting units, and reshaping raw API data

# NSW unit conversion factors: pphm -> µg/m³
NSW_CONVERSION_FACTORS <- list(
  OZONE = 21.4,
  NO    = 13.4,
  NO2   = 20.5,
  SO2   = 28.6,
  CO    = 1.25  # ppm -> mg/m³
)

# All possible pollutant column names after renaming
POSSIBLE_POLLUTANTS <- c(
  "CO (mg/m³)", "NO (µg/m³)", "NO2 (µg/m³)", "NOx (µg/m³)",
  "OZONE (µg/m³)", "SO2 (µg/m³)",
  "PM10 (µg/m³)", "PM2.5 (µg/m³)", "TSP (µg/m³)",
  "Wind Direction (°)", "Wind Speed (m/s)",
  "Temperature (°C)", "Solar Radiation (W/m²)", "Rainfall (mm)",
  "Humidity (%)"
)

# NSW column rename mapping: raw parameter code -> display name with units
NSW_COLUMN_RENAME_MAP <- c(
  "PM10"  = "PM10 (µg/m³)",
  "PM2.5" = "PM2.5 (µg/m³)",
  "TSP"   = "TSP (µg/m³)",
  "OZONE" = "OZONE (µg/m³)",
  "CO"    = "CO (mg/m³)",
  "SO2"   = "SO2 (µg/m³)",
  "NO"    = "NO (µg/m³)",
  "NO2"   = "NO2 (µg/m³)",
  "WDR"   = "Wind Direction (°)",
  "WSP"   = "Wind Speed (m/s)",
  "TEMP"  = "Temperature (°C)",
  "SOLAR" = "Solar Radiation (W/m²)",
  "RAIN"  = "Rainfall (mm)"
)

#' Extract and clean raw NSW observation data
#' @param df_raw data.frame from NSW API (with Parameter.ParameterCode, Parameter.Units, etc.)
#' @return data.frame with columns: Date, Hour, Value, parametercode, units
extract_nsw_columns <- function(df_raw) {
  parametercode <- df_raw$Parameter.ParameterCode
  units <- df_raw$Parameter.Units
  
  df <- subset(df_raw, select = c("Date", "Hour", "Value"))
  df$parametercode <- parametercode
  df$units <- units
  df$Date <- as.Date(df$Date, "%Y-%m-%d")
  df
}

#' Convert NSW measurement units (pphm -> µg/m³, ppm -> mg/m³)
#' @param df data.frame with columns: Value, units, parametercode
#' @return data.frame with converted Value and updated units columns
convert_nsw_units <- function(df) {
  df$Value <- ifelse(
    df$units == "pphm" & df$parametercode == "OZONE",
    df$Value * NSW_CONVERSION_FACTORS$OZONE,
    ifelse(df$units == "pphm" & df$parametercode == "NO",
      df$Value * NSW_CONVERSION_FACTORS$NO,
      ifelse(df$units == "pphm" & df$parametercode == "NO2",
        df$Value * NSW_CONVERSION_FACTORS$NO2,
        ifelse(df$units == "pphm" & df$parametercode == "SO2",
          df$Value * NSW_CONVERSION_FACTORS$SO2,
          ifelse(df$units == "ppm" & df$parametercode == "CO",
            df$Value * NSW_CONVERSION_FACTORS$CO,
            df$Value)))))
  
  df$units <- ifelse(
    df$units == "pphm" & df$parametercode %in% c("OZONE", "NO", "NO2", "SO2"),
    "µg/m³",
    ifelse(df$units == "ppm" & df$parametercode == "CO",
      "mg/m³",
      df$units))
  
  df
}

#' Clean NSW data: remove negatives, merge PM/TSP variants
#' Prefer hourly instrument (PM10) over daily instrument (PM10d) when both present.
#' @param df data.frame with columns: Date, Hour, Value, parametercode
#' @return data.frame with cleaned values and merged PM10d->PM10, PM2.5d->PM2.5, TSPd->TSP
clean_nsw_data <- function(df) {
  df$Value[df$Value < 0] <- NA
  
  # For each 'd' variant: only keep its rows where the non-d variant is absent
  # This prevents averaging daily-instrument values with hourly-instrument values
  for (pair in list(c("PM10d", "PM10"), c("PM2.5d", "PM2.5"), c("TSPd", "TSP"))) {
    d_code <- pair[1]
    h_code <- pair[2]
    if (d_code %in% df$parametercode && h_code %in% df$parametercode) {
      # Identify Date+Hour combinations where hourly instrument has data
      hourly_keys <- paste(df$Date[df$parametercode == h_code],
                           df$Hour[df$parametercode == h_code])
      daily_keys <- paste(df$Date[df$parametercode == d_code],
                          df$Hour[df$parametercode == d_code])
      # Remove 'd' variant rows that overlap with hourly data
      overlap <- daily_keys %in% hourly_keys
      if (any(overlap)) {
        remove_idx <- which(df$parametercode == d_code)[overlap]
        df <- df[-remove_idx, ]
      }
    }
  }
  
  df$parametercode <- gsub("^PM10d$", "PM10", df$parametercode)
  df$parametercode <- gsub("^PM2\\.5d$", "PM2.5", df$parametercode)
  df$parametercode <- gsub("^TSPd$", "TSP", df$parametercode)
  df
}

#' Reshape long-form NSW observations to wide-format hourly data
#' @param df data.frame with columns: Date, Hour, Value, parametercode
#' @return data.frame in wide format with renamed columns and single POSIXct Date column
reshape_nsw_to_hourly <- function(df) {
  df_wide <- dcast(df, Date + Hour ~ parametercode,
                   value.var = "Value",
                   fun.aggregate = function(x) mean(x, na.rm = TRUE))
  
  # dcast + mean(na.rm=TRUE) produces NaN for all-NA groups — convert to proper NA
  num_cols <- setdiff(names(df_wide), c("Date", "Hour"))
  df_wide[num_cols] <- lapply(df_wide[num_cols], function(x) { x[is.nan(x)] <- NA_real_; x })
  
  # Parse NSW local time with DST rules, then display it in fixed AEST.
  df_wide$Date <- as.POSIXct(paste(df_wide$Date, df_wide$Hour, "00", "00", sep = ":"),
                             format = "%Y-%m-%d:%H:%M:%S", tz = "Australia/Sydney")
  df_wide$Date <- lubridate::with_tz(df_wide$Date, tzone = "Australia/Brisbane")
  df_wide$Hour <- NULL
  
  # Rename columns with units
  for (code in names(NSW_COLUMN_RENAME_MAP)) {
    names(df_wide)[names(df_wide) == code] <- NSW_COLUMN_RENAME_MAP[[code]]
  }
  
  df_wide
}

#' Identify which pollutant columns are present in the data
#' @param df_hourly data.frame (wide format with renamed columns)
#' @return character vector of available pollutant column names
identify_pollutants <- function(df_hourly) {
  intersect(POSSIBLE_POLLUTANTS, colnames(df_hourly))
}

#' Calculate daily averages from hourly data
#' @param df_hourly data.frame with POSIXct Date and pollutant columns
#' @param available_pollutants character vector of pollutant column names
#' @return data.frame of daily averages (Date column is Date class)
calculate_daily <- function(df_hourly, available_pollutants) {
  df_hourly[available_pollutants] <- lapply(df_hourly[available_pollutants], as.numeric)
  
  tz <- attr(df_hourly$Date, "tzone")
  if (is.null(tz) || tz == "") tz <- Sys.timezone()
  
  df_hourly %>%
    mutate(Date = as.Date(Date, tz = tz)) %>%
    group_by(Date) %>%
    summarise(across(all_of(available_pollutants), \(x) mean(x, na.rm = TRUE)))
}

#' Full NSW processing pipeline: raw API data -> hourly + daily data frames
#' @param df_raw data.frame from NSW API
#' @return list(hourly = data.frame, daily = data.frame, pollutants = character)
process_nsw_observations <- function(df_raw) {
  df <- df_raw %>%
    extract_nsw_columns() %>%
    convert_nsw_units() %>%
    clean_nsw_data()
  
  df_hourly <- reshape_nsw_to_hourly(df)
  available <- identify_pollutants(df_hourly)
  df_daily <- calculate_daily(df_hourly, available)
  
  list(
    hourly = df_hourly,
    daily = df_daily,
    pollutants = available,
    total_records = nrow(df_raw)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# VIC EPA processing (local xlsx files)
# ══════════════════════════════════════════════════════════════════════════════

# ppb → µg/m³ at standard conditions (0°C, 101.325 kPa) — NEPM reference
# Consistent with NSW/QLD/SA conversions (molar volume = 22.414 L/mol at STP)
VIC_CONVERSION_PPB <- list(
  SO2 = 2.86,   # MW 64: 64/22.414 = 2.855
  NO2 = 2.05,   # MW 46: 46/22.414 = 2.052
  O3  = 2.14    # MW 48: 48/22.414 = 2.141
)
# CO: ppm → mg/m³ at 0°C
VIC_CO_FACTOR <- 1.25   # MW 28: 28/22.414 = 1.249

# VIC parameter → dashboard display column name
VIC_PARAM_MAP <- c(
  "PM10"  = "PM10 (µg/m³)",
  "PM2.5" = "PM2.5 (µg/m³)",
  "NO2"   = "NO2 (µg/m³)",
  "O3"    = "OZONE (µg/m³)",
  "SO2"   = "SO2 (µg/m³)",
  "CO"    = "CO (mg/m³)",
  "DBT"   = "Temperature (°C)",
  "SWD"   = "Wind Direction (°)",
  "SWS"   = "Wind Speed (m/s)"
)

#' Process VIC observations from normalised long format to hourly + daily
#' @param df_long data.frame with columns: Date, Hour, parameter, value, unit
#' @return list(hourly, daily, pollutants, total_records)
process_vic_observations <- function(df_long) {
  df <- df_long %>%
    filter(parameter %in% names(VIC_PARAM_MAP))

  # Unit conversions (keep negatives — VIC data is validated)
  for (gas in names(VIC_CONVERSION_PPB)) {
    mask <- df$parameter == gas & !is.na(df$value)
    df$value[mask] <- df$value[mask] * VIC_CONVERSION_PPB[[gas]]
  }
  # CO: ppm → mg/m³
  co_mask <- df$parameter == "CO" & !is.na(df$value)
  df$value[co_mask] <- df$value[co_mask] * VIC_CO_FACTOR

  # Map to display names
  df <- df %>%
    mutate(display_name = VIC_PARAM_MAP[parameter]) %>%
    filter(!is.na(display_name))

  # Reshape to wide
  df_hourly <- df %>%
    group_by(Date, Hour, display_name) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(value = ifelse(is.nan(value), NA, value)) %>%
    pivot_wider(names_from = display_name, values_from = value) %>%
    mutate(Date = as.POSIXct(paste(Date, Hour, "00", "00", sep = ":"),
                             format = "%Y-%m-%d:%H:%M:%S", tz = "Australia/Brisbane")) %>%
    select(-Hour) %>%
    arrange(Date)

  available <- identify_pollutants(df_hourly)
  df_daily <- calculate_daily(df_hourly, available)

  list(
    hourly = df_hourly,
    daily = df_daily,
    pollutants = available,
    total_records = nrow(df_long)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# QLD DES processing
# ══════════════════════════════════════════════════════════════════════════════

# Map QLD parameter_name (lowercase) -> dashboard display column name
QLD_PARAM_MAP <- c(
  "particle pm2.5"   = "PM2.5 (µg/m³)",
  "particle pm10"    = "PM10 (µg/m³)",
  "particle tsp"     = "TSP (µg/m³)",
  "ozone"            = "OZONE (µg/m³)",
  "carbon monoxide"  = "CO (mg/m³)",
  "sulfur dioxide"   = "SO2 (µg/m³)",
  "nitrogen dioxide" = "NO2 (µg/m³)",
  "nitrogen oxide"   = "NO (µg/m³)",
  "wind direction"   = "Wind Direction (°)",
  "wind speed"       = "Wind Speed (m/s)",
  "temperature"      = "Temperature (°C)",
  "humidity"         = "Humidity (%)",
  "solar radiation"  = "Solar Radiation (W/m²)",
  "rainfall"         = "Rainfall (mm)"
)

# QLD gas conversion factors: ppm -> µg/m³ (or mg/m³ for CO)
# NSW factors are pphm-based; 1 ppm = 100 pphm, so multiply NSW factors by 100
# CO: 1 ppm = 1.25 mg/m³ (same as NSW, already ppm)
QLD_CONVERSION_PPM <- list(
  "ozone"            = 2140,   # 21.4 * 100
  "nitrogen oxide"   = 1340,   # 13.4 * 100
  "nitrogen dioxide" = 2050,   # 20.5 * 100
  "sulfur dioxide"   = 2860,   # 28.6 * 100
  "carbon monoxide"  = 1.25    # ppm -> mg/m³
)

#' Process raw QLD measurements into hourly + daily data frames
#' @param df_raw data.frame from fetch_qld_observations()$data
#' @param param_lookup data.frame from fetch_qld_parameters()
#' @return list(hourly, daily, pollutants, total_records) — same structure as process_nsw_observations()
process_qld_observations <- function(df_raw, param_lookup) {
  # Join parameter names
  df <- df_raw %>%
    left_join(
      param_lookup %>% select(parameter_id, parameter_name),
      by = "parameter_id"
    ) %>%
    mutate(
      date_measured = ymd_hms(date_measured, tz = "UTC"),
      date_local    = with_tz(date_measured, "Australia/Brisbane"),
      mvalue        = as.numeric(mvalue)
    )
  
  # Remove invalid values
  df$mvalue[df$mvalue <= -1111] <- NA
  
  # Convert gas units: ppm -> µg/m³ (or mg/m³ for CO)
  param_key <- tolower(df$parameter_name)
  for (gas in names(QLD_CONVERSION_PPM)) {
    mask <- param_key == gas & !is.na(df$mvalue)
    df$mvalue[mask] <- df$mvalue[mask] * QLD_CONVERSION_PPM[[gas]]
  }
  
  # Map to display names, drop unmapped
  df <- df %>%
    mutate(display_name = QLD_PARAM_MAP[tolower(parameter_name)]) %>%
    filter(!is.na(display_name))
  
  # Reshape to wide format
  df_hourly <- df %>%
    mutate(
      Date = as.Date(date_local, tz = "Australia/Brisbane"),
      Hour = hour(date_local)
    ) %>%
    group_by(Date, Hour, display_name) %>%
    summarise(value = mean(mvalue, na.rm = TRUE), .groups = "drop") %>%
    mutate(value = ifelse(is.nan(value), NA, value)) %>%
    pivot_wider(names_from = display_name, values_from = value) %>%
    mutate(Date = as.POSIXct(paste(Date, Hour, "00", "00", sep = ":"),
                             format = "%Y-%m-%d:%H:%M:%S", tz = "Australia/Brisbane")) %>%
    select(-Hour) %>%
    arrange(Date)
  
  available <- identify_pollutants(df_hourly)
  df_daily <- calculate_daily(df_hourly, available)
  
  list(
    hourly = df_hourly,
    daily = df_daily,
    pollutants = available,
    total_records = nrow(df_raw)
  )
}
