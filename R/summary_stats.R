# Multi-State EPA Air Quality Summary Statistics
# Functions for calculating hourly and daily summary tables

# Safe aggregation helpers — return NA instead of -Inf/NaN when all values are NA
safe_max  <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else max(x) }
safe_mean <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else mean(x) }

#' Calculate hourly summary statistics (1-hour max, avg, data availability by year)
#' @param df_hourly data.frame with Date and pollutant columns
#' @param available_pollutants character vector of pollutant column names
#' @return data.frame with Measurement rows and Year columns
calculate_hourly_summary <- function(df_hourly, available_pollutants) {
  df_hourly <- df_hourly %>%
    filter(!is.na(Date)) %>%
    mutate(Year = year(Date))
  
  summary_table <- data.frame(
    Measurement = character(),
    Year = integer(),
    Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # 1-hour maximum
  for (pollutant in available_pollutants) {
    vals <- df_hourly %>%
      group_by(Year) %>%
      summarise(Value = safe_max(.data[[pollutant]])) %>%
      mutate(Measurement = paste(pollutant, "1 hour maximum"))
    summary_table <- rbind(summary_table, vals)
  }
  
  # 1-hour average
  for (pollutant in available_pollutants) {
    vals <- df_hourly %>%
      group_by(Year) %>%
      summarise(Value = safe_mean(.data[[pollutant]])) %>%
      mutate(Measurement = paste(pollutant, "1 hour average"))
    summary_table <- rbind(summary_table, vals)
  }
  
  # Data availability
  for (pollutant in available_pollutants) {
    vals <- df_hourly %>%
      group_by(Year) %>%
      summarise(Value = sum(!is.na(.data[[pollutant]])) / n() * 100) %>%
      mutate(Measurement = paste(pollutant, "Data availability"))
    summary_table <- rbind(summary_table, vals)
  }
  
  # Pivot to wide format (years as columns)
  summary_table <- summary_table %>%
    pivot_wider(names_from = Year, values_from = Value)
  
  summary_table$Measurement <- gsub(" Data availability", " Data Availability (%)",
                                    summary_table$Measurement)
  
  summary_table[order(summary_table$Measurement), ]
}

#' Calculate daily summary statistics (24-hour max, avg, criteria checks, percentiles)
#' @param df_daily data.frame with Date and pollutant columns
#' @param available_pollutants character vector of pollutant column names
#' @param source character: "NSW", "QLD", or "VIC" — determines PM2.5 criteria (NSW=20, others=25)
#' @return data.frame with Measurement rows and Year columns
calculate_daily_summary <- function(df_daily, available_pollutants, source = "NSW") {
  df_daily <- df_daily %>%
    filter(!is.na(Date)) %>%
    mutate(Year = year(Date))
  
  summary_table <- data.frame(
    Measurement = character(),
    Year = integer(),
    Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # 24-hour maximum
  for (pollutant in available_pollutants) {
    vals <- df_daily %>%
      group_by(Year) %>%
      summarise(Value = safe_max(.data[[pollutant]])) %>%
      mutate(Measurement = paste(pollutant, "24 hour maximum"))
    summary_table <- rbind(summary_table, vals)
  }
  
  # PM10 maximum < 50 µg/m³
  if ("PM10 (µg/m³)" %in% available_pollutants) {
    vals <- df_daily %>%
      filter(`PM10 (µg/m³)` < 50) %>%
      group_by(Year) %>%
      summarise(Value = safe_max(`PM10 (µg/m³)`)) %>%
      mutate(Measurement = "PM10 24 hour maximum less than assessment criteria (50 µg/m³)")
    summary_table <- rbind(summary_table, vals)
  }
  
  # PM2.5 maximum < criteria (NSW=20, others=25)
  pm25_criteria <- if (source == "NSW") 20 else 25
  if ("PM2.5 (µg/m³)" %in% available_pollutants) {
    vals <- df_daily %>%
      filter(`PM2.5 (µg/m³)` < pm25_criteria) %>%
      group_by(Year) %>%
      summarise(Value = safe_max(`PM2.5 (µg/m³)`)) %>%
      mutate(Measurement = paste0("PM2.5 24 hour maximum less than assessment criteria (", pm25_criteria, " µg/m³)"))
    summary_table <- rbind(summary_table, vals)
  }
  
  # 24-hour average
  for (pollutant in available_pollutants) {
    vals <- df_daily %>%
      group_by(Year) %>%
      summarise(Value = safe_mean(.data[[pollutant]])) %>%
      mutate(Measurement = paste(pollutant, "24 hour average"))
    summary_table <- rbind(summary_table, vals)
  }
  
  # PM10 exceedance days (24-hour average > 50 µg/m³)
  if ("PM10 (µg/m³)" %in% available_pollutants) {
    vals <- df_daily %>%
      group_by(Year) %>%
      summarise(Value = sum(`PM10 (µg/m³)` > 50, na.rm = TRUE)) %>%
      mutate(Measurement = "PM10 24 hour exceedance days (> 50 µg/m³)")
    summary_table <- rbind(summary_table, vals)
  }
  
  # PM2.5 exceedance days (24-hour average > criteria; NSW=20, others=25)
  if ("PM2.5 (µg/m³)" %in% available_pollutants) {
    vals <- df_daily %>%
      group_by(Year) %>%
      summarise(Value = sum(`PM2.5 (µg/m³)` > pm25_criteria, na.rm = TRUE)) %>%
      mutate(Measurement = paste0("PM2.5 24 hour exceedance days (> ", pm25_criteria, " µg/m³)"))
    summary_table <- rbind(summary_table, vals)
  }
  
  # 70th percentile for PM10 and PM2.5
  if ("PM10 (µg/m³)" %in% available_pollutants) {
    vals <- df_daily %>%
      group_by(Year) %>%
      summarise(Value = quantile(`PM10 (µg/m³)`, probs = 0.7, na.rm = TRUE)) %>%
      mutate(Measurement = "PM10 70th Percentile")
    summary_table <- rbind(summary_table, vals)
  }
  
  if ("PM2.5 (µg/m³)" %in% available_pollutants) {
    vals <- df_daily %>%
      group_by(Year) %>%
      summarise(Value = quantile(`PM2.5 (µg/m³)`, probs = 0.7, na.rm = TRUE)) %>%
      mutate(Measurement = "PM2.5 70th Percentile")
    summary_table <- rbind(summary_table, vals)
  }
  
  # Pivot to wide format
  summary_table <- summary_table %>%
    pivot_wider(names_from = Year, values_from = Value)
  
  summary_table[order(summary_table$Measurement), ]
}

#' List individual exceedance dates against NEPM criteria
#' PM10 (24h > 50), PM2.5 (24h > criteria), NO2 (1h > 80 µg/m³).
#' @param df_daily data.frame with Date (Date class) and pollutant columns
#' @param df_hourly data.frame with POSIXct Date and pollutant columns (for NO2 1-hour); may be NULL
#' @param available_pollutants character vector of pollutant column names
#' @param source character: determines PM2.5 criteria (NSW=20, others=25)
#' @return data.frame: Pollutant, Averaging, Criteria, Year, Date, Value (one row per exceedance)
calculate_exceedance_dates <- function(df_daily, df_hourly, available_pollutants, source = "NSW") {
  pm25_criteria <- if (source == "NSW") 20 else 25
  empty <- data.frame(
    Pollutant = character(), Averaging = character(), Criteria = numeric(),
    Year = integer(), Date = as.Date(character()), Value = numeric(),
    stringsAsFactors = FALSE
  )
  out <- list()
  
  # PM10 24-hour average > 50 µg/m³
  if ("PM10 (µg/m³)" %in% available_pollutants) {
    out[[length(out) + 1]] <- df_daily %>%
      filter(!is.na(Date), `PM10 (µg/m³)` > 50) %>%
      transmute(Pollutant = "PM10", Averaging = "24 hour", Criteria = 50,
                Date = as.Date(Date), Value = round(`PM10 (µg/m³)`, 1))
  }
  
  # PM2.5 24-hour average > criteria
  if ("PM2.5 (µg/m³)" %in% available_pollutants) {
    out[[length(out) + 1]] <- df_daily %>%
      filter(!is.na(Date), `PM2.5 (µg/m³)` > pm25_criteria) %>%
      transmute(Pollutant = "PM2.5", Averaging = "24 hour", Criteria = pm25_criteria,
                Date = as.Date(Date), Value = round(`PM2.5 (µg/m³)`, 1))
  }
  
  # NO2 1-hour average > 80 µg/m³ (report peak hourly value per exceedance day)
  if (!is.null(df_hourly) && "NO2 (µg/m³)" %in% available_pollutants &&
      "NO2 (µg/m³)" %in% names(df_hourly)) {
    tz <- attr(df_hourly$Date, "tzone")
    if (is.null(tz) || tz == "") tz <- Sys.timezone()
    out[[length(out) + 1]] <- df_hourly %>%
      filter(!is.na(Date), `NO2 (µg/m³)` > 80) %>%
      mutate(Date = as.Date(Date, tz = tz)) %>%
      group_by(Date) %>%
      summarise(Value = round(max(`NO2 (µg/m³)`, na.rm = TRUE), 1), .groups = "drop") %>%
      transmute(Pollutant = "NO2", Averaging = "1 hour", Criteria = 80, Date, Value)
  }
  
  if (length(out) == 0) return(empty)
  
  bind_rows(out) %>%
    mutate(Year = year(Date)) %>%
    arrange(Pollutant, Date) %>%
    select(Pollutant, Averaging, Criteria, Year, Date, Value) %>%
    as.data.frame()
}
