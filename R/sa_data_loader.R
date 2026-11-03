# SA EPA Air Quality Data Loader
# Loads pre-processed RDS files from SA-AQMS-Data/
# All unit conversion and reshaping done offline by SA_AQMS/preprocess_sa_data.R

SA_DATA_DIR <- "SA-AQMS-Data"

# Mapping: GeoJSON MONITORING name -> RDS file stem (where data exists)
# Sites without RDS data are excluded (no data available or pre-2020 only)
SA_GEOJSON_TO_RDS <- c(

  "Southern Adelaide - Christie Downs"           = "Christie_Downs",
  "North Western Adelaide - Le Fevre (Birkenhead)" = "Le_Fevre_1_Birkenhead",
  "Western Adelaide - Netley"                    = "Netley",
  "North Eastern Adelaide - Northfield"          = "Northfield",
  "Port Pirie - Oliver Street"                   = "Port_Pirie_Oliver_St",
  "Whyalla - Schulz Reserve"                     = "Whyalla_Schulz_Reserve",
  "North Western Adelaide - Le Fevre (North Haven)" = "Le_Fevre_2_North_Haven",
  "Adelaide CBD"                                 = "Adelaide_CBD",
  "Whyalla - Walls Street"                       = "Whyalla_Walls_St"
)

# Additional SA stations with RDS data but not in GeoJSON
# (coordinates sourced from SA EPA website / data inspection)
SA_EXTRA_STATIONS <- data.frame(
  site_name = c("Elizabeth", "Kensington", "Birkenhead Naval Reserve",
                "Peterhead", "Port Augusta", "Mt Barker",
                "Cowandilla", "Sellicks Beach Lurline"),
  rds_stem  = c("Elizabeth", "Kensington", "Birkenhead_Naval_Reserve",
                "Peterhead", "Port_Augusta", "Mt_Barker",
                "Cowandilla", "Sellicks_Beach_Lurline"),
  latitude  = c(-34.7283, -34.9214, -34.8361,
                -34.8331, -32.4906, -35.0685,
                -34.9378, -35.3347),
  longitude = c(138.6686, 138.6588, 138.4979,
                138.5042, 137.7827, 138.8571,
                138.5637, 138.4517),
  status    = c("ACTIVE", "INACTIVE", "ACTIVE",
                "ACTIVE", "INACTIVE", "INACTIVE",
                "ACTIVE", "ACTIVE"),
  parameters = c("PM10, PM2.5, NO2, O3, Met", "PM10, PM2.5, NO2, O3, Met",
                  "PM10", "PM10",
                  "PM10, PM2.5, Met", "PM10, PM2.5, Met",
                  "PM10, PM2.5, NO2, O3, Met", "PM10, Met"),
  stringsAsFactors = FALSE
)

#' Load SA site list from GeoJSON + extra stations
#' @return data.frame with columns: site_name, rds_stem, latitude, longitude, status, parameters
load_sa_site_list <- function() {
  geo <- jsonlite::fromJSON(
    file.path("SA_AQMS", "topo_epa_airqualitymonitoringsites_wgs84.geojson"))

  props <- geo[["features"]][["properties"]]
  coords <- geo[["features"]][["geometry"]][["coordinates"]]

  geo_df <- data.frame(
    site_name  = props[["MONITORING"]],
    latitude   = sapply(coords, `[`, 2),
    longitude  = sapply(coords, `[`, 1),
    status     = props[["STATUS"]],
    parameters = props[["PARAMETERS"]],
    stringsAsFactors = FALSE
  )

  # Add rds_stem from mapping; keep only sites that have RDS data
  geo_df$rds_stem <- SA_GEOJSON_TO_RDS[geo_df$site_name]
  geo_with_data <- geo_df[!is.na(geo_df$rds_stem), ]

  # Combine with extra stations
  extras <- SA_EXTRA_STATIONS
  sa_sites <- rbind(
    geo_with_data[, c("site_name", "rds_stem", "latitude", "longitude", "status", "parameters")],
    extras[, c("site_name", "rds_stem", "latitude", "longitude", "status", "parameters")]
  )

  # Only keep stations whose RDS file actually exists
  sa_sites$rds_path <- file.path(SA_DATA_DIR, paste0(sa_sites$rds_stem, ".rds"))
  sa_sites <- sa_sites[file.exists(sa_sites$rds_path), ]

  rownames(sa_sites) <- NULL
  sa_sites
}

#' Get available parameters for an SA station
#' @param rds_stem character — the RDS file stem (e.g. "Christie_Downs")
#' @return character vector of display-name parameters (e.g. "PM10 (µg/m³)")
get_sa_site_parameters <- function(rds_stem) {
  params <- tryCatch(
    readRDS(file.path(SA_DATA_DIR, "sa_station_params.rds")),
    error = function(e) NULL
  )
  if (!is.null(params) && rds_stem %in% names(params)) {
    return(params[[rds_stem]])
  }
  # Fallback: read actual RDS and inspect columns
  rds_path <- file.path(SA_DATA_DIR, paste0(rds_stem, ".rds"))
  if (file.exists(rds_path)) {
    df <- readRDS(rds_path)
    return(identify_pollutants(df))
  }
  character(0)
}

#' Load SA observations from pre-processed RDS
#' @param rds_stem character — the RDS file stem
#' @param start_year integer
#' @param end_year integer
#' @return data.frame in dashboard-ready wide format (POSIXct Date + pollutant columns)
load_sa_observations <- function(rds_stem, start_year, end_year) {
  rds_path <- file.path(SA_DATA_DIR, paste0(rds_stem, ".rds"))
  if (!file.exists(rds_path)) {
    stop("RDS file not found: ", rds_path)
  }
  df <- readRDS(rds_path)

  # Combine Date + Hour into single POSIXct Date column (ACST)
  if ("Hour" %in% names(df)) {
    df$Date <- as.POSIXct(paste(df$Date, df$Hour, "00", "00", sep = ":"),
                          format = "%Y-%m-%d:%H:%M:%S", tz = "Australia/Darwin")
    df$Hour <- NULL
  }

  # Filter to year range
  df <- df[lubridate::year(df$Date) >= start_year &
           lubridate::year(df$Date) <= end_year, ]

  df
}
