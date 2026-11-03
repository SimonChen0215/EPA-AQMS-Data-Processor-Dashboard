# Multi-State EPA Air Quality Plot Helpers
# Functions for building dust concentration and time series plots

PM10_CRITERION <- 50

#' Build a dust concentration bar chart (PM10 / PM2.5 with criterion lines)
#' @param df_daily data.frame with Date, PM10, PM2.5 columns
#' @param source character: "NSW", "QLD", or "VIC" — determines PM2.5 criterion (NSW=20, others=25)
#' @return ggplot object, or NULL if no PM data available
build_dust_plot <- function(df_daily, source = "NSW") {
  PM25_CRITERION <- if (source == "NSW") 20 else 25
  has_pm10 <- "PM10 (µg/m³)" %in% colnames(df_daily)
  has_pm25 <- "PM2.5 (µg/m³)" %in% colnames(df_daily)
  
  if (!has_pm10 && !has_pm25) return(NULL)
  
  plot_data <- df_daily %>%
    select(Date, any_of(c("PM10 (µg/m³)", "PM2.5 (µg/m³)"))) %>%
    mutate(across(any_of(c("PM10 (µg/m³)", "PM2.5 (µg/m³)")),
                  ~ifelse(. < 0, NA_real_, .)))
  
  p <- ggplot(plot_data, aes(x = Date))
  
  if (has_pm10) {
    p <- p + geom_bar(aes(y = `PM10 (µg/m³)`, fill = "PM10"),
                      stat = "identity", position = "dodge", color = "black", alpha = 0.7)
  }
  if (has_pm25) {
    p <- p + geom_bar(aes(y = `PM2.5 (µg/m³)`, fill = "PM2.5"),
                      stat = "identity", position = "dodge", color = "orange", alpha = 0.7)
  }
  if (has_pm10) {
    p <- p + geom_hline(aes(yintercept = PM10_CRITERION,
                            color = "PM10 Criterion (50 µg/m³)"),
                        linetype = "dashed", linewidth = 0.8)
  }
  pm25_label <- paste0("PM2.5 Criterion (", PM25_CRITERION, " µg/m³)")
  
  if (has_pm25) {
    p <- p + geom_hline(aes(yintercept = PM25_CRITERION,
                            color = pm25_label),
                        linetype = "dashed", linewidth = 0.8)
  }
  
  p <- p +
    labs(title = "Ambient Dust Concentrations",
         y = "Concentration (µg/m³)",
         fill = "Pollutant",
         color = "Guidelines") +
    scale_fill_manual(values = c("PM10" = "black", "PM2.5" = "orange")) +
    scale_color_manual(values = setNames(
      c("black", "orange"),
      c("PM10 Criterion (50 µg/m³)", pm25_label)
    )) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  p
}

#' Render a time series plot using openair::timePlot
#' @param df_hourly data.frame with Date, Hour, and pollutant columns
#' @param available_pollutants character vector of available pollutant column names
#' @return NULL (renders plot as side effect), or FALSE if no data
render_time_plot <- function(df_hourly, available_pollutants) {
  df_plot <- df_hourly %>%
    mutate(across(any_of(c("PM10 (µg/m³)", "PM2.5 (µg/m³)")),
                  ~ifelse(. < 0, NA_real_, .))) %>%
    rename(date = Date)
  
  pollutants_to_plot <- intersect(
    c("OZONE (µg/m³)", "SO2 (µg/m³)",
      "PM10 (µg/m³)", "PM2.5 (µg/m³)"),
    available_pollutants
  )
  
  if (length(pollutants_to_plot) == 0) return(FALSE)
  
  timePlot(df_plot,
           pollutant = pollutants_to_plot,
           avg.time = "day",
           statistic = "mean",
           main = "Daily Average Concentrations",
           ylab = "Concentration",
           group = TRUE,
           lwd = 2)
  
  TRUE
}
