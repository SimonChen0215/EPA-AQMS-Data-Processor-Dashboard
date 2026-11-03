# EPA Air Quality Dashboard - Server
# Handles reactive logic, API calls, data processing, and outputs

server <- function(input, output, session) {
  
  # Reactive values — NSW
  rv <- reactiveValues(
    nsw_site_details = NULL,
    nsw_param_details = NULL,
    nsw_selected_site = NULL,
    nsw_df_hourly = NULL,
    nsw_df_daily = NULL,
    nsw_summary_hourly = NULL,
    nsw_summary_daily = NULL,
    nsw_available_pollutants = NULL,
    nsw_status_messages = c(),
    # QLD
    qld_stations = NULL,
    qld_param_lookup = NULL,
    qld_selected_site = NULL,
    qld_selected_station_id = NULL,
    qld_df_hourly = NULL,
    qld_df_daily = NULL,
    qld_summary_hourly = NULL,
    qld_summary_daily = NULL,
    qld_available_pollutants = NULL,
    qld_status_messages = c(),
    # VIC
    vic_sites = NULL,
    vic_xlsx_sites = NULL,
    vic_selected_site = NULL,
    vic_selected_site_name = NULL,
    vic_location_id = NULL,
    vic_df_hourly = NULL,
    vic_df_daily = NULL,
    vic_summary_hourly = NULL,
    vic_summary_daily = NULL,
    vic_available_pollutants = NULL,
    vic_status_messages = c(),
    # SA
    sa_sites = NULL,
    sa_selected_site = NULL,
    sa_selected_rds_stem = NULL,
    sa_df_hourly = NULL,
    sa_df_daily = NULL,
    sa_summary_hourly = NULL,
    sa_summary_daily = NULL,
    sa_available_pollutants = NULL,
    sa_status_messages = c(),
    # Active source — which state's data to show in Results/Visualizations/Export
    active_source = "NSW",
    # Station search lookup
    station_lookup = NULL,
    # Last clicked source for Data Pull tab
    last_clicked_source = NULL,
    # Data Pull tab
    dp_hourly = NULL,
    dp_daily = NULL,
    dp_status_messages = c()
  )
  
  # ── Startup: load from cache (instant) ────────────────────────────────────
  observe({
    rv$nsw_site_details <- CACHED_DATA$nsw_sites
    rv$nsw_param_details <- CACHED_DATA$nsw_params
    rv$qld_stations <- CACHED_DATA$qld_sites
    rv$qld_param_lookup <- CACHED_DATA$qld_params
    rv$vic_sites <- CACHED_DATA$vic_sites
    rv$vic_xlsx_sites <- CACHED_DATA$vic_xlsx_sites
    rv$sa_sites <- CACHED_DATA$sa_sites
    
    if (!is.null(rv$nsw_param_details)) {
      updatePickerInput(session, "nsw_parameters",
                        choices = get_nsw_param_choices(rv$nsw_param_details))
    }
    
    # Build combined station lookup for search
    update_station_search(session, rv)
  })
  
  # ── Refresh stations button ─────────────────────────────────────────────
  observeEvent(input$refresh_stations, {
    withProgress(message = "Refreshing station data from APIs...", value = 0, {
      incProgress(0.5)
      refreshed <- refresh_station_cache()
      rv$nsw_site_details <- refreshed$nsw_sites
      rv$nsw_param_details <- refreshed$nsw_params
      rv$qld_stations <- refreshed$qld_sites
      rv$qld_param_lookup <- refreshed$qld_params
      rv$vic_sites <- refreshed$vic_sites
      rv$vic_xlsx_sites <- refreshed$vic_xlsx_sites
      rv$sa_sites <- refreshed$sa_sites
      
      if (!is.null(rv$nsw_param_details)) {
        updatePickerInput(session, "nsw_parameters",
                          choices = get_nsw_param_choices(rv$nsw_param_details))
      }
      incProgress(1)
    })
    update_station_search(session, rv)
    showNotification("Station data refreshed and cached!", type = "message", duration = 5)
  })
  
  # ── Station search: build combined lookup & handle selection ───────────────
  
  # Build a combined data frame of all stations and populate the selectize input
  update_station_search <- function(session, rv) {
    frames <- list()
    if (!is.null(rv$nsw_site_details) && nrow(rv$nsw_site_details) > 0) {
      frames$nsw <- data.frame(
        display_name = paste0("NSW: ", rv$nsw_site_details$SiteName),
        lat = rv$nsw_site_details$Latitude,
        lng = rv$nsw_site_details$Longitude,
        layerId = rv$nsw_site_details$SiteName,
        stringsAsFactors = FALSE
      )
    }
    if (!is.null(rv$qld_stations) && nrow(rv$qld_stations) > 0) {
      frames$qld <- data.frame(
        display_name = paste0("QLD: ", rv$qld_stations$station_name),
        lat = rv$qld_stations$latitude,
        lng = rv$qld_stations$longitude,
        layerId = paste0("QLD:", rv$qld_stations$station_name),
        stringsAsFactors = FALSE
      )
    }
    if (!is.null(rv$vic_sites) && nrow(rv$vic_sites) > 0) {
      frames$vic <- data.frame(
        display_name = paste0("VIC: ", rv$vic_sites$siteName),
        lat = rv$vic_sites$Latitude,
        lng = rv$vic_sites$Longitude,
        layerId = paste0("VIC:", rv$vic_sites$siteName),
        stringsAsFactors = FALSE
      )
    }
    if (!is.null(rv$sa_sites) && nrow(rv$sa_sites) > 0) {
      frames$sa <- data.frame(
        display_name = paste0("SA: ", rv$sa_sites$site_name),
        lat = rv$sa_sites$latitude,
        lng = rv$sa_sites$longitude,
        layerId = paste0("SA:", rv$sa_sites$site_name),
        stringsAsFactors = FALSE
      )
    }
    if (length(frames) > 0) {
      rv$station_lookup <- do.call(rbind, frames)
      updateSelectizeInput(session, "station_search",
                           choices = rv$station_lookup$display_name,
                           selected = character(0),
                           server = FALSE)
    }
  }
  
  # Fly to station on search selection
  observeEvent(input$station_search, {
    req(input$station_search, rv$station_lookup)
    match <- rv$station_lookup[rv$station_lookup$display_name == input$station_search, ]
    if (nrow(match) == 1) {
      leafletProxy("site_map") %>%
        flyTo(lng = match$lng, lat = match$lat, zoom = 14) %>%
        clearGroup("highlight") %>%
        addCircleMarkers(
          lat = match$lat, lng = match$lng,
          group = "highlight", radius = 12,
          color = "red", fillColor = "red", fillOpacity = 0.4,
          stroke = TRUE, weight = 2
        )
    }
  }, ignoreInit = TRUE)
  
  # ── Map (single map with both NSW + QLD markers) ─────────────────────────
  output$site_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 150.5, lat = -30, zoom = 5) %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=y&hl=en&x={x}&y={y}&z={z}",
              attribution = "Google")
  })
  
  # Add NSW markers when loaded
  observe({
    req(rv$nsw_site_details)
    leafletProxy("site_map") %>%
      clearGroup("NSW") %>%
      addCircleMarkers(
        data = rv$nsw_site_details,
        layerId = ~SiteName,
        group = "NSW",
        lat = ~Latitude, lng = ~Longitude,
        clusterOptions = markerClusterOptions(),
        popup = ~paste0(
          "<b>", SiteName, "</b> (NSW)<br>",
          "Site ID: ", Site_Id, "<br>",
          "Lat: ", round(Latitude, 3), ", Lng: ", round(Longitude, 3)
        ),
        radius = 6, color = "#97FFFF", fillColor = "#97FFFF", fillOpacity = 0.6
      )
  })
  
  # Add QLD markers when loaded
  observe({
    req(rv$qld_stations)
    leafletProxy("site_map") %>%
      clearGroup("QLD") %>%
      addCircleMarkers(
        data = rv$qld_stations,
        layerId = ~paste0("QLD:", station_name),
        group = "QLD",
        lat = ~latitude, lng = ~longitude,
        clusterOptions = markerClusterOptions(),
        popup = ~paste0(
          "<b>", station_name, "</b> (QLD)<br>",
          "Station ID: ", station_id, "<br>",
          "Region: ", region_id, "<br>",
          ifelse(!is.na(start_date) & start_date != "", paste0("Start Year: ", substr(start_date, 1, 4), "<br>"), ""),
          "Lat: ", round(latitude, 3), ", Lng: ", round(longitude, 3)
        ),
        radius = 6, color = "#FFD700", fillColor = "#FFD700", fillOpacity = 0.6
      )
  })
  
  # Add VIC markers when loaded
  observe({
    req(rv$vic_sites)
    leafletProxy("site_map") %>%
      clearGroup("VIC") %>%
      addCircleMarkers(
        data = rv$vic_sites,
        layerId = ~paste0("VIC:", siteName),
        group = "VIC",
        lat = ~Latitude, lng = ~Longitude,
        clusterOptions = markerClusterOptions(),
        popup = ~paste0(
          "<b>", siteName, "</b> (VIC)<br>",
          "Type: ", siteType, "<br>",
          "Lat: ", round(Latitude, 3), ", Lng: ", round(Longitude, 3)
        ),
        radius = 6, color = "#90EE90", fillColor = "#90EE90", fillOpacity = 0.6
      )
  })
  
  # Add SA markers when loaded
  observe({
    req(rv$sa_sites)
    leafletProxy("site_map") %>%
      clearGroup("SA") %>%
      addCircleMarkers(
        data = rv$sa_sites,
        layerId = ~paste0("SA:", site_name),
        group = "SA",
        lat = ~latitude, lng = ~longitude,
        clusterOptions = markerClusterOptions(),
        popup = ~paste0(
          "<b>", site_name, "</b> (SA)<br>",
          "Status: ", status, "<br>",
          "Parameters: ", parameters, "<br>",
          "Lat: ", round(latitude, 3), ", Lng: ", round(longitude, 3)
        ),
        radius = 6, color = "#FF6B35", fillColor = "#FF6B35", fillOpacity = 0.6
      )
  })
  
  # Unified marker click handler — distinguish NSW vs QLD by layerId prefix
  observeEvent(input$site_map_marker_click, {
    clicked <- input$site_map_marker_click$id
    if (is.null(clicked)) return()
    
    # ── Highlight clicked marker (separate group, no cluster interference) ──
    click_lat <- input$site_map_marker_click$lat
    click_lng <- input$site_map_marker_click$lng
    leafletProxy("site_map") %>%
      clearGroup("highlight") %>%
      addCircleMarkers(
        lat = click_lat, lng = click_lng,
        group = "highlight",
        radius = 8, color = "red", fillColor = "red",
        fillOpacity = 0.9, stroke = TRUE, weight = 2
      )
    
    # Dismiss any existing success modal from a previous fetch
    removeModal()
    
    if (startsWith(as.character(clicked), "QLD:")) {
      # ── QLD station clicked ──
      qld_name <- sub("^QLD:", "", clicked)
      rv$qld_selected_site <- qld_name
      
      rv$qld_selected_station_id <- rv$qld_stations %>%
        filter(station_name == qld_name) %>%
        pull(station_id) %>%
        `[`(1)
      
      shinyjs::show("qld_controls")
      shinyjs::hide("nsw_controls")
      shinyjs::hide("vic_controls")
      shinyjs::hide("sa_controls")
      rv$last_clicked_source <- "QLD"
      
      # Fetch station-specific parameters, filter to mapped ones, show friendly names
      tryCatch({
        station_params <- fetch_qld_station_parameters(rv$qld_selected_station_id)
        # Keep only parameters that have a mapping in QLD_PARAM_MAP
        mapped <- station_params %>%
          mutate(key = tolower(parameter_name)) %>%
          filter(key %in% names(QLD_PARAM_MAP))
        # Values = original parameter_name, labels = display names from QLD_PARAM_MAP
        choices <- mapped$parameter_name
        names(choices) <- QLD_PARAM_MAP[mapped$key]
        updatePickerInput(session, "qld_parameters", choices = choices)
        updatePickerInput(session, "dp_parameters", choices = choices)
      }, error = function(e) {
        showNotification(paste("Error fetching QLD station parameters:", e$message),
                         type = "error", duration = 5)
      })
      
    } else if (startsWith(as.character(clicked), "VIC:")) {
      # ── VIC station clicked ──
      vic_name <- sub("^VIC:", "", clicked)
      rv$vic_selected_site <- vic_name
      rv$vic_selected_site_name <- vic_name
      
      shinyjs::show("vic_controls")
      shinyjs::hide("nsw_controls")
      shinyjs::hide("qld_controls")
      shinyjs::hide("sa_controls")
      rv$last_clicked_source <- "VIC"
      
      # Find ALL matching xlsx location_ids by name (same station, relocated)
      vic_location_ids <- NULL
      if (!is.null(rv$vic_xlsx_sites)) {
        match_rows <- rv$vic_xlsx_sites %>%
          filter(tolower(location_name) == tolower(vic_name))
        if (nrow(match_rows) > 0) {
          vic_location_ids <- match_rows$location_id
        }
      }
      rv$vic_location_id <- vic_location_ids
      
      # Get parameters from API site data (already fetched at startup)
      api_params <- character(0)
      if (!is.null(rv$vic_sites)) {
        site_row <- rv$vic_sites %>% filter(siteName == vic_name)
        if (nrow(site_row) > 0) {
          api_params <- site_row$params[[1]]
        }
      }
      # Supplement with params from pre-processed RDS data (includes met data)
      rds_params <- character(0)
      if (!is.null(vic_location_ids)) {
        for (lid in vic_location_ids) {
          rds_params <- union(rds_params, unname(get_vic_site_parameters(lid)))
        }
      }
      # Merge API params with local RDS params to include met data
      all_params <- union(api_params, rds_params)
      # Filter to supported params and build pickerInput choices
      supported <- intersect(all_params, VIC_SUPPORTED_PARAMS)
      if (length(supported) > 0) {
        choices <- supported
        names(choices) <- VIC_PARAM_DISPLAY_NAMES[supported]
        updatePickerInput(session, "vic_parameters", choices = choices)
        updatePickerInput(session, "dp_parameters", choices = choices)
      } else {
        updatePickerInput(session, "vic_parameters", choices = character(0))
        updatePickerInput(session, "dp_parameters", choices = character(0))
      }
      
      if (is.null(vic_location_ids)) {
        showNotification(
          paste0("No local xlsx data found for '", vic_name, "'. Data fetching may not work for this station."),
          type = "warning", duration = 8)
      }
      
    } else if (startsWith(as.character(clicked), "SA:")) {
      # ── SA station clicked ──
      sa_name <- sub("^SA:", "", clicked)
      rv$sa_selected_site <- sa_name
      
      # Find rds_stem for this site
      if (!is.null(rv$sa_sites)) {
        match_row <- rv$sa_sites[rv$sa_sites$site_name == sa_name, ]
        if (nrow(match_row) > 0) {
          rv$sa_selected_rds_stem <- match_row$rds_stem[1]
        }
      }
      
      shinyjs::show("sa_controls")
      shinyjs::hide("nsw_controls")
      shinyjs::hide("qld_controls")
      shinyjs::hide("vic_controls")
      rv$last_clicked_source <- "SA"
      
      # Populate parameter picker
      if (!is.null(rv$sa_selected_rds_stem)) {
        sa_params <- get_sa_site_parameters(rv$sa_selected_rds_stem)
        updatePickerInput(session, "sa_parameters", choices = sa_params)
        updatePickerInput(session, "dp_parameters", choices = sa_params)
      }
      
    } else {
      # ── NSW station clicked ──
      rv$nsw_selected_site <- clicked
      
      shinyjs::show("nsw_controls")
      shinyjs::hide("qld_controls")
      shinyjs::hide("vic_controls")
      shinyjs::hide("sa_controls")
      rv$last_clicked_source <- "NSW"
      
      # Update Data Pull parameter picker with NSW params
      if (!is.null(rv$nsw_param_details)) {
        updatePickerInput(session, "dp_parameters",
                          choices = get_nsw_param_choices(rv$nsw_param_details))
      }
    }
  })
  
  observeEvent(input$nsw_panel_close, {
    shinyjs::hide("nsw_controls")
  })
  
  observeEvent(input$qld_panel_close, {
    shinyjs::hide("qld_controls")
  })
  
  observeEvent(input$vic_panel_close, {
    shinyjs::hide("vic_controls")
  })
  
  observeEvent(input$sa_panel_close, {
    shinyjs::hide("sa_controls")
  })
  
  output$nsw_selected_site_name <- renderText({
    if (is.null(rv$nsw_selected_site)) return("No station selected")
    rv$nsw_selected_site
  })
  
  output$qld_selected_site_name <- renderText({
    if (is.null(rv$qld_selected_site)) return("No station selected")
    rv$qld_selected_site
  })
  
  output$vic_selected_site_name <- renderText({
    if (is.null(rv$vic_selected_site)) return("No station selected")
    rv$vic_selected_site
  })
  
  output$sa_selected_site_name <- renderText({
    if (is.null(rv$sa_selected_site)) return("No station selected")
    rv$sa_selected_site
  })
  
  # ── Helper: disable/enable controls during fetch ─────────────────────────
  # IDs for each state's control panel + the Data Pull controls
  NSW_CONTROLS <- c("nsw_parameters", "nsw_start_year", "nsw_end_year", "nsw_fetch_data")
  QLD_CONTROLS <- c("qld_parameters", "qld_start_year", "qld_end_year", "qld_fetch_data")
  VIC_CONTROLS <- c("vic_parameters", "vic_start_year", "vic_end_year", "vic_fetch_data")
  SA_CONTROLS  <- c("sa_parameters", "sa_start_year", "sa_end_year", "sa_fetch_data")
  DP_CONTROLS  <- c("dp_parameters", "dp_start_date", "dp_end_date", "dp_fetch")
  
  disable_controls <- function(ids) for (id in ids) shinyjs::disable(id)
  enable_controls  <- function(ids) for (id in ids) shinyjs::enable(id)
  
  # ── NSW Fetch & process data ─────────────────────────────────────────────
  observeEvent(input$nsw_fetch_data, {
    req(rv$nsw_selected_site, input$nsw_parameters, input$nsw_start_year, input$nsw_end_year)
    
    if (is.null(rv$nsw_selected_site)) {
      showNotification("Please click a station on the map first.",
                       type = "error", duration = 5)
      return()
    }
    if (length(input$nsw_parameters) == 0) {
      showNotification("Please select at least one parameter.",
                       type = "error", duration = 5)
      return()
    }
    if (as.integer(input$nsw_start_year) > as.integer(input$nsw_end_year)) {
      showNotification("End year must not be before start year.",
                       type = "error", duration = 5)
      return()
    }
    
    disable_controls(NSW_CONTROLS)
    on.exit(enable_controls(NSW_CONTROLS), add = TRUE)
    
    rv$nsw_status_messages <- c()
    
    # Resolve site ID
    site_id <- rv$nsw_site_details %>%
      filter(SiteName == rv$nsw_selected_site) %>%
      pull(Site_Id)
    
    # Expand parameters (add PM10d / PM2.5d variants)
    chosen_params <- expand_nsw_params(input$nsw_parameters)
    
    # Fetch data
    withProgress(message = "Fetching air quality data...", value = 0, {
      result <- fetch_nsw_observations(
        site_ids = site_id,
        params = chosen_params,
        start_year = as.integer(input$nsw_start_year),
        end_year = as.integer(input$nsw_end_year),
        progress_callback = function(amt, msg) incProgress(amt, detail = msg)
      )
      
      rv$nsw_status_messages <- result$messages
      
      if (nrow(result$data) == 0) {
        showNotification("No data retrieved. Please check your selections.",
                         type = "error", duration = 10)
        return()
      }
      
      # Process
      incProgress(0.05, detail = "Processing data...")
      processed <- process_nsw_observations(result$data)
      
      rv$nsw_df_hourly <- processed$hourly
      rv$nsw_df_daily <- processed$daily
      rv$nsw_available_pollutants <- processed$pollutants
      rv$active_source <- "NSW"
      
      # Calculate summaries
      rv$nsw_summary_hourly <- calculate_hourly_summary(processed$hourly, processed$pollutants)
      rv$nsw_summary_daily <- calculate_daily_summary(processed$daily, processed$pollutants, source = "NSW")
      
      rv$nsw_status_messages <- c(
        rv$nsw_status_messages,
        paste("\n\u2713 Data processing complete!"),
        paste("\u2713 Total records:", processed$total_records),
        paste("\u2713 Pollutants found:", paste(processed$pollutants, collapse = ", "))
      )
      
      showModal(modalDialog(
        title = NULL,
        size = "s",
        easyClose = TRUE,
        div(style = "text-align:center; padding:20px 0 10px;",
          icon("check-circle", style = "font-size:48px; color:#4CAF50;"),
          tags$h4(style = "margin-top:12px; font-weight:600;", rv$nsw_selected_site),
          tags$p(style = "color:#888; margin:5px 0 15px;",
                 processed$total_records, " records processed")
        ),
        footer = div(style = "text-align:center;",
          actionButton("go_results", "Results",
                       class = "btn",
                       style = "display:block; width:80%; margin:5px auto; background:#2196F3; color:#fff; border:none;"),
          actionButton("go_viz", "Visualizations",
                       class = "btn",
                       style = "display:block; width:80%; margin:5px auto; background:#42A5F5; color:#fff; border:none;"),
          tags$button(type = "button", class = "btn",
                      `data-dismiss` = "modal",
                      style = "display:block; width:80%; margin:5px auto; background:#E3F2FD; color:#1565C0; border:none;",
                      "Close")
        )
      ))
    })
  })
  
  # ── QLD Fetch & process data ────────────────────────────────────────────
  observeEvent(input$qld_fetch_data, {
    req(rv$qld_selected_station_id, input$qld_start_year, input$qld_end_year)
    
    if (is.null(rv$qld_selected_station_id)) {
      showNotification("Please click a station on the QLD map first.",
                       type = "error", duration = 5)
      return()
    }
    if (length(input$qld_parameters) == 0) {
      showNotification("Please select at least one parameter.",
                       type = "error", duration = 5)
      return()
    }
    if (as.integer(input$qld_start_year) > as.integer(input$qld_end_year)) {
      showNotification("End year must not be before start year.",
                       type = "error", duration = 5)
      return()
    }
    
    disable_controls(QLD_CONTROLS)
    on.exit(enable_controls(QLD_CONTROLS), add = TRUE)
    
    rv$qld_status_messages <- c()
    
    withProgress(message = "Fetching QLD air quality data...", value = 0, {
      result <- fetch_qld_observations(
        station_id = rv$qld_selected_station_id,
        start_year = as.integer(input$qld_start_year),
        end_year = as.integer(input$qld_end_year),
        progress_callback = function(amt, msg) incProgress(amt, detail = msg)
      )
      
      rv$qld_status_messages <- result$messages
      
      if (nrow(result$data) == 0) {
        showNotification("No QLD data retrieved. Please check your selections.",
                         type = "error", duration = 10)
        return()
      }
      
      # Process
      incProgress(0.05, detail = "Processing QLD data...")
      processed <- process_qld_observations(result$data, rv$qld_param_lookup)
      
      rv$qld_df_hourly <- processed$hourly
      rv$qld_df_daily <- processed$daily
      rv$qld_available_pollutants <- processed$pollutants
      rv$active_source <- "QLD"
      
      # Calculate summaries
      rv$qld_summary_hourly <- calculate_hourly_summary(processed$hourly, processed$pollutants)
      rv$qld_summary_daily <- calculate_daily_summary(processed$daily, processed$pollutants, source = "QLD")
      
      rv$qld_status_messages <- c(
        rv$qld_status_messages,
        paste("\n\u2713 QLD data processing complete!"),
        paste("\u2713 Total records:", processed$total_records),
        paste("\u2713 Pollutants found:", paste(processed$pollutants, collapse = ", "))
      )
      
      showModal(modalDialog(
        title = NULL,
        size = "s",
        easyClose = TRUE,
        div(style = "text-align:center; padding:20px 0 10px;",
          icon("check-circle", style = "font-size:48px; color:#4CAF50;"),
          tags$h4(style = "margin-top:12px; font-weight:600;", rv$qld_selected_site),
          tags$p(style = "color:#888; margin:5px 0 15px;",
                 processed$total_records, " records processed")
        ),
        footer = div(style = "text-align:center;",
          actionButton("go_results", "Results",
                       class = "btn",
                       style = "display:block; width:80%; margin:5px auto; background:#2196F3; color:#fff; border:none;"),
          actionButton("go_viz", "Visualizations",
                       class = "btn",
                       style = "display:block; width:80%; margin:5px auto; background:#42A5F5; color:#fff; border:none;"),
          tags$button(type = "button", class = "btn",
                      `data-dismiss` = "modal",
                      style = "display:block; width:80%; margin:5px auto; background:#E3F2FD; color:#1565C0; border:none;",
                      "Close")
        )
      ))
    })
  })
  
  # ── VIC Fetch & process data ────────────────────────────────────────────
  observeEvent(input$vic_fetch_data, {
    req(rv$vic_location_id, input$vic_start_year, input$vic_end_year)
    
    if (is.null(rv$vic_location_id)) {
      showNotification("No local xlsx data available for this station.",
                       type = "error", duration = 5)
      return()
    }
    if (as.integer(input$vic_start_year) > as.integer(input$vic_end_year)) {
      showNotification("End year must not be before start year.",
                       type = "error", duration = 5)
      return()
    }
    
    disable_controls(VIC_CONTROLS)
    on.exit(enable_controls(VIC_CONTROLS), add = TRUE)
    
    rv$vic_status_messages <- c()
    
    withProgress(message = "Loading VIC air quality data...", value = 0, {
      # Try pre-processed RDS first (instant), fall back to xlsx
      incProgress(0.3, detail = "Loading pre-processed data...")
      rds_data <- load_vic_observations_rds(
        rv$vic_location_id,
        as.integer(input$vic_start_year),
        as.integer(input$vic_end_year),
        location_name = rv$vic_selected_site_name
      )
      
      if (!is.null(rds_data) && nrow(rds_data) > 0) {
        # RDS path — data already in dashboard-ready wide format
        # Filter columns to selected parameters
        selected_params <- input$vic_parameters
        if (length(selected_params) > 0) {
          display_cols <- VIC_PARAM_MAP[selected_params]
          display_cols <- display_cols[!is.na(display_cols)]
          keep_cols <- intersect(c("Date", display_cols), names(rds_data))
          rds_data <- rds_data[, keep_cols, drop = FALSE]
        }
        
        df_hourly <- rds_data
        available <- identify_pollutants(df_hourly)
        df_daily <- calculate_daily(df_hourly, available)
        total_records <- nrow(df_hourly)
        
        rv$vic_df_hourly <- df_hourly
        rv$vic_df_daily <- df_daily
        rv$vic_available_pollutants <- available
        rv$active_source <- "VIC"
        
        rv$vic_summary_hourly <- calculate_hourly_summary(df_hourly, available)
        rv$vic_summary_daily <- calculate_daily_summary(df_daily, available, source = "VIC")
        
        rv$vic_status_messages <- c(
          paste("\u2713 Loaded from pre-processed data"),
          paste("\u2713 Total records:", total_records),
          paste("\u2713 Pollutants found:", paste(available, collapse = ", "))
        )
      } else {
        # Fallback: read from xlsx files (slow)
        incProgress(0.1, detail = "No pre-processed data, reading xlsx files...")
        result <- load_vic_observations(
          location_id = rv$vic_location_id,
          start_year = as.integer(input$vic_start_year),
          end_year = as.integer(input$vic_end_year),
          progress_callback = function(amt, msg) incProgress(amt, detail = msg)
        )
        
        rv$vic_status_messages <- result$messages
        
        if (is.null(result$data) || nrow(result$data) == 0) {
          showNotification("No VIC data found for the selected period.",
                           type = "error", duration = 10)
          return()
        }
        
        incProgress(0.05, detail = "Processing VIC data...")
        processed <- process_vic_observations(result$data)
        total_records <- processed$total_records
        
        rv$vic_df_hourly <- processed$hourly
        rv$vic_df_daily <- processed$daily
        rv$vic_available_pollutants <- processed$pollutants
        rv$active_source <- "VIC"
        
        rv$vic_summary_hourly <- calculate_hourly_summary(processed$hourly, processed$pollutants)
        rv$vic_summary_daily <- calculate_daily_summary(processed$daily, processed$pollutants, source = "VIC")
        
        rv$vic_status_messages <- c(
          rv$vic_status_messages,
          paste("\n\u2713 VIC data processing complete!"),
          paste("\u2713 Total records:", total_records),
          paste("\u2713 Pollutants found:", paste(processed$pollutants, collapse = ", "))
        )
      }
      
      showModal(modalDialog(
        title = NULL,
        size = "s",
        easyClose = TRUE,
        div(style = "text-align:center; padding:20px 0 10px;",
          icon("check-circle", style = "font-size:48px; color:#4CAF50;"),
          tags$h4(style = "margin-top:12px; font-weight:600;", rv$vic_selected_site),
          tags$p(style = "color:#888; margin:5px 0 15px;",
                 total_records, " records loaded")
        ),
        footer = div(style = "text-align:center;",
          actionButton("go_results", "Results",
                       class = "btn",
                       style = "display:block; width:80%; margin:5px auto; background:#2196F3; color:#fff; border:none;"),
          actionButton("go_viz", "Visualizations",
                       class = "btn",
                       style = "display:block; width:80%; margin:5px auto; background:#42A5F5; color:#fff; border:none;"),
          tags$button(type = "button", class = "btn",
                      `data-dismiss` = "modal",
                      style = "display:block; width:80%; margin:5px auto; background:#E3F2FD; color:#1565C0; border:none;",
                      "Close")
        )
      ))
    })
  })
  
  # ── SA Fetch & process data ─────────────────────────────────────────────
  observeEvent(input$sa_fetch_data, {
    req(rv$sa_selected_rds_stem, input$sa_start_year, input$sa_end_year)
    
    if (is.null(rv$sa_selected_rds_stem)) {
      showNotification("Please click an SA station on the map first.",
                       type = "error", duration = 5)
      return()
    }
    if (length(input$sa_parameters) == 0) {
      showNotification("Please select at least one parameter.",
                       type = "error", duration = 5)
      return()
    }
    if (as.integer(input$sa_start_year) > as.integer(input$sa_end_year)) {
      showNotification("End year must not be before start year.",
                       type = "error", duration = 5)
      return()
    }
    
    disable_controls(SA_CONTROLS)
    on.exit(enable_controls(SA_CONTROLS), add = TRUE)
    
    rv$sa_status_messages <- c()
    
    withProgress(message = "Loading SA data from files...", value = 0, {
      incProgress(0.3, detail = "Reading pre-processed data...")
      
      tryCatch({
        df_hourly <- load_sa_observations(
          rv$sa_selected_rds_stem,
          as.integer(input$sa_start_year),
          as.integer(input$sa_end_year)
        )
        
        if (is.null(df_hourly) || nrow(df_hourly) == 0) {
          showNotification("No SA data found for the selected period.",
                           type = "error", duration = 10)
          return()
        }
        
        # Filter to selected parameters + Date
        selected_params <- input$sa_parameters
        keep_cols <- intersect(c("Date", selected_params), names(df_hourly))
        df_hourly <- df_hourly[, keep_cols, drop = FALSE]
        
        incProgress(0.3, detail = "Processing...")
        
        available <- identify_pollutants(df_hourly)
        df_daily <- calculate_daily(df_hourly, available)
        
        rv$sa_df_hourly <- df_hourly
        rv$sa_df_daily <- df_daily
        rv$sa_available_pollutants <- available
        rv$active_source <- "SA"
        
        # Calculate summaries
        rv$sa_summary_hourly <- calculate_hourly_summary(df_hourly, available)
        rv$sa_summary_daily <- calculate_daily_summary(df_daily, available, source = "SA")
        
        total_records <- nrow(df_hourly)
        rv$sa_status_messages <- c(
          paste("\u2713 SA data loaded successfully!"),
          paste("\u2713 Total records:", total_records),
          paste("\u2713 Pollutants found:", paste(available, collapse = ", "))
        )
        
        showModal(modalDialog(
          title = NULL,
          size = "s",
          easyClose = TRUE,
          div(style = "text-align:center; padding:20px 0 10px;",
            icon("check-circle", style = "font-size:48px; color:#4CAF50;"),
            tags$h4(style = "margin-top:12px; font-weight:600;", rv$sa_selected_site),
            tags$p(style = "color:#888; margin:5px 0 15px;",
                   total_records, " records loaded")
          ),
          footer = div(style = "text-align:center;",
            actionButton("go_results", "Results",
                         class = "btn",
                         style = "display:block; width:80%; margin:5px auto; background:#2196F3; color:#fff; border:none;"),
            actionButton("go_viz", "Visualizations",
                         class = "btn",
                         style = "display:block; width:80%; margin:5px auto; background:#42A5F5; color:#fff; border:none;"),
            tags$button(type = "button", class = "btn",
                        `data-dismiss` = "modal",
                        style = "display:block; width:80%; margin:5px auto; background:#E3F2FD; color:#1565C0; border:none;",
                        "Close")
          )
        ))
      }, error = function(e) {
        showNotification(paste("Error loading SA data:", e$message),
                         type = "error", duration = 10)
        rv$sa_status_messages <- c(paste("\u2717 Error:", e$message))
      })
    })
  })
  
  # ── Modal navigation buttons ────────────────────────────────────────────
  observeEvent(input$go_results, {
    removeModal()
    updateNavbarPage(session, "nav", selected = "Results")
  })
  observeEvent(input$go_viz, {
    removeModal()
    updateNavbarPage(session, "nav", selected = "Visualizations")
  })
  
  # ── Outputs: status ─────────────────────────────────────────────────────
  output$nsw_fetch_status <- renderText({
    paste(rv$nsw_status_messages, collapse = "\n")
  })
  
  output$qld_fetch_status <- renderText({
    paste(rv$qld_status_messages, collapse = "\n")
  })
  
  output$vic_fetch_status <- renderText({
    paste(rv$vic_status_messages, collapse = "\n")
  })
  
  output$sa_fetch_status <- renderText({
    paste(rv$sa_status_messages, collapse = "\n")
  })
  
  # ── Helper reactives: active data (NSW, QLD, or VIC) ────────────────────
  active_hourly <- reactive({
    switch(rv$active_source,
      "QLD" = rv$qld_df_hourly,
      "VIC" = rv$vic_df_hourly,
      "SA"  = rv$sa_df_hourly,
      rv$nsw_df_hourly)
  })
  active_daily <- reactive({
    switch(rv$active_source,
      "QLD" = rv$qld_df_daily,
      "VIC" = rv$vic_df_daily,
      "SA"  = rv$sa_df_daily,
      rv$nsw_df_daily)
  })
  active_summary_hourly <- reactive({
    switch(rv$active_source,
      "QLD" = rv$qld_summary_hourly,
      "VIC" = rv$vic_summary_hourly,
      "SA"  = rv$sa_summary_hourly,
      rv$nsw_summary_hourly)
  })
  active_summary_daily <- reactive({
    switch(rv$active_source,
      "QLD" = rv$qld_summary_daily,
      "VIC" = rv$vic_summary_daily,
      "SA"  = rv$sa_summary_daily,
      rv$nsw_summary_daily)
  })
  active_pollutants <- reactive({
    switch(rv$active_source,
      "QLD" = rv$qld_available_pollutants,
      "VIC" = rv$vic_available_pollutants,
      "SA"  = rv$sa_available_pollutants,
      rv$nsw_available_pollutants)
  })
  
  active_exceedance_dates <- reactive({
    req(active_daily())
    calculate_exceedance_dates(active_daily(), active_hourly(),
                               active_pollutants(), source = rv$active_source)
  })
  
  # ── Outputs: tables ─────────────────────────────────────────────────────
  output$hourly_summary <- renderDT({
    req(active_summary_hourly())
    datatable(active_summary_hourly(),
              options = list(pageLength = 25, scrollX = TRUE,
                             columnDefs = list(list(width = '450px', targets = 0))),
              rownames = FALSE) %>%
      formatRound(columns = 2:ncol(active_summary_hourly()), digits = 2)
  })
  
  output$daily_summary <- renderDT({
    req(active_summary_daily())
    datatable(active_summary_daily(),
              options = list(pageLength = 25, scrollX = TRUE,
                             columnDefs = list(list(width = '450px', targets = 0))),
              rownames = FALSE) %>%
      formatRound(columns = 2:ncol(active_summary_daily()), digits = 2)
  })
  
  output$exceedance_dates <- renderDT({
    req(active_exceedance_dates())
    df <- active_exceedance_dates()
    if (nrow(df) == 0) {
      return(datatable(
        data.frame(Message = "No exceedances of PM10, PM2.5 or NO2 criteria in the selected period.",
                   stringsAsFactors = FALSE),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    df$Date <- format(df$Date, "%d/%m/%Y")
    datatable(df,
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # ── Outputs: plots ──────────────────────────────────────────────────────
  output$dust_plot <- renderPlot({
    req(active_daily())
    p <- build_dust_plot(active_daily(), source = rv$active_source)
    if (is.null(p)) {
      plot.new()
      text(0.5, 0.5, "No PM10 or PM2.5 data available", cex = 1.5)
    } else {
      print(p)
    }
  })
  
  output$time_plot <- renderPlot({
    req(active_hourly(), active_pollutants())
    ok <- render_time_plot(active_hourly(), active_pollutants())
    if (isFALSE(ok)) {
      plot.new()
      text(0.5, 0.5, "No data available for time plot", cex = 1.5)
    }
  })
  
  # ── Outputs: downloads ──────────────────────────────────────────────────
  
  # Helper: get sanitized station name for filenames
  active_station_name <- reactive({
    name <- switch(rv$active_source,
      "QLD" = rv$qld_selected_site,
      "VIC" = rv$vic_selected_site,
      "SA"  = rv$sa_selected_site,
      rv$nsw_selected_site)
    gsub("[^A-Za-z0-9_-]", "_", if (is.null(name)) "station" else name)
  })
  
  # Helper: write AQMS CSV with UTF-8 encoding (fixes special characters in Excel) and format dates
  write_csv_aqms <- function(df, file) {
    # Format POSIXct Date column as "d/mm/yyyy H:MM" for export
    if ("Date" %in% names(df) && inherits(df$Date, "POSIXct")) {
      df$Date <- format(df$Date, "%d/%m/%Y %H:%M")
    } else if ("Date" %in% names(df) && inherits(df$Date, "Date")) {
      df$Date <- format(df$Date, "%d/%m/%Y")
    }
    con <- file(file, open = "wb")
    writeBin(charToRaw("\xEF\xBB\xBF"), con)
    close(con)
    write.table(df, file, sep = ",", row.names = FALSE, append = TRUE,
                fileEncoding = "UTF-8", quote = TRUE)
  }
  
  output$download_hourly <- downloadHandler(
    filename = function() paste0(active_station_name(), "_hourly_data.csv"),
    content = function(file) {
      req(active_hourly())
      write_csv_aqms(active_hourly(), file)
    }
  )
  
  output$download_daily <- downloadHandler(
    filename = function() paste0(active_station_name(), "_daily_data.csv"),
    content = function(file) {
      req(active_daily())
      write_csv_aqms(active_daily(), file)
    }
  )
  
  output$download_hourly_summary <- downloadHandler(
    filename = function() paste0(active_station_name(), "_hourly_summary.csv"),
    content = function(file) {
      req(active_summary_hourly())
      write_csv_aqms(active_summary_hourly(), file)
    }
  )
  
  output$download_daily_summary <- downloadHandler(
    filename = function() paste0(active_station_name(), "_daily_summary.csv"),
    content = function(file) {
      req(active_summary_daily())
      write_csv_aqms(active_summary_daily(), file)
    }
  )
  output$download_exceedance_dates <- downloadHandler(
    filename = function() paste0(active_station_name(), "_exceedance_dates.csv"),
    content = function(file) {
      req(active_exceedance_dates())
      write_csv_aqms(active_exceedance_dates(), file)
    }
  )
  output$download_dust_pdf <- downloadHandler(
    filename = function() paste0(active_station_name(), "_dust_concentrations.pdf"),
    content = function(file) {
      req(active_daily())
      pdf(file, width = 11, height = 8)
      p <- build_dust_plot(active_daily(), source = rv$active_source)
      if (!is.null(p)) print(p)
      dev.off()
    }
  )
  
  output$download_time_pdf <- downloadHandler(
    filename = function() paste0(active_station_name(), "_time_series.pdf"),
    content = function(file) {
      req(active_hourly(), active_pollutants())
      pdf(file, width = 11, height = 8)
      render_time_plot(active_hourly(), active_pollutants())
      dev.off()
    }
  )
  
  # ══════════════════════════════════════════════════════════════════════════
  # Data Pull tab
  # ══════════════════════════════════════════════════════════════════════════
  
  output$dp_station_info <- renderText({
    src <- rv$last_clicked_source
    if (is.null(src)) return("No station selected — click a station on the map first")
    
    switch(src,
      "NSW" = paste0(rv$nsw_selected_site, " (NSW)"),
      "QLD" = paste0(rv$qld_selected_site, " (QLD)"),
      "VIC" = paste0(rv$vic_selected_site, " (VIC)"),
      "SA"  = paste0(rv$sa_selected_site, " (SA)"),
      "No station selected")
  })
  
  # Data Pull fetch handler
  observeEvent(input$dp_fetch, {
    src <- rv$last_clicked_source
    if (is.null(src)) {
      showNotification("Please select a station on the map first.",
                       type = "error", duration = 5)
      return()
    }
    if (length(input$dp_parameters) == 0) {
      showNotification("Please select at least one parameter.",
                       type = "error", duration = 5)
      return()
    }
    if (input$dp_start_date > input$dp_end_date) {
      showNotification("End date must not be before start date.",
                       type = "error", duration = 5)
      return()
    }
    
    disable_controls(DP_CONTROLS)
    on.exit(enable_controls(DP_CONTROLS), add = TRUE)
    
    rv$dp_status_messages <- c()
    rv$dp_hourly <- NULL
    rv$dp_daily <- NULL
    
    withProgress(message = "Pulling data...", value = 0, {
      if (src == "NSW") {
        # NSW fetch by date range
        site_id <- rv$nsw_site_details %>%
          filter(SiteName == rv$nsw_selected_site) %>%
          pull(Site_Id)
        chosen_params <- expand_nsw_params(input$dp_parameters)
        
        result <- fetch_nsw_observations_daterange(
          site_ids = site_id,
          params = chosen_params,
          start_date = input$dp_start_date,
          end_date = input$dp_end_date,
          progress_callback = function(amt, msg) incProgress(amt, detail = msg)
        )
        
        rv$dp_status_messages <- result$messages
        
        if (nrow(result$data) == 0) {
          showNotification("No data retrieved.", type = "error", duration = 10)
          return()
        }
        
        incProgress(0.05, detail = "Processing...")
        processed <- process_nsw_observations(result$data)
        rv$dp_hourly <- processed$hourly
        rv$dp_daily <- processed$daily
        
      } else if (src == "QLD") {
        # QLD fetch by date range
        result <- fetch_qld_observations_daterange(
          station_id = rv$qld_selected_station_id,
          start_date = input$dp_start_date,
          end_date = input$dp_end_date,
          progress_callback = function(amt, msg) incProgress(amt, detail = msg)
        )
        
        rv$dp_status_messages <- result$messages
        
        if (nrow(result$data) == 0) {
          showNotification("No QLD data retrieved.", type = "error", duration = 10)
          return()
        }
        
        incProgress(0.05, detail = "Processing...")
        processed <- process_qld_observations(result$data, rv$qld_param_lookup)
        rv$dp_hourly <- processed$hourly
        rv$dp_daily <- processed$daily
        
      } else if (src == "VIC") {
        # VIC: try pre-processed RDS first, fall back to xlsx
        start_yr <- as.integer(format(input$dp_start_date, "%Y"))
        end_yr <- as.integer(format(input$dp_end_date, "%Y"))
        
        rds_data <- load_vic_observations_rds(
          rv$vic_location_id, start_yr, end_yr,
          location_name = rv$vic_selected_site_name
        )
        
        if (!is.null(rds_data) && nrow(rds_data) > 0) {
          # RDS path — filter to exact date range
          rds_data <- rds_data %>%
            filter(as.Date(Date, tz = "Australia/Brisbane") >= as.Date(input$dp_start_date) & as.Date(Date, tz = "Australia/Brisbane") <= as.Date(input$dp_end_date))
          
          if (nrow(rds_data) == 0) {
            showNotification("No VIC data found for the selected period.", type = "error", duration = 10)
            return()
          }
          
          incProgress(0.05, detail = "Processing...")
          available <- identify_pollutants(rds_data)
          rv$dp_hourly <- rds_data
          rv$dp_daily <- calculate_daily(rds_data, available)
        } else {
          # Fallback: read from xlsx files
          result <- load_vic_observations(
            location_id = rv$vic_location_id,
            start_year = start_yr,
            end_year = end_yr,
            progress_callback = function(amt, msg) incProgress(amt, detail = msg)
          )
          
          rv$dp_status_messages <- result$messages
          
          if (is.null(result$data) || nrow(result$data) == 0) {
            showNotification("No VIC data found.", type = "error", duration = 10)
            return()
          }
          
          result$data <- result$data %>%
            filter(Date >= as.Date(input$dp_start_date) & Date <= as.Date(input$dp_end_date))
          
          incProgress(0.05, detail = "Processing...")
          processed <- process_vic_observations(result$data)
          rv$dp_hourly <- processed$hourly
          rv$dp_daily <- processed$daily
        }
        
      } else if (src == "SA") {
        # SA fetch from pre-processed RDS files by date range
        start_yr <- as.integer(format(input$dp_start_date, "%Y"))
        end_yr <- as.integer(format(input$dp_end_date, "%Y"))
        
        tryCatch({
          df_hourly <- load_sa_observations(rv$sa_selected_rds_stem, start_yr, end_yr)
          
          # Filter to exact date range
          df_hourly <- df_hourly %>%
            filter(as.Date(Date, tz = "Australia/Darwin") >= as.Date(input$dp_start_date) & as.Date(Date, tz = "Australia/Darwin") <= as.Date(input$dp_end_date))
          
          if (nrow(df_hourly) == 0) {
            showNotification("No SA data found for the selected period.", type = "error", duration = 10)
            return()
          }
          
          incProgress(0.05, detail = "Processing...")
          available <- identify_pollutants(df_hourly)
          rv$dp_hourly <- df_hourly
          rv$dp_daily <- calculate_daily(df_hourly, available)
        }, error = function(e) {
          showNotification(paste("Error loading SA data:", e$message), type = "error", duration = 10)
          return()
        })
      }
      
      rv$dp_status_messages <- c(
        rv$dp_status_messages,
        paste("\n\u2713 Done!", nrow(rv$dp_hourly), "hourly rows,", nrow(rv$dp_daily), "daily rows")
      )
      showNotification("Data pulled successfully!", type = "message", duration = 5)
    })
  })
  
  output$dp_status <- renderText({
    paste(rv$dp_status_messages, collapse = "\n")
  })
  
  output$dp_preview <- renderDT({
    req(rv$dp_hourly)
    df_display <- rv$dp_hourly
    if ("Date" %in% names(df_display) && inherits(df_display$Date, "POSIXct")) {
      df_display$Date <- format(df_display$Date, "%d/%m/%Y %H:%M")
    }
    datatable(df_display,
              options = list(pageLength = 20, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$dp_download_hourly <- downloadHandler(
    filename = function() {
      name <- switch(rv$last_clicked_source,
        "QLD" = rv$qld_selected_site,
        "VIC" = rv$vic_selected_site,
        "SA"  = rv$sa_selected_site,
        rv$nsw_selected_site)
      name <- gsub("[^A-Za-z0-9_-]", "_", if (is.null(name)) "station" else name)
      paste0(name, "_hourly.csv")
    },
    content = function(file) {
      req(rv$dp_hourly)
      write_csv_aqms(rv$dp_hourly, file)
    }
  )
  
  output$dp_download_daily <- downloadHandler(
    filename = function() {
      name <- switch(rv$last_clicked_source,
        "QLD" = rv$qld_selected_site,
        "VIC" = rv$vic_selected_site,
        "SA"  = rv$sa_selected_site,
        rv$nsw_selected_site)
      name <- gsub("[^A-Za-z0-9_-]", "_", if (is.null(name)) "station" else name)
      paste0(name, "_daily.csv")
    },
    content = function(file) {
      req(rv$dp_daily)
      write_csv_aqms(rv$dp_daily, file)
    }
  )
}
