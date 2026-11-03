# Multi-State EPA Air Quality Dashboard - UI
# Defines the user interface layout

ui <- bootstrapPage(
  
  navbarPage(
    theme = shinytheme("cosmo"),
    collapsible = TRUE,
    position = "fixed-top",
    HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">EPA Air Quality Analysis</a>'),
    id = "nav",
    windowTitle = "EPA Air Quality Analysis",
    
    header = tags$head(tags$style(HTML("
      body { padding-top: 70px; }
      .btn { margin-bottom: 10px; }
      .form-group { margin-bottom: 15px; }
      /* Animated stripes on Shiny progress bar */
      .shiny-notification .progress .progress-bar {
        background-image: linear-gradient(
          -45deg,
          rgba(255,255,255,.15) 25%, transparent 25%,
          transparent 50%, rgba(255,255,255,.15) 50%,
          rgba(255,255,255,.15) 75%, transparent 75%,
          transparent
        );
        background-size: 40px 40px;
        animation: progress-stripes 1s linear infinite;
      }
      @keyframes progress-stripes {
        from { background-position: 40px 0; }
        to   { background-position: 0 0; }
      }
    "))),
    
    ## Tab 1: Station Map ########################################################
    tabPanel("AQMS Stations",
      div(class = "outer",
        tags$head(includeCSS("www/styles.css")),
        leafletOutput("site_map", width = "100%", height = "100%"),
        
        # Station search bar (top-center)
        absolutePanel(
          top = 6, left = "50%", fixed = FALSE,
          style = "margin-left: -160px; z-index: 1000; width: 320px;",
          selectizeInput("station_search", label = NULL, choices = NULL,
                         options = list(placeholder = "Search station...",
                                        maxOptions = 15))
        ),
        
        # Refresh button (top-right corner)
        absolutePanel(
          top = 0, right = 10, fixed = FALSE,
          actionButton("refresh_stations", "Refresh Stations",
                       class = "btn-sm",
                       style = "background:#E3F2FD; color:#1565C0; border:none;")
        ),
        
        # NSW controls panel
        hidden(
          absolutePanel(
            id = "nsw_controls", class = "panel panel-default",
            top = "15%", left = 187, fixed = FALSE,
            draggable = FALSE, height = "auto",
            
            actionButton("nsw_panel_close", "X",
              style = "position:absolute;right:2px;top:12px;background-color:#FFFFFF;color:#000000;"),
            
            span(tags$b(h3(textOutput("nsw_selected_site_name"))), style = "color:#000000"),
            shinyjs::useShinyjs(),
            
            pickerInput(
              inputId = "nsw_parameters",
              label = "Select Parameter(s):",
              choices = NULL,
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                `selected-text-format` = "count > 3"
              )
            ),
            
            selectInput(
              inputId = "nsw_start_year",
              label = "Start Year:",
              choices = 2000:2025,
              selected = 2021
            ),
            
            selectInput(
              inputId = "nsw_end_year",
              label = "End Year:",
              choices = 2000:2025,
              selected = 2025
            ),
            
            actionButton(
              inputId = "nsw_fetch_data",
              label = "Fetch Data",
              class = "btn-block",
              style = "background:#2196F3; color:#fff; border:none;"
            ),
            
            div(style = "max-height:200px; overflow-y:auto;",
              verbatimTextOutput("nsw_fetch_status")
            )
          )
        ),
        
        # QLD controls panel
        hidden(
          absolutePanel(
            id = "qld_controls", class = "panel panel-default",
            top = "15%", left = 187, fixed = FALSE,
            draggable = FALSE, height = "auto",
            
            actionButton("qld_panel_close", "X",
              style = "position:absolute;right:2px;top:12px;background-color:#FFFFFF;color:#000000;"),
            
            span(tags$b(h3(textOutput("qld_selected_site_name"))), style = "color:#000000"),
            
            pickerInput(
              inputId = "qld_parameters",
              label = "Select Parameter(s):",
              choices = NULL,
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                `selected-text-format` = "count > 3"
              )
            ),
            
            selectInput(
              inputId = "qld_start_year",
              label = "Start Year:",
              choices = 2000:2025,
              selected = 2021
            ),
            
            selectInput(
              inputId = "qld_end_year",
              label = "End Year:",
              choices = 2000:2025,
              selected = 2025
            ),
            
            actionButton(
              inputId = "qld_fetch_data",
              label = "Fetch Data",
              class = "btn-block",
              style = "background:#2196F3; color:#fff; border:none;"
            ),
            
            div(style = "max-height:200px; overflow-y:auto;",
              verbatimTextOutput("qld_fetch_status")
            )
          )
        ),
        
        # VIC controls panel
        hidden(
          absolutePanel(
            id = "vic_controls", class = "panel panel-default",
            top = "15%", left = 187, fixed = FALSE,
            draggable = FALSE, height = "auto",
            
            actionButton("vic_panel_close", "X",
              style = "position:absolute;right:2px;top:12px;background-color:#FFFFFF;color:#000000;"),
            
            span(tags$b(h3(textOutput("vic_selected_site_name"))), style = "color:#000000"),
            
            pickerInput(
              inputId = "vic_parameters",
              label = "Select Parameter(s):",
              choices = NULL,
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                `selected-text-format` = "count > 3"
              )
            ),
            
            selectInput(
              inputId = "vic_start_year",
              label = "Start Year:",
              choices = 2016:2026,
              selected = 2024
            ),
            
            selectInput(
              inputId = "vic_end_year",
              label = "End Year:",
              choices = 2016:2026,
              selected = 2026
            ),
            
            actionButton(
              inputId = "vic_fetch_data",
              label = "Fetch Data",
              class = "btn-block",
              style = "background:#2196F3; color:#fff; border:none;"
            ),
            
            div(style = "max-height:200px; overflow-y:auto;",
              verbatimTextOutput("vic_fetch_status")
            )
          )
        ),
        
        # SA controls panel
        hidden(
          absolutePanel(
            id = "sa_controls", class = "panel panel-default",
            top = "15%", left = 187, fixed = FALSE,
            draggable = FALSE, height = "auto",
            
            actionButton("sa_panel_close", "X",
              style = "position:absolute;right:2px;top:12px;background-color:#FFFFFF;color:#000000;"),
            
            span(tags$b(h3(textOutput("sa_selected_site_name"))), style = "color:#000000"),
            
            pickerInput(
              inputId = "sa_parameters",
              label = "Select Parameter(s):",
              choices = NULL,
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                `selected-text-format` = "count > 3"
              )
            ),
            
            selectInput(
              inputId = "sa_start_year",
              label = "Start Year:",
              choices = 2020:2026,
              selected = 2024
            ),
            
            selectInput(
              inputId = "sa_end_year",
              label = "End Year:",
              choices = 2020:2026,
              selected = 2026
            ),
            
            actionButton(
              inputId = "sa_fetch_data",
              label = "Fetch Data",
              class = "btn-block",
              style = "background:#2196F3; color:#fff; border:none;"
            ),
            
            div(style = "max-height:200px; overflow-y:auto;",
              verbatimTextOutput("sa_fetch_status")
            )
          )
        )
      )
    ),
    
    ## Tab 2: Data Pull ########################################################
    tabPanel("Data Pull",
      fluidPage(
        style = "padding-left:15px; padding-top:80px;",
        fluidRow(
          column(4,
            tags$h3("Data Pull & Download"),
            tags$p(
              tags$b("Selected Station: "),
              textOutput("dp_station_info", inline = TRUE)
            ),
            
            pickerInput(
              inputId = "dp_parameters",
              label = "Select Parameter(s):",
              choices = NULL,
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                `selected-text-format` = "count > 3"
              )
            ),
            
            dateInput("dp_start_date", "Start Date:",
                      value = as.Date("2025-01-01"),
                      min = as.Date("2000-01-01"),
                      max = as.Date("2026-12-31")),
            
            dateInput("dp_end_date", "End Date:",
                      value = as.Date("2025-12-31"),
                      min = as.Date("2000-01-01"),
                      max = as.Date("2026-12-31")),
            
            actionButton("dp_fetch", "Pull Data",
                          class = "btn-block",
                          style = "background:#2196F3; color:#fff; border:none;"),
            
            tags$hr(),
            
            fluidRow(
              column(6, downloadButton("dp_download_hourly", "Hourly CSV",
                                       class = "btn-block",
                                       style = "background:#42A5F5; color:#fff; border:none;")),
              column(6, downloadButton("dp_download_daily", "Daily CSV",
                                       class = "btn-block",
                                       style = "background:#42A5F5; color:#fff; border:none;"))
            ),
            
            tags$hr(),
            verbatimTextOutput("dp_status")
          ),
          column(8,
            tags$h4("Data Preview"),
            DTOutput("dp_preview")
          )
        )
      )
    ),
    
    ## Tab 3: Results ########################################################
    tabPanel("Results",
      fluidPage(
        style = "padding-left:15px; padding-top:80px;",
        fluidRow(
          column(12,
            tags$h3("Hourly Summary Statistics"),
            DTOutput("hourly_summary"),
            tags$br(),
            tags$h3("Daily Summary Statistics"),
            DTOutput("daily_summary"),
            tags$br(),
            tags$h3("Exceedance Dates (PM10, PM2.5 & NO2)"),
            tags$p("Individual days exceeding NEPM criteria: PM10 24-hour > 50 µg/m³, ",
                   "PM2.5 24-hour > criteria, NO2 1-hour > 80 µg/m³."),
            DTOutput("exceedance_dates")
          )
        )
      )
    ),
    
    ## Tab 4: Visualizations ########################################################
    tabPanel("Visualizations",
      fluidPage(
        style = "padding-left:15px; padding-top:80px;",
        fluidRow(
          column(12,
            tags$h3("Ambient Dust Concentrations"),
            plotOutput("dust_plot", height = "500px"),
            tags$br(),
            tags$h3("Time Series Plot"),
            plotOutput("time_plot", height = "500px")
          )
        )
      )
    ),
    
    ## Tab 5: Export ########################################################
    tabPanel("Export",
      fluidPage(
        style = "padding-left:15px; padding-top:80px;",
        fluidRow(
          column(12,
            tags$h3("Download processed data as CSV files"),
            fluidRow(
              column(3, downloadButton("download_hourly", "Hourly Data",
                                       class = "btn-block",
                                       style = "background:#2196F3; color:#fff; border:none;")),
              column(3, downloadButton("download_daily", "Daily Data",
                                       class = "btn-block",
                                       style = "background:#2196F3; color:#fff; border:none;")),
              column(3, downloadButton("download_hourly_summary", "Hourly Summary",
                                       class = "btn-block",
                                       style = "background:#2196F3; color:#fff; border:none;")),
              column(3, downloadButton("download_daily_summary", "Daily Summary",
                                       class = "btn-block",
                                       style = "background:#2196F3; color:#fff; border:none;"))
            ),
            tags$br(),
            fluidRow(
              column(3, downloadButton("download_exceedance_dates", "Exceedance Dates",
                                       class = "btn-block",
                                       style = "background:#2196F3; color:#fff; border:none;"))
            ),
            tags$hr(),
            tags$h3("Download visualizations"),
            fluidRow(
              column(6, downloadButton("download_dust_pdf", "Dust Plot (PDF)",
                                       class = "btn-block",
                                       style = "background:#42A5F5; color:#fff; border:none;")),
              column(6, downloadButton("download_time_pdf", "Time Series (PDF)",
                                       class = "btn-block",
                                       style = "background:#42A5F5; color:#fff; border:none;"))
            )
          )
        )
      )
    ),
    
    ## Tab 6: Guide ########################################################
    tabPanel("Guide",
      fluidPage(
        style = "padding-left:15px; padding-top:80px; max-width:900px;",
        tags$h2("Multi-State Air Quality Dashboard — User Guide"),
        tags$hr(),
        
        # ── Overview ──
        tags$h3("Overview"),
        tags$p("This dashboard provides access to air quality monitoring data from four Australian states:"),
        tags$ul(
          tags$li(tags$span(style = "color:#97FFFF; font-weight:bold;", "\u25CF"), " ",
                  tags$b("NSW"), " — Real-time API data from NSW DPE (Department of Planning and Environment)"),
          tags$li(tags$span(style = "color:#FFD700; font-weight:bold;", "\u25CF"), " ",
                  tags$b("QLD"), " — Real-time API data from QLD DES (Department of Environment and Science)"),
          tags$li(tags$span(style = "color:#90EE90; font-weight:bold;", "\u25CF"), " ",
                  tags$b("VIC"), " — Pre-processed historical data from VIC EPA (2016\u20132026)"),
          tags$li(tags$span(style = "color:#FF6B35; font-weight:bold;", "\u25CF"), " ",
                  tags$b("SA"), " — Pre-processed historical data from SA EPA (2020+)")
        ),
        
        tags$hr(),
        
        # ── Step 1 ──
        tags$h3("Step 1 — Select a Station"),
        tags$p("Use the ", tags$b("AQMS Stations"), " tab to find a monitoring station:"),
        tags$ul(
          tags$li(tags$b("Click a map marker"), " — coloured by state (see legend above)"),
          tags$li(tags$b("Use the search bar"), " (top of map) — type a station name to fly to it"),
          tags$li(tags$b("Zoom/cluster"), " — markers are clustered; zoom in to reveal individual stations")
        ),
        tags$p("Once selected, a control panel appears showing the station name and available options."),
        
        # ── Step 2 ──
        tags$h3("Step 2 — Choose Parameters & Date Range"),
        tags$p("In the control panel that appears:"),
        tags$ul(
          tags$li("Select one or more parameters (e.g. PM10, PM2.5, NO2, O3, CO, SO2, meteorology)"),
          tags$li("Set the start and end ", tags$b("years"), " for the data you need")
        ),
        tags$p(tags$em("Available parameters vary by station — the picker only shows what's available for the selected station.")),
        
        # ── Step 3 ──
        tags$h3("Step 3 — Fetch Data"),
        tags$p("Click ", tags$b("Fetch Data"), ". What happens next depends on the state:"),
        tags$ul(
          tags$li(tags$b("NSW / QLD"), " — Data is downloaded live from the EPA API (month by month). This may take 30\u201360 seconds for large date ranges."),
          tags$li(tags$b("VIC / SA"), " — Data is loaded instantly from pre-processed local files (no internet required).")
        ),
        tags$p("A progress bar shows the status. When complete, a dialog offers quick links to Results or Visualizations."),
        
        # ── Step 4 ──
        tags$h3("Step 4 — View Results"),
        tags$p("The ", tags$b("Results"), " tab shows summary statistics for each pollutant:"),
        tags$ul(
          tags$li(tags$b("Hourly Summary"), " — Count, Mean, Median, SD, Min, Max, P90, P95, P99"),
          tags$li(tags$b("Daily Summary"), " — Same statistics plus exceedance day counts against NEPM criteria:",
                  tags$ul(
                    tags$li("PM10 > 50 \u00b5g/m\u00b3 (24-hour average)"),
                    tags$li("PM2.5 > 25 \u00b5g/m\u00b3 (24-hour average)"),
                    tags$li("NO2 > 80 \u00b5g/m\u00b3 (1-hour average)")
                  )),
          tags$li(tags$b("Exceedance Dates"), " — A detailed list of every individual day that exceeded the PM10, PM2.5 or NO2 criteria, with the year, date and measured value. Downloadable as CSV from the Export tab.")
        ),
        
        # ── Step 5 ──
        tags$h3("Step 5 — Visualizations"),
        tags$p("The ", tags$b("Visualizations"), " tab provides two charts:"),
        tags$ul(
          tags$li(tags$b("Dust Concentration Plot"), " — Daily average PM10 & PM2.5 with NEPM criteria lines (dashed). Useful for identifying exceedance events."),
          tags$li(tags$b("Time Series Plot"), " — Hourly data for all fetched parameters, powered by the ", tags$code("openair"), " package.")
        ),
        
        # ── Step 6 ──
        tags$h3("Step 6 — Data Pull (Custom Date Range)"),
        tags$p("The ", tags$b("Data Pull"), " tab provides a flexible date-range export tool:"),
        tags$ul(
          tags$li("Works with whichever station you last clicked on the map"),
          tags$li("Pick exact ", tags$b("start/end dates"), " (not just years)"),
          tags$li("Preview data in a scrollable table"),
          tags$li("Download hourly or daily CSV instantly")
        ),
        tags$p(tags$em("Ideal for extracting a specific event period (e.g. bushfire episode, dust storm).")),
        
        # ── Step 7 ──
        tags$h3("Step 7 — Export"),
        tags$p("The ", tags$b("Export"), " tab provides download buttons:"),
        tags$ul(
          tags$li(tags$b("CSV downloads"), " — Hourly data, Daily data, Hourly summary, Daily summary"),
          tags$li(tags$b("PDF downloads"), " — Dust concentration plot, Time series plot")
        ),
        tags$p("All CSV files include a UTF-8 BOM header for correct display of \u00b5g/m\u00b3 and \u00b0C symbols in Excel."),
        
        tags$hr(),
        
        # ── Data Sources ──
        tags$h3("Data Sources & Coverage"),
        tags$table(
          class = "table table-bordered table-sm",
          style = "max-width:700px;",
          tags$thead(tags$tr(
            tags$th("State"), tags$th("Source"), tags$th("Coverage"), tags$th("Update Frequency")
          )),
          tags$tbody(
            tags$tr(tags$td("NSW"), tags$td("DPE API (data.airquality.nsw.gov.au)"), tags$td("2012\u2013present"), tags$td("Live (real-time API)")),
            tags$tr(tags$td("QLD"), tags$td("DES API (data.qld.gov.au)"), tags$td("2016\u2013present"), tags$td("Live (real-time API)")),
            tags$tr(tags$td("VIC"), tags$td("EPA Victoria annual xlsx"), tags$td("2016\u20132026"), tags$td("Annual (manual update)")),
            tags$tr(tags$td("SA"), tags$td("SA EPA / data.sa.gov.au"), tags$td("2020\u2013present"), tags$td("Periodic (auto-download)"))
          )
        ),
        
        tags$hr(),
        
        # ── Tips ──
        tags$h3("Tips & Troubleshooting"),
        tags$ul(
          tags$li(tags$b("Empty parameter picker?"), " — Click ", tags$b("Refresh Stations"), " (top-right of map) to reload metadata from APIs."),
          tags$li(tags$b("VIC station shows warning?"), " — Some new/relocated VIC stations may not have pre-processed data available yet."),
          tags$li(tags$b("Slow fetch for NSW/QLD?"), " — Large date ranges (5+ years) require many API calls. The app fetches month-by-month to avoid timeouts."),
          tags$li(tags$b("Units"), " — All gaseous pollutants are converted to \u00b5g/m\u00b3 (or mg/m\u00b3 for CO) at STP. PM values are reported as measured.")
        ),
        
        tags$hr(),
        tags$p(style = "color:#888; font-size:0.9em;",
               "Multi-State Air Quality Dashboard | Built with R Shiny | Data sourced from NSW DPE, QLD DES, VIC EPA, SA EPA")
      )
    )
  )
)
