ui <- dashboardPage(
  dashboardHeader(
    title = "Jakarta Education Analysis",
    titleWidth = 350,
    disable = TRUE  # Disable default header
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$title("Jakarta Education Analysis"),
      tags$link(rel = "icon", type = "image/png", href = "education.png"),
      tags$style(HTML("
        /* Navbar base styling */
        .navbar {
            background-color: #3c8dbc;
            border-color: #357ca5;
            border-radius: 0;
            position: fixed;
            width: 100%;
            z-index: 1030;
        }
        
        /* Navbar brand and links */
        .navbar-default .navbar-nav > li > a,
        .navbar-brand {
            color: #fff !important;
        }
        
        /* Active and hover states */
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover,
        .navbar-default .navbar-nav > li > a:hover {
            background-color: #357ca5;
            color: #fff;
        }
        
        /* Brand/title styling */
        .navbar-brand {
            font-size: 20px;
            font-weight: 500;
        }
        
        /* Dropdown menu base */
        .dropdown-menu {
            background-color: #1e2835 !important;
            border: 1px solid #34495e !important;
            box-shadow: 0 6px 12px rgba(0,0,0,.175);
        }
        
        /* Dropdown items */
        .dropdown-menu > li > a {
            color: #ffffff !important;
            padding: 8px 20px;
        }
        
        /* Dropdown hover and focus states */
        .dropdown-menu > li > a:hover,
        .dropdown-menu > li > a:focus {
            background-color: #3c8dbc !important;
            color: #ffffff !important;
        }
        
        /* Active dropdown items */
        .dropdown-menu > .active > a,
        .dropdown-menu > .active > a:focus,
        .dropdown-menu > .active > a:hover {
            background-color: #3c8dbc !important;
            color: #ffffff !important;
        }
        
        /* Open dropdown state */
        .navbar-default .navbar-nav > .open > a,
        .navbar-default .navbar-nav > .open > a:focus,
        .navbar-default .navbar-nav > .open > a:hover {
            background-color: #357ca5 !important;
            color: #ffffff !important;
        }
        
        /* Dropdown divider */
        .dropdown-menu .divider {
            background-color: #34495e;
            margin: 5px 0;
        }
        
        /* Content wrapper adjustment */
        .content-wrapper {
            padding-top: 60px;
        }
        
        /* Navbar collapse styling */
        .navbar-default .navbar-collapse,
        .navbar-default .navbar-form {
            border-color: #357ca5;
        }
        
        /* Mobile toggle button */
        .navbar-default .navbar-toggle {
            border-color: #ffffff;
        }
        
        .navbar-default .navbar-toggle:focus,
        .navbar-default .navbar-toggle:hover {
            background-color: #357ca5;
        }
        
        .navbar-default .navbar-toggle .icon-bar {
            background-color: #ffffff;
        }
        
        /* Fix for dropdown positioning */
        .navbar .dropdown-menu {
            margin-top: 0;
        }
        
        /* Prevent text selection in navbar */
        .navbar {
            -webkit-user-select: none;
            -moz-user-select: none;
            -ms-user-select: none;
            user-select: none;
        }
    
  "))
    ),
    
    # Navigation Bar
    navbarPage(
      title = "Jakarta Education Analysis",
      id = "mainNav",
      theme = "navbar-default",
      position = "fixed-top",
      
      # Introduction Tab
      tabPanel(
        "Introduction",
        value = "intro",
        fluidRow(
          column(12,
                 box(width = 12,
                     status = "primary",
                     solidHeader = TRUE,
                     title = "Spatial Distribution Analysis of Education Facilities in Jakarta: Impact on Accessibility and Urban Planning (G1T7)",
                     div(
                       class = "intro-content",
                       style = "padding-left: 20px;, padding-right: 20px;, padding-bottom:20px;",
                       
                       h3("Project Overview", style = "color: #3c8dbc;"),
                       p("This study conducts a comprehensive spatial analysis of educational facilities across Jakarta 
                   to inform evidence-based policy making and urban development strategies. By examining spatial patterns, 
                   clustering tendencies, and accessibility relationships, we aim to identify areas needing educational 
                   resource intervention and optimize future school placement decisions."),
                       
                       h3("Project Motivation", style = "color: #3c8dbc;"),
                       p("Educational access and equity are fundamental to social mobility and economic development. 
                   Jakarta, as Indonesia's capital and largest city, faces unique urban planning challenges:"),
                       tags$ul(
                         tags$li("Rapid urbanization creating uneven population distribution"),
                         tags$li("Complex transportation networks affecting school accessibility"),
                         tags$li("Socioeconomic disparities influencing educational opportunities"),
                         tags$li("Need for data-driven approaches to educational resource allocation")
                       ),
                       
                       h3("Analysis Components", style = "color: #3c8dbc;"),
                       
                       h4("1. Spatial Points Patterns Analysis (SPPA)", style = "color: #444;"),
                       tags$ul(
                         tags$li("Examines the distribution and clustering of schools at provincial and city levels"),
                         tags$li("Utilizes techniques like Kernel Density Estimation (KDE), nearest neighbor analysis, and K/L functions"),
                         tags$li("Identifies significant patterns in school placement across different education levels")
                       ),
                       
                       h4("2. Local Indicators of Spatial Autocorrelation (LISA)", style = "color: #444;"),
                       tags$ul(
                         tags$li("Detects spatial clusters and outliers at the district level"),
                         tags$li("Analyzes relationships between school density and local characteristics"),
                         tags$li("Identifies high-high and low-low concentration areas requiring attention")
                       ),
                       
                       h4("3. Geographically Weighted Regression (GWR)", style = "color: #444;"),
                       tags$ul(
                         tags$li("Models relationships between education levels and school accessibility"),
                         tags$li("Accounts for spatial variation in these relationships"),
                         tags$li("Provides localized insights for targeted policy interventions")
                       ),
                       
                       h3("Significance", style = "color: #3c8dbc;"),
                       p("This spatial analysis approach offers several key benefits:"),
                       tags$ul(
                         tags$li("Evidence-based support for educational policy decisions"),
                         tags$li("Identification of underserved areas requiring intervention"),
                         tags$li("Understanding of spatial factors affecting educational access"),
                         tags$li("Framework for optimizing future school placement")
                       ),
                       
                       p(style = "margin-top: 20px;",
                         "Through this multi-scale analysis, we aim to contribute to more equitable educational access 
                   across Jakarta while providing actionable insights for urban planners and policy makers.")
                     )
                 )
          )
        )
      ),
      
      # Spatial Points Patterns Analysis Dropdown
      navbarMenu(
        "Spatial Points Patterns Analysis",
        tabPanel("Provincial Analysis",
                 value = "provincial",
                 # Provincial analysis content...
                 fluidRow(
                   column(3,
                          # Provincial controls from sidebar
                          div(class = "sidebar-section",
                              h4("Analysis Controls"),
                              
                              # School Levels with pretty checkboxes
                              prettyCheckboxGroup(
                                inputId = "provincialSchoolLevels",
                                label = "School Levels:",
                                choices = c(
                                  "Elementary", "Junior High",
                                  "Senior High", "Special Education"
                                ),
                                selected = NULL,
                                status = "primary",
                                animation = "smooth",
                                icon = icon("check")
                              ),
                              
                              # School Type with pretty radio buttons
                              div(style = "margin: 10px 0;",
                                  prettyRadioButtons(
                                    inputId = "provincialSchoolType",
                                    label = "School Type:",
                                    choices = c("All", "Public", "Private"),
                                    selected = "All",
                                    status = "primary",
                                    animation = "smooth",
                                    icon = icon("check")
                                  )
                              ),
                              
                              # KDE Bandwidth with styled slider
                              div(class = "slider-container",
                                  sliderTextInput(
                                    inputId = "provincialBandwidth",
                                    label = "KDE Bandwidth:",
                                    choices = seq(0.1, 1, 0.1),
                                    selected = 0.5,
                                    grid = TRUE
                                  ),
                                  bsTooltip(
                                    "provincialBandwidth",
                                    "Adjust the smoothing parameter for density estimation",
                                    placement = "right",
                                    options = list(container = "body")
                                  )
                              )
                          )
                   ),
                   column(9,
                          # Provincial content
                          fluidRow(
                            box(width = 6,
                                plotlyOutput("schoolDistPlot"),
                                title = "School Distribution by City",
                                subtitle = "Overview of school counts across Jakarta",
                                status = "primary",
                                solidHeader = TRUE,
                                footer = div(
                                  class = "text-muted",
                                  style = "font-style: italic; padding: 10px;",
                                  "Click on bars to filter the other visualizations"
                                )
                            ),
                            box(width = 6,
                                plotOutput("densityMap"),
                                title = "Kernel Density Estimation",
                                subtitle = "Spatial concentration of schools",
                                status = "primary",
                                solidHeader = TRUE,
                                footer = div(
                                  class = "text-muted",
                                  style = "font-style: italic; padding: 10px;",
                                  "Darker areas indicate higher school density"
                                )
                            )
                          ),
                          fluidRow(
                            box(width = 6,
                                plotlyOutput("levelDistPlot"),
                                title = "School Level Distribution",
                                subtitle = "Breakdown by education level",
                                status = "primary",
                                solidHeader = TRUE,
                                footer = div(
                                  class = "text-muted",
                                  style = "font-style: italic; padding: 10px;",
                                  "Stacked bars show proportion of school levels"
                                )
                            ),
                            box(width = 6,
                                plotOutput("populationMap"),
                                title = "Population Density Distribution",
                                subtitle = "Background context for school distribution",
                                status = "primary",
                                solidHeader = TRUE,
                                footer = div(
                                  class = "text-muted",
                                  style = "font-style: italic; padding: 10px;",
                                  "Compare with school density to identify gaps"
                                )
                            )
                          )
                   )
                 )
        ),
        tabPanel("City Analysis",
                 value = "city",
                 fluidRow(
                   column(3,
                          # City analysis controls
                          div(class = "sidebar-section",
                              h4("City Analysis Settings", 
                                 style = "color: #fff; padding-bottom: 5px; border-bottom: 1px solid #4f5962;"),
                              
                              # City Selection
                              div(class = "filter-group",
                                  pickerInput(
                                    inputId = "citySelect",
                                    label = "Select City:",
                                    choices = c(
                                      "Kota Jakarta Timur", 
                                      "Kota Jakarta Barat",
                                      "Kota Jakarta Selatan", 
                                      "Kota Jakarta Utara",
                                      "Kota Jakarta Pusat"
                                    ),
                                    options = list(
                                      style = "btn-primary",
                                      size = 5,
                                      `live-search` = TRUE
                                    )
                                  )
                              ),
                              
                              # School Levels
                              div(class = "filter-group",
                                  prettyCheckboxGroup(
                                    inputId = "citySchoolLevels",
                                    label = "School Levels:",
                                    choices = c(
                                      "Elementary", "Junior High",
                                      "Senior High", "Special Education"
                                    ),
                                    selected = NULL,
                                    status = "primary",
                                    animation = "smooth",
                                    icon = icon("check")
                                  )
                              ),
                              
                              # School Type
                              div(class = "filter-group",
                                  prettyRadioButtons(
                                    inputId = "citySchoolType",
                                    label = "School Type:",
                                    choices = c("All", "Public", "Private"),
                                    selected = "All",
                                    status = "primary",
                                    animation = "smooth",
                                    icon = icon("check")
                                  ),
                                  bsTooltip(
                                    "citySchoolType",
                                    "Filter schools by ownership type",
                                    placement = "right"
                                  )
                              )
                          )
                   ),
                   column(9,
                          # City analysis content
                          fluidRow(
                            box(width = 6,
                                plotOutput("cityKDE"),
                                title = "Kernel Density Estimation of Schools",
                                subtitle = "Spatial distribution of schools with population density overlay",
                                status = "primary",
                                solidHeader = TRUE,
                                footer = p("Darker areas indicate higher concentration of schools",
                                           style = "font-style: italic; color: #666;")
                            ),
                            box(width = 6,
                                plotOutput("typeRatio"),
                                title = "Public-Private School Distribution",
                                subtitle = "Comparison of school ownership types",
                                status = "primary",
                                solidHeader = TRUE,
                                footer = p("Ratio of public to private schools by area",
                                           style = "font-style: italic; color: #666;")
                            )
                          ),
                          fluidRow(
                            box(width = 6,
                                plotOutput("nnPlot"),
                                title = "Nearest Neighbor Analysis",
                                subtitle = "Spatial clustering assessment",
                                status = "primary",
                                solidHeader = TRUE,
                                footer = p("R < 1 indicates clustering, R > 1 indicates dispersion",
                                           style = "font-style: italic; color: #666;")
                            ),
                            box(width = 6,
                                DT::dataTableOutput("cityStats"),
                                title = "City Statistics Overview",
                                subtitle = "Summary statistics by school type and level",
                                status = "primary",
                                solidHeader = TRUE,
                                footer = p("Click column headers to sort data",
                                           style = "font-style: italic; color: #666;")
                            )
                          )
                   )
                 )
        ),
        tabPanel("Statistical Tests",
                 value = "stats",
                 fluidRow(
                   column(3,
                          # Statistical tests controls
                          div(class = "sidebar-section",
                              h4("Statistical Analysis Settings", 
                                 style = "color: #fff; padding-bottom: 5px; border-bottom: 1px solid #4f5962;"),
                              
                              # City Selection
                              div(class = "filter-group",
                                  pickerInput(
                                    inputId = "statsSelect",
                                    label = "Select City:",
                                    choices = c(
                                      "All",
                                      "Kota Jakarta Timur", 
                                      "Kota Jakarta Barat",
                                      "Kota Jakarta Selatan", 
                                      "Kota Jakarta Utara",
                                      "Kota Jakarta Pusat"
                                    ),
                                    options = list(
                                      style = "btn-primary",
                                      size = 6,
                                      `live-search` = TRUE
                                    )
                                  ),
                                  bsTooltip(
                                    "statsSelect",
                                    "Select 'All' for province-wide analysis",
                                    placement = "right"
                                  )
                              ),
                              
                              # School Levels
                              div(class = "filter-group",
                                  prettyCheckboxGroup(
                                    inputId = "statsSchoolLevels",
                                    label = "School Levels:",
                                    choices = c(
                                      "Elementary", "Junior High",
                                      "Senior High", "Special Education"
                                    ),
                                    selected = NULL,
                                    status = "primary",
                                    animation = "smooth",
                                    icon = icon("check")
                                  )
                              ),
                              
                              # School Type
                              div(class = "filter-group",
                                  prettyRadioButtons(
                                    inputId = "statsSchoolType",
                                    label = "School Type:",
                                    choices = c("All", "Public", "Private"),
                                    selected = "All",
                                    status = "primary",
                                    animation = "smooth",
                                    icon = icon("check")
                                  )
                              ),
                              
                              conditionalPanel(
                                condition = "input.statsSelect != 'All'",
                                div(class = "filter-group",
                                    sliderInput("simulations",
                                                "Number of CSR Simulations:",
                                                min = 0,
                                                max = 99,
                                                value = 0,
                                                step = 1),
                                    bsTooltip(
                                      "simulations",
                                      "Number of simulations for Monte Carlo test",
                                      placement = "right",
                                      options = list(container = "body")
                                    )
                                )
                              )
                          )
                   ),
                   column(9,
                          # Statistical tests content
                          fluidRow(
                            box(width = 6,
                                plotOutput("quadratPlot"),
                                verbatimTextOutput("quadratStats"),
                                title = "Quadrat Analysis",
                                subtitle = "Testing spatial randomness",
                                status = "primary",
                                solidHeader = TRUE,
                                footer = p("Chi-square test results for Complete Spatial Randomness",
                                           style = "font-style: italic; color: #666;")
                            ),
                            box(width = 6,
                                plotOutput("crossK"),
                                title = "Cross K Function Analysis",
                                subtitle = "Public-Private school interaction",
                                status = "primary",
                                solidHeader = TRUE,
                                footer = p("Values above theoretical line indicate attraction between types",
                                           style = "font-style: italic; color: #666;")
                            )
                          ),
                          fluidRow(
                            box(width = 6,
                                plotOutput("kFunction"),
                                title = "K Function Analysis",
                                subtitle = "Overall spatial dependency",
                                status = "primary",
                                solidHeader = TRUE,
                                footer = p("Ripley's K function measuring spatial clustering at different scales",
                                           style = "font-style: italic; color: #666;")
                            ),
                            box(width = 6,
                                plotOutput("lFunction"),
                                title = "L Function Analysis",
                                subtitle = "Normalized spatial dependency",
                                status = "primary",
                                solidHeader = TRUE,
                                footer = p("Linearized version of K function for easier interpretation",
                                           style = "font-style: italic; color: #666;")
                            )
                          )
                   )
                 )
        )
      ),
      
      # LISA and Hotspot Analysis
      tabPanel(
        "LISA and Hotspot Analysis",
        value = "lisa",
        fluidRow(
          column(3,
                 # LISA controls
                 div(class = "sidebar-section",
                     h4("LISA Analysis Settings"),
                     
                     div(class = "filter-group",
                         prettyRadioButtons(
                           inputId = "analysis_type",
                           label = "Analysis Type:",
                           choices = c(
                             "LISA (Local Moran's I)" = "lisa",
                             "Hot Spot (Getis-Ord G*)" = "gstar"
                           ),
                           selected = "lisa",
                           status = "primary",
                           animation = "smooth",
                           icon = icon("check")
                         )
                     ),
                     
                     conditionalPanel(
                       condition = "input.analysis_type == 'lisa'",
                       div(class = "filter-group",
                           prettyRadioButtons(
                             inputId = "weight_type",
                             label = "Neighborhood Definition:",
                             choices = c(
                               "Queen Contiguity" = "queen",
                               "Adaptive Distance Weight" = "distance"
                             ),
                             selected = "queen",
                             status = "primary",
                             animation = "smooth",
                             icon = icon("check")
                           )
                       ),
                       
                       conditionalPanel(
                         condition = "input.weight_type == 'distance'",
                         div(class = "slider-container",
                             sliderInput("k_neighbors",
                                         "Number of Nearest Neighbors (k)",
                                         min = 2,
                                         max = 10,
                                         value = 4,
                                         step = 1
                             )
                         )
                       )
                     ),
                     
                     div(class = "filter-group",
                         pickerInput(
                           inputId = "metric",
                           label = "Select Metric:",
                           choices = c(
                             "School Density" = "school_density",
                             "Elementary Density" = "elementary_density",
                             "Junior High Density" = "junior_high_density",
                             "Senior High Density" = "senior_high_density",
                             "Special Ed Density" = "special_ed_density",
                             "Public Ratio" = "public_ratio"
                           ),
                           options = list(
                             style = "btn-primary",
                             size = 6,
                             `live-search` = TRUE
                           )
                         )
                     ),
                     
                     div(class = "filter-group",
                         prettyRadioButtons(
                           inputId = "significance",
                           label = "Significance Level:",
                           choices = c(
                             "99% Confidence (α = 0.01)" = 0.01,
                             "95% Confidence (α = 0.05)" = 0.05,
                             "90% Confidence (α = 0.10)" = 0.10
                           ),
                           selected = 0.05,
                           status = "primary",
                           animation = "smooth",
                           icon = icon("check")
                         )
                     )
                 )
          ),
          column(9,
                 # LISA content
                 fluidRow(
                   box(width = 12,
                       plotOutput("analysis_map", height = "600px"),
                       title = "Spatial Analysis Map",
                       status = "primary",
                       solidHeader = TRUE,
                       footer = div(
                         class = "text-muted",
                         style = "font-style: italic; padding: 10px;",
                         "Spatial clusters and outliers based on selected metric"
                       )
                   )
                 ),
                 fluidRow(
                   box(width = 12,
                       verbatimTextOutput("cluster_stats"),
                       title = "Cluster Analysis Statistics",
                       status = "primary",
                       solidHeader = TRUE,
                       footer = div(
                         class = "text-muted",
                         style = "font-style: italic; padding: 10px;",
                         "Distribution of significant spatial patterns"
                       )
                   )
                 )
          )
        )
      ),
      
      # GWR Analysis
      tabPanel(
        "Geographically Weighted Regression",
        value = "gwr",
        fluidRow(
          column(3,
                 # GWR controls
                 div(class = "sidebar-section",
                     h4("GWR Analysis Settings"),
                     
                     div(class = "filter-group",
                         pickerInput(
                           inputId = "education_level",
                           label = "Education Level:",
                           choices = c(
                             "Elementary" = "elementary",
                             "Junior High" = "junior",
                             "Senior High" = "senior"
                           ),
                           options = list(
                             style = "btn-primary",
                             size = 3
                           )
                         )
                     ),
                     
                     div(class = "filter-group",
                         prettyRadioButtons(
                           inputId = "model_type",
                           label = "Model Type:",
                           choices = c(
                             "Fixed Bandwidth" = "fixed",
                             "Adaptive Bandwidth" = "adaptive"
                           ),
                           selected = "fixed",
                           status = "primary",
                           animation = "smooth",
                           icon = icon("check")
                         )
                     ),
                     
                     div(class = "filter-group",
                         pickerInput(
                           inputId = "coefficient",
                           label = "Display Coefficient:",
                           choices = c(
                             "Public School Count" = "count_public",
                             "Private School Count" = "count_private",
                             "Distance to Nearest Public School" = "nearest_public",
                             "Distance to Nearest Private School" = "nearest_private",
                             "School Density (KDE)" = "kde",
                             "Local R²" = "r2"
                           ),
                           options = list(
                             style = "btn-primary",
                             size = 6,
                             `live-search` = TRUE
                           )
                         )
                     ),
                     
                     downloadButton(
                       "download_data", 
                       "Download Results",
                       class = "btn-block",
                       style = "
    color: #333;
    background-color: #fff;
    border-color: #ccc;
    margin-top: 20px;
    font-weight: 500;
    opacity: 1;
  "
                     )
                 )
          ),
          column(9,
                 # GWR content
                 fluidRow(
                   box(width = 12,
                       leafletOutput("map", height = "600px"),
                       title = "GWR Analysis Map",
                       status = "primary",
                       solidHeader = TRUE,
                       footer = div(
                         class = "text-muted",
                         style = "font-style: italic; padding: 10px;",
                         "Geographical distribution of regression coefficients"
                       )
                   )
                 ),
                 fluidRow(
                   box(width = 6,
                       tableOutput("model_summary"),
                       title = "Model Statistics",
                       status = "primary",
                       solidHeader = TRUE,
                       footer = div(
                         class = "text-muted",
                         style = "font-style: italic; padding: 10px;",
                         "Comparison of fixed and adaptive bandwidth models"
                       )
                   ),
                   box(width = 6,
                       DTOutput("results_table"),
                       title = "Detailed Results",
                       status = "primary",
                       solidHeader = TRUE,
                       footer = div(
                         class = "text-muted",
                         style = "font-style: italic; padding: 10px;",
                         "Local coefficient estimates and statistics"
                       )
                   )
                 )
          )
        )
      )
    )
  ),
  
  # Keep existing styles
  tags$head(
    tags$style(HTML("
        .sidebar-section {
          padding: 15px;
          margin: 15px 0;
          background: #1e2835;  /* Darker background */
          border-radius: 4px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.3);
        }
        .sidebar-section h4 {
          color: #ffffff;
          font-size: 16px;
          margin-bottom: 15px;
          border-bottom: 1px solid #4f5962;
          padding-bottom: 10px;
        }
        .pretty {
          margin-right: 0;
        }
        .pretty .state label {
          color: #ffffff !important;
        }
        .slider-container {
          padding: 10px 0;
          background: #1e2835;
        }
        /* Slider customization */
        .irs--shiny .irs-bar {
          background: #3c8dbc;
          border-top: 1px solid #3c8dbc;
          border-bottom: 1px solid #3c8dbc;
        }
        .irs--shiny .irs-handle {
          border-color: #3c8dbc;
          background-color: #2c3e50;
        }
        .irs--shiny .irs-line {
          background: #34495e;
          border: 1px solid #2c3e50;
        }
        .irs--shiny .irs-grid-text {
          color: #ffffff;
        }
        .irs--shiny .irs-min, .irs--shiny .irs-max, .irs--shiny .irs-single {
          background: #34495e;
          color: #ffffff;
        }
        /* Control labels */
        .control-label {
          color: #ffffff;
          font-weight: 400;
          margin-bottom: 5px;
        }
        .radio-inline, .checkbox-inline {
          color: #ffffff;
          margin-right: 15px;
        }
        .filter-group {
          margin-bottom: 20px;
          padding-bottom: 10px;
          border-bottom: 1px solid rgba(255, 255, 255, 0.1);
          background: #1e2835;
        }
        .filter-group:last-child {
          border-bottom: none;
        }
        /* Dropdown customization */
        .selectize-input {
          background: #2c3e50 !important;
          border: 1px solid #34495e !important;
          color: #ffffff !important;
        }
        .selectize-dropdown {
          background: #2c3e50;
          border: 1px solid #34495e;
        }
        .selectize-dropdown-content {
          color: #ffffff;
        }
        .selectize-dropdown-content .active {
          background: #3c8dbc;
        }
        /* Checkbox customization */
        .pretty.p-default input:checked~.state label:after {
          background-color: #3c8dbc !important;
        }
        /* Additional background fixes */
        .control-sidebar-bg {
          background: #1e2835;
        }
        /* Make sure inputs have white text */
        input, select, textarea {
          color: #ffffff !important;
          background-color: #2c3e50 !important;
        }
        /* Fix for dropdown text */
        .selectize-input > input {
          color: #ffffff !important;
        }
        .intro-content h3 {
          margin-top: 15px;
          margin-bottom: 15px;
          font-weight: 500;
        }
        
        .intro-content h4 {
          margin-top: 10px;
          margin-bottom: 10px;
          font-weight: 500;
        }
        
        .intro-content p {
          font-size: 14px;
          line-height: 1.6;
          color: #333;
          margin-bottom: 15px;
        }
        
        .intro-content ul {
          margin-bottom: 20px;
          padding-left: 20px;
        }
        
        .intro-content li {
          font-size: 14px;
          line-height: 1.6;
          color: #333;
          margin-bottom: 5px;
        }
        
        .box.box-solid.box-primary {
          border: none;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        
        .box.box-solid.box-primary > .box-header {
          background: #3c8dbc;
          color: #fff;
        }
    ")))
)