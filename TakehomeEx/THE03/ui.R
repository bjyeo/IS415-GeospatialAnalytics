ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "line-height: 1.1; font-size: 15px; padding-top: 8px;",
      tags$span("Spatial Points Patterns Analysis", style = "display: block;"),
      tags$span("Jakarta Schools Distribution", style = "display: block; font-size: 14px;")
    ),
    titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Provincial Overview", tabName = "provincial", icon = icon("map")),
      menuItem("City Analysis", tabName = "city", icon = icon("building")),
      menuItem("Statistical Tests", tabName = "stats", icon = icon("chart-line"))
    ),
    
    tags$head(
      tags$style(HTML("
    .sidebar-section {
      padding: 10px;
      margin: 10px 0;
      background: rgba(255, 255, 255, 0.05);
      border-radius: 4px;
    }
    .sidebar-section h4 {
      color: #fff;
      font-size: 16px;
      margin-bottom: 10px;
      border-bottom: 1px solid #4f5962;
      padding-bottom: 8px;
    }
    .pretty {
      margin-right: 0;
    }
    .pretty .state label {
      color: #fff !important;
    }
    .slider-container {
      padding: 10px 0;
    }
    .irs--shiny .irs-bar {
      background: #3c8dbc;
    }
    .irs--shiny .irs-handle {
      border-color: #3c8dbc;
    }
    .control-label {
      color: #fff;
      font-weight: 400;
      margin-bottom: 5px;
    }
    .radio-inline {
      color: #fff;
      margin-right: 15px;
    }
    .checkbox-inline {
      color: #fff;
      margin-bottom: 5px;
    }
    .filter-group {
      margin-bottom: 20px;
      padding-bottom: 10px;
      border-bottom: 1px solid rgba(255, 255, 255, 0.1);
    }
    .filter-group:last-child {
      border-bottom: none;
    }
    .filter-label {
      color: #fff;
      font-size: 14px;
      margin-bottom: 10px;
      font-weight: 500;
    }
    .selectize-input {
      background: rgba(255, 255, 255, 0.1) !important;
      border: 1px solid rgba(255, 255, 255, 0.2) !important;
      color: #fff !important;
    }
    .selectize-dropdown {
      background: #2c3b41;
      border: 1px solid rgba(255, 255, 255, 0.2);
    }
    .selectize-dropdown-content {
      color: #fff;
    }
    .selectize-dropdown-content .active {
      background: #367fa9;
    }
    .pretty.p-default input:checked~.state label:after {
      background-color: #3c8dbc !important;
    }
    .control-sidebar-bg {
      background: rgba(255, 255, 255, 0.1);
    }
    
  "))
    ),
    
    # Update the provincial panel controls
    conditionalPanel(
      condition = "input.sidebarmenu === 'provincial'",
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
    
    # Data Filters for City Analysis
    conditionalPanel(
      condition = "input.sidebarmenu === 'city'",
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
    
    conditionalPanel(
      condition = "input.sidebarmenu === 'stats'",
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
          )
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "provincial",
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
      ),
      
      tabItem(tabName = "city",
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
      ),
      
      tabItem(tabName = "stats",
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
)