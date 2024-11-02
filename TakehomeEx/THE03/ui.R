ui <- dashboardPage(
  dashboardHeader(title = "Jakarta Schools Distribution"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Provincial Overview", tabName = "provincial"),
      menuItem("City Analysis", tabName = "city"),
      menuItem("Statistical Tests", tabName = "stats")
    ),
    
    # Data Filters for Provincial Overview
    conditionalPanel(
      condition = "input.sidebarmenu === 'provincial'",
      checkboxGroupInput("provincialSchoolLevels",
                         "School Levels:",
                         choices = c(
                           "Elementary", "Junior High",
                           "Senior High", "Special Education"
                         ),
                         selected = NULL
      ),
      radioButtons("provincialSchoolType",
                   "School Type:",
                   choices = c("All", "Public", "Private"),
                   selected = "All"
      ),
      sliderInput("provincialBandwidth",
                  "KDE Bandwidth:",
                  min = 0.1, max = 1,
                  value = 0.5, step = 0.1
      )
    ),
    
    # Data Filters for City Analysis
    conditionalPanel(
      condition = "input.sidebarmenu === 'city'",
      selectInput("citySelect",
                  "Select City:",
                  choices = c(
                    "Kota Jakarta Timur", 
                    "Kota Jakarta Barat",
                    "Kota Jakarta Selatan", 
                    "Kota Jakarta Utara",
                    "Kota Jakarta Pusat"
                  )
      ),
      checkboxGroupInput("citySchoolLevels",
                         "School Levels:",
                         choices = c(
                           "Elementary", "Junior High",
                           "Senior High", "Special Education"
                         ),
                         selected = NULL
      ),
      radioButtons("citySchoolType",
                   "School Type:",
                   choices = c("All", "Public", "Private"),
                   selected = "All"
      )
    ),
    
    # Data Filters for Statistical Tests
    conditionalPanel(
      condition = "input.sidebarmenu === 'stats'",
      selectInput("statsSelect",
                  "Select City:",
                  choices = c(
                    "All",
                    "Kota Jakarta Timur", 
                    "Kota Jakarta Barat",
                    "Kota Jakarta Selatan", 
                    "Kota Jakarta Utara",
                    "Kota Jakarta Pusat"
                  )
      ),
      checkboxGroupInput("statsSchoolLevels",
                         "School Levels:",
                         choices = c(
                           "Elementary", "Junior High",
                           "Senior High", "Special Education"
                         ),
                         selected = NULL
      ),
      radioButtons("statsSchoolType",
                   "School Type:",
                   choices = c("All", "Public", "Private"),
                   selected = "All"
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "provincial",
              fluidRow(
                box(plotlyOutput("schoolDistPlot"),
                    title = "School Distribution"),
                box(plotOutput("densityMap"),
                    title = "School Density")
              ),
              fluidRow(
                box(plotlyOutput("levelDistPlot"),
                    title = "Distribution by Level"),
                box(plotOutput("populationMap"),
                    title = "Population Density")
              )
      ),
      
      tabItem(tabName = "city",
              fluidRow(
                box(plotOutput("cityKDE"),
                    title = "Kernel Density Estimation"),
                box(plotOutput("typeRatio"),
                    title = "Public School Ratio")
              ),
              fluidRow(
                box(plotOutput("nnPlot"),
                    title = "Nearest Neighbor Analysis"),
                box(DT::dataTableOutput("cityStats"),
                    title = "City Statistics")
              )
      ),
      
      tabItem(tabName = "stats",
              fluidRow(
                box(plotOutput("quadratPlot"),
                    verbatimTextOutput("quadratStats"),
                    title = "Quadrat Analysis",
                    width = 6),
                box(plotOutput("crossK"),
                    title = "Cross K Function",
                    width = 6)
              ),
              fluidRow(
                box(plotOutput("kFunction"),
                    title = "K Function",
                    width = 6),
                box(plotOutput("lFunction"),
                    title = "L Function",
                    width = 6)
              )
      )
    )
  )
)