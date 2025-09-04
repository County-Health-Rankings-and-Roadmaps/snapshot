# app.R — 
# ----------------------------------------------------
# - Load relational county-health data files from a GitHub repo (by year)
# - Let user pick a Year and County
# - Join category, factor, focus area, and measure tables
# - Show a snapshot table and a simple bar chart for selected county
# - Include a placeholder "Latest" option that currently points to the newest year in the repo

suppressPackageStartupMessages({
  library(shiny)
  library(httr)
  library(jsonlite)
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(ggplot2)
  library(memoise)
})

# ---- Repo Config (EDIT THESE) ----
GITHUB_OWNER  <- Sys.getenv("CHD_GITHUB_OWNER",  unset = "County-Health-Rankings-and-Roadmaps")
GITHUB_REPO   <- Sys.getenv("CHD_GITHUB_REPO",   unset = "chrr_measure_calcs")
GITHUB_BRANCH <- Sys.getenv("CHD_GITHUB_BRANCH", unset = "main")
DATA_DIR      <- Sys.getenv("CHD_DATA_DIR",      unset = "relational_data")
GITHUB_TOKEN  <- Sys.getenv("GITHUB_TOKEN", unset = NA)

# ---- Helpers: GitHub API and Raw URLs ----
raw_url <- function(path) {
  paste0(
    "https://raw.githubusercontent.com/",
    GITHUB_OWNER, "/", GITHUB_REPO, "/", GITHUB_BRANCH, "/", path
  )
}

api_headers <- function() {
  h <- c(Accept = "application/vnd.github+json",
         "User-Agent" = "county-health-dashboard-mvp")
  if (!is.na(GITHUB_TOKEN) && nzchar(GITHUB_TOKEN)) {
    h <- c(Authorization = paste("Bearer", GITHUB_TOKEN), h)
  }
  h
}


#define default years so something shows before api call 
available_years <- reactiveVal(c("2023", "2022"))

list_year_dirs <- memoise(function() {
  url <- paste0("https://api.github.com/repos/", GITHUB_OWNER, "/", GITHUB_REPO,
                "/contents/", utils::URLencode(DATA_DIR))
  resp <- httr::GET(url, httr::add_headers(.headers = api_headers()))
  if (httr::status_code(resp) >= 300) {
    warning("GitHub API call failed: ", httr::status_code(resp))
    return(character())
  }
  items <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
  if (!length(items)) return(character())
  years <- items %>%
    as_tibble() %>%
    filter(type == "dir") %>%
    pull(name) %>%
    keep(~ str_detect(.x, "^\\d{4}$")) %>%
    sort(decreasing = TRUE)
  years
})

# ---- Load Relational Data ----
read_year_data <- memoise(function(year) {
  base <- file.path(DATA_DIR, year)
  files <- c(
    category = file.path(base, "t_category_data.csv"),
    factor   = file.path(base, "t_factor_data.csv"),
    focus    = file.path(base, "t_focus_area_data.csv"),
    measure  = file.path(base, "t_measure_data.csv")
  )
  
  read_csv_github <- function(path) {
    url <- raw_url(path)
    readr::read_csv(url, show_col_types = FALSE, progress = FALSE)
  }
  
  cat_df <- read_csv_github(files["category"])
  fac_df <- read_csv_github(files["factor"])
  foc_df <- read_csv_github(files["focus"])
  mea_df <- read_csv_github(files["measure"])
  
  joined <- mea_df %>%
    left_join(fac_df,  by = "factor_id") %>%
    left_join(foc_df,  by = "focus_area_id") %>%
    left_join(cat_df,  by = "category_id")
  
  joined %>%
    select(
      county_fips, county_name, state,
      category_name, focus_area_name, factor_name,
      measure_id, measure_name, value
    )
})

available_years <- shiny::reactiveVal(NULL)



# ---- Load county list ----
get_county_list <- function() {
  url <- "https://github.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/raw/main/inputs/county_fips.sas7bdat"
  temp <- tempfile(fileext = ".sas7bdat")
  download.file(url, temp, mode = "wb")  # wb = write binary
  counties <- haven::read_sas(temp)
  
  return(counties)
}

county_choices <- get_county_list()
state_choices <- sort(unique(county_choices$state))



##################################################################################
# ---- UI ----
ui <- fluidPage(
  titlePanel("County Snapshot"),
  sidebarLayout(
    sidebarPanel(
      # State selection
      selectInput(
        inputId = "state",
        label = "Select State:",
        choices = state_choices,
        selected = state_choices[1]
      ),
      
      # County dropdown filtered by state
      uiOutput("county_ui"),
      
      uiOutput("year_ui"), 
      
      tags$hr(),
      helpText("Data loaded live from GitHub repo: ",
               code(sprintf("%s/%s@%s/%s", GITHUB_OWNER, GITHUB_REPO, GITHUB_BRANCH, DATA_DIR)))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Snapshot",
                 br(),
                 uiOutput("note_latest"),
                 tableOutput("summary_table"),
                 br(),
                 plotOutput("bar_chart")
        ),
        tabPanel("Data",
                 br(),
                 dataTableOutput("raw_table")
        )
      )
    )
  )
)





#############################################################################
# ---- Server ----
server <- function(input, output, session) {
  
  
  # provide default years immediately, update later with GitHub API
  available_years <- reactiveVal(c("2023", "2022"))
  
  observe({
    yrs <- tryCatch(list_year_dirs(), error = function(e) character())
    if (length(yrs) == 0) {
      yrs <- c("2023", "2022")
    }
    available_years(yrs)
  })
  
  output$year_ui <- renderUI({
    yrs <- available_years()
    selectInput(
      "year",
      "Release year",
      choices = c("Latest" = "Latest", setNames(yrs, yrs)),
      selected = "Latest"
    )
  })
  
  # Dynamically update county dropdown
  output$county_ui <- renderUI({
    req(input$state)
    counties_in_state <- county_choices %>%
      filter(state == input$state)
    
    selectInput("county", "Select County:",
                choices = setNames(counties_in_state$fipscode, counties_in_state$county),
                selected = counties_in_state$fipscode[1])
  })
  
  resolved_year <- reactive({
    if (identical(input$year, "Latest")) {
      yrs <- available_years(); req(yrs)
      max(yrs)
    } else {
      input$year
    }
  })
  
  year_data <- reactive({
    y <- resolved_year(); req(y)
    tryCatch(read_year_data(y), error = function(e) {
      validate(need(FALSE, paste("Failed to load data:", conditionMessage(e))))
      NULL
    })
  })
  

  output$note_latest <- renderUI({
    if (identical(input$year, "Latest")) {
      HTML(sprintf("<em>Showing data from most recent available year: <b>%s</b>.</em>", resolved_year()))
    }
  })
  
  county_df <- reactive({
    df <- year_data(); req(df, input$county)
    df %>% filter(county_name == input$county)
  })
  
  output$summary_table <- renderTable({
    cdf <- county_df(); req(nrow(cdf) > 0)
    cdf %>% arrange(category_name, focus_area_name, measure_name) %>%
      select(category_name, focus_area_name, measure_name, value) %>% head(15)
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")
  
  output$bar_chart <- renderPlot({
    cdf <- county_df(); req(nrow(cdf) > 0)
    top_measures <- cdf %>% arrange(desc(abs(suppressWarnings(as.numeric(value))))) %>% slice_head(n = 8)
    ggplot(top_measures, aes(x = reorder(measure_name, suppressWarnings(as.numeric(value))), y = suppressWarnings(as.numeric(value)))) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Value",
           title = paste0("Top measures for ", input$county, " (", resolved_year(), ")"))
  })
  
  output$raw_table <- renderDataTable({
    year_data()
  })
  
  snapshot_data <- reactive({
    year <- input$year
    data_list <- read_year_data(year)
    snapshot <- build_snapshot(data_list)
    snapshot
  })
  
  output$snapshot <- DT::renderDataTable({
    snapshot_data()
  })
  
}

shinyApp(ui, server)
