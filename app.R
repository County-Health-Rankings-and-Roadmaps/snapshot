# app.R — 
# ----------------------------------------------------
# - Load relational county-health data files from a GitHub repo (by year)
# - Let user pick a Year and County
# - Join category, factor, focus area, and measure tables
# - Show a snapshot table 
# - Provide users with the option to download the data as a csv 
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
  library(gt) 
})

# ---- Repo Config ----
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

# helper to build GitHub raw URL
read_csv_github <- function(path) {
  url <- raw_url(path)
  readr::read_csv(url, show_col_types = FALSE, progress = FALSE)
}



# load the names datasets that are not year, county, or measure specific (ie these are always loaded) 

cat_names <- read_csv_github(file.path("relational_data/t_category.csv"))
fac_names <- read_csv_github(file.path("relational_data/t_factor.csv"))
foc_names <- read_csv_github(file.path("relational_data/t_focus_area.csv"))
mea_names <- read_csv_github(file.path("relational_data/t_measure_years.csv"))

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
                 gt::gt_output("snapshot")
                 
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
    
    mea_df <- read_csv_github(file.path(paste0("relational_data/", y, "/t_measure_data_", y, ".csv"))) 
    
   
    mea_df %>%
        select(
        county_fips, state_fips, measure_id, 
        raw_value, ci_low, ci_high
      )
  })
  

  output$note_latest <- renderUI({
    if (identical(input$year, "Latest")) {
      HTML(sprintf("<em>Showing data from most recent release year: <b>%s</b>.</em>", resolved_year()))
    }
  })
  
  county_df <- reactive({
    req(input$state, input$county)
    
    # find the fips codes for the chosen state + county
    chosen <- county_choices %>%
      filter(state == input$state & fipscode == input$county) #note that the input county is the fipscode 
    
    #validate(
    #  need(nrow(chosen) == 1, 
    #       if (nrow(chosen) == 0) "No county found for that selection." 
    #       else "Multiple counties found with same FIPS code.")
    #)
    
    state_fips <- chosen$statecode
    county_fips <- chosen$countycode
    
    df <- year_data(); req(df)
    
    # filter measure data by fips
    df %>%
      filter(state_fips == !!state_fips, county_fips == !!county_fips)
  })
  
 snapshot_data <- reactive({
    req(input$county, input$year)
    year <- input$year
    base <- file.path("relational_data", year)
    
    
    # Build the full mapping chain -----------------------------------
    measure_map <- mea_names %>%
      # connect measure_id -> focus area
      left_join(foc_names, by = c("measure_parent" = "focus_area_id", "year")) %>%
      # connect focus area -> factor
      left_join(fac_names, by = c("focus_area_parent" = "factor_id", "year")) %>%
      # connect factor -> category
      left_join(cat_names, by = c("factor_parent" = "category_id", "year")) %>%
      select(
        measure_id,
        measure_name,
        years_used,
        factor_name,
        category_name
      )
    
    
    
    
    # Attach measure values ------------------------------------------
    measure_values <- county_df() %>%
      left_join(measure_map, by = "measure_id") %>%
      mutate(
        measure_display = paste0(measure_name, " (", years_used, ")"),
        value_ci = paste0(raw_value, " [", ci_low, ", ", ci_high, "]")
      )
    
    # Build a clean table --------------------------------------------
    final_table <- measure_values %>%
      select(category_name, factor_name, measure_display, value_ci) %>%
      arrange(category_name, factor_name, measure_display)
    
    # Pretty table with headers --------------------------------------
    final_table %>%
      gt::gt(rowname_col = "measure_display") %>%
      gt::tab_spanner(
        label = "Category",
        columns = "category_name"
      ) %>%
      gt::tab_spanner(
        label = "Factor",
        columns = "factor_name"
      ) %>%
      gt::cols_label(
        category_name = "Category",
        factor_name = "Factor",
        value_ci = "Value (95% CI)"
      )
    
    
  })
  
  output$snapshot <- gt::render_gt({
    snapshot_data()
  })
  
}

shinyApp(ui, server)
