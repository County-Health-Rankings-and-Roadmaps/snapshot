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
  library(bslib)
  library(digest)
  library(shiny.semantic)
  library(htmltools)
})

# ---- Repo Config ----
GITHUB_OWNER  <- Sys.getenv("CHD_GITHUB_OWNER",  unset = "County-Health-Rankings-and-Roadmaps")
GITHUB_REPO   <- Sys.getenv("CHD_GITHUB_REPO",   unset = "relational_data")
GITHUB_BRANCH <- Sys.getenv("CHD_GITHUB_BRANCH", unset = "main")
#DATA_DIR      <- Sys.getenv("CHD_DATA_DIR",      unset = "relational_data")
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

cat_names <- read_csv_github(file.path("t_category.csv"))
fac_names <- read_csv_github(file.path("t_factor.csv"))
foc_names <- read_csv_github(file.path("t_focus_area.csv"))
mea_years <- read_csv_github(file.path("t_measure_years.csv")) %>% select(year, measure_id, years_used)
mea_compare <- read_csv_github(file.path("t_measure.csv"))
# this has JRs comparable codes: compare_states and compare_years
# where -1 = unknown, 0 = no, 1 = yes, and 2= with caution 


mea_names = mea_years %>%
  full_join(mea_compare, by = c("measure_id", "year")) 


#define default years so something shows before api call 
available_years <- reactiveVal(c("2023", "2022"))

list_year_dirs <- memoise(function() {
  # Build GitHub API URL for the repo directory
  url <- paste0(
    "https://api.github.com/repos/", GITHUB_OWNER, "/", GITHUB_REPO,
    "/contents/"
  )
  
  # Fetch the contents
  resp <- httr::GET(url)
  if (httr::status_code(resp) >= 300) {
    warning("GitHub API call failed: ", httr::status_code(resp))
    return(character())
  }
  
  # Parse JSON response
  items <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
  
  if (!length(items)) return(character())
  
  # Extract years from filenames
  years <- items %>%
    as_tibble() %>%
    filter(type == "file") %>%
    pull(name) %>%
    str_extract("_([0-9]{4})\\.csv") %>%   # match "_20XX.csv"
    str_remove_all("[^0-9]") %>%          # remove non-numeric chars
    na.omit() %>%
    unique() %>%
    sort(decreasing = TRUE)
  
  return(years)
})





#available_years <- shiny::reactiveVal(NULL)



# ---- Load county list ----
get_county_list <- function() {
  url <- "https://github.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/raw/main/inputs/county_fips.sas7bdat"
  temp <- tempfile(fileext = ".sas7bdat")
  download.file(url, temp, mode = "wb")  # wb = write binary
  counties <- haven::read_sas(temp)
  
  return(counties)
}

# add the full state name in 
state_lookup <- data.frame(
  state = state.abb,
  state_name = state.name,
  stringsAsFactors = FALSE
)
#need DC too 
state_lookup <- rbind(
  state_lookup,
  data.frame(
    state = "DC",
    state_name = "Washington, D.C.",
    stringsAsFactors = FALSE
  )
)

county_choices <- get_county_list() %>%
  dplyr::left_join(state_lookup, by = "state")


state_choices <- county_choices %>%
  dplyr::distinct(state, state_name) %>%
  dplyr::arrange(state_name)



##################################################################################
# ---- UI ----
ui <- semanticPage(
  title = "County Snapshot",
  
  div(class = "ui container",
      
      
      h2(class = "ui header", "County Snapshot"),
      
      div(class = "ui divider"),
      
      # Grid layout: left panel (filters) + right panel (main content)
      div(class = "ui stackable grid",
          
          # --- Sidebar ---
          div(class = "four wide column",
              div(class = "ui raised segment",
                  h4(class = "ui header", ""),
                  
                  selectInput(
                    inputId = "state",
                    label = "Select State:",
                    choices = setNames(
                      state_choices$state, #this is what input$state is defined as 
                      state_choices$state_name), # this is waht the user selects  
                    selected = state_choices$state[1]
                  ),
                  
                  uiOutput("county_ui"),
                  uiOutput("year_ui"),
                  
                  tags$hr(),
                  helpText(
                    "Data loaded from:",
                    a(
                      sprintf("%s/%s@%s", GITHUB_OWNER, GITHUB_REPO, GITHUB_BRANCH),
                      href = sprintf("https://github.com/%s/%s/tree/%s",
                                     GITHUB_OWNER, GITHUB_REPO, GITHUB_BRANCH)
                      #, target = "_blank"
                    )
                  )
              )
          ),
          
          # --- Main Panel ---
          div(class = "twelve wide column",
              
              div(class = "ui segment",
                  uiOutput("note_latest"),
                  uiOutput("download_data_ui"),
                  tags$br(), tags$br(), 
                  helpText(HTML("
                    <b>Legend:</b> <br>
                    ✓ Comparable with prior years<br>
                    ✗ Not comparable with prior years<br>
                    ⚠ Use caution when comparing with prior years
                  ")),
                  
                  # no accordion, just separate gts for each cat  
                  uiOutput("category_tables_ui")
              )
      )
  )
)
)





#############################################################################
# ---- Server ----
server <- function(input, output, session) {
  
  # ---- Compute available years from mea_names ----
  #available_years <- reactive({
  #  req(mea_names)  # make sure the data is loaded
  #  mea_names %>%
  #    pull(year) %>%       # extract the year column
  #    unique() %>%         # keep unique values
  #    sort(decreasing = TRUE)  # newest first
  #})
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
    
    req(length(yrs) > 0)
    
    # Make sure they're sorted numerically (descending, newest first)
    yrs_num <- sort(as.numeric(yrs), decreasing = TRUE)
    # Remove the maximum year (will be represented by "Latest")
    yrs_no_latest <- yrs_num[yrs_num != max(yrs_num)]
    selectInput(
      "year",
      "Release year",
      choices = c("Latest" = "Latest", setNames(yrs_no_latest, yrs_no_latest)),
      selected = "Latest"
    )
  })
  
  # Dynamically update county dropdown
  output$county_ui <- renderUI({
    req(input$state)
    counties_in_state <- county_choices %>%
      filter(state == input$state)
    
    if (nrow(counties_in_state) == 0) {
      return(selectInput("county", "Select County:", choices = "Select a state first"))
    }
    
    selectInput("county", "Select County:",
                #choices = setNames(counties_in_state$fipscode, counties_in_state$county),
                choices = counties_in_state$county, 
                #selected = counties_in_state$fipscode[1])
                selected = counties_in_state$county[1])
  })
  
  
  resolved_year <- reactive({
    if (identical(input$year, "Latest")) {
      yrs <- available_years(); req(yrs)
      max(yrs)
    } else {
      input$year
    }
  })
  
  output$download_data_ui <- renderUI({
    req(
      nzchar(input$state),
      nzchar(input$county),
      nzchar(input$year)
    )
    
    label <- paste0(
      "Download data for ",
      input$county,
      ", ",
      input$state, 
      " from release year: ",
      resolved_year(),
      " as a CSV"
    )
    
    downloadButton(
      outputId = "download_data",
      label = label
    )
  })
  
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      req(
        nzchar(input$state),
        nzchar(input$county),
        nzchar(input$year)
      )
      
      paste0(
        input$state, "_",
        input$county, "_",
        resolved_year(),
        ".csv"
      )
    },
    content = function(file) {
      req(
        nzchar(input$state),
        nzchar(input$county),
        nzchar(input$year)
      )
      write.csv(measure_values, file, row.names = FALSE)
    }
  )
  
  
  
  year_data <- reactive({
    y <- resolved_year(); req(y)
    
    mea_df <- read_csv_github(file.path(paste0("t_measure_data_", y, ".csv"))) 
    
    
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
      filter(state == input$state & county == input$county) #note that the input county is the county name 
    
    #chosen = county_choices %>% filter(state == "MN" & county == "Olmsted County")
    
    # Defensive check
    req(nrow(chosen) > 0)
    
    state_fips <- chosen$statecode[1]
    county_fips <- chosen$countycode[1]
    req(state_fips, county_fips)
    
    df <- year_data()
    req(df)
    
    
    # filter measure data by fips
    df %>%
      filter(state_fips == !!state_fips, county_fips == !!county_fips)
    
    # for quick n dirty testing 
    #county_df = mea_df %>% filter(state_fips == !!state_fips, county_fips == !!county_fips)
  })
  
  state_df <- reactive({
    req(input$state, input$county, input$year)
    y <- resolved_year(); req(y) 
    
    # find the fips codes for the chosen state + county
    chosen <- county_choices %>%
      filter(state == input$state & county == input$county) #note that the input county is the county name
    # Defensive check
    req(nrow(chosen) > 0)  # ensures chosen is not empty
    
    #for quick n dirty testing 
    #chosen = county_choices %>% filter(state == "MN" & county == "Olmsted County")

    
    
    input_state_fips <- chosen$statecode[1]
    req(input_state_fips)
    county_fips <- "000"
    
    # construct path to state data CSV for the chosen year
    # state_file = sprintf("https://github.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/raw/main/relational_data/%s/t_state_data_%s.csv", 2023, 2023)
    
    state_file <- sprintf(
      "https://github.com/County-Health-Rankings-and-Roadmaps/relational_data/raw/main/t_state_data_%s.csv",
      y)
    
    df <- tryCatch(
      readr::read_csv(state_file, show_col_types = FALSE),
      error = function(e) { 
        warning("Failed to load state data: ", e$message)
        return(NULL)
      }
    )
    
    # filter for the chosen state
    #state_df = 
    df %>%
      dplyr::filter(state_fips == !!input_state_fips) %>%
      dplyr::select(measure_id, state_fips, raw_value, ci_low,ci_high) %>% 
      rename(stateval = raw_value, 
             state_ci_low = ci_low, 
             state_ci_high = ci_high)
  })
  
  ntl_df <- reactive({
    req(input$year)
    y <- resolved_year(); req(y) 
    
    # construct path to state data CSV for the chosen year
    # state_file = sprintf("https://github.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/raw/main/relational_data/%s/t_state_data_%s.csv", 2023, 2023)
    
    state_file <- sprintf(
      "https://github.com/County-Health-Rankings-and-Roadmaps/relational_data/raw/main/t_state_data_%s.csv",
      y)
    
    df <- tryCatch(
      readr::read_csv(state_file, show_col_types = FALSE),
      error = function(e) { 
        warning("Failed to load state data: ", e$message)
        return(NULL)
      }
    )
    req(df)  # only if downstream code needs df
    
    # filter ntl vals only 
    #ntl_df = 
      df %>%
      dplyr::filter(state_fips == "00") %>%
      dplyr::select(measure_id, state_fips, raw_value, ci_low,ci_high) %>% 
      rename(ntlval = raw_value, 
             ntl_ci_low = ci_low, 
             ntl_ci_high = ci_high)
  })
  
  
  
  # Reactive: raw measure values for the selected county/year
  measure_values_data <- reactive({
    req(input$county, input$year, input$state)
    y <- resolved_year(); req(y) 
    
    # Require data from upstream reactives to be ready
    req(county_df(), state_df(), ntl_df())
    
    # Build the measure mapping
    measure_map <- mea_names %>%
      filter(year == y) %>% 
      left_join(foc_names, by = c("measure_parent" = "focus_area_id", "year")) %>%
      left_join(fac_names, by = c("focus_area_parent" = "factor_id", "year")) %>%
      left_join(cat_names, by = c("factor_parent" = "category_id", "year")) %>%
      select(measure_id, measure_name, years_used, factor_name, category_name, display_precision, format_type, compare_states, compare_years, description)
    
    # Join county data and map
    measure_values <- county_df() %>%
      #measure_values = county_df %>% 
      #left_join(state_df, by = c("measure_id", "state_fips")) %>% 
      #left_join(ntl_df, by = c("measure_id", "state_fips")) %>% 
      left_join(state_df(), by = c("measure_id", "state_fips")) %>% 
      left_join(ntl_df(), by = c("measure_id", "state_fips")) %>% 
      left_join(measure_map, by = "measure_id") %>%
      mutate(
        measure_display = paste0(measure_name, " (", years_used, ")"),
        value_ci = case_when(
          # Case: missing CI
          is.na(ci_low) | is.na(ci_high) ~ as.character(
            case_when(
              format_type == 0 ~ scales::number(raw_value, accuracy = 1 / (10^display_precision)), # rate
              format_type == 1 ~ scales::percent(raw_value, accuracy = 1 / (10^display_precision)), # percentage
              format_type == 2 ~ scales::dollar(raw_value, accuracy = 1 / (10^display_precision)),  # dollars
              format_type == 3 ~ scales::number(raw_value, accuracy = 1 / (10^display_precision))   # ratio
            )
          ),
          # Case: with CI
          TRUE ~ case_when(
            format_type == 0 ~ paste0(
              scales::number(raw_value, accuracy = 1 / (10^display_precision)), " (",
              scales::number(ci_low, accuracy = 1 / (10^display_precision)), ", ",
              scales::number(ci_high, accuracy = 1 / (10^display_precision)), ")"
            ),
            format_type == 1 ~ paste0(
              scales::percent(raw_value, accuracy = 1 / (10^display_precision)), " (",
              scales::percent(ci_low, accuracy = 1 / (10^display_precision)), ", ",
              scales::percent(ci_high, accuracy = 1 / (10^display_precision)), ")"
            ),
            format_type == 2 ~ paste0(
              scales::dollar(raw_value, accuracy = 1 / (10^display_precision)), " (",
              scales::dollar(ci_low, accuracy = 1 / (10^display_precision)), ", ",
              scales::dollar(ci_high, accuracy = 1 / (10^display_precision)), ")"
            ),
            format_type == 3 ~ paste0(
              scales::number(raw_value, accuracy = 1 / (10^display_precision)), " (",
              scales::number(ci_low, accuracy = 1 / (10^display_precision)), ", ",
              scales::number(ci_high, accuracy = 1 / (10^display_precision)), ")"
            )
          )
        ),
        # State value + CI
        stateval_fmt = case_when(
          is.na(state_ci_low) | is.na(state_ci_high) ~ as.character(
            case_when(
              format_type == 0 ~ scales::number(stateval, accuracy = 1 / (10^display_precision), big.mark = ","),
              format_type == 1 ~ scales::percent(stateval, accuracy = 1 / (10^display_precision)),
              format_type == 2 ~ scales::dollar(stateval, accuracy = 1 / (10^display_precision)),
              format_type == 3 ~ scales::number(stateval, accuracy = 1 / (10^display_precision), big.mark = ",")
            )
          ),
          TRUE ~ case_when(
            format_type == 0 ~ paste0(
              scales::number(stateval, accuracy = 1 / (10^display_precision), big.mark = ","), " (",
              scales::number(state_ci_low, accuracy = 1 / (10^display_precision)), ", ",
              scales::number(state_ci_high, accuracy = 1 / (10^display_precision)), ")"
            ),
            format_type == 1 ~ paste0(
              scales::percent(stateval, accuracy = 1 / (10^display_precision)), " (",
              scales::percent(state_ci_low, accuracy = 1 / (10^display_precision)), ", ",
              scales::percent(state_ci_high, accuracy = 1 / (10^display_precision)), ")"
            ),
            format_type == 2 ~ paste0(
              scales::dollar(stateval, accuracy = 1 / (10^display_precision)), " (",
              scales::dollar(state_ci_low, accuracy = 1 / (10^display_precision)), ", ",
              scales::dollar(state_ci_high, accuracy = 1 / (10^display_precision)), ")"
            ),
            format_type == 3 ~ paste0(
              scales::number(stateval, accuracy = 1 / (10^display_precision), big.mark = ","), " (",
              scales::number(state_ci_low, accuracy = 1 / (10^display_precision)), ", ",
              scales::number(state_ci_high, accuracy = 1 / (10^display_precision)), ")"
            )
          )
        ),
        
        # State value + CI
        ntlval_fmt = case_when(
          is.na(ntl_ci_low) | is.na(ntl_ci_high) ~ as.character(
            case_when(
              format_type == 0 ~ scales::number(ntlval, accuracy = 1 / (10^display_precision), big.mark = ","),
              format_type == 1 ~ scales::percent(ntlval, accuracy = 1 / (10^display_precision)),
              format_type == 2 ~ scales::dollar(ntlval, accuracy = 1 / (10^display_precision)),
              format_type == 3 ~ scales::number(ntlval, accuracy = 1 / (10^display_precision), big.mark = ",")
            )
          ),
          TRUE ~ case_when(
            format_type == 0 ~ paste0(
              scales::number(ntlval, accuracy = 1 / (10^display_precision), big.mark = ","), " (",
              scales::number(ntl_ci_low, accuracy = 1 / (10^display_precision)), ", ",
              scales::number(ntl_ci_high, accuracy = 1 / (10^display_precision)), ")"
            ),
            format_type == 1 ~ paste0(
              scales::percent(ntlval, accuracy = 1 / (10^display_precision)), " (",
              scales::percent(ntl_ci_low, accuracy = 1 / (10^display_precision)), ", ",
              scales::percent(ntl_ci_high, accuracy = 1 / (10^display_precision)), ")"
            ),
            format_type == 2 ~ paste0(
              scales::dollar(ntlval, accuracy = 1 / (10^display_precision)), " (",
              scales::dollar(ntl_ci_low, accuracy = 1 / (10^display_precision)), ", ",
              scales::dollar(ntl_ci_high, accuracy = 1 / (10^display_precision)), ")"
            ),
            format_type == 3 ~ paste0(
              scales::number(ntlval, accuracy = 1 / (10^display_precision), big.mark = ","), " (",
              scales::number(ntl_ci_low, accuracy = 1 / (10^display_precision)), ", ",
              scales::number(ntl_ci_high, accuracy = 1 / (10^display_precision)), ")"
            )
          )
        )
      )
    measure_values
  })
  
  
  snapshot_table <- reactive({
    final_table <- measure_values_data() %>%
      #final_table = measure_values %>% 
      mutate(
        measure_display_fmt = paste0(
          measure_display, " ",
          case_when(
            compare_years == -1 ~ "?",
            compare_years == 0  ~ "✗",
            compare_years == 1  ~ "✓",
            compare_years == 2  ~ "⚠",
            TRUE ~ ""
          )
        ),
        state_comparison_note = case_when(
          compare_states == -1 ~ "Use caution if comparing these data across states",
          compare_states == 0  ~ "These data are incomparable across states",
          compare_states == 1  ~ "These data can be compared across states",
          compare_states == 2  ~ "Use caution if comparing these data across states",
          TRUE ~ ""
        )
      ) %>%
      select(
        category_name,
        factor_name,
        measure_display_fmt,
        description,
        state_comparison_note,
        value_ci,
        stateval_fmt,
        ntlval_fmt
      ) %>%
      arrange(category_name, factor_name)
    
    # Step 1: Create a modified category for Demographics
    final_table_mod <- final_table %>%
      mutate(
        category_name_mod = ifelse(factor_name == "Demographics",
                                   "Demographics Category",  # artificial category
                                   category_name)
      )
    
    # Step 2: Split by the modified category
    category_list <- split(final_table_mod, final_table_mod$category_name_mod)
    
    # Step 3: Create a list of gt tables
    gt_tables <- map(names(category_list), function(cat_name) {
      
      cat_df <- category_list[[cat_name]]
      
      # Make row group based on factor_name only
      cat_df <- cat_df %>%
        mutate(row_group = factor_name) %>%
        select(-category_name, -category_name_mod, -factor_name)  # remove original grouping cols
      # Build the gt table
      gt_tbl <- gt::gt(
        cat_df,
        rowname_col = "measure_display_fmt",
        groupname_col = "row_group"
      ) %>%
        # Make row group bold
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_row_groups()
        ) %>%
        # Add column labels
        gt::cols_label(
          value_ci = paste0(input$county, " (95% CI)"),
          stateval_fmt = paste0(input$state, " (95% CI)"),
          ntlval_fmt = "United States",
          description = "",
          state_comparison_note = ""
        ) %>%
        # Set table options
        gt::tab_options(
          row_group.as_column = FALSE,
          container.width = gt::pct(100),
          table.width = gt::pct(100),
          data_row.padding = gt::px(6),
          heading.align = "left"
        ) %>%
        # Add a caption/header
        gt::tab_header(
          title = paste0("Category: ", cat_name)
        )
      
      gt_tbl
    })
  })
    
    
    output$category_tables_ui <- renderUI({
      req(input$state, input$county, input$year)  # only show after selections
      
      tbls <- snapshot_table()
      
      # Create a gt_output for each table
      tbl_ui <- map(names(tbls), function(cat_name) {
        output_id <- paste0("gt_", gsub("\\s+", "_", cat_name))  # unique output ID
        
        # assign render_gt dynamically
        output[[output_id]] <- gt::render_gt({ tbls[[cat_name]] })
        
        # wrap in a div with some spacing
        div(
          class = "ui segment",
          gt_output(output_id),
          style = "margin-bottom: 20px;"  # spacing between tables
        )
      })
      
      # Wrap all tables in a vertical container
      do.call(tagList, tbl_ui)
    })
    
  

  #output$snapshot_semantic <- gt::render_gt({
  #  req(nrow(measure_values_data()) > 0)
  #  snapshot_table()
  #})
  
  
  
  # Download handler for CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(
        gsub(" ", "_", input$state), "_",
        gsub(" ", "_", input$county), "_",
        resolved_year(),
        ".csv"
      )
    },
    content = function(file) {
      write.csv(measure_values_data(), file, row.names = FALSE)
    }
  )
  }

shinyApp(ui = ui, server = server)