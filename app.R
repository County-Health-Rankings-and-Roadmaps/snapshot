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
                  h4(class = "ui header", "Filters"),
                  
                  selectInput(
                    inputId = "state",
                    label = "Select State:",
                    choices = state_choices,
                    selected = state_choices[1]
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
                  downloadButton("download_data", "Download these data as a csv"),
                  tags$br(), tags$br(), 
                  helpText(HTML("
                    <b>Legend:</b> <br>
                    ✓ Comparable with prior years<br>
                    ✗ Not comparable with prior years<br>
                    ⚠ Use caution when comparing with prior years
                  ")),
                  
                  # Replace accordion with semantic layout (custom module, for example)
                  uiOutput("snapshot_semantic")
              )
          )
      )
  )
)





#############################################################################
# ---- Server ----
server <- function(input, output, session) {
  
  # ---- Compute available years from mea_names ----
  available_years <- reactive({
    req(mea_names)  # make sure the data is loaded
    mea_names %>%
      pull(year) %>%       # extract the year column
      unique() %>%         # keep unique values
      sort(decreasing = TRUE)  # newest first
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
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(
        input$state, "_",
        input$county, "_",
        input$year,
        ".csv"
      )
    },
    content = function(file) {
      write.csv(measure_values, file, row.names = FALSE)
    }
  )
  
  
  
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

    
    
    state_fips <- chosen$statecode[1]
    req(state_fips)
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
    df %>%
      dplyr::filter(state_fips == !!state_fips) %>%
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
    
    # filter for the chosen state
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
  
  
  
  
  # Reactive: formatted gt table for display
  snapshot_data <- reactive({
    #quick n dirty 
    #final_table = measure_values %>% left_join(state_df, by = c("measure_id", "state_fips")) %>% 
    #  select(category_name, factor_name, measure_display, value_ci, stateval) %>%
    #  arrange(category_name, factor_name, measure_display)
    
    final_table <- measure_values_data() %>%
      # Add state comparison symbols next to the measure name
      mutate(
        measure_display_fmt = paste0(
          measure_display,
          " ",
          case_when(
            compare_years == -1 ~ "?",
            compare_years == 0  ~ "✗",
            compare_years == 1  ~ "✓",
            compare_years == 2  ~ "⚠",
            TRUE ~ ""
          )
        ),
        # Add text guidance for year comparison
        state_comparison_note = case_when(
          compare_states == -1 ~ "Use caution if comparing these data across states",
          compare_states == 0  ~ "These data are incomparable across states",
          compare_states == 1  ~ "These data can be compared across states",
          compare_states == 2  ~ "Use caution if comparing these data across states",
          TRUE ~ ""
        )
      ) %>%
      select(description, state_comparison_note,category_name, factor_name, value_ci, stateval_fmt, ntlval_fmt, 
             measure_display_fmt) %>%
      arrange(category_name, factor_name)
    
    final_table %>%
      gt::gt(rowname_col = "measure_display_fmt") %>%
      gt::tab_spanner(label = "Category", columns = "category_name") %>%
      gt::tab_spanner(label = "Factor", columns = "factor_name") %>%
      #gt::tab_spanner(label = "Comparison", columns = c("compare_states_fmt", "compare_years_fmt")) %>%
      gt::cols_label(
        category_name = "Category",
        factor_name = "Factor",
        value_ci = paste0(input$county, " (95% CI)"),
        stateval_fmt = paste0(input$state, " (95% CI)"),
        ntlval_fmt = "United States",
        state_comparison_note = "",
        description = ""
      )
  })
  
  output$snapshot_semantic <- renderUI({
    df <- measure_values_data() %>%
      mutate(
        measure_display_fmt = paste0(
          measure_display,
          " ",
          case_when(
            compare_years == -1 ~ "?",
            compare_years == 0  ~ "✗",
            compare_years == 1  ~ "✓",
            compare_years == 2  ~ "⚠",
            TRUE ~ ""
          )
        )
      )
    
    req(nrow(df) > 0)
    
    category_list <- split(df, df$category_name)
    
    # --- Build category blocks ---
    category_blocks <- lapply(names(category_list), function(cat) {
      cat_df <- category_list[[cat]]
      factor_list <- split(cat_df, cat_df$factor_name)
      
      # --- Build factor blocks ---
      factor_blocks <- lapply(names(factor_list), function(fac) {
        fac_df <- factor_list[[fac]]
        
        output_id_ui <- paste0("gt_", digest::digest(fac))
        output_id_gt <- paste0("gt_inner_", digest::digest(fac))
        
        # Create outputs
        output[[output_id_ui]] <- renderUI({ gt::gt_output(outputId = output_id_gt) })
        output[[output_id_gt]] <- gt::render_gt({
          fac_df %>%
            select(
              measure_display = measure_display_fmt,
              description,
              value_ci,
              stateval_fmt,
              ntlval_fmt
            ) %>%
            arrange(measure_display) %>%
            gt::gt() %>%
            gt::cols_label(
              measure_display = "Measure",
              description = "Description",
              value_ci = paste0(input$county, " (95% CI)"),
              stateval_fmt = paste0(input$state, " (95% CI)"),
              ntlval_fmt = "United States"
            )
        })
        
        # Factor block: show content by default
        div(
          class = "ui segment factor-segment",
          h4(class = "ui header", fac),
          uiOutput(output_id_ui)
        )
      })
      
      div(
        class = "ui raised segment",
        h3(class = "ui blue header", cat),
        factor_blocks
      )
    })
    
    div(class = "ui segments", category_blocks)
    
    
    # --- Wrap everything in top-level accordion ---
    top_accordion <- div(class = "ui styled fluid accordion", category_blocks)
    
    
    
    
    # Initialize Semantic UI accordions via JS
    session$sendCustomMessage("initAccordion", list())
    top_accordion
  })
  
  observe({
    df <- measure_values_data()
    req(nrow(df) > 0)
    
    # Iterate through Factors and create render_gt outputs
    df_split <- split(df, df$factor_name)
    
    for (fac in names(df_split)) {
      local({
        f <- fac
        fac_df <- df_split[[f]] %>%
          mutate(
            measure_display_fmt = paste0(
              measure_display,
              " ",
              case_when(
                compare_years == -1 ~ "?",
                compare_years == 0  ~ "✗",
                compare_years == 1  ~ "✓",
                compare_years == 2  ~ "⚠",
                TRUE ~ ""
              )
            )
          )
        output_id <- paste0("gt_", digest::digest(f))
        
        output[[output_id]] <- gt::render_gt({
          fac_df %>%
            select(
              measure_display = measure_display_fmt,
              description,
              value_ci,
              stateval_fmt,
              ntlval_fmt
            ) %>%
            arrange(measure_display) %>%
            gt::gt() %>%
            gt::cols_label(
              measure_display = "Measure",
              description = "Description",
              value_ci = paste0(input$county, " (95% CI)"),
              stateval_fmt = paste0(input$state, " (95% CI)"),
              ntlval_fmt = "United States"
            ) 
        })
      })
    }
  })
  
  
  
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

shinyApp(ui, server)