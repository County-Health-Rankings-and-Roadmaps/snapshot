read_year_data <- memoise(function(year) {
  base <- file.path("relational_data", year)
  
  files <- c(
    category = file.path(base, "t_category_data.csv"),
    factor   = file.path(base, "t_factor_data.csv"),
    focus    = file.path(base, "t_focus_area_data.csv"),
    measure  = file.path(base, "t_measure_data.csv")
  )
  
  # helper to build GitHub raw URL
  read_csv_github <- function(path) {
    url <- raw_url(path)
    readr::read_csv(url, show_col_types = FALSE, progress = FALSE)
  }
  
  cat_df <- read_csv_github(files["category"])
  fac_df <- read_csv_github(files["factor"])
  foc_df <- read_csv_github(files["focus"])
  mea_df <- read_csv_github(files["measure"])
  
  # Join them all — adjust keys as needed based on actual column names
  joined <- mea_df %>%
    left_join(fac_df,  by = "factor_id") %>%
    left_join(foc_df,  by = "focus_area_id") %>%
    left_join(cat_df,  by = "category_id")
  
  # Select useful columns
  joined %>%
    select(
      county_fips, county_name, state,
      category_name, focus_area_name, factor_name,
      measure_id, measure_name, value
    )
})
