build_snapshot <- function(data_list) {
  measure <- data_list$measure
  factor <- data_list$factor
  focus_area <- data_list$focus_area
  category <- data_list$category
  
  snapshot <- measure %>%
    dplyr::left_join(factor, by = "factor_id") %>%
    dplyr::left_join(focus_area, by = "focus_area_id") %>%
    dplyr::left_join(category, by = "category_id") %>%
    dplyr::select(
      fips, county_name, state_name,
      measure_id, measure_name, value, numerator, denominator,
      z_score, se, lower_ci, upper_ci,
      rank, 
      factor_name, focus_area_name, category_name
    )
  
  return(snapshot)
}
