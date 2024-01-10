#' Clean and filter optimism measurements.
#' 
#' @param dat A dataframe.
#' @examples
#' df_raw <- data.frame(sample_biomarker_data)
#' df_optimism <- filter_optimism(df_raw)
filter_optimism <- function(df) {
  # Specify optimism measurement columns
  optimism_meas_columns <- c('B1SORIEN')
  
  # Drop rows with NA values in the lipid measurement columns
  res <- df %>%
    drop_na(any_of(optimism_meas_columns))
  
  return(res)
}
