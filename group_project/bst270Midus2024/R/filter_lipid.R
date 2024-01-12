library(tidyverse)

#' Clean and filter lipid measurements.
#' 
#' @param df A dataframe.
#' @examples
#' df_raw <- data.frame(sample_biomarker_data)
#' df_lipid <- filter_lipid(df_raw)
filter_lipid <- function(df) {
  # Specify lipid measurement columns
  lipid_meas_columns <- c("B4BCHOL", "B4BTRIGL", "B4BHDL", "B4BLDL")
  
  # NOTE: Biomarker data has been partially preprocessed, INAPP (999) and MISSING (998) values are classified as NA
  # Drop rows with NA values in the lipid measurement columns
  res <- df %>%
    drop_na(any_of(lipid_meas_columns))
  
  return(res)
}