library(tidyverse)

#' Clean and filter lipid measurements
#' 
#' @param dat A dataframe.
#' @examples
#' dat <- data.frame(sample_data)
#' filter_lipid(dat)
filter_lipid <- function(dat) {
  # Specify lipid measurement columns
  lipid_meas_columns <- c("B4BCHOL", "B4BTRIGL", "B4BHDL", "B4BLDL")
  
  # NOTE: Biomarker data has been partially preprocessed, INAPP (999) and MISSING (998) values are classified as NA
  # Drop rows with NA values in the lipid measurement columns
  res <- dat %>%
    drop_na(any_of(lipid_meas_columns))
  
  return(res)
}