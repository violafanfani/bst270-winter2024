library("tidyverse")

#' Clean and filter confounders
#' 
#' @param df A dataframe.
#' @param group2_columns Confounder columns.
#' @examples
#' dat <- data.frame(sample_data)
#' filter_confounders(dat)

filter_confounders <- function(df, group2_columns) {
  filt_df <- df %>%
    drop_na(any_of(group2_columns)) %>%
    dplyr::filter(B1STINC1 != -1 & B1STINC1 != 9999998) %>%
    dplyr::filter((B4H26 != "Missing (8)") & (B4H26 != "Inapplicable (9)")) %>%
    # Converting education levels to match paper
    dplyr::mutate(B1PB1 = dplyr::case_when(
      grepl("(08)|(09)|(10)|(11)|(12)", B1PB1) ~ 4, # College or more
      grepl("(06)|(07)", B1PB1) ~ 3, # Some college
      grepl("(05)", B1PB1) ~ 2, # High school degree
      grepl("(01)|(02)|(03)|(04)", B1PB1) ~ 1 # Less than high school degree
    )) %>%
    dplyr::mutate(B1SCHROX = ifelse(grepl("No", B1SCHROX), 0, 1), # Changing chronic conditions
                  B4H26 = ifelse(grepl("No", B4H26), 0, 1), # Changing smoking
                  B4H33 = ifelse(grepl("No", B4H33), 0, 1), # Changing alcohol consumption
                  B4H25 = ifelse(grepl("No", B4H25), 0, 1)) # Changing regular exercise
  
  return(filt_df)
}

