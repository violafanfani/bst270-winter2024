library(tidyverse)
library(janitor)
library(knitr)


#' Display lipid measurement summary statistics by optimism level.
#' 
#' This function assumes pre-cleaned and filtered lipid measurements and optimism scores.
#' 
#' @param df A dataframe with combined survey and biomarker data.
#'
#' @examples
#' df_raw <- data.frame(sample_survey_and_biomarker_data)
#' df_sum_stats_lipid <- gen_table1_lipid(df_raw)
gen_table1_lipid <- function(df) {
  lipid_by_opt <- df %>%
    mutate(
      opt_round = round(B1SORIEN),
      optimism_cat = case_when(
        opt_round < 23 ~ 'Low',
        opt_round >= 23 & opt_round < 27 ~ 'Moderate',
        opt_round >= 27 ~ 'High',
        .default = NA
        )) %>%
    group_by(optimism_cat) %>%
    summarize(
      mean_chol = mean(B4BCHOL),
      sd_chol = sd(B4BCHOL),
      mean_hdl = mean(B4BHDL),
      sd_hdl = sd(B4BHDL),
      mean_ldl = mean(B4BLDL),
      sd_ldl = sd(B4BLDL),
      mean_trigl = mean(B4BTRIGL),
      sd_trigl = sd(B4BTRIGL)) %>%
    ungroup() %>%
    group_by(optimism_cat) %>%
    summarize(
      total_chol = paste0(round(mean_chol, digits=2), '+-', round(sd_chol, digits=2)),
      hdl_chol = paste0(round(mean_hdl, digits=2), '+-', round(sd_hdl, digits=2)),
      ldl_chol = paste0(round(mean_ldl, digits=2), '+-', round(sd_ldl, digits=2)),
      trigl = paste0(round(mean_trigl, digits=2), '+-', round(sd_trigl, digits=2))
    )
  
  sum_stats_lipid <- data.frame(t(lipid_by_opt[c(2, 3, 1),])) %>%
    row_to_names(row_number = 1)
  
  rownames(sum_stats_lipid) <- c("Total cholesterol", "HDL cholesterol", "LDL cholesterol", "Triglycerides")
  
  return(kable(sum_stats_lipid,
               caption = "Distribution of participant lipid measurements according to optimism level"))
}