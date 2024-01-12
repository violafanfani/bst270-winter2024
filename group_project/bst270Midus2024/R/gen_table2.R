

#' Display Table2: Associations between optimism and selected confounders
#' 
#' This function assumes pre-cleaned and filtered confounder and optimism scores.
#' 
#' @param df A dataframe with surveydata.
#' @export
#' @examples
#' df_raw <- data.frame(sample_surveydata)
#' gen_table2(df_raw)


gen_table2 <- function(df){
  table2_variables <- c("Optimism", "Age", "Gender", "Race", "Education", "Income", 
                        "Interval between assessments",
                         "Chronic conditions", 
                        "Blood pressure medication", "Body mass index", 
                        "Smoking status", "Alcohol consumption", "Prudent diet", 
                        "Regular exercise", "Negative affect")
  
  # Instantiate dataframe to hold table2 data
  # data_for_table2 <- data.frame(
  #   matrix(NA, nrow = nrow(data_after_fp), ncol = length(table2_variables))
  #   )
  # colnames(data_for_table2) <- table2_variables
  
  # Fill data with format specified in table2
  data_for_table2 <- df %>% mutate(
    "Optimism" = B1SORIEN,
    "Age" = B1PAGE_M2,
    "Gender" = case_when(
      B1PRSEX == "(1) Male"  ~ 0,
      B1PRSEX == "(2) Female" ~ 1
    ),
    "Race" = case_when(
      B1PF7A == "(1) White" ~ 0,
      .default = 1
    ),
    "Education" = B1PB1, #Reformatted in filter_confounders
    "Income" = B1STINC1/1000,
    "Interval between assessments" = case_when(
      B1PA25IN == "(1) Months" ~ 0,
      B1PA25IN == "(2) Years" ~ 1
    ),
    "Chronic conditions" = B1SCHROX, #Reformatted in filter_confounders
    "Blood pressure medication" = as.numeric(B1PA24C == '(1) Yes'),
    "Body mass index" = B4PBMI,
    "Smoking status" = case_when(
      smoking_status == "current smoker" ~ 0,
      smoking_status == "past smoker" ~ 1,
      smoking_status == "never smoker" ~ 2,
    ),#Reformatted in filter_pathways
    "Alcohol consumption" = B4H33, #Reformatted in filter_confounders
    "Prudent diet" = score_sum,
    "Regular exercise" = B4H25, #Reformatted in filter_confounders
    "Negative affect" = B1SNEGAF
  ) %>%
    select(all_of(table2_variables))
  
  table2_data <- data.frame(
    Characteristic = colnames(data_for_table2)[-1],
    r = NA_real_,
    p = NA_real_
  )
  
  #Loop through each variable and get pearson R and p from SLR
  for(i in 1:nrow(table2_data)){
    var_i <- table2_data[i, "Characteristic"]
    cor_out <- cor.test(data_for_table2[["Optimism"]], data_for_table2[[var_i]],
                        use="pairwise.complete.obs")
    
    table2_data[i, "p"] <- as.numeric(cor_out[["p.value"]])
    table2_data[i, "r"] <- cor_out[["estimate"]]
  }
  
  #Format p and r columns to match table2
  #P values should be formatted with special indications for p < 0.001 and p < 0.0001.
  #Otherwise they will be rounded to 2 decimals
  table2_data[["p"]] <- table2_data[["p"]] %>% as.numeric
  table2_data[["p"]] <- case_when(
    table2_data[["p"]] < 0.0001 ~ "<0.0001",
    table2_data[["p"]] > 0.0001 & table2_data[["p"]] < 0.001 ~ "<0.001",
    table2_data[["p"]] >= 0.001 ~ round(table2_data[["p"]], 2) %>% as.character
  )
  
  #Correlation is simply rounded to 2 decimals
  table2_data["r"] <- round(table2_data["r"], 2)
  
  # 3. Create the DT object and print out
  datatable(table2_data)
}  
  
