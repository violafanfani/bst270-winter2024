# Cleaning pathway variables

library(tidyverse)
library(dplyr)

#' Get the alcohol consumption
#' 
#' @param dat A dataframe
#' @returns data
get_bmi <- function(dat){
  return(dat)
}

#' Get the alcohol consumption
#' 
#' @param dat A dataframe
#' @returns data including the alcohol consumption as average number of drinks per week
get_alchol_consumption <- function(dat) {
  # Alcohol Consumption
  # B4H34: Past month, how often did you drink any alcoholic beverages, on the average?
  # B4H36: Past month, on the days when you drank, about how many drinks did you drink on average?
  # Average alcohol consumption is based on the report of the number of drinks that a person drank on average, when they drank.
  # it is not clear if the paper considered how many drinks per day that a person drank, however.  For this consideration, the average 
  # days for the given category are used as the number of days per week that a person drank.
  dat |> mutate(alcohol_days=case_when(B4H34 == '(1) Everyday' ~ 7,
                                       B4H34 == '(2) 5 Or 6 days/week' ~ 5.5,
                                       B4H34 == '(3) 3 Or 4 days/week' ~ 3.5,
                                       B4H34 == '(4) 1 Or 2 days/week' ~ 1.5,
                                       B4H34 == '(5) Less than one day/week' ~ 0.5,
                                       B4H34 == '(6) Never drinks' ~ 0,
                                       .default = 0),
                alcohol_consumption=if_else(B4H33 == '(2) No', 0, B4H36),
                alcohol_consumption_adjusted=if_else(alcohol_days==0, 0, B4H36*alcohol_days/7)) 
}

### These functions clean the following variables from ICPSR_29282/DS0001/29282-0001-Data.rda:
# data$B4H26 indicates "ever" smoker
# data$B4H26A indicates "current" smoker
# data$B4H25 indicates regular exercise

### Function to generate smoking_status
generate_smoking_status <- function(data) {
  
  data$smoking_status <- ifelse(data$B4H26 == "(2) No", "never smoker", 
                                ifelse(data$B4H26A == "(1) Yes", "current smoker", 
                                       ifelse(data$B4H26A == "(2) No" & data$B4H26 == "(1) Yes", "past smoker", NA)))
  
  # Optional: Generate never_smoker and current_smoker (dummy variables for smoking_status)
  # data$never_smoker <- ifelse(data$smoking_status == "never smoker", 1, 0)
  # data$current_smoker <- ifelse(data$smoking_status == "current smoker", 1, 0)
  
  return(data)
}

### Function to generate reg_exercise
generate_reg_exercise <- function(data) {
  
  data$reg_exercise <- ifelse(data$B4H25 == "(1) Yes", 1,
                              ifelse(data$B4H25 == "(2) No", 0, NA))
  
  return(data)
}


# Calculate diet score
calculate_score_sum <- function(data) {
  # Create a new column 'score_sum' with initial value 0 for each patient
  data$score_sum <- 0
  
  # Define scoring logic for each category in a named list
  scoring_logic <- list(
    B4H21 = c("(3) 3-4 Servings/day", "(4) 5 Or more servings/day"),
    B4H22 = c("(3) 3-4 Servings/day", "(4) 5 Or more servings/day"),
    B4H23A = c("(3) 1-2 X/week", "(4) 3-4 X/week", "(5) 5 Or more x/week"),
    B4H23C = c("(3) 1-2 X/week", "(4) 3-4 X/week", "(5) 5 Or more x/week"),
    B4H20 = c("(1) None"),
    B4H23B = c("(1) Never", "(2) Less than once/week", "(3) 1-2 X/week"),
    B4H24 = c("(1) Never", "(2) Less than once/week")
  )
  
  # Loop through columns and apply scoring logic
  for (column in names(scoring_logic)) {
    data$score_sum <- ifelse(data[, column] %in% scoring_logic[[column]], data$score_sum + 1, data$score_sum)
  }
  
  return(data)
}

#####################
# Example usage

filter_pathway<-function(data){
  
  data<-get_bmi(data)
  data<-get_alchol_consumption(data)
  data<-generate_smoking_status(data)
  data<-generate_reg_exercise(data)
  
  # We remove NA from the 7 diet variables
  columns_to_clean <- c("B4H21", "B4H22", "B4H23A", "B4H23C", "B4H20", "B4H23B", "B4H24")
  data <- data[complete.cases(data[columns_to_clean]),]
  data<-calculate_score_sum(data)
  
  # Then run complete case on B4PBMI, alcohol_consumption, smoking_status, reg_exercise,score_sum
  complete_data<-data%>%drop_na(B4PBMI, alcohol_consumption, smoking_status, reg_exercise,score_sum)
    
  return(complete_data)  
   
}




