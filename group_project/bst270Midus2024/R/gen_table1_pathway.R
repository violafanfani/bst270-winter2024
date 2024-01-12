library(tableone)

gen_table1_pathway <- function(data){
  data <- data |> mutate(opt_round = round(B1SORIEN),
                         Optimism = case_when(
                           opt_round < 23 ~ 'Low',
                           opt_round >= 23 & opt_round < 27 ~ 'Moderate',
                           opt_round >= 27 ~ 'High',
                           .default = NA ),
                         Optimism = factor(Optimism, levels=c("Low", "Moderate", "High")),
                         BMI = B4PBMI,
                         "Alcohol Consumption" = alcohol_consumption,
                         "Smoking status" = smoking_status, 
                         "Regular Exercise" = if_else(reg_exercise==1, "Yes", "No"),
                         "Prudent Diet" = score_sum)
  
  table_one_pathway = CreateTableOne(vars = c("BMI", "Alcohol Consumption", "Smoking status", "Regular Exercise", "Prudent Diet"), data=data, strata="Optimism", test=FALSE)
  #kableone(table_one_pathway)
  return(table_one_pathway)  
}
