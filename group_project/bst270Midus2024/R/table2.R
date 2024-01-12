library(table1)
library(gt)

group2_columns <- c('B1PB1','B1STINC1','B4ZB1SLG','B1SCHROX','B4H26','B4H33','B4H25','B4PBMI','B1SNEGAF')

table1_data <- data_after_fc %>% 
  select_at(vars(intersect(c("M2ID", "M2FAMNUM", "B1SORIEN", group2_columns), names(.)))) %>% 
  mutate(Optimism = case_when(
    B1SORIEN <= 22 ~ "Low",
    B1SORIEN <= 26 ~ "Moderate",
    TRUE ~ "High"
  )) %>% 
  mutate_at(vars(Optimism), ~factor(., levels = c("Low", "Moderate", "High"))) %>% 
  mutate_at(vars(c(B1SCHROX, B4H26, B4H25)), ~factor(., levels = c(0,1))) %>% 
  mutate_at(vars(B1PB1), ~factor(., levels = c(1,2,3,4))) %>% 
  mutate_at(vars(c(B4H26)), ~factor(., labels = c(3,1)  %>% as.character %>% as.numeric))

labels <- list(
  variables=list(B1PB1="Education",
                 B1STINC1="Household_Income",
                 B1SCHROX="Chronic_Conditions",
                 B4H26="Smoking",
                 B4H33 = "Alcohol",
                 B4H25 = "Regular_Exercise",
                 B4PBMI = "BMI"),
  groups=list("Optimism"))

strata <- c(list(Total=table1_data), split(table1_data, table1_data$Optimism))

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

table1 <- table1(strata, labels, render.continuous = my.render.cont, render.categorical = my.render.cat)
#table1

vector_cors <- c()
for(i in group2_columns){
  vector_cors[i] <- cor(as.numeric(table1_data[,i]), as.numeric(table1_data$B1SORIEN))
}

vector_cors 
labels_table2 <- list(
  variables=list(B1PB1="Education",
                 B1STINC1="Household_Income",
                 B1SCHROX="Chronic_Conditions",
                 B4H26="Smoking",
                 B4H33 = "Alcohol",
                 B4H25 = "Regular_Exercise",
                 B4PBMI = "BMI"))

table2 <- data.frame("Characteristic" = c("Education", "Household Income","LAG", "Chronic Conditions",
                                          "Smoking", "Alcohol", "Regular Exercise", "BMI", "Negative affect"),
                     "r" = vector_cors)
gt(table2) %>% 
  tab_header(title = "Correlation Coefficients for optimism and confounders") 
  

