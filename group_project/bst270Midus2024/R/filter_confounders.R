library(tidyverse)

load("~/Library/Mobile Documents/com~apple~CloudDocs/Harvard/Harvard Winter 2024/BST270/bst270-winter2024/group_project/bst270Midus2024/data/29282-0001-Data.rda")
load("~/Library/Mobile Documents/com~apple~CloudDocs/Harvard/Harvard Winter 2024/BST270/bst270-winter2024/group_project/bst270Midus2024/data/04652-0001-Data.rda")

data = inner_join(da04652.0001, da29282.0001, by = c("M2ID", "M2FAMNUM"),suffix = c('','.2'))

columns <- c('B1PB1','B1STINC1','B4ZB1SLG','B1SCHROX','B4H26','B4H33','B4H25','B4PBMI','B1SNEGAF')

#filter_confounders <- function(data){
#  res <- data %>% 
#    select(columns) %>% 
#    filter(!is.na()) 
#}

test <- data %>% drop_na(any_of(columns))

table(test$B1PB1)

  