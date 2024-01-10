
#Import libraries
library(tidyverse)

# Import dataset
#midus <- load("../data/04652-0001-Data.rda")
#midus <- da04652.0001
# For testing purposes

# Plot optimism score using variable from dataset
gen_fig1 <- function(df){
  # Plot scores, note that x axis label will also print number of rows after temoving NA optimism scores
  fig1 <- df %>% filter(!is.na(B1SORIEN)) %>% 
    mutate(opt_round = round(B1SORIEN),
           optimism_cat = case_when(
             opt_round < 23 ~ 'low',
             opt_round >= 23 & opt_round < 27~ 'med',
             opt_round >= 27 ~ 'high',
             .default = NA
           ))  %>% # categories based on description of tertiles from paper)
    ggplot(aes(opt_round, fill = optimism_cat)) + 
    geom_bar() + xlab(paste0("Optimism Score (n = ", nrow(df[!is.na(df$B1SORIEN),]), ")")) + ylab("Frequency") + 
      theme(legend.position="none")
  
  # Output plot
  fig1
}

# Test case
#gen_fig1(midus)

