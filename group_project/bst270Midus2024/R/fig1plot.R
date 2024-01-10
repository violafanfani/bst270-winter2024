
#Import libraries
library(tidyverse)

# Import dataset
#midus <- load("../data/04652-0001-Data.rda")
#midus <- da04652.0001
# For testing purposes

# Generate optimism score
gen_fig1 <- function(df){
  # Note positive/reverse-scored questions are A, C, F
  df <- df %>% mutate(optA = case_when(
    B1SE10A == "(1) Agree a lot" ~ 5,
    B1SE10A == "(2) Agree a little" ~ 4,
    B1SE10A == "(3) Neither agree or disagree" ~ 3,
    B1SE10A == "(4) Disagree a little" ~ 2,
    B1SE10A == "(5) Disagree a lot" ~ 1,
    .default = NA
  ),
  optB = case_when(
    B1SE10B == "(1) Agree a lot" ~ 1,
    B1SE10B == "(2) Agree a little" ~ 2,
    B1SE10B == "(3) Neither agree or disagree" ~ 3,
    B1SE10B == "(4) Disagree a little" ~ 4,
    B1SE10B == "(5) Disagree a lot" ~ 5,
    .default = NA
  ),
  optC = case_when(
    B1SE10C == "(1) Agree a lot" ~ 5,
    B1SE10C == "(2) Agree a little" ~ 4,
    B1SE10C == "(3) Neither agree or disagree" ~ 3,
    B1SE10C == "(4) Disagree a little" ~ 2,
    B1SE10C == "(5) Disagree a lot" ~ 1,
    .default = NA
  ),
  optD = case_when(
    B1SE10D == "(1) Agree a lot" ~ 1,
    B1SE10D == "(2) Agree a little" ~ 2,
    B1SE10D == "(3) Neither agree or disagree" ~ 3,
    B1SE10D == "(4) Disagree a little" ~ 4,
    B1SE10D == "(5) Disagree a lot" ~ 5,
    .default = NA
  ),
  optE = case_when(
    B1SE10E == "(1) Agree a lot" ~ 1,
    B1SE10E == "(2) Agree a little" ~ 2,
    B1SE10E == "(3) Neither agree or disagree" ~ 3,
    B1SE10E == "(4) Disagree a little" ~ 4,
    B1SE10E == "(5) Disagree a lot" ~ 5,
    .default = NA
  ),
  optF = case_when(
    B1SE10F == "(1) Agree a lot" ~ 5,
    B1SE10F == "(2) Agree a little" ~ 4,
    B1SE10F == "(3) Neither agree or disagree" ~ 3,
    B1SE10F == "(4) Disagree a little" ~ 2,
    B1SE10F == "(5) Disagree a lot" ~ 1,
    .default = NA
  ),
  optimism = optA + optB + optC + optD + optE + optF, # Sum of the 6 components
  optimism_cat = case_when(
    optimism < 23 ~ 'low',
    optimism >= 23 & optimism < 27~ 'med',
    optimism >= 27 ~ 'high',
    .default = NA
  ) # Cateogries based on description of tertiles from paper
  )
  
  
  # Plot scores, note that x axis label will also print number of rows after temoving NA optimism scores
  fig1 <- df %>% filter(!is.na(optimism)) %>%
    ggplot(aes(as.factor(optimism), fill = optimism_cat)) + 
    geom_bar() + xlab(paste0("Optimism Score (n = ", nrow(df[!is.na(df$optimism),]), ")")) + ylab("Frequency") + theme(legend.position="none")
  
  # Output plot
  fig1
}

# Display test output
#gen_fig1(midus)
