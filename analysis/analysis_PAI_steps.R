library(tidyverse)

# Merging related databases -----------------------------------------------
# Hip
hip.steps  <- read.csv("data/PAI_steps/raw/Hip_pri_steps_means.csv") %>%
  as.tibble()
hip.card <- read.csv("data/PAI_steps/raw/Hip_pri_raw.csv") %>%
  as.tibble()

hip.df <- hip.card %>%
  select(-c(BF, V.E, V.CO2, Evaluation, Date)) %>%
  left_join(hip.steps, by = c("ID", "speed"))