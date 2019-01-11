library(tidyverse)

# Merging related databases -----------------------------------------------
# Back
back.acc  <- read.csv("data/counts_vert_vs_res/raw/Back_pri_acc_metrics_means.csv") %>%
  as.tibble()
back.card <- read.csv("data/counts_vert_vs_res/raw/Back_pri_raw.csv") %>%
  as.tibble()

back.df <- back.card %>%
  select(-c(BF, V.E, V.CO2, Evaluation, Date)) %>%
  left_join(back.acc, by = c("ID", "speed"))

# Hip
hip.acc  <- read.csv("data/counts_vert_vs_res/raw/Hip_pri_acc_metrics_means.csv") %>%
  as.tibble()
hip.card <- read.csv("data/counts_vert_vs_res/raw/Hip_pri_raw.csv") %>%
  as.tibble()

hip.df <- hip.card %>%
  select(-c(BF, V.E, V.CO2, Evaluation, Date)) %>%
  left_join(hip.acc, by = c("ID", "speed"))