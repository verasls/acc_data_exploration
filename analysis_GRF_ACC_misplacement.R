library(tidyverse)
library(nlme)
library(plyr)
library(ggplot2)
source("R/cvLR.R")
source("R/accuracyIndices.R")

# File preparation --------------------------------------------------------

back <- read.csv("data/GRF_ACC_misplacement/Back_sec.csv") %>%
  as.tibble() %>% 
  select(-name)

hip <- read.csv("data/GRF_ACC_misplacement/Hip_sec.csv") %>%
  as.tibble() %>% 
  select(-name)

# Data analysis -----------------------------------------------------------

# ** Linear regressions ---------------------------------------------------

back.LR <- lm(pRGRF_N ~ weight + pRACC_g, data = back)
hip.LR  <- lm(pRGRF_N ~ weight + pRACC_g, data = hip)

# ** Leave-one-out cross validation ---------------------------------------

back.LOOCV <- do.call(rbind, (lapply(unique(back$ID), cvLR, df = back, formula = pRGRF_N ~ weight + pRACC_g)))
hip.LOOCV  <- do.call(rbind, (lapply(unique(hip$ID), cvLR, df = hip, formula = pRGRF_N ~ weight + pRACC_g)))

# ** Predict model based on other placement data --------------------------

back.data.by.hip.LR <- cbind(back, predict(hip.LR, newdata = back, level = 0)) %>% 
  as.tibble()
hip.data.by.back.LR <- cbind(hip, predict(back.LR, newdata = hip, level = 0)) %>% 
  as.tibble()
names(back.data.by.hip.LR)[8] <- "back.data.by.hip.LR"
names(hip.data.by.back.LR)[8] <- "hip.data.by.back.LR"

# ** Indices of accuracy --------------------------------------------------

back.accuracy        <- accuracyIndices(back.LOOCV, "pRGRF_N_predicted", "pRGRF_N")
hip.accuracy         <- accuracyIndices(hip.LOOCV, "pRGRF_N_predicted", "pRGRF_N")
back.by.hip.accuracy <- accuracyIndices(back.data.by.hip.LR, "back.data.by.hip.LR", "pRGRF_N")
hip.by.back.accuracy <- accuracyIndices(hip.data.by.back.LR, "hip.data.by.back.LR", "pRGRF_N")

# ** Mixed model ----------------------------------------------------------


# **** Building dataframe -------------------------------------------------

back.by.back <- back.LOOCV %>% 
  select(ID, speed, pRGRF_N, pRGRF_N_predicted)
hip.by.hip <- hip.LOOCV %>% 
  select(ID, speed, pRGRF_N_predicted)
back.by.hip <- back.data.by.hip.LR %>% 
  select(ID, speed, back.data.by.hip.LR)
hip.by.back <- hip.data.by.back.LR %>% 
  select(ID, speed, hip.data.by.back.LR)

names(back.by.back)[4] <- "pRGRF_N_back_by_back"
names(hip.by.hip)[3]   <- "pRGRF_N_hip_by_hip"
names(back.by.hip)[3]  <- "pRGRF_N_back_by_hip"
names(hip.by.back)[3]  <- "pRGRF_N_hip_by_back"

all.predicted <- join_all(
  list(back.by.back, hip.by.hip, back.by.hip, hip.by.back), 
  by = c("ID", "speed"), 
  type = "left"
  ) %>% 
  as.tibble() %>% 
  gather(
    pRGRF_N, pRGRF_N_back_by_back, pRGRF_N_hip_by_hip, pRGRF_N_back_by_hip, pRGRF_N_hip_by_back,
    key = "valor",
    value = "by"
  )

# **** Mixed model analysis -----------------------------------------------
pd <- position_dodge(0.5)

ggplot(data = all.predicted, aes(x = speed, y = by, colour = valor)) + 
  stat_summary(fun.y = mean, geom = "point", position = pd) +
  stat_summary(fun.y = mean, geom = "line", position = pd) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = pd)
  
ggplot(data = back.LOOCV, aes(x = speed, y = pRGRF_N)) + 
  stat_summary(fun.y = mean, geom = "point", position = pd) +
  stat_summary(fun.y = mean, geom = "line", position = pd) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = pd)

ggplot(data = ANOVA_R, aes(Speed, GRF, colour = Code_Factor)) + 
  stat_summary(fun.y = mean, geom = "point", position = pd) +
  stat_summary(fun.y = mean, geom = "line", position = pd) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = pd)