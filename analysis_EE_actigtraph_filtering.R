library(tidyverse)
library(nlme)
source("R/cvMixedModel.R")
source("R/accuracyIndices.R")

# File preparation --------------------------------------------------------

pri.base <- read_csv("data/EE_actigraph_filtering/Hip_pri_base.csv")
pri.acc  <- read_csv("data/EE_actigraph_filtering/Hip_pri_acc_metrics.csv")
sec.base <- read_csv("data/EE_actigraph_filtering/Hip_sec_base.csv")
sec.acc  <- read_csv("data/EE_actigraph_filtering/Hip_sec_acc_metrics.csv")

pri <- pri.base %>% 
  left_join(pri.acc, by = c("ID", "speed")) %>% 
  select(-c(Evaluation, Date, Counts))

sec <- sec.base %>% 
  left_join(sec.acc, by = c("ID", "speed")) %>% 
  select(-c(Evaluation, Date))


# Data analysis -----------------------------------------------------------

# ** Sample descriptives --------------------------------------------------

samp.desc <- read_csv("data/EE_actigraph_filtering/sample_descriptives_data.csv")
descriptives <- summarise(
  .data = samp.desc,
  age_mean    = round(mean(age), digits = 2),
  age_sd      = round(sd(age), digits = 2),
  weight_mean = round(mean(weight_kg), digits = 2),
  weight_sd   = round(sd(weight_kg), digits = 2),
  height_mean = round(mean(height_cm), digits = 2),
  height_sd   = round(sd(height_cm), digits = 2),
  BMI_mean    = round(mean(BMI_kgm2), digits = 2),
  BMI_sd      = round(sd(BMI_kgm2), digits = 2),
  fat_mean    = round(mean(body_fat), digits = 2),
  fat_sd      = round(sd(body_fat), digits = 2)
)
sex <- table(samp.desc$sex)

# ** Mixed models ---------------------------------------------------------

model.MAD <- lme(
  fixed = VO2.kg ~ MAD + I(MAD^2) + Age,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = sec,
  na.action = na.omit
)

model.ENMO <- lme(
  fixed = VO2.kg ~ ENMO + I(ENMO^2) + Age,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = sec,
  na.action = na.omit
)

# ** Leave-one-out cross validation ---------------------------------------

fix.eff   <- VO2.kg ~ MAD + I(MAD^2) + Age
rand.eff  <- ~ 1 | ID
MAD.LOOCV <- do.call(rbind, (lapply(unique(sec$ID), cvMixedModel, df = sec)))

fix.eff    <- VO2.kg ~ ENMO + I(ENMO^2) + Age
rand.eff   <- ~ 1 | ID
ENMO.LOOCV <- do.call(rbind, (lapply(unique(sec$ID), cvMixedModel, df = sec)))

# ** Predict models with primary accelerometer metrics --------------------

MAD.pred <- predict(
  object = model.MAD,
  newdata = na.omit(pri),
  level = 0
)
pri.MAD.prediction <- cbind(na.omit(pri), MAD.pred)
pri.MAD.prediction <- as.tibble(pri.MAD.prediction)
names(pri.MAD.prediction)[16] <- "VO2.kg_predicted"

ENMO.pred <- predict(
  object = model.ENMO,
  newdata = na.omit(pri),
  level = 0
)
pri.ENMO.prediction <- cbind(na.omit(pri), ENMO.pred)
pri.ENMO.prediction <- as.tibble(pri.ENMO.prediction)
names(pri.ENMO.prediction)[16] <- "VO2.kg_predicted"

# ** Indices of accuracy --------------------------------------------------

MAD.LOOCV.accuracy  <- accuracyIndices(MAD.LOOCV, "VO2.kg_predicted", "VO2.kg")
ENMO.LOOCV.accuracy <- accuracyIndices(ENMO.LOOCV, "VO2.kg_predicted", "VO2.kg")
MAD.pri.accuracy    <- accuracyIndices(pri.MAD.prediction, "VO2.kg_predicted", "VO2.kg")
ENMO.pri.accuracy   <- accuracyIndices(pri.ENMO.prediction, "VO2.kg_predicted", "VO2.kg")