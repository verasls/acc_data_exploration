library(tidyverse)
library(pROC)
library(irr)
source("R/getMET.R")
source("R/getMETcategories.R")
source("R/cvROC.R")
source("R/getPercentAgreement.R")

# File preparation --------------------------------------------------------

# ** Merging related databases -----------------------------------------------

hip.steps  <- read.csv("data/PAI_steps/raw/Hip_pri_steps_means.csv") %>%
  as.tibble()
hip.card <- read.csv("data/PAI_steps/raw/Hip_pri_raw.csv") %>%
  as.tibble()

hip.df <- hip.card %>%
  select(-c(BF, V.E, V.CO2, Evaluation, Date)) %>%
  left_join(hip.steps, by = c("ID", "speed"))


# ** Calculating MET values -----------------------------------------------

hip.df <- do.call(rbind, (lapply(unique(hip.df$ID), getMET, df = hip.df)))
hip.df <- getMETcategories(hip.df)


# Statistical analysis  ---------------------------------------------------

# ** Sample descriptives --------------------------------------------------

samp.desc <- read.csv("data/PAI_steps/raw/sample_descriptives_data.csv")
descriptives <- summarise(
  .data       = samp.desc,
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

# ** ROC curves -----------------------------------------------------------

ROC.mod    <- roc(response = hip.df$MOD_MET_CAT, predictor = hip.df$steps, ci = TRUE, na.rm = TRUE)
ROC.vig    <- roc(response = hip.df$VIG_MET_CAT, predictor = hip.df$steps, ci = TRUE, na.rm = TRUE)
cp.ROC.mod <- coords(ROC.mod, x = "best", best.method = "closest.topleft")
cp.ROC.vig <- coords(ROC.vig, x = "best", best.method = "closest.topleft")

intens.cat  <- c("moderate", "vigorous")
threshold   <- c(cp.ROC.mod[1], cp.ROC.vig[1])
AUC         <- c(as.numeric(ROC.mod[9]), as.numeric(ROC.vig[9]))
AUC.95CI    <- c(capture.output(ROC.mod[16])[2], capture.output(ROC.vig[16])[2])
sensitivity <- c(cp.ROC.mod[3], cp.ROC.vig[3])
specificity <- c(cp.ROC.mod[2], cp.ROC.vig[2])
ROC.summary <- data.frame(intens.cat, threshold, AUC, AUC.95CI, sensitivity, specificity)

# ** ROC leave-one-out cross validation -----------------------------------

ROC.LOOCV <- do.call(rbind, (lapply(unique(hip.df$ID), cvROC, df = hip.df)))
write.csv(
  ROC.LOOCV,
  file = "~/Dropbox/Projects/ECSS_2019/ROC_LOOCV.csv",
  row.names = FALSE
)

# ** Kappa statistics -----------------------------------------------------

K.mod <- kappa2(ROC.LOOCV[, c("MOD_MET_CAT", "MOD_steps_CAT")])
K.vig <- kappa2(ROC.LOOCV[, c("VIG_MET_CAT", "VIG_steps_CAT")])

# ** Percent agreement ----------------------------------------------------

mod.agree <- getPercentAgreement(ROC.LOOCV, "MOD_MET_CAT", "MOD_steps_CAT")
vig.agree <- getPercentAgreement(ROC.LOOCV, "VIG_MET_CAT", "VIG_steps_CAT")