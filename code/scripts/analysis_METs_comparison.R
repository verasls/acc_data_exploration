# Load packages and functions ---------------------------------------------
library(tidyverse)
library(pROC)
library(irr)
source("code/functions/getMET.R")
source("code/functions/getMETstd.R")
source("code/functions/getMETcategories2.R")
source("code/functions/cvROC2.R")
source("code/functions/getPercentAgreement.R")

# File preparation --------------------------------------------------------

hip.base <- read_csv("data/regressions_comparison/Hip_sec_base.csv")
hip.acc  <- read_csv("data/regressions_comparison/Hip_sec_acc_metrics.csv")

hip <- hip.base %>% 
  left_join(hip.acc, by = c("ID", "speed")) %>% 
  dplyr::select(-c(Evaluation, Date, V.O2, RER, HR, BF, V.E, V.CO2, Weight, BMI, Age, Sex, MAD))

# Data analysis -----------------------------------------------------------

# ** Sample descriptives --------------------------------------------------

samp.desc <- read_csv("data/regressions_comparison/sample_descriptives_data.csv")
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

RMR <- hip %>% 
  filter(speed == 0) %>% 
  summarise(
    mean = round(mean(VO2.kg), digits = 2), 
    std = round(sd(VO2.kg), digits = 2)
  )

# ** Computing MET values -------------------------------------------------

# Individual MET (based on individual resting metabolic rate)
hip <- do.call(rbind, (lapply(unique(hip$ID), getMET, df = hip)))
# Standard MET (based on 3.5 mlO2/kg/min)
hip <- do.call(rbind, (lapply(unique(hip$ID), getMETstd, df = hip)))

names(hip)[5] <- "MET_ind"
hip <- na.omit(hip)

# Getting MVPA categories
hip$MVPA_MET_ind_CAT <- NA
hip$MVPA_MET_std_CAT <- NA
for (i in 1:nrow(hip)) {
  # MET_ind
  if (hip$MET_ind[i] >= 3) {
    hip$MVPA_MET_ind_CAT[i] <- 1
  } else {
    hip$MVPA_MET_ind_CAT[i] <- 0
  }
  # MET_std
  if (hip$MET_std[i] >= 3) {
    hip$MVPA_MET_std_CAT[i] <- 1
  } else {
    hip$MVPA_MET_std_CAT[i] <- 0
  }
}

# ** ROC curves -----------------------------------------------------------

ROC.ind     <- roc(MVPA_MET_ind_CAT ~ ENMO, data = hip, ci = TRUE) 
ROC.std     <- roc(MVPA_MET_std_CAT ~ ENMO, data = hip, ci = TRUE) 
cp.MVPA.ind <- coords(ROC.ind, x = "best", best.method = "closest.topleft")
cp.MVPA.std <- coords(ROC.std, x = "best", best.method = "closest.topleft")

ROC.summary <- data.frame(
  MET_used    = c("individual", "standard"),
  threshold   = c(cp.MVPA.ind[1], cp.MVPA.std[1]),
  AUC         = c(as.numeric(ROC.ind[9]), as.numeric(ROC.std[9])),
  AUC_95Ci    = c(capture.output(ROC.ind[16])[2], capture.output(ROC.std[16])[2]),
  sensitivity = c(cp.MVPA.ind[3], cp.MVPA.std[3]),
  specificity = c(cp.MVPA.ind[2], cp.MVPA.std[2])
  )

# Plot and save ROC curves
# Individual METs
pdf("figs/abstract_5_fig_1.pdf")
plot(smooth(ROC.ind), col = "blue", legacy.axes = TRUE, identity = FALSE, axes = FALSE, xlab = "", ylab = "")
axis(1, pos = 0, at = seq(0, 1, 0.2), labels = c("1.0", 0.8, 0.6, 0.4, 0.2, "0.0"))
axis(2, pos = 1, at = seq(0, 1, 0.2))
axis(3, pos = 1, labels = FALSE, lwd.ticks = 0)
axis(4, pos = 0, labels = FALSE, lwd.ticks = 0)
mtext("1 - Specificity", side=1, line = 3)
mtext("Sensitivity", side=2, line = 2)
dev.off()

# Standard METs
pdf("figs/abstract_5_fig_2.pdf")
plot(smooth(ROC.std), col = "blue", legacy.axes = TRUE, identity = FALSE, axes = FALSE, xlab = "", ylab = "")
axis(1, pos = 0, at = seq(0, 1, 0.2), labels = c("1.0", 0.8, 0.6, 0.4, 0.2, "0.0"))
axis(2, pos = 1, at = seq(0, 1, 0.2))
axis(3, pos = 1, labels = FALSE, lwd.ticks = 0)
axis(4, pos = 0, labels = FALSE, lwd.ticks = 0)
mtext("1 - Specificity", side=1, line = 3)
mtext("Sensitivity", side=2, line = 2)
dev.off()


# ** ROC leave-one-out cross validation -----------------------------------

ROC.LOOCV <- do.call(rbind, (lapply(unique(hip$ID), cvROC, df = hip)))

# ** Kappa statistics -----------------------------------------------------

K.ind <- kappa2(ROC.LOOCV[, c("MVPA_MET_ind_CAT", "MVPA_MET_ind_by_ENMO_CAT")])
K.std <- kappa2(ROC.LOOCV[, c("MVPA_MET_ind_CAT", "MVPA_MET_std_by_ENMO_CAT")])

# ** Percent agreement ----------------------------------------------------

ind.agree <- getPercentAgreement(ROC.LOOCV, "MVPA_MET_ind_CAT", "MVPA_MET_ind_by_ENMO_CAT")
std.agree <- getPercentAgreement(ROC.LOOCV, "MVPA_MET_ind_CAT", "MVPA_MET_std_by_ENMO_CAT")