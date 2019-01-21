# Load packages and functions ---------------------------------------------
library(tidyverse)
library(nlme)
source("R/cvMixedModel.R")
source("R/accuracyIndices.R")

# File preparation --------------------------------------------------------

hip.base <- read_csv("data/regressions_comparison/Hip_sec_base.csv")
hip.acc  <- read_csv("data/regressions_comparison/Hip_sec_acc_metrics.csv")

hip <- hip.base %>% 
  left_join(hip.acc, by = c("ID", "speed")) %>% 
  dplyr::select(-c(Evaluation, Date))

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

# ** Mixed models ---------------------------------------------------------

model.MAD <- lme(
  fixed = VO2.kg ~ MAD + I(MAD^2) + Age,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip,
  na.action = na.omit
)

model.ENMO <- lme(
  fixed = VO2.kg ~ ENMO + I(ENMO^2) + Age,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip,
  na.action = na.omit
)

# ** Leave-one-out cross validation ---------------------------------------

fix.eff   <- VO2.kg ~ MAD + I(MAD^2) + Age
rand.eff  <- ~ 1 | ID
MAD.LOOCV <- do.call(rbind, (lapply(unique(hip$ID), cvMixedModel, df = hip)))

fix.eff    <- VO2.kg ~ ENMO + I(ENMO^2) + Age
rand.eff   <- ~ 1 | ID
ENMO.LOOCV <- do.call(rbind, (lapply(unique(hip$ID), cvMixedModel, df = hip)))

MAD.LOOCV  <- select(MAD.LOOCV, ID, speed, VO2.kg, VO2.kg_predicted)
ENMO.LOOCV <- select(ENMO.LOOCV, ID, speed, VO2.kg, VO2.kg_predicted)


# ** Prediction models developed for non-obese people ---------------------

# MAD
# Vaha-Ypya H, et al. PloS one. 2015;10(8):e0134813
# (prediction model based on linear regression for walking only)
MAD.VahY <- hip %>% 
  select(ID, speed, VO2.kg, MAD) %>% 
  na.omit()
MAD.VahY$VO2.kg_predicted <- NA
for (i in 1:nrow(MAD.VahY)) {
  MAD.VahY$VO2.kg_predicted[i] <- 7.92 + 0.0331 * MAD.VahY$MAD[i]
}

# ENMO
# Hildebrand M, et al. Med Sci Sports Exerc. 2014 Sep;46(9):1816-24
# (equation for adults, actigraph at hip)
ENMO.Hild <- hip %>% 
  select(ID, speed, VO2.kg, ENMO) %>% 
  na.omit()
ENMO.Hild$VO2.kg_predicted <- NA
for (i in 1:nrow(ENMO.Hild)) {
  ENMO.Hild$VO2.kg_predicted[i] <- 0.0554 * ENMO.Hild$ENMO[i] + 6.67
}

# ** Indices of accuracy --------------------------------------------------

MAD.LOOCV.accuracy  <- accuracyIndices(MAD.LOOCV, "VO2.kg_predicted", "VO2.kg")
MAD.VahY.accuracy   <- accuracyIndices(MAD.VahY, "VO2.kg_predicted", "VO2.kg")
ENMO.LOOCV.accuracy <- accuracyIndices(ENMO.LOOCV, "VO2.kg_predicted", "VO2.kg")
ENMO.Hild.accuracy  <- accuracyIndices(ENMO.Hild, "VO2.kg_predicted", "VO2.kg")

accuracy.all <- rbind(MAD.LOOCV.accuracy, MAD.VahY.accuracy, ENMO.LOOCV.accuracy, ENMO.Hild.accuracy)
accuracy.all$metric <- c(rep("MAD", 2), rep("ENMO", 2))
accuracy.all$equation <- c("Our", "Vaha-Ypya", "Our", "Hildebrand")
accuracy.all <- accuracy.all[, c(15, 16, 1:14)]


# ** Bland-Altman plots ---------------------------------------------------
# MAD - our equation
BAplot.MAD.our <- ggplot(data = MAD.LOOCV, aes(x = ((VO2.kg + VO2.kg_predicted) / 2), y = (VO2.kg - VO2.kg_predicted))) +
  geom_point() +
  geom_hline(yintercept = mean(MAD.LOOCV$VO2.kg - MAD.LOOCV$VO2.kg_predicted)) +
  geom_hline(
    yintercept = mean(MAD.LOOCV$VO2.kg - MAD.LOOCV$VO2.kg_predicted) + 
                 (1.96 * sd(MAD.LOOCV$VO2.kg - MAD.LOOCV$VO2.kg_predicted)),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(MAD.LOOCV$VO2.kg - MAD.LOOCV$VO2.kg_predicted) - 
      (1.96 * sd(MAD.LOOCV$VO2.kg - MAD.LOOCV$VO2.kg_predicted)),
    linetype = "dotted"
  ) +
  scale_y_continuous(limits = c(-6, 6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 20), expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.line = element_line(size = 1, colour = "Black"),
    axis.ticks = element_line(size = 1, colour = "Black"),
    axis.ticks.length = unit(2, "mm"),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Figure 1",
    x = bquote("Mean of Measured and Predicted" ~ VO[2] ~ (ml^. ~ kg^-1~ . ~ min^-1)),
    y = bquote("Difference of Measured and Predicted" ~ VO[2] ~ (ml^. ~ kg^-1~ . ~ min^-1))
  )

# MAD - Vaha-Ypya equation
BAplot.MAD.VahY <- ggplot(data = MAD.VahY, aes(x = ((VO2.kg + VO2.kg_predicted) / 2), y = (VO2.kg - VO2.kg_predicted))) +
  geom_point() +
  geom_hline(yintercept = mean(MAD.VahY$VO2.kg - MAD.VahY$VO2.kg_predicted)) +
  geom_hline(
    yintercept = mean(MAD.VahY$VO2.kg - MAD.VahY$VO2.kg_predicted) + 
      (1.96 * sd(MAD.VahY$VO2.kg - MAD.VahY$VO2.kg_predicted)),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(MAD.VahY$VO2.kg - MAD.VahY$VO2.kg_predicted) - 
      (1.96 * sd(MAD.VahY$VO2.kg - MAD.VahY$VO2.kg_predicted)),
    linetype = "dotted"
  ) +
  scale_y_continuous(limits = c(-12, 4), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 25), expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.line = element_line(size = 1, colour = "Black"),
    axis.ticks = element_line(size = 1, colour = "Black"),
    axis.ticks.length = unit(2, "mm"),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Figure 2",
    x = bquote("Mean of Measured and Predicted" ~ VO[2] ~ (ml^. ~ kg^-1~ . ~ min^-1)),
    y = bquote("Difference of Measured and Predicted" ~ VO[2] ~ (ml^. ~ kg^-1~ . ~ min^-1))
  )

# ENMO - our equation
BAplot.ENMO.our <- ggplot(data = ENMO.LOOCV, aes(x = ((VO2.kg + VO2.kg_predicted) / 2), y = (VO2.kg - VO2.kg_predicted))) +
  geom_point() +
  geom_hline(yintercept = mean(ENMO.LOOCV$VO2.kg - ENMO.LOOCV$VO2.kg_predicted)) +
  geom_hline(
    yintercept = mean(ENMO.LOOCV$VO2.kg - ENMO.LOOCV$VO2.kg_predicted) + 
      (1.96 * sd(ENMO.LOOCV$VO2.kg - ENMO.LOOCV$VO2.kg_predicted)),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(ENMO.LOOCV$VO2.kg - ENMO.LOOCV$VO2.kg_predicted) - 
      (1.96 * sd(ENMO.LOOCV$VO2.kg - ENMO.LOOCV$VO2.kg_predicted)),
    linetype = "dotted"
  ) +
  scale_y_continuous(limits = c(-6, 6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 20), expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.line = element_line(size = 1, colour = "Black"),
    axis.ticks = element_line(size = 1, colour = "Black"),
    axis.ticks.length = unit(2, "mm"),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Figure 3",
    x = bquote("Mean of Measured and Predicted" ~ VO[2] ~ (ml^. ~ kg^-1~ . ~ min^-1)),
    y = bquote("Difference of Measured and Predicted" ~ VO[2] ~ (ml^. ~ kg^-1~ . ~ min^-1))
  )

# ENMO - Hildebrand equation
BAplot.ENMO.Hild <- ggplot(data = ENMO.Hild, aes(x = ((VO2.kg + VO2.kg_predicted) / 2), y = (VO2.kg - VO2.kg_predicted))) +
  geom_point() +
  geom_hline(yintercept = mean(ENMO.Hild$VO2.kg - ENMO.Hild$VO2.kg_predicted)) +
  geom_hline(
    yintercept = mean(ENMO.Hild$VO2.kg - ENMO.Hild$VO2.kg_predicted) + 
      (1.96 * sd(ENMO.Hild$VO2.kg - ENMO.Hild$VO2.kg_predicted)),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(ENMO.Hild$VO2.kg - ENMO.Hild$VO2.kg_predicted) - 
      (1.96 * sd(ENMO.Hild$VO2.kg - ENMO.Hild$VO2.kg_predicted)),
    linetype = "dotted"
  ) +
  scale_y_continuous(limits = c(-10, 2), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 25), expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.line = element_line(size = 1, colour = "Black"),
    axis.ticks = element_line(size = 1, colour = "Black"),
    axis.ticks.length = unit(2, "mm"),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Figure 4",
    x = bquote("Mean of Measured and Predicted" ~ VO[2] ~ (ml^. ~ kg^-1~ . ~ min^-1)),
    y = bquote("Difference of Measured and Predicted" ~ VO[2] ~ (ml^. ~ kg^-1~ . ~ min^-1))
  )