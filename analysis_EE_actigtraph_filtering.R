# Load packages and functions ---------------------------------------------
library(tidyverse)
library(plyr)
library(nlme)
library(lsmeans)
source("R/cvMixedModel.R")
source("R/accuracyIndices.R")

# File preparation --------------------------------------------------------

pri.base <- read_csv("data/EE_actigraph_filtering/Hip_pri_base.csv")
pri.acc  <- read_csv("data/EE_actigraph_filtering/Hip_pri_acc_metrics.csv")
sec.base <- read_csv("data/EE_actigraph_filtering/Hip_sec_base.csv")
sec.acc  <- read_csv("data/EE_actigraph_filtering/Hip_sec_acc_metrics.csv")

pri <- pri.base %>% 
  left_join(pri.acc, by = c("ID", "speed")) %>% 
  dplyr::select(-c(Evaluation, Date, Counts))

sec <- sec.base %>% 
  left_join(sec.acc, by = c("ID", "speed")) %>% 
  dplyr::select(-c(Evaluation, Date))


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
MAD.pri.prediction <- cbind(na.omit(pri), MAD.pred)
MAD.pri.prediction <- as.tibble(MAD.pri.prediction)
names(MAD.pri.prediction)[16] <- "VO2.kg_predicted"

ENMO.pred <- predict(
  object = model.ENMO,
  newdata = na.omit(pri),
  level = 0
)
ENMO.pri.prediction <- cbind(na.omit(pri), ENMO.pred)
ENMO.pri.prediction <- as.tibble(ENMO.pri.prediction)
names(ENMO.pri.prediction)[16] <- "VO2.kg_predicted"

# ** Select desired speeds ------------------------------------------------

MAD.LOOCV <- MAD.LOOCV %>% 
  filter(speed == 3 | speed == 4 | speed == 5)
ENMO.LOOCV <- ENMO.LOOCV %>% 
  filter(speed == 3 | speed == 4 | speed == 5)
MAD.pri.prediction <- MAD.pri.prediction %>% 
  filter(speed == 3 | speed == 4 | speed == 5)
ENMO.pri.prediction <- ENMO.pri.prediction %>% 
  filter(speed == 3 | speed == 4 | speed == 5)

# ** Indices of accuracy --------------------------------------------------
# Overall
MAD.LOOCV.accuracy  <- accuracyIndices(MAD.LOOCV, "VO2.kg_predicted", "VO2.kg")
MAD.pri.accuracy    <- accuracyIndices(MAD.pri.prediction, "VO2.kg_predicted", "VO2.kg")
ENMO.LOOCV.accuracy <- accuracyIndices(ENMO.LOOCV, "VO2.kg_predicted", "VO2.kg")
ENMO.pri.accuracy   <- accuracyIndices(ENMO.pri.prediction, "VO2.kg_predicted", "VO2.kg")

accuracy.all <- rbind(MAD.LOOCV.accuracy, MAD.pri.accuracy, ENMO.LOOCV.accuracy, ENMO.pri.accuracy)
accuracy.all$metric <- c("MAD", "MAD", "ENMO", "ENMO")
accuracy.all$accelerometer <- c("sec", "pri", "sec", "pri")
accuracy.all <- accuracy.all[, c(15, 16, 1:14)]

# By speed
MAD.LOOCV.accuracy.3  <- accuracyIndices(filter(MAD.LOOCV, speed == 3), "VO2.kg_predicted", "VO2.kg")
MAD.LOOCV.accuracy.4  <- accuracyIndices(filter(MAD.LOOCV, speed == 4), "VO2.kg_predicted", "VO2.kg")
MAD.LOOCV.accuracy.5  <- accuracyIndices(filter(MAD.LOOCV, speed == 5), "VO2.kg_predicted", "VO2.kg")
MAD.pri.accuracy.3    <- accuracyIndices(filter(MAD.pri.prediction, speed == 3), "VO2.kg_predicted", "VO2.kg")
MAD.pri.accuracy.4    <- accuracyIndices(filter(MAD.pri.prediction, speed == 4), "VO2.kg_predicted", "VO2.kg")
MAD.pri.accuracy.5    <- accuracyIndices(filter(MAD.pri.prediction, speed == 5), "VO2.kg_predicted", "VO2.kg")
ENMO.LOOCV.accuracy.3 <- accuracyIndices(filter(ENMO.LOOCV, speed == 3), "VO2.kg_predicted", "VO2.kg")
ENMO.LOOCV.accuracy.4 <- accuracyIndices(filter(ENMO.LOOCV, speed == 4), "VO2.kg_predicted", "VO2.kg")
ENMO.LOOCV.accuracy.5 <- accuracyIndices(filter(ENMO.LOOCV, speed == 5), "VO2.kg_predicted", "VO2.kg")
ENMO.pri.accuracy.3   <- accuracyIndices(filter(ENMO.pri.prediction, speed == 3), "VO2.kg_predicted", "VO2.kg")
ENMO.pri.accuracy.4   <- accuracyIndices(filter(ENMO.pri.prediction, speed == 4), "VO2.kg_predicted", "VO2.kg")
ENMO.pri.accuracy.5   <- accuracyIndices(filter(ENMO.pri.prediction, speed == 5), "VO2.kg_predicted", "VO2.kg")

accuracy.speeds <- rbind(
  MAD.LOOCV.accuracy.3, MAD.LOOCV.accuracy.4, MAD.LOOCV.accuracy.5,
  MAD.pri.accuracy.3, MAD.pri.accuracy.4, MAD.pri.accuracy.5,
  ENMO.LOOCV.accuracy.3, ENMO.LOOCV.accuracy.4, ENMO.LOOCV.accuracy.5,
  ENMO.pri.accuracy.3, ENMO.pri.accuracy.4, ENMO.pri.accuracy.5
)
accuracy.speeds$metric <- c(rep("MAD", 6), rep("ENMO", 6))
accuracy.speeds$accelerometer <- rep(c(rep("sec", 3), rep("pri", 3)), 2)
accuracy.speeds$speed <- rep(c(3, 4, 5), 4)
accuracy.speeds <- accuracy.speeds[, c(15, 16, 17, 1:14)]

# ** Mixed models ---------------------------------------------------------

# **** Building dataframe -------------------------------------------------
# Get accelerometer metric from primary and secondary accelerometers
MAD.acc.pri <- pri.acc %>% 
  dplyr::select(-c(Counts, ENMO)) %>% 
  na.omit() %>% 
  filter(speed == 3 | speed == 4 | speed == 5)
MAD.acc.sec <- sec.acc %>% 
  dplyr::select(-ENMO) %>% 
  na.omit() %>% 
  filter(speed == 3 | speed == 4 | speed == 5)
ENMO.acc.pri <- pri.acc %>% 
  dplyr::select(-c(Counts, MAD)) %>% 
  na.omit() %>% 
  filter(speed == 3 | speed == 4 | speed == 5)
ENMO.acc.sec <- sec.acc %>% 
  dplyr::select(-MAD) %>% 
  na.omit() %>% 
  filter(speed == 3 | speed == 4 | speed == 5)

names(MAD.acc.pri)[3]  <- "primary"
names(MAD.acc.sec)[3]  <- "secondary"
names(ENMO.acc.pri)[3] <- "primary"
names(ENMO.acc.sec)[3] <- "secondary"

# Merge dataframes
MAD.acc <- join_all(
  list(MAD.acc.pri, MAD.acc.sec),
  by = c("ID", "speed"),
  type = "left"
) %>% 
  as.tibble() %>% 
  gather(
    primary, secondary,
    key = "accelerometer",
    value = "MAD"
  )
MAD.acc$speed <- as.factor(MAD.acc$speed)
MAD.acc$accelerometer <- as.factor(MAD.acc$accelerometer)

ENMO.acc <- join_all(
  list(ENMO.acc.pri, ENMO.acc.sec),
  by = c("ID", "speed"),
  type = "left"
) %>% 
  as.tibble() %>% 
  gather(
    primary, secondary,
    key = "accelerometer",
    value = "ENMO"
  )
ENMO.acc$speed <- as.factor(ENMO.acc$speed)
ENMO.acc$accelerometer <- as.factor(ENMO.acc$accelerometer)

# Get measured and predicted VO2/kg values
MAD.pri <- MAD.pri.prediction %>% 
  dplyr::select(ID, speed, VO2.kg_predicted, VO2.kg)
MAD.sec <- MAD.LOOCV %>% 
  dplyr::select(ID, speed, VO2.kg_predicted)
ENMO.pri <- ENMO.pri.prediction %>% 
  dplyr::select(ID, speed, VO2.kg_predicted, VO2.kg)
ENMO.sec <- ENMO.LOOCV %>% 
  dplyr::select(ID, speed, VO2.kg_predicted)

names(MAD.pri)[3]  <- "primary_acc"
names(MAD.pri)[4]  <- "measured"
names(MAD.sec)[3]  <- "secondary_acc"
names(ENMO.pri)[3] <- "primary_acc"
names(ENMO.pri)[4] <- "measured"
names(ENMO.sec)[3] <- "secondary_acc"

# Merge dataframes
MAD.VO2.kg <- join_all(
  list(MAD.pri, MAD.sec),
  by = c("ID", "speed"),
  type = "left"
) %>% 
  as.tibble() %>% 
  gather(
    primary_acc, secondary_acc, measured,
    key = "group",
    value = "VO2.kg"
  )
MAD.VO2.kg$speed <- as.factor(MAD.VO2.kg$speed)
MAD.VO2.kg$group <- as.factor(MAD.VO2.kg$group)

ENMO.VO2.kg <- join_all(
  list(ENMO.pri, ENMO.sec),
  by = c("ID", "speed"),
  type = "left"
) %>% 
  as.tibble() %>% 
  gather(
    primary_acc, secondary_acc, measured,
    key = "group",
    value = "VO2.kg"
  )
ENMO.VO2.kg$speed <- as.factor(ENMO.VO2.kg$speed)
ENMO.VO2.kg$group <- as.factor(ENMO.VO2.kg$group)

# **** Mixed models analysis ----------------------------------------------
### Accelerometer metrics output
# MAD
MAD.acc.baseline <- lme(
  fixed = MAD ~ 1,
  random = ~ 1 | ID/speed/accelerometer,
  method = "ML",
  data = MAD.acc
)

MAD.acc.group <- lme(
  fixed = MAD ~ accelerometer,
  random = ~ 1 | ID/speed/accelerometer,
  method = "ML",
  data = MAD.acc
)

MAD.acc.interact <- lme(
  fixed = MAD ~ accelerometer * speed,
  random = ~ 1 | ID/speed/accelerometer,
  method = "ML",
  data = MAD.acc
)

anova(MAD.acc.baseline, MAD.acc.group, MAD.acc.interact)

MAD.acc.posthoc <- lsmeans(MAD.acc.interact, pairwise ~ accelerometer * speed, adjust = "tukey")

# ENMO
ENMO.acc.baseline <- lme(
  fixed = ENMO ~ 1,
  random = ~ 1 | ID/speed/accelerometer,
  method = "ML",
  data = ENMO.acc
)

ENMO.acc.group <- lme(
  fixed = ENMO ~ accelerometer,
  random = ~ 1 | ID/speed/accelerometer,
  method = "ML",
  data = ENMO.acc
)

ENMO.acc.interact <- lme(
  fixed = ENMO ~ accelerometer * speed,
  random = ~ 1 | ID/speed/accelerometer,
  method = "ML",
  data = ENMO.acc
)

anova(ENMO.acc.baseline, ENMO.acc.group, ENMO.acc.interact)

ENMO.acc.posthoc <- lsmeans(ENMO.acc.interact, pairwise ~ accelerometer * speed, adjust = "tukey")

## Accelerometer metrics descriptives
MAD.summary <- MAD.acc %>% 
  group_by(accelerometer, speed) %>%
  dplyr::summarise(
    MAD_mean = round(mean(MAD), digits = 0),
    MAD_sd   = round(sd(MAD), digits = 0)
  )

ENMO.summary <- ENMO.acc %>% 
  group_by(accelerometer, speed) %>% 
  dplyr::summarise(
    ENMO_mean = round(mean(ENMO), digits = 0),
    ENMO_sd   = round(sd(ENMO), digits = 0)
  )
##

### VO2 predictions
# VO2 by MAD
MAD.baseline <- lme(
  fixed = VO2.kg ~ 1,
  random = ~ 1 | ID/speed/group,
  method = "ML",
  data = MAD.VO2.kg
)

MAD.group <- lme(
  fixed = VO2.kg ~ group,
  random = ~ 1 | ID/speed/group,
  method = "ML",
  data = MAD.VO2.kg
)

MAD.interact <- lme(
  fixed = VO2.kg ~ group * speed,
  random = ~ 1 | ID/speed/group,
  method = "ML",
  data = MAD.VO2.kg
)

anova(MAD.baseline, MAD.group, MAD.interact)

MAD.posthoc <- lsmeans(MAD.interact, pairwise ~ group * speed, adjust = "tukey")

# VO2 by ENMO
ENMO.baseline <- lme(
  fixed = VO2.kg ~ 1,
  random = ~ 1 | ID/speed/group,
  method = "ML",
  data = ENMO.VO2.kg
)

ENMO.group <- lme(
  fixed = VO2.kg ~ group,
  random = ~ 1 | ID/speed/group,
  method = "ML",
  data = ENMO.VO2.kg
)

ENMO.interact <- lme(
  fixed = VO2.kg ~ group * speed,
  random = ~ 1 | ID/speed/group,
  method = "ML",
  data = ENMO.VO2.kg
)

anova(ENMO.baseline, ENMO.group, ENMO.interact)

ENMO.posthoc <- lsmeans(MAD.interact, pairwise ~ group * speed, adjust = "tukey")

# ** Graphs ---------------------------------------------------------------
# MAD
MAD.graph.df <- MAD.VO2.kg
MAD.graph.df$speed <- as.numeric(MAD.graph.df$speed)
MAD.graph.df$speed[MAD.graph.df$speed == 3] <- 5
MAD.graph.df$speed[MAD.graph.df$speed == 2] <- 4
MAD.graph.df$speed[MAD.graph.df$speed == 1] <- 3

MAD.graph <- ggplot(data = MAD.graph.df, aes(x = speed, y = VO2.kg, colour = group)) +
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(0.5)) +
  stat_summary(fun.y = mean, geom = "line", position = position_dodge(0.5)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = c(3, 4, 5)) +
  scale_y_continuous(breaks = seq(from = 9, to = 15)) + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(size = 0.5, colour = "Black"),
    axis.ticks = element_line(size = 0.5, colour = "Black"),
    axis.ticks.length = unit(2, "mm"),
    plot.title = element_text(face = "bold")
  ) +
  ggtitle("Figure 1")

# ENMO
ENMO.graph.df <- ENMO.VO2.kg
ENMO.graph.df$speed <- as.numeric(ENMO.graph.df$speed)
ENMO.graph.df$speed[ENMO.graph.df$speed == 3] <- 5
ENMO.graph.df$speed[ENMO.graph.df$speed == 2] <- 4
ENMO.graph.df$speed[ENMO.graph.df$speed == 1] <- 3

ENMO.graph <- ggplot(data = ENMO.graph.df, aes(x = speed, y = VO2.kg, colour = group)) +
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(0.5)) +
  stat_summary(fun.y = mean, geom = "line", position = position_dodge(0.5)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = c(3, 4, 5)) +
  scale_y_continuous(breaks = seq(from = 9, to = 15)) + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(size = 0.5, colour = "Black"),
    axis.ticks = element_line(size = 0.5, colour = "Black"),
    axis.ticks.length = unit(2, "mm"),
    plot.title = element_text(face = "bold")
  ) +
  ggtitle("Figure 2")