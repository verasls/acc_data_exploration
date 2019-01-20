library(tidyverse)
library(plyr)
library(nlme)
library(lsmeans)
library(ggplot2)
source("R/cvLR.R")
source("R/accuracyIndices.R")

# File preparation --------------------------------------------------------

back <- read.csv("data/GRF_ACC_misplacement/Back_sec.csv") %>%
  as.tibble() %>% 
  dplyr::select(-name)

hip <- read.csv("data/GRF_ACC_misplacement/Hip_sec.csv") %>%
  as.tibble() %>% 
  dplyr::select(-name)

# Data analysis -----------------------------------------------------------

# ** Linear regressions ---------------------------------------------------

back.LR <- lm(pRGRF_N ~ weight + pRACC_g, data = back)
hip.LR  <- lm(pRGRF_N ~ weight + pRACC_g, data = hip)

# ** Leave-one-out cross validation ---------------------------------------

back.LOOCV <- do.call(rbind, (lapply(unique(back$ID), cvLR, df = back, formula = pRGRF_N ~ weight + pRACC_g)))
hip.LOOCV  <- do.call(rbind, (lapply(unique(hip$ID), cvLR, df = hip, formula = pRGRF_N ~ weight + pRACC_g)))

write.csv(
  back.LOOCV,
  file = "~/Dropbox/Projects/ECSS_2019/abstract2_back_ACC_by_back_LR_LOOCV.csv",
  row.names = FALSE
)
write.csv(
  hip.LOOCV,
  file = "~/Dropbox/Projects/ECSS_2019/abstract2_hip_ACC_by_hip_LR_LOOCV.csv",
  row.names = FALSE
)

# ** Predict model based on other placement data --------------------------

back.data.by.hip.LR <- cbind(back, predict(hip.LR, newdata = back, level = 0)) %>% 
  as.tibble()
hip.data.by.back.LR <- cbind(hip, predict(back.LR, newdata = hip, level = 0)) %>% 
  as.tibble()
names(back.data.by.hip.LR)[8] <- "back_ACC_by_hip_LR"
names(hip.data.by.back.LR)[8] <- "hip_ACC_by_back_LR"

write.csv(
  back.data.by.hip.LR,
  file = "~/Dropbox/Projects/ECSS_2019/abstract2_back_ACC_by_hip_LR_LOOCV.csv",
  row.names = FALSE
)
write.csv(
  hip.data.by.back.LR,
  file = "~/Dropbox/Projects/ECSS_2019/abstract2_hip_ACC_by_back_LR_LOOCV.csv",
  row.names = FALSE
)

# ** Indices of accuracy --------------------------------------------------

back.accuracy        <- accuracyIndices(back.LOOCV, "pRGRF_N_predicted", "pRGRF_N")
hip.accuracy         <- accuracyIndices(hip.LOOCV, "pRGRF_N_predicted", "pRGRF_N")
back.by.hip.accuracy <- accuracyIndices(back.data.by.hip.LR, "back_ACC_by_hip_LR", "pRGRF_N")
hip.by.back.accuracy <- accuracyIndices(hip.data.by.back.LR, "hip_ACC_by_back_LR", "pRGRF_N")

accuracy <- rbind(back.accuracy, hip.accuracy, back.by.hip.accuracy, hip.by.back.accuracy)
accuracy$model_name <- c("back by back", "hip by hip", "back by hip", "hip by back")
accuracy <- accuracy[, c(15, 1:14)]

# ** Mixed model ----------------------------------------------------------

# **** Building dataframe -------------------------------------------------
# Get predicted pRGRF values
back.by.back <- back.LOOCV %>% 
  dplyr::select(ID, speed, pRGRF_N_predicted)
hip.by.hip <- hip.LOOCV %>% 
  dplyr::select(ID, speed, pRGRF_N_predicted)
back.by.hip <- back.data.by.hip.LR %>% 
  dplyr::select(ID, speed, back_ACC_by_hip_LR)
hip.by.back <- hip.data.by.back.LR %>% 
  dplyr::select(ID, speed, hip_ACC_by_back_LR)

names(back.by.back)[3] <- "back_ACC_by_back_LR"
names(hip.by.hip)[3]   <- "hip_ACC_by_hip_LR"

back.by.back <- back.by.back %>% 
  spread(key = speed, value = back_ACC_by_back_LR) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = back_ACC_by_back_LR
  )

hip.by.hip <- hip.by.hip %>% 
  spread(key = speed, value = hip_ACC_by_hip_LR) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = hip_ACC_by_hip_LR
  )

back.by.hip <- back.by.hip %>% 
  spread(key = speed, value = back_ACC_by_hip_LR) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = back_ACC_by_hip_LR
  )

hip.by.back <- hip.by.back %>%
  spread(key = speed, value = hip_ACC_by_back_LR) %>%
  na.omit() %>%
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = hip_ACC_by_back_LR
  )

# Get measured pRGRF value as average of back and hip
back.measured.pRGRF <- back %>% 
  dplyr::select(c(ID, speed, pRGRF_N)) %>% 
  spread(key = speed, value = pRGRF_N) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = back_measured_pRGRF
  )

hip.measured.pRGRF <- hip %>% 
  dplyr::select(c(ID, speed, pRGRF_N)) %>% 
  spread(key = speed, value = pRGRF_N) %>% 
  na.omit() %>% 
  gather(
    `2`, `3`, `4`, `5`, `6`,
    key = speed,
    value = hip_measured_pRGRF
  )

measured.pRGRF <- back.measured.pRGRF %>% 
  left_join(hip.measured.pRGRF, by = c("ID", "speed"))

measured.pRGRF$measured <- NA
for (i in 1:nrow(measured.pRGRF)) {
  measured.pRGRF$measured[i] <- mean(
    c(
      measured.pRGRF$back_measured_pRGRF[i], 
      measured.pRGRF$hip_measured_pRGRF[i]
    )
  )
}

measured.pRGRF <- dplyr::select(measured.pRGRF, c(ID, speed, measured))

# Merge all dataframes
all.pRGRF <- join_all(
  list(measured.pRGRF, back.by.back, hip.by.hip, back.by.hip, hip.by.back),
  by = c("ID", "speed"),
  type = "left"
) %>% 
  as.tibble() %>% 
  gather(
    measured, back_ACC_by_back_LR, hip_ACC_by_hip_LR, back_ACC_by_hip_LR, hip_ACC_by_back_LR,
    key = "group",
    value = "pRGRF"
  )
all.pRGRF$speed <- as.factor(all.pRGRF$speed)
all.pRGRF$group <- as.factor(all.pRGRF$group)

# **** Mixed model analysis -----------------------------------------------

baseline <- lme(
  fixed = pRGRF ~ 1,
  random = ~ 1 | ID/speed/group,
  method = "ML",
  data = all.pRGRF
)

group.model <- lme(
  fixed = pRGRF ~ group,
  random = ~ 1 | ID/speed/group,
  method = "ML",
  data = all.pRGRF
)

interact.model <- lme(
  fixed = pRGRF ~ group * speed,
  random = ~ 1 | ID/speed/group,
  method = "ML",
  data = all.pRGRF
)

anova(baseline, group.model, interact.model)

posthoc <- lsmeans(interact.model, pairwise ~ group * speed, adjust = "tukey")

# ** Graph ----------------------------------------------------------------
plot.df <- all.pRGRF
plot.df$speed <- as.numeric(plot.df$speed)

pRGRF_by_group <- ggplot(data = plot.df, aes(x = speed, y = pRGRF, colour = group)) + 
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(0.5)) +
  stat_summary(fun.y = mean, geom = "line", position = position_dodge(0.5)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, position = position_dodge(0.5)) +
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
  scale_y_continuous(breaks = seq(from = 800, to = 1500, by = 100)) +
  expand_limits(y = c(800, 1500)) +
  ggtitle("Figure 1")


# ** pRGRF means by group -------------------------------------------------

pRGRF_mean_by_group <- all.pRGRF %>% 
  group_by(group, speed) %>% 
  dplyr::summarise(mean = mean(pRGRF)) %>% 
  spread(key = group, value = mean)
# Reorder
pRGRF_mean_by_group <- pRGRF_mean_by_group[, c(1, 6, 2, 5, 3, 4)]