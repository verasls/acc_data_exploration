library(tidyverse)
source("R/getMET.R")
source("R/doROC.R")
source("R/getMETcategories.R")

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

# Statistical analysis  ---------------------------------------------------

# ** ROC curves -----------------------------------------------------------

hip.df <- getMETcategories(hip.df)

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