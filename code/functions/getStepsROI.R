getStepsROI <- function(ID, time1, time2, time3, time4, time5, time6) {
  # Selects a 30 seconds region of interest for each speed
  # Does it for hip steps
  #
  # Args:
  #   ID: subject ID number
  #   time1-6: start time for each speed region of interest
  # obs: if an ID does not have determined speed, timex = FALSE
  #
  # Returns:
  #   A csv file containing only the selected region of interest and its respective speed
  
  require(tidyverse)
  
  datapath = "/Volumes/LV_HD/Accelerometry/PAI_ACC/projects/ECSS_2019/"
  
  steps.hip <- read.delim(
    file = paste(datapath, "counts_hip/ID_0", ID, ".csv", sep = ""),
    skip = 10,
    sep  = ";"
  )
  
  steps.hip <- steps.hip %>%
    select(Time, Steps) %>%
    rename(time = Time, steps = Steps)
  
  # Speed 1 (standing rest)
  start1  <- which(steps.hip$time == time1) + 6 # adds 6 to change from 90s to 30s ROI
  end1    <- start1 + 5
  hSpeed1 <- steps.hip[start1:end1, ]
  hSpeed1$speed <- 1
  
  # Speed 2
  if (time2 == FALSE) {
    hSpeed2 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(hSpeed1)))
    names(hSpeed2) <- names(hSpeed1)
  } else {
    start2  <- which(steps.hip$time == time2) + 12 # adds 12 to change from 90s to 30s ROI
    end2    <- start2 + 5
    hSpeed2 <- steps.hip[start2:end2, ]
    hSpeed2$speed <- 2
  }
  
  # Speed 3
  if (time3 == FALSE) {
    hSpeed3 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(hSpeed1)))
    names(hSpeed3) <- names(hSpeed1)
  } else {
    start3  <- which(steps.hip$time == time3) + 12 # adds 12 to change from 90s to 30s ROI
    end3    <- start3 + 5
    hSpeed3 <- steps.hip[start3:end3, ]
    hSpeed3$speed <- 3
  }
  
  # Speed 4
  if (time4 == FALSE) {
    hSpeed4 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(hSpeed1)))
    names(hSpeed4) <- names(hSpeed1)
  } else {
    start4  <- which(steps.hip$time == time4) + 12 # adds 12 to change from 90s to 40s ROI
    end4    <- start4 + 5
    hSpeed4 <- steps.hip[start4:end4, ]
    hSpeed4$speed <- 4
  }
  
  # Speed 5
  if (time5 == FALSE) {
    hSpeed5 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(hSpeed1)))
    names(hSpeed5) <- names(hSpeed1)
  } else {
    start5  <- which(steps.hip$time == time5) + 12 # adds 12 to change from 90s to 50s ROI
    end5    <- start5 + 5
    hSpeed5 <- steps.hip[start5:end5, ]
    hSpeed5$speed <- 5
  }
  
  # Speed 6
  if (time6 == FALSE) {
    hSpeed6 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(hSpeed1)))
    names(hSpeed6) <- names(hSpeed1)
  } else {
    start6  <- which(steps.hip$time == time6) + 12 # adds 12 to change from 90s to 60s ROI
    end6    <- start6 + 5
    hSpeed6 <- steps.hip[start6:end6, ]
    hSpeed6$speed <- 6
  }
  
  hSpeeds <- 
    rbind(hSpeed1, hSpeed2, hSpeed3, hSpeed4, hSpeed5, hSpeed6) %>%
    na.omit()
  hSpeeds$ID <- ID
  
  dir.create(paste(datapath, "steps_hip", sep = ""), showWarnings = FALSE)
  dir.create(paste(datapath, "steps_hip/output_30secROI", sep = ""), showWarnings = FALSE)
  
  write.csv(
    hSpeeds,
    file = paste(datapath, "steps_hip/output_30secROI/ID_0", ID, "_steps_30secROI.csv", sep = ""),
    row.names = FALSE
  )
}