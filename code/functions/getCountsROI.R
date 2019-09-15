getCountsROI <- function(ID, time1, time2, time3, time4, time5, time6) {
  # Selects a 30 seconds region of interest for each speed
  # Does it for back and hip counts
  # Gets vertical and resultant counts
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
  
  counts.back <- read.delim(
    file = paste(datapath, "counts_back/ID_0", ID, ".csv", sep = ""),
    skip = 10,
    sep  = ";"
  )
  
  counts.hip <- read.delim(
    file = paste(datapath, "counts_hip/ID_0", ID, ".csv", sep = ""),
    skip = 10,
    sep  = ";"
  )
  
  counts.back <- counts.back %>%
    select(Time, Axis1, Vector.Magnitude) %>%
    rename(time = Time, counts_vertical = Axis1, counts_resultant = Vector.Magnitude)
  
  counts.hip <- counts.hip %>%
    select(Time, Axis1, Vector.Magnitude) %>%
    rename(time = Time, counts_vertical = Axis1, counts_resultant = Vector.Magnitude)
  
  # Speed 1 (standing rest)
  start1  <- which(counts.back$time == time1) + 6 # adds 6 to change from 90s to 30s ROI
  end1    <- start1 + 5
  bSpeed1 <- counts.back[start1:end1, ]
  hSpeed1 <- counts.hip[start1:end1, ]
  bSpeed1$speed <- 1
  hSpeed1$speed <- 1
  
  # Speed 2
  if (time2 == FALSE) {
    bSpeed2 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(bSpeed1)))
    hSpeed2 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(hSpeed1)))
    names(bSpeed2) <- names(bSpeed1)
    names(hSpeed2) <- names(hSpeed1)
    } else {
      start2  <- which(counts.back$time == time2) + 12 # adds 12 to change from 90s to 30s ROI
      end2    <- start2 + 5
      bSpeed2 <- counts.back[start2:end2, ]
      hSpeed2 <- counts.hip[start2:end2, ]
      bSpeed2$speed <- 2
      hSpeed2$speed <- 2
    }
  
  # Speed 3
  if (time3 == FALSE) {
    bSpeed3 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(bSpeed1)))
    hSpeed3 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(hSpeed1)))
    names(bSpeed3) <- names(bSpeed1)
    names(hSpeed3) <- names(hSpeed1)
  } else {
    start3  <- which(counts.back$time == time3) + 12 # adds 12 to change from 90s to 30s ROI
    end3    <- start3 + 5
    bSpeed3 <- counts.back[start3:end3, ]
    hSpeed3 <- counts.hip[start3:end3, ]
    bSpeed3$speed <- 3
    hSpeed3$speed <- 3
  }
  
  # Speed 4
  if (time4 == FALSE) {
    bSpeed4 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(bSpeed1)))
    hSpeed4 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(hSpeed1)))
    names(bSpeed4) <- names(bSpeed1)
    names(hSpeed4) <- names(hSpeed1)
  } else {
    start4  <- which(counts.back$time == time4) + 12 # adds 12 to change from 90s to 30s ROI
    end4    <- start4 + 5
    bSpeed4 <- counts.back[start4:end4, ]
    hSpeed4 <- counts.hip[start4:end4, ]
    bSpeed4$speed <- 4
    hSpeed4$speed <- 4
  }
  
  # Speed 5
  if (time5 == FALSE) {
    bSpeed5 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(bSpeed1)))
    hSpeed5 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(hSpeed1)))
    names(bSpeed5) <- names(bSpeed1)
    names(hSpeed5) <- names(hSpeed1)
  } else {
    start5  <- which(counts.back$time == time5) + 12 # adds 12 to change from 90s to 30s ROI
    end5    <- start5 + 5
    bSpeed5 <- counts.back[start5:end5, ]
    hSpeed5 <- counts.hip[start5:end5, ]
    bSpeed5$speed <- 5
    hSpeed5$speed <- 5
  }
  
  # Speed 6
  if (time6 == FALSE) {
    bSpeed6 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(bSpeed1)))
    hSpeed6 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(hSpeed1)))
    names(bSpeed6) <- names(bSpeed1)
    names(hSpeed6) <- names(hSpeed1)
  } else {
    start6  <- which(counts.back$time == time6) + 12 # adds 12 to change from 90s to 30s ROI
    end6    <- start6 + 5
    bSpeed6 <- counts.back[start6:end6, ]
    hSpeed6 <- counts.hip[start6:end6, ]
    bSpeed6$speed <- 6
    hSpeed6$speed <- 6
  }
  
  bSpeeds <- 
    rbind(bSpeed1, bSpeed2, bSpeed3, bSpeed4, bSpeed5, bSpeed6) %>%
    na.omit()
  bSpeeds$ID <- ID
  
  hSpeeds <- 
    rbind(hSpeed1, hSpeed2, hSpeed3, hSpeed4, hSpeed5, hSpeed6) %>%
    na.omit()
  hSpeeds$ID <- ID
  
  dir.create(paste(datapath, "counts_back/output_30secROI", sep = ""), showWarnings = FALSE)
  dir.create(paste(datapath, "counts_hip/output_30secROI", sep = ""), showWarnings = FALSE)
  
  write.csv(
    bSpeeds,
    file = paste(datapath, "counts_back/output_30secROI/ID_0", ID, "_counts_30secROI.csv", sep = ""),
    row.names = FALSE
  )
  
  write.csv(
    hSpeeds,
    file = paste(datapath, "counts_hip/output_30secROI/ID_0", ID, "_counts_30secROI.csv", sep = ""),
    row.names = FALSE
  )
}