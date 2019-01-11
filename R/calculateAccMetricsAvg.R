calculateAccMetricsAvg = function(files.path) {
  # Calculates accelerometer metrics average for each speed
  #
  # Args:
  #   files.path: path to directory containing accelerometer metrics by region of interest
  #
  # Returns:
  #   A csv file containing accelerometer metrics average for each speed for each file in a directory
  
  require(tidyverse)
  
  files <- list.files(path = files.path, pattern = "csv")
  dir.create(path = paste(files.path, "/means", sep = ""), showWarnings = FALSE)
  
  # Temporary dataframe
  example <- read.csv(paste(files.path, "/", files[1], sep = ""))
  temp.df <- data.frame(speed = numeric(), ID = numeric(), counts_vertical = numeric(), counts_resultant = numeric())  
  
  # Reading each file within the range and append them to create one file
  for (i in 1:length(files)) {
    print(paste("Calculating accelerometer metrics means for file ", i, " out of ", length(files), sep = ""))
    current.df <- read.csv(paste(files.path, "/", files[i], sep = "")) %>%
      select(-time) %>%
      ddply(
        .variables = "speed",
        .fun = function(x) {
          ID <- unique(x$ID)
          counts_vertical <- round(mean(x$counts_vertical), digits = 0)
          counts_resultant <- round(mean(x$counts_resultant), digits = 0)
          avg <- data.frame(ID, counts_vertical, counts_resultant)
        }
      )
    
    temp.df <- rbind(temp.df, current.df)
  }
  
  temp.df <- temp.df[, c(2, 1, 3, 4)]
  
  write.csv(
    temp.df,
    file = paste(files.path, "/means/acc_metrics_means.csv", sep = ""),
    row.names = FALSE
  )
  
  print("Done")
}