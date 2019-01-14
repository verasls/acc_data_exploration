calculateStepsAvg = function(files.path) {
  # Calculates steps average for each speed
  #
  # Args:
  #   files.path: path to directory containing steps by region of interest
  #
  # Returns:
  #   A csv file containing steps average for each speed for each file in a directory
  
  require(plyr)
  
  files <- list.files(path = files.path, pattern = "csv")
  dir.create(path = paste(files.path, "/means", sep = ""), showWarnings = FALSE)
  
  # Temporary dataframe
  temp.df <- data.frame(speed = numeric(), ID = numeric(), steps = numeric())  
  
  # Reading each file within the range and append them to create one file
  for (i in 1:length(files)) {
    print(paste("Calculating steps means for file ", i, " out of ", length(files), sep = ""))
    current.df <- read.csv(paste(files.path, "/", files[i], sep = "")) %>%
      select(-time) %>%
      ddply(
        .variables = "speed",
        .fun = function(x) {
          ID <- unique(x$ID)
          steps <- round(mean(x$steps), digits = 0)
          avg <- data.frame(ID, steps)
        }
      )
    
    temp.df <- rbind(temp.df, current.df)
  }
  
  temp.df <- temp.df[, c(2, 1, 3)]
  
  write.csv(
    temp.df,
    file = paste(files.path, "/means/steps_means.csv", sep = ""),
    row.names = FALSE
  )
  
  print("Done")
}