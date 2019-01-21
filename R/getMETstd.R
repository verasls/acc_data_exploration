getMETstd <- function(df, ID) {
  # Computes MET values for a single ID based on standard MET value
  # (3.5 mlO2/kg/min)
  # 
  # Args:
  #   df: a dataframe containing each ID VO2/kg value 
  #   ID: subject ID
  #
  # Returns:
  #   A dataframe adding a colunm with computed MET values to the base dataframe
  
  D <- df[which(df$ID == ID), ]
  D$MET_std <- NA
  
  for (i in 1:nrow(D)) {
    D$MET_std[i] <- D$VO2.kg[i] / 3.5
  }
  return(D)
}