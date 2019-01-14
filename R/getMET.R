getMET <- function(df, ID) {
  # Computes MET values for a single ID based on first step VO2/kg value
  # Args:
  #   id.num: subject ID
  #
  # Returns:
  #   A dataframe adding a colunm with computed MET values to the base dataframe
  
  D <- df[which(df$ID == ID), ]
  D$MET <- NA
  
  for (i in 1:nrow(D)) {
    D$MET[i] <- D$VO2.kg[i] / D$VO2.kg[1]
  }
  return(D)
}
