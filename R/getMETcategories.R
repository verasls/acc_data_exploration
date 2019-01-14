getMETcategories <- function(df) {
  # Creates categorical variables for sedentary activity (SA) and light, moderate and vigorous physical activity 
  # instensities (PAI) based on MET values
  #
  # Args:
  #   df: dataframe to run analysis
  #
  # Return:
  #   adds a column with computed MET values to imput dataframe
  
  df$SED_MET_CAT    <- NA
  df$LIG_MET_CAT    <- NA
  df$MOD_MET_CAT    <- NA
  df$VIG_MET_CAT    <- NA
  df$INTENS_MET_CAT <- NA
  
  for (i in 1:length(df$MET)) 
  {
    # 1 = TRUE; 0 = FALSE
    # Sedentary
    if (df$MET[i] <= 1.5) 
    {
      df$SED_MET_CAT[i] <- 1
    } else {df$SED_MET_CAT[i] <- 0}
    # Light
    if (df$MET[i] > 1.5 & df$MET[i] < 3) 
    {
      df$LIG_MET_CAT[i] <- 1
    } else {df$LIG_MET_CAT[i] <- 0}
    # Moderate
    if (df$MET[i] >= 3 & df$MET[i] < 6) 
    {
      df$MOD_MET_CAT[i] <- 1
    } else {df$MOD_MET_CAT[i] <- 0}
    # Vigorous
    if (df$MET[i] >= 6) 
    {
      df$VIG_MET_CAT[i] <- 1
    } else {df$VIG_MET_CAT[i] <- 0}
    
    # 1 = Sedentary; 2 = Light; 3 = Moderate; 4 = Vigorous
    if (df$SED_MET_CAT[i] == 1) 
    {
      df$INTENS_MET_CAT[i] <- 1
    } else {
      if (df$LIG_MET_CAT[i] == 1) 
      {
        df$INTENS_MET_CAT[i] <- 2
      } else {
        if (df$MOD_MET_CAT[i] == 1) 
        {
          df$INTENS_MET_CAT[i] <- 3
        } else {
          if (df$VIG_MET_CAT[i] == 1) 
          {
            df$INTENS_MET_CAT[i] <- 4
          }
        }
      }
    }
  }
  return(df)
}