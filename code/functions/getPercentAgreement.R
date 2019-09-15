getPercentAgreement <- function(df, var1, var2) {
  # Calculates percent agreement of two categorical variables
  #
  # Args:
  #   df: a dataframe containing the categorical variables for comparison
  #   var1, var2: variables to calculate percent agreement
  #
  # Returns:
  #   percent agreement between the two selected variables as an 2-digit
  # numeric value
  
  # Recoding variables
  agreement <- rep(NA, nrow(df))
  for (i in 1:nrow(df)) {
    if (df[i, var1] == df[i, var2]) {
      agreement[i] <- 1
    } else {
      agreement[i] <- 0
    }
  }

  PercentAgreement <- round(mean(agreement) * 100, digits = 2)
  return(PercentAgreement)
}