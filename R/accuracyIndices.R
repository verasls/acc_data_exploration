accuracyIndices <- function(df, predicted.var, dependent.var) {
  # Computes multiple measurement indices of accuracy (MMIA)
  #
  # Args:
  #   df: dataframe containing the data used to compute the indices
  #   predicted.var: predicted variable name (as character)
  #   dependent.var: dependent variable name (as character)
  #
  # Returns:
  #   A dataframe containing the MMIA values
  
  df <- as.data.frame(df)
  
  # Absolute values
  bias.mean <- round(mean(df[ , dependent.var] - df[ , predicted.var]), digits = 2)
  bias.sd   <- round(sd(df[ , dependent.var] - df[ , predicted.var]), digits = 2)
  LoA.inf   <- round(bias.mean - (1.96 * bias.sd), digits = 2)
  LoA.sup   <- round(bias.mean + (1.96 * bias.sd), digits = 2)
  MAE       <- round(mean(abs(df[ , dependent.var] - df[ , predicted.var])), digits = 2)
  MAE.sd    <- round(sd(abs(df[ , dependent.var] - df[ , predicted.var])), digits = 2)
  RMSE      <- round(sqrt(mean((df[ , dependent.var] - df[ , predicted.var])^2)), digits = 2)
  
  # Percent values
  bias.mean.p <- round(mean(((df[ , dependent.var] - df[ , predicted.var]) / df[ , dependent.var])) * 100, digits = 2)
  bias.sd.p   <- round(sd(((df[ , dependent.var] - df[ , predicted.var]) / df[ , dependent.var])) * 100, digits = 2)
  LoA.inf.p   <- round(bias.mean.p - (1.96 * bias.sd.p), digits = 2)
  LoA.sup.p   <- round(bias.mean.p + (1.96 * bias.sd.p), digits = 2)
  MAPE        <- round(mean(abs((df[ , dependent.var] - df[ , predicted.var]) / df[ , dependent.var])) * 100, digits = 2)
  MAPE.sd     <- round(sd(abs((df[ , dependent.var] - df[ , predicted.var]) / df[ , dependent.var])) * 100, digits = 2)
  CV.RMSE     <- round((RMSE / mean(df[ , dependent.var])) * 100, digits = 2)
  
  accuracy <- data.frame(
    bias.mean, bias.sd, LoA.inf, LoA.sup, MAE, MAE.sd, RMSE, 
    bias.mean.p, bias.sd.p, LoA.inf.p, LoA.sup.p, MAPE, MAPE.sd, CV.RMSE
  )
  return(accuracy)
}