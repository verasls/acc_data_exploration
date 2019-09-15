cvMixedModel <- function(df, ID) {
  # Cross validates the model, separating sample in:
  #  training dataset: used to build the model
  #  testing dataset: used to predict the model
  #
  # Args:
  #   df: dataframe containing data used to build and test the models
  #   ID: subject ID number to be assigned to testing dataset
  #   fix.eff, rand.eff: these mixed model parameters need to be assigned
  # outside of function call
  #
  # Returns:
  #   A dataframe containing testing dataset predictions
  
  require(nlme)
  
  df <- na.omit(df)
  
  training <- df[which(df$ID != ID), ]
  testing  <- df[which(df$ID == ID), ]
  
  cv.MM <- lme(
    fixed = fix.eff,
    random = rand.eff,
    method = "ML",
    correlation = corAR1(),
    data = training
  )

  testing$predicted <- predict(object = cv.MM, newdata = testing, level = 0)
  names(testing)[ncol(testing)] <- paste(fix.eff[[2]], "_predicted", sep = "")

  return(testing)
}