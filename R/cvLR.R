cvLR <- function(df, ID, formula) {
  # Cross validates the model, separating sample in:
  #  training dataset: used to build the model
  #  testing dataset: used to predict the model
  #
  # Args:
  #   df: dataframe containing data used to build and test the models
  #   ID: subject ID number to be assigned to testing dataset
  #   formula: linear regression formula
  #
  # Returns:
  #   A dataframe containing testing dataset predictions
  
  training <- df[which(df$ID != ID), ]
  testing  <- df[which(df$ID == ID), ]
  
  cv.LR <- lm(formula, data = training)
  testing$predicted <- predict(object = cv.LR, newdata = testing, level = 0)
  names(testing)[ncol(testing)] <- paste(formula[[2]], "_predicted", sep = "")
  
  return(testing)
}