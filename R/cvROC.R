cvROC <- function(df, ID) {
  # Cross validates the model, separating sample in:
  #  training dataset: used to build the model
  #  testing dataset: used to predict the model
  #
  # Args:
  #   df: a dataframe containing data used to build ROC curves
  #   ID: subject ID number to be assigned to testing dataset
  #
  # Returns:
  #   A dataframe containing testing dataset predictions
  
  df <- na.omit(df)
  
  training <- df[which(df$ID != ID), ]
  testing  <- df[which(df$ID == ID), ]
  
  # Builds a ROC curve for each of the intensity categories using the training dataset
  ROC.mod.cv    <- roc(response = training$MOD_MET_CAT, predictor = training$steps, ci = TRUE, na.rm = TRUE)
  ROC.vig.cv    <- roc(response = training$VIG_MET_CAT, predictor = training$steps, ci = TRUE, na.rm = TRUE)
  cp.ROC.mod.cv <- coords(ROC.mod.cv, x = "best", best.method = "closest.topleft")
  cp.ROC.vig.cv <- coords(ROC.vig.cv, x = "best", best.method = "closest.topleft")
  
  # Creates categorical variables for moderate and vigorous physical activity 
  # instensities based on steps values in the testing dataset
  # Creates and names the variables
  testing$MOD_steps_CAT <- 0
  testing$VIG_steps_CAT <- 0

  # Fills the variables
  for (i in 1:nrow(testing)) {
    # 1 = TRUE; 0 = FALSE
    # Moderate
    if (testing$steps[i] >= cp.ROC.mod.cv[1] &
        testing$steps[i] <  cp.ROC.vig.cv[1]) {
      testing$MOD_steps_CAT[i] <- 1 
    } else {testing$MOD_steps_CAT[i] <- 0}
    # Vigorous
    if (testing$steps[i] >= round(cp.ROC.vig[1], digits = 0)) {
      testing$VIG_steps_CAT[i] <- 1
    } else {testing$VIG_steps_CAT[i] <- 0}
  }
  return(testing)
}