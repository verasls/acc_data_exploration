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
  
  require(pROC)
  
  df <- na.omit(df)
  
  training <- df[which(df$ID != ID), ]
  testing  <- df[which(df$ID == ID), ]
  
  # Builds a ROC curve for MVPA intensity category using the training dataset
  ROC.ind.cv      <- roc(response = training$MVPA_MET_ind_CAT, predictor = training$ENMO, ci = TRUE)
  cp.MVPA.ind.cv  <- coords(ROC.ind.cv, x = "best", best.method = "closest.topleft")
  ROC.std.cv      <- roc(response = training$MVPA_MET_std_CAT, predictor = training$ENMO, ci = TRUE)
  cp.MVPA.std.cv  <- coords(ROC.std.cv, x = "best", best.method = "closest.topleft")
  
  # Creates categorical variables for MVPA intensity category
  # based on ENMO values in the testing dataset
  # Creates and names the variables
  testing$MVPA_MET_ind_by_ENMO_CAT <- 0
  testing$MVPA_MET_std_by_ENMO_CAT <- 0
  
  # Fills the variables
  for (i in 1:nrow(testing)) {
    # 1 = TRUE; 0 = FALSE
    # MET_ind
    if (testing$ENMO[i] >= cp.MVPA.ind.cv[1]) {
      testing$MVPA_MET_ind_by_ENMO_CAT[i] <- 1
    } else {
      testing$MVPA_MET_ind_by_ENMO_CAT[i] <- 0
    }
    # MET_std
    if (testing$ENMO[i] >= cp.MVPA.std.cv[1]) {
      testing$MVPA_MET_std_by_ENMO_CAT[i] <- 1
    } else {
      testing$MVPA_MET_std_by_ENMO_CAT[i] <- 0
    }
  }

  return(testing)
}