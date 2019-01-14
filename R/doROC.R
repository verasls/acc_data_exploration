doROC <- function(df, model.name, predictor) {
  # Creates categorical variables for sedentary activity (SA) and light, moderate and vigorous physical activity 
  # instensities (PAI) based on MET values
  # Builds a ROC curve and selects a cutpoint based on closest-to-(0,1) criterion for each category
  # Applies a leave-one-out cross validation process (LOOCV)
  # Computes kappa statistics using the LOOCV testing dataset predictions
  # Args:
  #   accelerometer: a character string containing activity monitor condition name
  #   accelerometer metric (defined outside the function call)
  #
  # Returns:
  #   A csv file inside output/ROC directory adding MET based intensity categories to the data/processed file
  #   A csv file inside output/ROC directory containing ROC curves summary
  #   A csv file inside output/ROC/LOOCV directory adding a column with LOOCV testing dataset predictions 
  # to the data/processed file
  #   A csv file inside output/ROC/LOOCV directory containing kappa statistics summary
  
  require(pROC)
  require(irr)
  
  D <- na.omit(df)
  
  # Creates categorical variables for SA and PAI based on MET values
  print("Creating categorical variables for sedentary behaviour and physical activity instensities based on MET values")
  for (i in 1:length(D$MET)) 
  {
    # 1 = TRUE; 0 = FALSE
    # Sedentary
    if (D$MET[i] <= 1.5) 
    {
      D$SED_MET_CAT[i] <- 1
    } else {D$SED_MET_CAT[i] <- 0}
    # Light
    if (D$MET[i] > 1.5 & D$MET[i] < 3) 
    {
      D$LIG_MET_CAT[i] <- 1
    } else {D$LIG_MET_CAT[i] <- 0}
    # Moderate
    if (D$MET[i] >= 3 & D$MET[i] < 6) 
    {
      D$MOD_MET_CAT[i] <- 1
    } else {D$MOD_MET_CAT[i] <- 0}
    # Vigorous
    if (D$MET[i] >= 6) 
    {
      D$VIG_MET_CAT[i] <- 1
    } else {D$VIG_MET_CAT[i] <- 0}
    
    # 1 = Sedentary; 2 = Light; 3 = Moderate; 4 = Vigorous
    if (D$SED_MET_CAT[i] == 1) 
    {
      D$INTENS_MET_CAT[i] <- 1
    } else {
      if (D$LIG_MET_CAT[i] == 1) 
      {
        D$INTENS_MET_CAT[i] <- 2
      } else {
        if (D$MOD_MET_CAT[i] == 1) 
        {
          D$INTENS_MET_CAT[i] <- 3
        } else {
          if (D$VIG_MET_CAT[i] == 1) 
          {
            D$INTENS_MET_CAT[i] <- 4
          }
        }
      }
    }
  }
  
  dir.create("output", showWarnings = FALSE)
  dir.create("output/ROC", showWarnings = FALSE)
  print("Writing csv file containing MET based SB and PAI intensities categories")
  write.csv(
    D, 
    file = paste("output/ROC/", model.name, "__MET_ROC_categories.csv", sep = ""), 
    row.names = FALSE
  )
  
  # Builds a ROC curve for each of the intensity categories
  ROC.sed <- roc(response = D$SED_MET_CAT, predictor = D[ , predictor], data = D, ci = TRUE)
  ROC.mod <- roc(response = D$MOD_MET_CAT, predictor = D[ , predictor], data = D, ci = TRUE)
  ROC.vig <- roc(response = D$VIG_MET_CAT, predictor = D[ , predictor], data = D, ci = TRUE)
  
  # Selects a cutpoint for each of the intensity categories based on closest topleft method
  cp.ROC.sed <- coords(ROC.sed, x = "best", best.method = "closest.topleft")
  cp.ROC.mod <- coords(ROC.mod, x = "best", best.method = "closest.topleft")
  cp.ROC.vig <-  coords(ROC.vig, x = "best", best.method = "closest.topleft")
  
  # Creates and writes a dataframe containing summary statistics for the ROC curves created
  ROC.summary <- data.frame(
    intens.cat  <- c("sedentary", "moderate", "vigorous"),
    threshold   <- c(cp.ROC.sed[1], cp.ROC.mod[1], cp.ROC.vig[1]),
    AUC         <- c(as.numeric(ROC.sed[9]), as.numeric(ROC.mod[9]), as.numeric(ROC.vig[9])),
    AUC.95CI    <- c(capture.output(ROC.sed[16])[2], capture.output(ROC.mod[16])[2], capture.output(ROC.vig[16])[2]),
    sensitivity <- c(cp.ROC.sed[3], cp.ROC.mod[3], cp.ROC.vig[3]),
    specificity <- c(cp.ROC.sed[2], cp.ROC.mod[2], cp.ROC.vig[2])
  )
  
  print("Writing ROC summary file")
  write.csv(
    ROC.summary, 
    file = paste("output/ROC/", model.name, "__MET_by_", predictor, "_ROC_summary.csv", sep = ""), 
    row.names = FALSE
  )
  
  cv <- function(i) {
    # Cross validates the model, separating sample in:
    #  training dataset: used to build the model
    #  testing dataset: used to predict the model
    #
    # Args:
    #   i: subject ID number to be assigned to testing dataset
    #
    # Returns:
    #   A dataframe containing testing dataset predictions
    
    training <- D[which(D$ID != unique(D$ID)[i]), ]
    testing  <- D[which(D$ID == unique(D$ID)[i]), ]
    
    # Builds a ROC curve for each of the intensity categories using the training dataset
    ROC.sed.cv <- roc(response = training$SED_MET_CAT, predictor = training[ , predictor], data = training)
    ROC.mod.cv <- roc(response = training$MOD_MET_CAT, predictor = training[ , predictor], data = training)
    ROC.vig.cv <- roc(response = training$VIG_MET_CAT, predictor = training[ , predictor], data = training)
    
    # Creates categorical variables for sedentary, light, moderate and vigorous physical activity 
    # instensities based on selected accelerometer metric values in the testing dataset
    # Creates and names the variables
    testing$sed_cat_acc <- 0
    names(testing)[length(names(testing))] <- paste("SED_", predictor, "_CAT", sep = "")
    testing$lig_cat_acc <- 0
    names(testing)[length(names(testing))] <- paste("LIG_", predictor, "_CAT", sep = "")
    testing$mod_cat_acc <- 0
    names(testing)[length(names(testing))] <- paste("MOD_", predictor, "_CAT", sep = "")
    testing$vig_cat_acc <- 0
    names(testing)[length(names(testing))] <- paste("VIG_", predictor, "_CAT", sep = "")
    testing$intens_cat_acc <- 0
    names(testing)[length(names(testing))] <- paste("INTENS_", predictor, "_CAT", sep = "")
    # Fills the variables
    for (i in 1:length(testing[ , predictor])) 
    {
      # 1 = TRUE; 0 = FALSE
      # Sedentary
      if (testing[i, predictor] <= cp.ROC.sed[1]) 
      {
        testing[i, paste("SED_", predictor, "_CAT", sep = "")] <- 1
      } else {testing[i, paste("SED_", predictor, "_CAT", sep = "")] <- 0}
      # Light
      if (testing[i, predictor] > cp.ROC.sed[1] & testing[i, predictor] < cp.ROC.mod[1]) 
      {
        testing[i, paste("LIG_", predictor, "_CAT", sep = "")] <- 1
      } else {testing[i, paste("LIG_", predictor, "_CAT", sep = "")] <- 0}
      # Moderate
      if (testing[i, predictor] >= cp.ROC.mod[1] & testing[i, predictor] < cp.ROC.vig[1]) 
      {
        testing[i, paste("MOD_", predictor, "_CAT", sep = "")] <- 1
      } else {testing[i, paste("MOD_", predictor, "_CAT", sep = "")] <- 0}
      # Vigorous
      if (testing[i, predictor] >= cp.ROC.vig[1]) 
      {
        testing[i, paste("VIG_", predictor, "_CAT", sep = "")] <- 1
      } else {testing[i, paste("VIG_", predictor, "_CAT", sep = "")] <- 0}
      
      # 1 = Sedentary; 2 = Light; 3 = Moderate; 4 = Vigorous
      if (testing[i, paste("SED_", predictor, "_CAT", sep = "")] == 1) 
      {
        testing[i, paste("INTENS_", predictor, "_CAT", sep = "")] <- 1
      } else {
        if (testing[i, paste("LIG_", predictor, "_CAT", sep = "")] == 1) 
        {
          testing[i, paste("INTENS_", predictor, "_CAT", sep = "")] <- 2
        } else {
          if (testing[i, paste("MOD_", predictor, "_CAT", sep = "")] == 1) 
          {
            testing[i, paste("INTENS_", predictor, "_CAT", sep = "")] <- 3
          } else {
            if (testing[i, paste("VIG_", predictor, "_CAT", sep = "")] == 1) 
            {
              testing[i, paste("INTENS_", predictor, "_CAT", sep = "")] <- 4
            }
          }
        }
      }
    }
    return(testing)
  }
  
  # Applies cv function to each ID in the whole dataset
  ids <- 1:length(unique(D$ID))
  print("Applying leave-one-out cross validation process")
  loocv <- do.call(rbind, (lapply(ids, cv)))
  
  print("Writing leave-one-out cross validation predictions file")
  dir.create("output/ROC/LOOCV", showWarnings = FALSE)
  write.csv(
    loocv, 
    file = paste("output/ROC/LOOCV/", model.name, "_MET_by_", predictor, "_predictions.csv", sep = ""), 
    row.names = FALSE
  )
  
  # Kappa statistic
  print("Computing kappa statistics")
  K <- read.csv(
    paste("output/ROC/LOOCV/", model.name, "_MET_by_", predictor, "_predictions.csv", sep = ""),
    stringsAsFactors = FALSE
  )
  
  K.sed    <- kappa2(K[, c("SED_MET_CAT", paste("SED_", predictor, "_CAT", sep = ""))])
  K.lig    <- kappa2(K[, c("LIG_MET_CAT", paste("LIG_", predictor, "_CAT", sep = ""))])
  K.mod    <- kappa2(K[, c("MOD_MET_CAT", paste("MOD_", predictor, "_CAT", sep = ""))])
  K.vig    <- kappa2(K[, c("VIG_MET_CAT", paste("VIG_", predictor, "_CAT", sep = ""))])
  K.intens <- kappa2(K[, c("INTENS_MET_CAT", paste("INTENS_", predictor, "_CAT", sep = ""))], weight = "squared")
  
  # Creates and writes a dataframe containing kappa statistics summary
  kappa.summary <- data.frame(
    comparing <- c(
      paste("SED_MET_CAT_vs_SED_", predictor, "_CAT", sep = ""),
      paste("LIG_MET_CAT_vs_LIG_", predictor, "_CAT", sep = ""),
      paste("MOD_MET_CAT_vs_MOD_", predictor, "_CAT", sep = ""),
      paste("VIG_MET_CAT_vs_VIG_", predictor, "_CAT", sep = ""),
      paste("INTENS_MET_CAT_vs_INTES_", predictor, "_CAT", sep = "")
    ),
    kappa <- c(
      as.numeric(K.sed[5]), as.numeric(K.lig[5]), as.numeric(K.mod[5]), 
      as.numeric(K.vig[5]), as.numeric(K.intens[5])
    ),
    type <- c(rep("unweighted", 4), "squared")
  )
  
  print("Writing kappa statistics summary file")
  write.csv(
    kappa.summary,
    file = paste("output/ROC/LOOCV/", model.name, "_MET_by_", predictor, "_kappa_summary.csv", sep = ""),
    row.names = FALSE
  )
  
  print("Done")
}