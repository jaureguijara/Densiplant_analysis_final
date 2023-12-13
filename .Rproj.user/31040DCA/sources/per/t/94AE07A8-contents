library(dplyr)
library(tidyverse)
library(caret)
library(glmnet)
library(e1071)
library(rpart)
library(partykit)

# library(tidyr)
#library(broom)
#library(PairedData)
# library(car)
# library(Hmisc)
# library(stringr)
#library(corrplot)
# library(ggfortify)
# library(partykit)
# library(caret)
# library(rattle)
# library(rpart)

if(Sys.info()["user"] == "jaure"){
in.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/datasets/2022")
out.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/outputs/training/ML_results")
fig.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/outputs/training/figures")
}else if(Sys.info()["user"] == "jaure"){
  in.dir <- file.path("")
  out.dir <- file.path("")
  fig.dir <- file.path("")
}

setwd(in.dir)

df <- read.csv("combined_densiplant_dataset_2022_rgba_rgbg.csv", header = T)
df <- df[,-1]

df$Sowing_density<- as.character(df$Sowing_density)
colnames(df)[colnames(df) == "count_mean"] <- "Density"
df <- relocate(df, Sowing_density, .after = Plots) %>% 
  relocate(Density, .after = Sowing_density)


#### With 4-fold 10 repeated CV #### 

set.seed(12) 

data <- df

results_df_final <- NULL
lr_coefficients_df_final <- NULL
pred_df_final <- NULL
  
  
# df <- df[df$Sowing_density != "560",]

lr_coefficients_df <- NULL
results_df <- NULL
pred_df <- NULL


for(i in 1:length(unique(data$Date))){
  current_date <- unique(data$Date)[i]
  
  # Subset data per date 
  
  subset_date <- data[data$Date == current_date,]
  subset_date$Genotype <- as.numeric(as.factor(subset_date$Genotype))
  
  # 1 Separating per date and altitude
  
  for(j in 1:length(unique(subset_date$Altitude))){
    
    current_alt <- unique(subset_date$Altitude)[j]
    
    subset_alt <-  subset_date[subset_date$Altitude == current_alt, ]
    
    model_data <- dplyr::select(subset_alt, -Image.Name, -Date, -Altitude, -Rows, -Plots, -Genotype) 
    
    
    print(paste(current_date, unique(subset_alt$Altitude), sep ="_"))
    
    
    model_data_nodens <- dplyr::select(model_data, -Density, -Sowing_density)
    
    
    ######## 1. MODELS WITH PCA DIMENSIONALITY REDUCTION  ######## 
    
    pca <- prcomp(model_data_nodens, center = T, scale. = T)
    eigenval <- pca$sdev ^ 2
    
    PC_selected <- length(eigenval[eigenval > 1])
    
    model_data_pc <- data.frame(Density = model_data$Density, pca$x[,1:PC_selected])
    
    
    ### Principal Component Regression ### 
    
    model <- train(
      Density ~ .,  
      data = model_data_pc,  
      method = "lm",  
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 10),  
    )
    
    
    # Save Error metrics 
    
    results <- model$results # mean error metrics for the 4 folds of the CV
    
    results$tuning <- paste("intercept =", results$intercept)
    results <- results[,-1]
    results$PC <- PC_selected
    results$model <- "PCR"
    results$altitude <- current_alt
    results$date <- current_date
    
    results_df <- rbind(results_df, results)
    
    
    # Predicted vs actual density
    
    pred <- predict(model, newdata = model_data_pc)
    
    pred_vs_dens <- data.frame(Prediction = pred, Actual_density = model_data$Density, Sowing_density = model_data$Sowing_density)
    
    pred_error <- as.data.frame(t(postResample(pred, model_data$Density)))
    
    pred_vs_dens$RMSE <- pred_error$RMSE
    pred_vs_dens$Rsquared <- pred_error$Rsquared
    pred_vs_dens$MAE <- pred_error$MAE
    
    pred_vs_dens$PC <- PC_selected
    pred_vs_dens$model <- "PCR"
    pred_vs_dens$altitude <- current_alt
    pred_vs_dens$date <- current_date
    
    pred_df <- rbind(pred_df, pred_vs_dens)
    
    ### DECISSION (CLASSIFICATION) CART TREES ### 
    
    model <- train(
      Density ~ .,  
      data = model_data_pc,  
      method = "rpart",  
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 10),  
      tuneGrid = expand.grid(cp=c(0.001)),  
      control = rpart.control(maxdepth = 5, minbucket = 5) # maxdepth = 5 levels maximum, 4 splits max allowed, 
      # minbucket = terminal node must contain at least 5 observations
    )
    
    
    # Save Error metrics 
    

    results <- model$results # mean error metrics for the 4 folds of the CV
    
    
    results$tuning <- paste("cp = ", results$cp)
    results <- results[,-1]
    results$PC <- PC_selected
    results$model <- "CART"
    results$altitude <- current_alt
    results$date <- current_date
    
    results_df <- rbind(results_df, results)
    
    
    # Predicted vs actual density
    
    pred <- predict(model, newdata = model_data_pc)
    
    pred_vs_dens <- data.frame(Prediction = pred, Actual_density = model_data$Density, Sowing_density = model_data$Sowing_density)
    
    pred_error <- as.data.frame(t(postResample(pred, model_data$Density)))
    
    pred_vs_dens$RMSE <- pred_error$RMSE
    pred_vs_dens$Rsquared <- pred_error$Rsquared
    pred_vs_dens$MAE <- pred_error$MAE
    
    pred_vs_dens$PC <- PC_selected
    pred_vs_dens$model <- "CART"
    pred_vs_dens$altitude <- current_alt
    pred_vs_dens$date <- current_date
    
    pred_df <- rbind(pred_df, pred_vs_dens)
    
    
    ### CONDITIONAL INFERENCE (ctree) TREES ###
    
    
    model <- train(
      Density ~ .,  
      data = model_data_pc,  
      method = "ctree",  
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 10),  
      tuneGrid = expand.grid(mincriterion=c(0.05))
    )
    
    
    # Save Error metrics 
    
    results <- model$results # mean error metrics for the 4 folds of the CV
    
    results$tuning <- paste("mincriterion = ", results$mincriterion)
    results <- results[,-1]
    results$PC <- PC_selected
    results$model <- "CIT"
    results$altitude <- current_alt
    results$date <- current_date
    
    
    results_df <- rbind(results_df, results)
    
    
    # Predicted vs actual density
    
    pred <- predict(model, newdata = model_data_pc)
    
    pred_vs_dens <- data.frame(Prediction = pred, Actual_density = model_data$Density, Sowing_density = model_data$Sowing_density)
    
    pred_error <- as.data.frame(t(postResample(pred, model_data$Density)))
    
    pred_vs_dens$RMSE <- pred_error$RMSE
    pred_vs_dens$Rsquared <- pred_error$Rsquared
    pred_vs_dens$MAE <- pred_error$MAE
    
    pred_vs_dens$PC <- PC_selected
    pred_vs_dens$model <- "CIT"
    pred_vs_dens$altitude <- current_alt
    pred_vs_dens$date <- current_date
    
    pred_df <- rbind(pred_df, pred_vs_dens)
    
    
    ### RANDOM FOREST ###
    
    model <- train(
      Density ~ .,  
      data = model_data_pc,  
      method = "rf",  # 500 trees as caret default
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 10),  
      proximity = TRUE, 
      importance = TRUE
    )
    
    
    # Save Error metrics 
    
    results <- model$results 
    results <- results[results$MAE == min(results$MAE),]
    
    results$tuning <- paste("mtry = ", results$mtry)
    results <- results[,-1]
    results$PC <- PC_selected
    results$model <- "RF"
    results$altitude <- current_alt
    results$date <- current_date
    
    
    results_df <- rbind(results_df, results)
    
    # Predicted vs actual density
    
    pred <- predict(model, newdata = model_data_pc)
    
    pred_vs_dens <- data.frame(Prediction = pred, Actual_density = model_data$Density, Sowing_density = model_data$Sowing_density)
    
    pred_error <- as.data.frame(t(postResample(pred, model_data$Density)))
    
    pred_vs_dens$RMSE <- pred_error$RMSE
    pred_vs_dens$Rsquared <- pred_error$Rsquared
    pred_vs_dens$MAE <- pred_error$MAE
    
    pred_vs_dens$PC <- PC_selected
    pred_vs_dens$model <- "RF"
    pred_vs_dens$altitude <- current_alt
    pred_vs_dens$date <- current_date
    
    pred_df <- rbind(pred_df, pred_vs_dens)
    
    
    ### Support Vector Regression ### 
    
    tuneGrid = expand.grid(C = c(0.25, 0.5, 0.75, 1, 5, 10), sigma = c(0.01, 0.1, 1, 2, 5, 10))
    model <- train(
      Density ~ .,  
      data = model_data_pc,  
      method = "svmRadial",  
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 10, verboseIter = FALSE),
      metric = "MAE", 
      tuneGrid = tuneGrid
      
    )
    
    
    # Save Error metrics 
    
    results <- model$results 
    results <- results[results$C == model$bestTune$C & results$sigma == model$bestTune$sigma,]
    
    results$tuning <- paste("C = ", results$C, ", sigma = ", results$sigma)
    results <- results[, c(-1, -2)]
    results$PC <- PC_selected
    results$model <- "SVR"
    results$altitude <- current_alt
    results$date <- current_date
    
    
    results_df <- rbind(results_df, results)
    
    # Predicted vs actual density
    
    pred <- predict(model, newdata = model_data_pc)
    
    pred_vs_dens <- data.frame(Prediction = pred, Actual_density = model_data$Density, Sowing_density = model_data$Sowing_density)
    
    pred_error <- as.data.frame(t(postResample(pred, model_data$Density)))
    
    pred_vs_dens$RMSE <- pred_error$RMSE
    pred_vs_dens$Rsquared <- pred_error$Rsquared
    pred_vs_dens$MAE <- pred_error$MAE
    
    pred_vs_dens$PC <- PC_selected
    pred_vs_dens$model <- "SVR"
    pred_vs_dens$altitude <- current_alt
    pred_vs_dens$date <- current_date
    
    pred_df <- rbind(pred_df, pred_vs_dens)
    
  
    ######## 2. MODELS WITHOUT DIMENSIONALITY REDUCTION, USING VIs  ######## 
    
    # Data pre-processing for lasso and ridge regression
    
    preprocessParams <- predict(preProcess(method= c("center", "scale"), model_data_nodens),  model_data_nodens)
    
    r_data <- data.frame(Density = model_data$Density, preprocessParams)
    
    
    ### Lasso / Ridge Regression ###
    lambdaGrid <-  c(seq(0.1, 2, by = 0.1) ,  seq(2, 5, 0.5) , seq(5, 100, 1), seq(100, 500, 20))
    
    
    # Lasso
    lasso <- train(
      Density ~ .,
      data = r_data,
      method = "glmnet",
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 10),
      tuneGrid = expand.grid(alpha = 1, lambda = lambdaGrid)
      
    )
    
    # Ridge
    ridge <- train(
      Density ~ .,
      data = r_data,
      method = "glmnet",
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 10),
      tuneGrid = expand.grid(alpha = 0, lambda = lambdaGrid)
      
    )
    
    #To get variable lr_coefficients
    
    lr_coefficients <- data.frame(t(data.frame(
      ridge = as.data.frame.matrix(coef(ridge$finalModel, ridge$finalModel$lambdaOpt)),
      lasso = as.data.frame.matrix(coef(lasso$finalModel, lasso$finalModel$lambdaOpt))) %>%
        dplyr::rename(ridge = s1, lasso = s1.1))) %>% 
      mutate(date = current_date,
             alt = current_alt)
    lr_coefficients <- tibble::rownames_to_column(lr_coefficients, var = "model")
    
    lr_coefficients_df <- rbind(lr_coefficients_df, lr_coefficients)
    
    # Save Error metrics 
    
    lasso_results <- lasso$results 
    lasso_results <- lasso_results[lasso_results$lambda == lasso$finalModel$lambdaOpt,]
    
    lasso_results$tuning <- paste("lambda = ", lasso_results$lambda)
    lasso_results$PC <- 0
    lasso_results$model <- "Lasso"
    lasso_results <- lasso_results[,-c(1,2)]
    
    ridge_results <- ridge$results 
    ridge_results <- ridge_results[ridge_results$lambda == ridge$finalModel$lambdaOpt,]
    
    ridge_results$tuning <- paste("lambda = ", ridge_results$lambda)
    ridge_results$PC <- 0
    ridge_results$model <- "Ridge"
    ridge_results <- ridge_results[,-c(1,2)]
    
    results <- rbind(lasso_results, ridge_results)
    results$altitude <- current_alt
    results$date <- current_date
    


    results_df <- rbind(results_df, results)
    
    # Predicted vs actual density
    
    # Lasso
    
    pred <- predict(lasso, newdata = r_data)
    
    pred_vs_dens <- data.frame(Prediction = pred, Actual_density = model_data$Density, Sowing_density = model_data$Sowing_density)
    
    pred_error <- as.data.frame(t(postResample(pred, model_data$Density)))
    
    pred_vs_dens$RMSE <- pred_error$RMSE
    pred_vs_dens$Rsquared <- pred_error$Rsquared
    pred_vs_dens$MAE <- pred_error$MAE
    
    pred_vs_dens$PC <- PC_selected
    pred_vs_dens$model <- "Lasso"
    pred_vs_dens$altitude <- current_alt
    pred_vs_dens$date <- current_date
    
    pred_df <- rbind(pred_df, pred_vs_dens)
    
    
    # Ridge
    
    pred <- predict(ridge, newdata = r_data)
    
    pred_vs_dens <- data.frame(Prediction = pred, Actual_density = model_data$Density, Sowing_density = model_data$Sowing_density)
    
    pred_error <- as.data.frame(t(postResample(pred, model_data$Density)))
    
    pred_vs_dens$RMSE <- pred_error$RMSE
    pred_vs_dens$Rsquared <- pred_error$Rsquared
    pred_vs_dens$MAE <- pred_error$MAE
    
    pred_vs_dens$PC <- PC_selected
    pred_vs_dens$model <- "Ridge"
    pred_vs_dens$altitude <- current_alt
    pred_vs_dens$date <- current_date
    
    pred_df <- rbind(pred_df, pred_vs_dens)

  }
}

results_df$w560 <- 1
results_df_final <- rbind(results_df_final, results_df)

lr_coefficients_df$w560 <- 1
lr_coefficients_df_final <- rbind(lr_coefficients_df_final, lr_coefficients_df)

pred_df$w560 <- 1
pred_df_final <- rbind(pred_df_final, pred_df)

### SAVE OVERALL DATASET ##
# 
# setwd(out.dir)
# write.csv(results_df_final, "ML_4-fold_10repeatedcv.csv")

