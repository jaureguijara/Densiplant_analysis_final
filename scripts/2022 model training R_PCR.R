library(dplyr)
library(tidyverse)
library(caret)
library(glmnet)
library(e1071)
library(rpart)
library(partykit)

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

r_coefficients_df <- NULL
pcr_coefficients_df <- NULL
results_df <- NULL
resamples_df <- NULL
pred_df <- NULL

data <- df

for(i in 1:length(unique(data$Date))){
  current_date <- unique(data$Date)[i]
  
  # Subset data per date 
  
  subset_date <- data[data$Date == current_date,]
  subset_date$Genotype <- as.numeric(as.factor(subset_date$Genotype))
  
  # 1 Separating per date and altitude
  
  for(j in 1:length(unique(subset_date$Altitude))){
    
    current_alt <- unique(subset_date$Altitude)[j]
    
    subset_alt <-  subset_date[subset_date$Altitude == current_alt, ]
    
    model_data <- dplyr::select(subset_alt, -Image.Name, 
                                -Date, -Altitude, -Rows, -Plots, -Genotype) 
    
    
    print(paste(current_date, unique(subset_alt$Altitude), sep ="_"))
    
    
    model_data_nodens <- dplyr::select(model_data, -Density, -Sowing_density)
    
    ####  1. Ridge Regression ####
    
    ### Data pre-processing for ridge regression ###
    
    preprocessParams <- predict(preProcess(method= c("center", "scale"), 
                                           model_data_nodens),  
                                model_data_nodens)
    
    r_data <- data.frame(Density = model_data$Density, preprocessParams)
    r_data_nodens <- dplyr::select(r_data, -Density)
    

    lambdaGrid <-  c(seq(0.1, 2, by = 0.1) ,  seq(2, 5, 0.5) ,
                     seq(5, 100, 1), seq(100, 500, 20))
    
    # Ridge
    ridge <- train(
      Density ~ .,
      data = r_data,
      method = "glmnet",
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 10, 
                               savePredictions = T),
      metric = "MAE",
      tuneGrid = expand.grid(alpha = 0, lambda = lambdaGrid)
      
    )
    
    #To get model coefficients
    
    r_coefficients <- data.frame(t(data.frame(
      ridge = as.data.frame.matrix(coef(ridge$finalModel, 
                                        ridge$finalModel$lambdaOpt))) %>%
        dplyr::rename(ridge = s1))) %>% 
      mutate(date = current_date,
             alt = current_alt, 
             lambda  = ridge$finalModel$lambdaOpt)
    r_coefficients <- tibble::rownames_to_column(r_coefficients, var = "model")
    
    r_coefficients_df <- rbind(r_coefficients_df, r_coefficients)
    
    # Save Error metrics 
    
    ridge_results<- ridge$results 
    ridge_results<- ridge_results[ridge_results$lambda == ridge$finalModel$lambdaOpt,]
    
    ridge_results$tuning <- paste("lambda = ", ridge_results$lambda)
    ridge_results$model <- "Ridge"
    ridge_results<- ridge_results[,-c(1,2)]
    
    ridge_results$altitude <- current_alt
    ridge_results$date <- current_date
    
    
    
    results_df <- rbind(results_df, ridge_results)
    
    # Get resample error metrics

    ridge_resample <- ridge$resample
    ridge_resample$tuning <- ridge_results$tuning
    ridge_resample$model <- "Ridge"
    
    ridge_resample$altitude <- current_alt
    ridge_resample$date <- current_date
    
    resamples_df <- rbind(resamples_df, ridge_resample)
    
    # Predicted vs actual density
    
    model_data$rowIndex <- seq.int(nrow(model_data))
    
    merge_data<- model_data[,c("Sowing_density","Density", "rowIndex")]
    
    # predicted vs actual density
    
    pred <- ridge$pred
    pred <- pred[pred$lambda ==  ridge$finalModel$lambdaOpt,]
    
    pred_vs_dens <- merge(pred, merge_data, by = "rowIndex") %>% 
      mutate(tuning = paste("lambda = ", lambda)) %>% 
      dplyr::select(pred, Density, Sowing_density, tuning, Resample)
    
    pred_vs_dens$RMSE <- ridge_results$RMSE
    pred_vs_dens$Rsquared <- ridge_results$Rsquared
    pred_vs_dens$MAE <- ridge_results$MAE
    
    pred_vs_dens$model <- "Ridge"
    pred_vs_dens$altitude <- current_alt
    pred_vs_dens$date <- current_date
    
    pred_df <- rbind(pred_df, pred_vs_dens)
    
    
    ####  2. Principal Component Regression ####
    
    pca <- prcomp(model_data_nodens, center = T, scale. = T)
    eigenval <- pca$sdev ^ 2
    
    # K-G criterion
    PC_selected <- length(eigenval[eigenval > 1])
    
    model_data_pc <- data.frame(Density = model_data$Density, pca$x[,1:PC_selected])
    model_data_pc_nodens <- dplyr::select(model_data_pc, -Density)
    
    ### Principal Component Regression ### 
    
    pcr <- train(
      Density ~ .,  
      data = model_data_pc,  
      method = "lm",  
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 10, 
                               savePredictions = T),  
      metric = "MAE"
    )
    
    # To get model coefficients
    
    pcr_coefficients <- as.data.frame(t(coef(pcr$finalModel)))
    pcr_coefficients$date <- current_date
    pcr_coefficients$alt <- current_alt
      
    pcr_coefficients$model <- "PCR"
    
    if(length(pcr_coefficients) == 6){
      pcr_coefficients$PC3 <- 0 
      pcr_coefficients$PC4 <- 0 
      
    }
    
    if(length(pcr_coefficients) == 7){
      pcr_coefficients$PC4 <- 0 
      
    }
    
    
    pcr_coefficients_df <- rbind(pcr_coefficients_df, pcr_coefficients)

    
    # Save Error metrics 
    
    pcr_results <- pcr$results

    pcr_results$tuning <- paste("intercept = ", pcr_results$intercept)
    pcr_results$model <- "PCR"
    pcr_results <- pcr_results[,- 1]

    pcr_results$altitude <- current_alt
    pcr_results$date <- current_date



    results_df <- rbind(results_df, pcr_results)

    # Get resample error metrics

    pcr_resample <- pcr$resample
    pcr_resample$tuning <- pcr_results$tuning
    pcr_resample$model <- "PCR"

    pcr_resample$altitude <- current_alt
    pcr_resample$date <- current_date

    resamples_df <- rbind(resamples_df, pcr_resample)


    # predicted vs actual density

    pred <- pcr$pred

    pred_vs_dens <- merge(pred, merge_data, by = "rowIndex") %>%
      mutate(tuning = paste("intercept = ", intercept)) %>% 
      dplyr::select(pred, Density, Sowing_density, tuning, Resample)

    pred_vs_dens$RMSE <- pcr_results$RMSE
    pred_vs_dens$Rsquared <- pcr_results$Rsquared
    pred_vs_dens$MAE <- pcr_results$MAE

    pred_vs_dens$model <- "PCR"
    pred_vs_dens$altitude <- current_alt
    pred_vs_dens$date <- current_date

    pred_df <- rbind(pred_df, pred_vs_dens)
    
    ### 3. Random Forest  ### 
    rf_data <- dplyr::select(model_data, -Sowing_density, -rowIndex)
    rf <- train(
      Density ~ .,  
      data = rf_data,  
      method = "rf",  # 500 trees as caret default
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 10, 
                               savePredictions = T),  
      metric = "MAE",
      proximity = TRUE
    )
    
    # Save Error metrics 
    
    rf_results <- rf$results 
    rf_results <- rf_results[rf_results$mtry == rf$bestTune$mtry,]
    
    rf_results$tuning <- paste("mtry = ", rf_results$mtry)
    rf_results <- rf_results[, c(-1)]
    rf_results$model <- "RF"
    rf_results$altitude <- current_alt
    rf_results$date <- current_date
    
    
    results_df <- rbind(results_df, rf_results)
    
    # Get resample error metrics
    
    rf_resample <- rf$resample
    rf_resample$tuning <- rf_results$tuning
    rf_resample$model <- "RF"
    
    rf_resample$altitude <- current_alt
    rf_resample$date <- current_date
    
    resamples_df <- rbind(resamples_df, rf_resample)
    
    # predicted vs actual density
    
    pred <- rf$pred
    
    pred_vs_dens <- merge(pred, merge_data, by = "rowIndex") %>%
      mutate(tuning =  paste("mtry = ", rf_results$mtry)) %>% 
      dplyr::select(pred, Density, Sowing_density, tuning, Resample)
    
    pred_vs_dens$RMSE <- rf_results$RMSE
    pred_vs_dens$Rsquared <- rf_results$Rsquared
    pred_vs_dens$MAE <- rf_results$MAE
    
    pred_vs_dens$model <- "RF"
    pred_vs_dens$altitude <- current_alt
    pred_vs_dens$date <- current_date
    
    pred_df <- rbind(pred_df, pred_vs_dens)
  }
}


### SAVE OVERALL DATASET ##

setwd(out.dir)


write.csv(results_df, "performance_R_PCR_4-fold_10repeatedcv2.csv")
write.csv(r_coefficients_df, "coefficients_Ridge2.csv")
write.csv(pcr_coefficients_df, "coefficients_pcr2.csv")
write.csv(pred_df, "pred_vs_actual_density_R_PCR_4-fold_10repeatedcv2.csv")
write.csv(resamples_df, "resamples_R_PCR_4-fold_10repeatedcv2.csv")
