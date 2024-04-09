library(tidyverse)
library(HH)
library(dplyr)
library(caret)
library(glmnet)
library(e1071)
library(rpart)
library(partykit)

if(Sys.info()["user"] == "jaure"){
  in.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/datasets")
  out.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/outputs")
  fig.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/outputs/training/figures")
}else if(Sys.info()["user"] == "jaure"){
  in.dir <- file.path("")
  out.dir <- file.path("")
  fig.dir <- file.path("")
}

setwd(paste(in.dir, "2022", sep = "/"))
df22 <- read.csv("combined_densiplant_dataset_2022_rgba_rgbg.csv", header = T)
df22 <- df22[,-1]
df22[is.na(df22)] <- 0
colnames(df22)[colnames(df22) == "Date"] <- "date"
df22$date <- as.Date(df22$date)
colnames(df22)[colnames(df22) == "count_mean"] <- "Density"


haun <- read.csv("Plant_Haun_phenology_summary_2022.csv", header = T)


df22 <- df22 %>% 
  merge(haun, by = "date")

#df22 <- df22[df22$Sowing_density != 560,]
setwd(paste(out.dir, "training", "ML_results", sep = "/"))

#### 2022 model selection ###

df_model <- read.csv("performance_R_PCR_4-fold_10repeatedcv.csv", header = TRUE)
df_model[is.na(df_model)] <- 0
df_model$date <- as.Date(df_model$date)
df_model <- df_model[,-1]

df_model <- df_model %>% 
  merge(haun, by = "date") %>% 
  mutate(model_ID =  paste(Haun, model, altitude))


selected_models <- c("5.7 - 5.9 Ridge ground",
                     "5.7 - 5.9 Ridge 15m",
                     "5.7 - 5.9 Ridge 30m",
                     "7.1 - 7.3 Ridge 50m",
                     "5.7 - 5.9 PCR 15m",
                     "5.7 - 5.9 PCR 30m",
                     "7.1 - 7.3 PCR 50m")

df_model <- df_model[df_model$model_ID %in% selected_models,]


## Load test 2023 data ##

setwd(paste(in.dir, "2023", sep = "/"))

df23 <- read.csv("combined_densiplant_dataset_2023_rgba_rgbg.csv", header = T)
df23 <- df23[,-1]
df23[is.na(df23)] <- 0
colnames(df23)[colnames(df23) == "Date"] <- "date"
df23$date <- as.Date(df23$date)
colnames(df23)[colnames(df23) == "count_mean"] <- "Density"

haun <- read.csv("Plant_Haun_phenology_summary_2023.csv", header = T)

df23 <- df23 %>% 
  merge(haun, by = "date")

# df23 <- df23[df23$Sowing_density != 35 & df23$Sowing_density != 70,]
# 
# # Remove outlier
# df23 <- df23[df23$Image.Name != "R-001_Plot-003", ]


pred_df <- NULL

for(ID in unique(df_model$model_ID)){
  
  print(ID)
  
  subset <- df_model[df_model$model_ID == ID,]
  stage <- subset$Haun
  alt <- subset$altitude
  model <- subset$model
  PC <- subset$PC
  
  train_data <- df22[df22$Haun == stage & df22$Altitude == alt, ]
  
  train_data <- dplyr::select(train_data, -Image.Name, -date, -Altitude, -Rows, -Plots, -Genotype, -Haun) 
  
  train_data_nodens <- dplyr::select(train_data, -Density, -Sowing_density)
  train_data_dens <- dplyr::select(train_data, -Sowing_density)
  
  ####  1. Ridge Regression ####
  
  # Data pre-processing ridge regression
  
  preprocessParams <- predict(preProcess(method= c("center", "scale"), train_data_nodens),  train_data_nodens)
  
  r_data <- data.frame(Density = train_data_dens$Density, preprocessParams)
  
  if(model == "Ridge"){
    
    param_string <- subset$tuning
    param_parts <- strsplit(gsub(" ", "", param_string), "=")[[1]]
    param_name <- param_parts[1]
    param_value <- as.numeric(param_parts[2])
    
    trained_model <- train(
      Density ~ .,
      data = r_data,
      method = "glmnet",
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 10),
      tuneGrid = expand.grid(alpha = 1, lambda = param_value)
      
    )
  }
  
  ####  2. Principal Component Regression ####
  
  pca <- prcomp(train_data_nodens, center = T, scale. = T)
  eigenval <- pca$sdev ^ 2
  
  PC_selected <- length(eigenval[eigenval > 1])
  
  train_data_pc <- data.frame(Density = train_data_dens$Density, pca$x[,1:PC_selected])
  
  if(model == "PCR"){
    
    trained_model <- train(
      Density ~ .,  
      data = train_data_pc,  
      method = "lm",  
      trControl = trainControl(method = "repeatedcv", number = 4, repeats = 10),  
    )
  }
  

  
  for(test_stage in unique(df23$Haun)){
    
    test_subset <- df23[df23$Haun == test_stage, ]
    
    for(test_altitude in unique(test_subset$Altitude)){
      
      test_data <- test_subset[test_subset$Altitude == test_altitude,]
      
      print(paste(unique(test_data$date), unique(test_data$Altitude), sep ="_"))
      
      test_data <- dplyr::select(test_data, -Image.Name, -date, -Altitude, -Rows, -Plots, -Genotype, -Haun) 
      
      test_data_nodens <- dplyr::select(test_data, -Density, -Sowing_density)
      test_data_dens <- dplyr::select(test_data,-Sowing_density)
      
      pca <- prcomp(test_data_nodens, center = T, scale. = T)
      
      test_data_pc <- data.frame(Density = test_data$Density, pca$x[,1:PC_selected])
      test_data_pc_nodens <-  dplyr::select(test_data_pc,-Density)
      
      if(model == "PCR"){
        
        # Predicted vs actual density
        
        pred <- predict(trained_model, newdata = test_data_pc_nodens)
        
      }
      
      if(model == "Ridge"){
        preprocessParams <- predict(preProcess(method= c("center", "scale"), test_data_nodens),  test_data_nodens)
        r_test_data <- data.frame(Density = test_data$Density, preprocessParams)
        r_test_data_nodens <- dplyr::select(r_test_data, -Density)
        pred <- predict(trained_model, newdata = r_test_data_nodens)
      }
      
      
      # Predicted vs actual density
      
      pred_vs_dens <- data.frame(Prediction = pred, Actual_density = test_data$Density, Sowing_density = test_data$Sowing_density)
      
      pred_error <- as.data.frame(t(postResample(pred, test_data$Density)))
      
      pred_vs_dens$RMSE <- pred_error$RMSE
      pred_vs_dens$Rsquared <- pred_error$Rsquared
      pred_vs_dens$MAE <- pred_error$MAE
      
      pred_vs_dens$model_ID <- ID
      pred_vs_dens$altitude_test <- test_altitude
      pred_vs_dens$date_test <- unique(test_data$date)
      pred_vs_dens$Haun_test <- test_stage
      
      
      pred_df <- rbind(pred_df, pred_vs_dens)
      
    }
    
  }
  
}

setwd(paste(out.dir, "testing", "ML_results", sep = "/"))
write.csv(pred_df, "2023_testing_R_PCR.csv")
