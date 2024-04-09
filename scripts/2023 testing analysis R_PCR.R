library(tidyverse)
library(HH)
library(ggplot2)
library(multcomp)
library(emmeans)

if(Sys.info()["user"] == "jaure"){
  in.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/outputs/testing/ML_results")
  out.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/outputs/testing/analysis")
  fig.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/outputs/testing/figures")
}else if(Sys.info()["user"] == "jaure"){
  in.dir <- file.path("")
  out.dir <- file.path("")
  fig.dir <- file.path("")
}

setwd("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/datasets/2023")

haun <- read.csv("Plant_Haun_phenology_summary_2023.csv", header = T)

setwd(in.dir)

# Load dataset with 35 and 70 sowing density classes 

df2 <- read.csv("2023_testing_R_PCR.csv", header = T)
df2[is.na(df2)] <- 0
df2 <- df2[,-1]


df3 <- df2 %>% 
  dplyr::select(Prediction, Actual_density, Sowing_density, model_ID, 
                altitude_test, Haun_test, MAE, RMSE, Rsquared) %>% 
  mutate(pred_vs_dens = Prediction - Actual_density) %>% 
  group_by(model_ID, Haun_test, altitude_test, Sowing_density) %>% 
  mutate(residuals_sd = sd(pred_vs_dens), 
         residuals_mean = mean(pred_vs_dens), 
         MAPE_perclass = mean(abs((Actual_density-Prediction)/Actual_density)) * 100) %>% 
  ungroup() %>% 
  group_by(model_ID, Haun_test, altitude_test) %>% 
  mutate(MAPE_overall = mean(abs((Actual_density-Prediction)/Actual_density)) * 100, 
         MAPE_sd = sd(MAPE_perclass)) %>% 
  ungroup() %>% 
  dplyr::select(model_ID, altitude_test, Haun_test, Sowing_density, residuals_mean, residuals_sd, 
                MAPE_perclass, MAPE_overall, MAPE_sd, MAE, RMSE, Rsquared) %>%
  distinct()

setwd(out.dir)
write.csv(df3, "performance_w3570.csv")

df35 <- df3[df3$Sowing_density == 35, ]
round(mean(df35$MAPE_perclass),2)
round(sd(df35$MAPE_perclass), 2)

df70 <- df3[df3$Sowing_density == 70, ]
round(mean(df70$MAPE_perclass),2)
round(sd(df70$MAPE_perclass),2)

df140 <- df3[df3$Sowing_density == 140,]
round(mean(df140$MAPE_perclass), 2)
round(sd(df140$MAPE_perclass), 2)

df280 <- df3[df3$Sowing_density == 280,]
round(mean(df280$MAPE_perclass),2)
round(sd(df280$MAPE_perclass),2)

df560 <- df3[df3$Sowing_density == 560,]
round(mean(df560$MAPE_perclass),2)
round(sd(df560$MAPE_perclass),2)

df3_7 <- df3[df3$model_ID == "7.1 - 7.3 Ridge 50m",]
round(mean(df3_7$MAPE_overall), 2)
round(sd(df3_7$MAPE_overall), 2)

# ANOVA for MAPE and residuals using Sowing_density as factor
lm <- aov(MAPE_perclass ~ as.factor(Sowing_density), data = df3)
an <- anova(lm)
an

em <- emmeans(lm, ~  Sowing_density, mode = "kenward-roger", na.rm =TRUE)
p <- 0.05
HSD_pwpm <- pwpm(em, alpha = p, adjust = 'Tukey')
HSD <- cld(em, alpha= p, Letters = letters, sort= F, adjust = 'Tukey')
HSD

# Load dataset without 35 and 70 sowing density classes 
setwd(in.dir)
df <- read.csv("2023_testing_R_PCR_hd.csv", header = T)
df[is.na(df)] <- 0
df <- df[,-1]

df1 <- df %>% 
  dplyr::select(Prediction, Actual_density, Sowing_density, model_ID, 
                altitude_test, Haun_test, MAE, RMSE, Rsquared) %>% 
  mutate(pred_vs_dens = Prediction - Actual_density) %>% 
  group_by(model_ID, Haun_test, altitude_test, Sowing_density) %>% 
  mutate(residuals_sd = sd(pred_vs_dens), 
         residuals_mean = mean(pred_vs_dens), 
         MAPE_perclass = mean(abs((Actual_density-Prediction)/Actual_density)) * 100) %>% 
  ungroup() %>% 
  group_by(model_ID, Haun_test, altitude_test) %>% 
  mutate(MAPE_overall = mean(abs((Actual_density-Prediction)/Actual_density)) * 100, 
         MAPE_sd = sd(MAPE_perclass)) %>% 
  ungroup() %>% 
  dplyr::select(model_ID, altitude_test, Haun_test, Sowing_density, residuals_mean, residuals_sd, 
                MAPE_perclass, MAPE_overall, MAPE_sd, MAE, RMSE, Rsquared) %>%
  distinct()

setwd(out.dir)
write.csv(df1, "performance_wo3570.csv")

df140 <- df1[df1$Sowing_density == 140,]
round(mean(df140$MAPE_perclass), 2)
round(sd(df140$MAPE_perclass), 2)

df280 <- df1[df1$Sowing_density == 280,]
round(mean(df280$MAPE_perclass),2)
round(sd(df280$MAPE_perclass),2)

df560 <- df1[df1$Sowing_density == 560,]
round(mean(df560$MAPE_perclass),2)
round(sd(df560$MAPE_perclass),2)

df7 <- df1[df1$model_ID == "7.1 - 7.3 Ridge 50m",]
round(mean(df7$MAPE_overall), 2)
round(sd(df7$MAPE_overall), 2)

# MAPE comparison between models with and without 35 & 70
t.test(df3$MAPE_overall, df1$MAPE_overall)

# R2 comparison between models with and without 35 & 70
t.test(df3$Rsquared, df1$Rsquared)

# Statistical analysis
lm <- aov(MAPE_overall ~ model_ID+Haun_test+altitude_test + model_ID:Haun_test +
            model_ID:altitude_test + altitude_test:Haun_test, data = df1)
an <- anova(lm)

variance<- an$`Sum Sq`[1]/(an$`Sum Sq`[1] + an$`Sum Sq`[2])

em <- emmeans(lm, ~  model_ID, mode = "kenward-roger", na.rm =TRUE)
p <- 0.05
HSD_pwpm <- pwpm(em, alpha = p, adjust = 'Tukey')
HSD <- cld(em, alpha= p, Letters = letters, sort= F, adjust = 'Tukey')
HSD

setwd(out.dir)
write.csv(HSD, "Model_ID_Tukey.csv")


### Barplot comparison of two datasets ###

df7$densgroup <- "Without 35 and 70"
df3_7$densgroup <- "With 35 and 70"

df7_c <- rbind(df7, df3_7)

df7_c$altitude_test <- factor(df7_c$altitude_test, levels = c("ground","15m", "30m", "50m"))

cbPalette <- c( "#CC79A7","#000000", "#E69F00",  "#009E73", "#0072B2", "#D55E00")

# Calculate mean and sd values for each facet
summary_values <- df7_c %>%
  group_by(densgroup) %>%
  summarise(mean_Rsquared = mean(Rsquared),
            sd_Rsquared = sd(Rsquared),
            mean_MAE = mean(MAE),
            sd_MAE = sd(MAE),
            mean_MAPE = mean(MAPE_overall),
            sd_MAPE = sd(MAPE_overall))

# Merge summary values back into the original data frame
df7_c <- left_join(df7_c, summary_values, by = "densgroup")

# Plotting
p1 <- ggplot(df7_c, aes(x = Haun_test, y = MAPE_overall, fill = altitude_test)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = df7_c, aes(x = Inf, y = Inf,
                                       label = paste(
                                         "mean R-squared: ", round(mean_Rsquared, 2), " (SD: ", round(sd_Rsquared, 2), ")",
                                         "\nmean MAE: ", round(mean_MAE, 2), " (SD: ", round(sd_MAE, 2), ")",
                                         "\nmean MAPE: ", round(mean_MAPE, 2), " (SD: ", round(sd_MAPE, 2), ")"
                                       )),
            vjust = 1.1, hjust = 1.01, size = 4, color = "black", show.legend = FALSE) +
  facet_wrap(densgroup ~ .) +
  labs(fill = "Test Altitude") +
  scale_fill_manual(values = cbPalette) +
  ggtitle(paste(unique(df7$model_ID), " performance in 2023 data")) +
  theme_bw() +
  ylab("Mean Absolute Percentage Error") +
  theme(axis.title = element_text(size = 16),
        plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 0.9, size = 12),
        strip.text = element_text(size = 12)) +
  xlab("2023 Test stage") +
  scale_y_continuous(limits = c(0, 225), breaks = seq(0, 225, 25))

p1
#setwd(fig.dir)
#png(paste(unique(df7$model_ID), ".png", sep ="_"), width = 8, height = 8, units = 'in', res = 300)
#plot(p1)
#dev.off()


#### Visualize over prediction for 35 and 70 classes #####

data <- df[df$model_ID == "7.1 - 7.3 Ridge 50m" & df$Haun_test == "5.6 - 5.9" &
             df$altitude_test == "50m",]
data

data2 <- df2[df2$model_ID == "7.1 - 7.3 Ridge 50m" & df2$Haun_test == "5.6 - 5.9" &
               df2$altitude_test == "50m",]

data$densgroup <- "Without 35 and 70"
data2$densgroup <- "With 35 and 70" 

plot_data <- rbind(data, data2)

cbPalette <- c("#009E73","#000000", "#E69F00",  "#0072B2", "#D55E00", "#CC79A7")

plot_data <- plot_data %>% 
  group_by(densgroup) %>% 
  mutate(MAPE = mean(abs((Actual_density-Prediction)/Actual_density)) * 100) %>% 
  ungroup()

p2 <- ggplot(plot_data, aes(x = Actual_density, y = Prediction, 
                            color = as.factor(Sowing_density), 
                            shape = as.factor(Sowing_density)))+
  facet_wrap(densgroup~.,nrow =2, strip.position = "bottom")+
  geom_point(size = 1)+ 
  scale_color_manual(values= cbPalette)+
  scale_shape_manual(values = c(3,7, 16, 17, 15))+
  geom_abline(lty = 2, col = "black")+ 
  ggtitle(paste(unique(data$model_ID), " tested on 2023 5.6 - 5.9 50m"))+
  xlab("Actual Density")+ 
  ylab("Predicted Density")+
  lims(x = c(0,500), y = c(0,500))+
  labs(color = "Sowing Density", shape = "Sowing Density")+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 10),
    strip.text.x = element_text(size = 10), 
    strip.text.y = element_text(size = 10), 
    
    axis.text.x = element_text(size = 10),  
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 8), 
    legend.title = element_text(size = 8),
    legend.position = "top",  # Adjust the legend position
    legend.box.margin = margin(0, 0, 0, 10)  # Add margin to the legend box
  ) +
  geom_text(data = plot_data, aes(label = paste("R-squared: ", round(Rsquared, 2), "\nMAE: ", round(MAE, 2), "\nMAPE: ", round(MAPE, 2))),
            x = -Inf, y = Inf, vjust = 1, hjust = 0, size = 2.5, color = "black", show.legend = FALSE)
p2



# setwd(fig.dir)
# png(paste(unique(data$model_ID), "pred_vs_dens_comparison.png", sep ="_"), width = 4, height = 8, units = 'in', res = 300)
# plot(p2)
# dev.off()

## Predicted vs actual density plot ##

data <- df[df$model_ID == "7.1 - 7.3 Ridge 50m" & (df$Haun_test == "4.4 - 4.6"| 
                                                     df$Haun_test == "5.6 - 5.9" | 
                                                     df$Haun_test == "7.9 - 8.4") ,]
data$altitude_test <- factor(data$altitude_test, levels = c("ground","15m", "30m", "50m"))
cbPalette <- c( "#E69F00",  "#0072B2", "#D55E00","#009E73","#000000", "#CC79A7")

data <- data %>% 
  group_by(model_ID, Haun_test, altitude_test) %>% 
  mutate(MAPE = mean(abs((Actual_density-Prediction)/Actual_density)) * 100) %>% 
  ungroup()

p3 <- ggplot(data, aes(x = Actual_density, y = Prediction, 
                       color = as.factor(Sowing_density), 
                       shape = as.factor(Sowing_density)))+
  facet_wrap(Haun_test + altitude_test ~., ncol = 4, strip.position = "right")+
  geom_point(size = 1)+ 
  scale_color_manual(values= cbPalette)+
  geom_abline(lty = 2, col = "black")+ 
  ggtitle(paste(unique(data$model_ID), " tested on 2023 data"))+
  xlab("Actual Density")+ 
  ylab("Predicted Density")+
  lims(x = c(0,500), y = c(0,500))+
  labs(color = "Sowing Density", shape = "Sowing Density")+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    strip.text.x = element_text(size = 10), 
    strip.text.y = element_text(size = 10), 
    
    axis.text.x = element_text(size = 8),  
    axis.text.y = element_text(size = 8),
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 10),
    legend.position = "top",  # Adjust the legend position
    legend.box.margin = margin(0, 0, 0, 10)  # Add margin to the legend box
  ) +
  geom_text(data = data, aes(label = paste("R-squared: ", round(Rsquared, 2), "\nMAE: ", round(MAE, 2), "\nMAPE: ", round(MAPE, 2))),
            x = -Inf, y = Inf, vjust = 1, hjust = 0, size = 2.5, color = "black", show.legend = FALSE)
p3



# setwd(fig.dir)
# png(paste(unique(data$model_ID), "pred_vs_dens_hd.png", sep ="_"), width = 10, height = 7, units = 'in', res = 300)
# plot(p3)
# dev.off()


### ---------------------------------------------------------------------------------------------####

### Outlier analysis ###

model <- lm(Actual_density ~ Prediction, data = df)

residuals <- residuals(model)

standard_deviation <- sqrt(sum(residuals^2) / (length(residuals) - 2))

standardized_residuals <- residuals / standard_deviation

df_o <- data.frame(df, std_residuals =standardized_residuals) %>% 
  group_by(Actual_density) %>% 
  mutate(mean_st_res = mean(std_residuals), 
         sd_st_res = sd(std_residuals)) %>% 
  ungroup()

outliers <- df_o[abs(df_o$mean_st_res) > 2,]

setwd(out.dir)
write.csv(df_o, "outlier_analysis.csv")

# Only observation density 442

### -------------------------------------------------------------------------------------------- ###

# Analysis without outlier

setwd("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/datasets/2023")

haun <- read.csv("Plant_Haun_phenology_summary_2023.csv", header = T)

setwd(in.dir)

df4 <- read.csv("2023_testing_R_PCR_hd_no_outlier.csv", header = T)
df4[is.na(df4)] <- 0
df4 <- df4[,-1]


df5 <- df4 %>% 
  dplyr::select(Prediction, Actual_density, Sowing_density, model_ID, 
                altitude_test, Haun_test, MAE, RMSE, Rsquared) %>% 
  mutate(pred_vs_dens = Prediction - Actual_density) %>% 
  group_by(model_ID, Haun_test, altitude_test, Sowing_density) %>% 
  mutate(residulas_sd = sd(pred_vs_dens), 
         residuals_mean = mean(pred_vs_dens), 
         MAPE_perclass = mean(abs((Actual_density-Prediction)/Actual_density)) * 100) %>% 
  ungroup() %>% 
  group_by(model_ID, Haun_test, altitude_test) %>% 
  mutate(MAPE_overall = mean(abs((Actual_density-Prediction)/Actual_density)) * 100, 
         MAPE_sd = sd(MAPE_perclass)) %>% 
  ungroup() %>% 
  dplyr::select(model_ID, altitude_test, Haun_test, Sowing_density, residuals_mean, residulas_sd, 
                MAPE_perclass, MAPE_overall, MAPE_sd, MAE, RMSE, Rsquared) %>%
  distinct()

setwd(out.dir)
write.csv(df5, "performance_wo3570_no_outlier.csv")

df7_2 <- df5[df5$model_ID == "7.1 - 7.3 Ridge 50m",]

round(mean(df7$Rsquared), 2)
round(sd(df7$Rsquared), 2)

round(mean(df7_2$Rsquared), 2)
round(sd(df7_2$Rsquared), 2)

# MAPE comparison between models without 35 & 70 with or without outlier
t.test(df1$MAPE_overall, df5$MAPE_overall)

# R2 comparison between models without 35 & 70 with or without outlier
t.test(df1$Rsquared, df5$Rsquared)


# Statistical analysis
df5r <- df5[str_detect(df5$model_ID, "Ridge"), ]
lm <- aov(MAPE_overall ~ model_ID+Haun_test+altitude_test + model_ID:Haun_test +
            model_ID:altitude_test + altitude_test:Haun_test, data = df5r)
an <- anova(lm)

variance<- an$`Sum Sq`[1]/(an$`Sum Sq`[1] + an$`Sum Sq`[2])

em <- emmeans(lm, ~  model_ID:altitude_test, mode = "kenward-roger", na.rm =TRUE)
p <- 0.05
HSD_pwpm <- pwpm(em, alpha = p, adjust = 'Tukey')
HSD <- cld(em, alpha= p, Letters = letters, sort= F, adjust = 'Tukey')
HSD

setwd(out.dir)
write.csv(HSD, "Ridge_IDxaltitude_Tukey_no_outlier.csv")


## Predicted vs actual density plot ##

data <- df4[df4$model_ID == "7.1 - 7.3 Ridge 50m" & (df4$Haun_test == "4.4 - 4.6"|
                                                       df4$Haun_test == "5.6 - 5.9" | 
                                                       df4$Haun_test == "7.9 - 8.4") ,]

data$altitude_test <- factor(data$altitude_test, levels = c("ground","15m", "30m", "50m"))
cbPalette <- c( "#E69F00",  "#0072B2", "#D55E00","#009E73","#000000", "#CC79A7")

data <- data %>% 
  group_by(model_ID, Haun_test, altitude_test) %>% 
  mutate(MAPE = mean(abs((Actual_density-Prediction)/Actual_density)) * 100) %>% 
  ungroup()

p4 <- ggplot(data, aes(x = Actual_density, y = Prediction, 
                       color = as.factor(Sowing_density), 
                       shape = as.factor(Sowing_density)))+
  facet_wrap(Haun_test + altitude_test ~., ncol = 4, strip.position = "right")+
  geom_point(size = 1)+ 
  scale_color_manual(values= cbPalette)+
  geom_abline(lty = 2, col = "black")+ 
  ggtitle(paste(unique(data$model_ID), " tested on 2023 data"))+
  xlab("Actual Density")+ 
  ylab("Predicted Density")+
  lims(x = c(0,500), y = c(0,500))+
  labs(color = "Sowing Density", shape = "Sowing Density")+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    strip.text.x = element_text(size = 10), 
    strip.text.y = element_text(size = 10), 
    
    axis.text.x = element_text(size = 8),  
    axis.text.y = element_text(size = 8),
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 10),
    legend.position = "top",  # Adjust the legend position
    legend.box.margin = margin(0, 0, 0, 10)  # Add margin to the legend box
  ) +
  geom_text(data = data, aes(label = paste("R-squared: ", round(Rsquared, 2), "\nMAE: ", round(MAE, 2), "\nMAPE: ", round(MAPE, 2))),
            x = -Inf, y = Inf, vjust = 1, hjust = 0, size = 2.5, color = "black", show.legend = FALSE)
p4



setwd(fig.dir)
png(paste(unique(data$model_ID), "pred_vs_dens_hd_no_outlier.png", sep ="_"), width = 10, height = 7, units = 'in', res = 300)
plot(p4)
dev.off()
