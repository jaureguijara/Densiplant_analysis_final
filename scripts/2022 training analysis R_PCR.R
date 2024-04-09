library(tidyverse)
library(HH)
library(emmeans)
library(multcomp)


if(Sys.info()["user"] == "jaure"){
  in.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/outputs/training/ML_results")
  out.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/outputs/training/analysis")
  fig.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/outputs/training/figures")
}else if(Sys.info()["user"] == "jaure"){
  in.dir <- file.path("")
  out.dir <- file.path("")
  fig.dir <- file.path("")
}

setwd("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/datasets/2022")

haun <- read.csv("Plant_Haun_phenology_summary_2022.csv", header = T)
haun_order <- c("2.6 - 2.8", "3 - 3.2", "5.7 - 5.9", "7.1 - 7.3", "8.5 - 9", "10.5 - 11.1", "10.9 - 11.5")
haun$Haun <- factor(haun$Haun, levels = haun_order)

setwd(in.dir)

#### 2022 model training results #####

setwd(in.dir)
df <- read.csv("pred_vs_actual_density_R_PCR_4-fold_10repeatedcv.csv", header = T)
df <- df[,-1] %>% 
  merge(haun, by = "date") %>% 
  mutate(model_ID = paste(Haun, model, altitude)) %>% 
  dplyr::select(-RMSE, -Rsquared, -MAE)

df2 <- read.csv("resamples_R_PCR_4-fold_10repeatedcv2.csv", header = T) 
df2 <- df2[,-1] %>% 
  merge(haun, by = "date") %>% 
  mutate(model_ID = paste(Haun, model, altitude)) %>% 
  dplyr::select(model_ID, Resample, MAE, Rsquared, RMSE)

data <- merge(df, df2, by = c("model_ID", "Resample"))%>% 
  group_by(model_ID, Sowing_density, Resample) %>% 
  mutate(MAPE_perclass = mean(abs((Density-pred)/Density)) * 100) %>% 
  ungroup() %>% 
  group_by(model_ID, Resample) %>% 
  mutate(MAPE_overall = mean(abs((Density-pred)/Density)) * 100) %>% 
  ungroup() %>% 
  dplyr::select(model_ID, model, Sowing_density, Resample, RMSE,Rsquared, MAE, MAPE_perclass, MAPE_overall, altitude, Haun) %>%
  distinct()

PCR <- data[data$model == "PCR",]


PCR_order <- c("2.6 - 2.8 PCR ground", 
                 "2.6 - 2.8 PCR 15m", 
                 "2.6 - 2.8 PCR 30m", 
                 "2.6 - 2.8 PCR 50m", 
                 "3 - 3.2 PCR ground",
                 "3 - 3.2 PCR 15m",
                 "3 - 3.2 PCR 30m",
                 "3 - 3.2 PCR 50m",
                 "5.7 - 5.9 PCR ground",
                 "5.7 - 5.9 PCR 15m",
                 "5.7 - 5.9 PCR 30m",
                 "5.7 - 5.9 PCR 50m",
                 "7.1 - 7.3 PCR 50m",
                 "8.5 - 9 PCR 50m",
                 "10.5 - 11.1 PCR 50m",
                 "10.9 - 11.5 PCR 50m")

PCR$model_ID <- factor(PCR$model_ID, levels = PCR_order)

lm <- aov(MAPE_overall ~ model_ID, data = PCR)
an <- anova(lm)

variance_PCR <- an$`Sum Sq`[1]/(an$`Sum Sq`[1] + an$`Sum Sq`[2])

em <- emmeans(lm, ~ model_ID, mode = "kenward-roger", na.rm =TRUE)
p <- 0.05
HSD_pwpm <- pwpm(em, alpha = p, adjust = 'Tukey')
HSD_PCR <- cld(em, alpha= p, Letters = letters, sort= F, adjust = 'Tukey')
HSD_PCR



Ridge <- data[data$model == "Ridge",]


Ridge_order <- c("2.6 - 2.8 Ridge ground", 
               "2.6 - 2.8 Ridge 15m", 
               "2.6 - 2.8 Ridge 30m", 
               "2.6 - 2.8 Ridge 50m", 
               "3 - 3.2 Ridge ground",
               "3 - 3.2 Ridge 15m",
               "3 - 3.2 Ridge 30m",
               "3 - 3.2 Ridge 50m",
               "5.7 - 5.9 Ridge ground",
               "5.7 - 5.9 Ridge 15m",
               "5.7 - 5.9 Ridge 30m",
               "5.7 - 5.9 Ridge 50m",
               "7.1 - 7.3 Ridge 50m",
               "8.5 - 9 Ridge 50m",
               "10.5 - 11.1 Ridge 50m",
               "10.9 - 11.5 Ridge 50m")

Ridge$model_ID <- factor(Ridge$model_ID, levels = Ridge_order)

lm <- aov(MAPE_overall ~ model_ID, data = Ridge)
an <- anova(lm)

variance_Ridge <- an$`Sum Sq`[1]/(an$`Sum Sq`[1] + an$`Sum Sq`[2])

em <- emmeans(lm, ~ model_ID, mode = "kenward-roger", na.rm =TRUE)
p <- 0.05
HSD_pwpm <- pwpm(em, alpha = p, adjust = 'Tukey')
HSD_Ridge <- cld(em, alpha= p, Letters = letters, sort= F, adjust = 'Tukey')
HSD_Ridge

# 
# RF <- data[data$model == "RF",]
# 
# 
# RF_order <- c("2.6 - 2.8 RF ground", 
#                  "2.6 - 2.8 RF 15m", 
#                  "2.6 - 2.8 RF 30m", 
#                  "2.6 - 2.8 RF 50m", 
#                  "3 - 3.2 RF ground",
#                  "3 - 3.2 RF 15m",
#                  "3 - 3.2 RF 30m",
#                  "3 - 3.2 RF 50m",
#                  "5.7 - 5.9 RF ground",
#                  "5.7 - 5.9 RF 15m",
#                  "5.7 - 5.9 RF 30m",
#                  "5.7 - 5.9 RF 50m",
#                  "7.1 - 7.3 RF 50m",
#                  "8.5 - 9 RF 50m",
#                  "10.5 - 11.1 RF 50m",
#                  "10.9 - 11.5 RF 50m")
# 
# RF$model_ID <- factor(RF$model_ID, levels = RF_order)
# 
# lm <- aov(MAPE_overall ~ model_ID, data = RF)
# an <- anova(lm)
# 
# variance_RF <- an$`Sum Sq`[1]/(an$`Sum Sq`[1] + an$`Sum Sq`[2])
# 
# em <- emmeans(lm, ~ model_ID, mode = "kenward-roger", na.rm =TRUE)
# p <- 0.05
# HSD_pwpm <- pwpm(em, alpha = p, adjust = 'Tukey')
# HSD_RF <- cld(em, alpha= p, Letters = letters, sort= F, adjust = 'Tukey')
# HSD_RF


summary_data_1 <- data %>% 
  group_by(model_ID, model, altitude, Sowing_density, Haun) %>% 
  dplyr::summarise(MAPESD_perclass = sd(MAPE_perclass), 
                   MAPE_perclass = mean(MAPE_perclass))

summary_data_2 <- data %>% 
  group_by(model_ID, model, altitude, Haun) %>% 
  dplyr::summarise(MAPESD_overall = sd(MAPE_overall),
                   MAPE_overall = mean(MAPE_overall),
                   MAESD = sd(MAE),
                   MAE = mean(MAE),
                   R2SD = sd(Rsquared),
                   R2 = mean(Rsquared),
                   RMSESD = sd(RMSE),
                   RMSEmean = mean(RMSE))

summary_data <- merge(summary_data_1, summary_data_2,
                      by = c("model_ID", "model", "altitude", "Haun"))

setwd(out.dir)
write.csv(summary_data, "R_PCR_training_summary.csv")

plot_data <- summary_data_2

HSD_df <- data.frame(rbind(HSD_PCR, HSD_Ridge)) %>% 
  merge(plot_data, by = "model_ID")

setwd(out.dir)
write.csv(data.frame(HSD_df), "Tukey_R_PCR_ID_comparison_pc.csv") 

cbPalette <- c( "#CC79A7","#000000", "#E69F00",  "#009E73", "#0072B2", "#D55E00")



haun_order <- c("2.6 - 2.8", "3 - 3.2", "5.7 - 5.9", "7.1 - 7.3", "8.5 - 9", "10.5 - 11.1", "10.9 - 11.5")
HSD_df$Haun <- factor(HSD_df$Haun, levels = haun_order)
HSD_df$altitude <- factor(HSD_df$altitude, levels = c("ground","15m", "30m", "50m") )


p1 <- ggplot(HSD_df, aes(y = MAPE_overall, fill = altitude, x = Haun)) +
  geom_errorbar(aes(ymin = MAPE_overall - 0.5, ymax = MAPE_overall + MAPESD_overall), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y= MAPE_overall + MAPESD_overall + 5, label = gsub(" ", "", .group) ),
            position= position_dodge(0.9)) +
  facet_grid(model~ ., scales = "free", switch = "both", space = "free") +
  scale_fill_manual(values = cbPalette) +
  ggtitle(expression("Density Prediction 2022 Model Performance")) + 
  xlab("Haun stage") +
  ylab("Mean Absolute Percentage Error") +
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 120, 20))+
  theme_bw()+
  theme(axis.title = element_text(size = 20),
        plot.title = element_text(size = 15),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 0.9, size = 15), # angle = 45, vjust = 0.9, hjust = 0.9,
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))+
  labs(fill = "Altitude")

p1


setwd(fig.dir)
png("2022_R_PCR_MAPE.png", width = 15, height = 10, units = 'in', res = 300)
plot(p1)
dev.off()
