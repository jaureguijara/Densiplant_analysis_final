
library(dplyr)


setwd("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/datasets/2023")

temp <- read.csv("Gimenells - estacio (2023).csv", header = T)
temp$Data  <- as.Date(temp$Data)  
temp <- temp[temp$Data >= "2022-12-06",]

temp_ggd <- temp %>% 
  group_by(Data) %>% 
  summarise(max = max(Temperatura.max), min = min(Temperatura.min)) %>% 
  mutate(maxmin2 = (max - min)/2)


subs <- temp_ggd[temp_ggd$Data <= as.Date("2023-04-25"), ]
sum(subs$maxmin2)
