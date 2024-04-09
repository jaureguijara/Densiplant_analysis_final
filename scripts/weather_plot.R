library(dplyr)
library(lubridate)

setwd("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/datasets/2022")

gim22 <- read.csv("Gimenells - estacio (2022).csv", header = T)
gim22$Data <- as.Date(gim22$Data)

setwd("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/datasets/2023")

gim23 <- read.csv("Gimenells - estacio (2023).csv", header = T)
gim23$Data <- as.Date(gim23$Data)

gim22<- gim22 %>% 
  mutate(day = day(Data),
         Month = month(Data, label = TRUE),
         Year = year(Data)) %>% 
  group_by(Month, Year) %>% 
  summarise(
    Temp= mean(Temperatura),
    Temp.max = max(Temperatura),
    Temp.min = min(Temperatura),
    Prec = sum(Precipitacio),
    Eto_cum = sum(ETO, na.rm = TRUE), 
    Irr = sum(Irrigation)
  )

gim22$Year <- "2022"
  

gim23<- gim23 %>% 
  mutate(day = day(Data),
         Month = month(Data, label = TRUE),
         Year = year(Data)) %>% 
  group_by(Month, Year) %>% 
  summarise(
    Temp= mean(Temperatura),
    Temp.max = max(Temperatura),
    Temp.min = min(Temperatura),
    Prec = sum(Precipitacio),
    Eto_cum = sum(ETO, na.rm = TRUE), 
    Irr = sum(Irrigation)
  )

gim23$Year <- "2023"

data <- rbind(gim22, gim23) %>%
  pivot_longer(cols = c(Prec, Irr), names_to = "water_type", values_to = "water_input") %>%
  filter(!is.na(water_input))
month_order <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun") #"Nov", "Dec",
data$Month <- factor(data$Month, levels = month_order)

library(ggplot2)
library(ggpattern)


plot <- ggplot(data, aes(fill = water_type, y = water_input, x = Month, group = factor(Year))) +
  geom_bar(stat = "identity", color = "black", position = "dodge") +
  geom_point(aes(y = Eto_cum, color = factor(Year)), position = position_dodge(width = 0.8)) +
  geom_line(aes(y = Eto_cum, color = factor(Year)), position = position_dodge(width = 0.8)) +
  scale_x_discrete(limits = month_order) +
  scale_y_continuous(
    name = expression("Cumulative water input (mm)" ~ (degree * C)),
    sec.axis = sec_axis(~ . * 4, name = "Cumulative Monthly ETo (mm)")
  ) +
  scale_color_manual(name = "ETo", values = c("blue", "darkorange")) +
  scale_fill_manual(name = "Water Type", values = c("Prec" = "lightgray", "Irr" = "white")) +
  facet_wrap(~Year, scales = "free_y") +
  theme_bw() +
  theme(text = element_text(size = 15))


plot


plot <- ggplot(data, aes(fill = water_type, y = water_input, x = Month, group = interaction(factor(Year), water_type),
                         color = factor(Year))) +
  geom_bar(stat = "identity", position = position_stack(), color = "black") +
  geom_point(aes(y = Eto_cum)) +
  geom_line(aes(y = Eto_cum)) +
  scale_x_discrete(limits = month_order) +
  scale_y_continuous(
    name = expression("Cumulative monthly water input (mm)"),
    sec.axis = sec_axis(~ ., name = "Cumulative monthly ETo (mm)")
  ) +
  facet_wrap(Year~.) +
  scale_color_manual(name = "ETo", values = c("2022" = "blue", "2023" = "darkorange")) +
  
  scale_fill_manual(name = "Water Type", values = c("Prec" = "lightgray", "Irr" = "white"), 
                    labels = c( "Irr" = "Irrigation",   "Prec" = "Precipitation")) +
  ggtitle("Monthly cumulative Water Input and ETo") +
  theme_bw() +
  theme(text = element_text(size = 15),
        legend.background = element_rect(fill = "white"),  # Set legend background color to white
        legend.position = "right") 
plot





setwd("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/outputs")
png("ETo_water_input_2022_2023.png", width = 10, height = 8, units = 'in', res = 300)
plot(plot)
dev.off()
