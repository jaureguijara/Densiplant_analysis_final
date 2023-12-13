library(dplyr)
library(lubridate)

setwd("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/datasets/2022")

gim22 <- read.csv("Gimenells - estacio (2022).csv", header = T)
gim22$Data <- as.Date(gim22$Data)

setwd("C:/Users/jaure/OneDrive - udl.cat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis/datasets/2023")

gim23 <- read.csv("Gimenells - estacio (2023).csv", header = T)
gim23$Data <- as.Date(gim23$Data)

gim22<- gim22 %>% 
  mutate(day = day(Data),
         Month = month(Data, label = TRUE),
         Year = year(Data)) %>% 
  group_by(Month, Year) %>% 
  summarise(
    Temp= mean(Temperatura),
    Tempe.max = mean(Temperatura.max),
    Temp.min = mean(Temperatura.min),
    Prec = mean(Precipitacio)
  )

gim22$Year <- "2022"
  

gim23<- gim23 %>% 
  mutate(day = day(Data),
         Month = month(Data, label = TRUE),
         Year = year(Data)) %>% 
  group_by(Month, Year) %>% 
  summarise(
    Temp= mean(Temperatura),
    Tempe.max = mean(Temperatura.max),
    Temp.min = mean(Temperatura.min),
    Prec = mean(Precipitacio)
  )

gim23$Year <- "2023"

data <- rbind(gim22, gim23)
month_order <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun") #"Nov", "Dec",
data$Month <- factor(data$Month, levels = month_order)
data$Prec <- data$Prec *100

library(ggplot2)

plot <- ggplot(data, aes(x = Month)) +
  geom_bar(aes(y = Temp, fill = factor(Year)), stat = "identity", position = "dodge", color = "black") +
  geom_point(aes(y = Prec, color = factor(Year)), position = position_dodge(width = 0.8)) +
  geom_line(aes(y = Prec, color = factor(Year), group = Year), position = position_dodge(width = 0.8)) +
  scale_x_discrete(limits = month_order) +
  scale_y_continuous(
    expression("Average Monthly Temperature " (degree*C)), 
    sec.axis = sec_axis(~ . * 4, name = "Cumulative Precipitation (mm)")
  ) +
  scale_color_manual(name = "Precipitation", values = c( "blue", "darkorange"))+
  scale_fill_manual(name = "Temperature", values = c("lightgray",  "white"))+
  theme_bw()+
  theme(text = element_text(size = 15))#,
        # axis.text.x = element_text(angle = 45, hjust = 1))

setwd("C:/Users/jaure/OneDrive - udl.cat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis/outputs")
png("temp_precipitation_2022_2023.png", width = 8, height = 6, units = 'in', res = 300)
plot(plot)
dev.off()
