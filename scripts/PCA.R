library(dplyr)
library(tidyr)
library(broom)
library(PairedData)
library(car)
library(Hmisc)
library(stringr)
#library(corrplot)
library(ggfortify)
library(rgl)
library(FactoMineR)
library(factoextra)


if(Sys.info()["user"] == "jaure"){
  in.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/datasets/2023")
  out.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/outputs/testing/PCA")
  fig.dir <- file.path("C:/Users/jaure/OneDrive - Universitat de Barcelona (1)/UB/Doctorat/Agrotecnio/DENSIPLANT/DENSIPLANT_analysis_final/outputs//testing/PCA/figures")
}else if(Sys.info()["user"] == "jaure"){
  in.dir <- file.path("")
  out.dir <- file.path("")
  fig.dir <- file.path("")
}


setwd(in.dir)

df <- read.csv("combined_densiplant_dataset_2023_rgba_rgbg.csv", header = T)
df <- df[,-1]

df$Sowing_density<- as.character(df$Sowing_density)
colnames(df)[colnames(df) == "count_mean"] <- "Density"
df <- relocate(df, Sowing_density, .after = Plots) %>% 
  relocate(Density, .after = Sowing_density)


####### PCA #######

cbPalette <- c("#000000", "#E69F00",  "#009E73", "#0072B2", "#D55E00", "#CC79A7")
densorder <- c("35","70","140", "280", "560")
data <- df

eigenval_df <- NULL
contrib_df <- NULL

for(i in 1:length(unique(data$Date))){
  current_date <- unique(data$Date)[i]
  
  # Subset data per date 
  
  subset_date <- data[data$Date == current_date,]
  alt_combi_subset <- NULL #to merge de dates having different index columns per altitude later
  
  # 1 Separating per date and altitude
  
  for(j in 1:length(unique(subset_date$Altitude))){
    
    current_alt <- unique(subset_date$Altitude)[j]
    
    subset_alt <-  subset_date[subset_date$Altitude == current_alt, ]
    subset_alt$Sowing_density <- factor(subset_alt$Sowing_density, levels = densorder)
    
    pca_data <- subset_alt[,-c(2,3,5,6)] # Date, Altitude, Rows, Plots, Sowing_density
    

    print(paste(current_date, unique(subset_alt$Altitude), sep ="_"))
    
    pca_data <- pca_data  %>% 
      dplyr::select(-Density, -Image.Name, -Genotype, -Sowing_density)
    
    
    # PCA
    pca <- prcomp(pca_data,  center = T, scale. = T)
    res.pca <- PCA(pca_data,scale. = T,graph = FALSE)
    eigenval <- pca$sdev ^ 2
    
    PC_selected <- length(eigenval[eigenval > 1])
    
    PC_selected <- length(eigenval[eigenval > 1])
    
    var_explained <- eigenval/sum(eigenval)
    var_explained <- paste(as.character(round(sum(var_explained[1:PC_selected])*100,2)), "%", sep = "")
    var_explained <- paste(var_explained, "explained", sep = " ")
    
    # Plot PCA
    setwd(fig.dir)
    p1 <- autoplot(pca, data = subset_alt, colour = 'Sowing_density',
                   shape = "Genotype",
                   main =  paste(current_date, 
                                 current_alt, "PC", PC_selected, var_explained, sep = "_")) +
      scale_color_manual(values=cbPalette)
    
    
    png(paste(current_date,current_alt, "PCA", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p1)
    dev.off()
    
    p2 <- fviz_contrib(pca, choice = "var", axes = 1:2) + ggtitle(paste("Contributions 2PC ", current_date, 
                                                                        current_alt, "PC", PC_selected, var_explained, sep = "_"))
    
    png(paste(current_date,current_alt, "Contributions2PC", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p2)
    dev.off()
    
    p3 <- fviz_pca_var(pca, col.var = "black", repel = TRUE)
    
    png(paste(current_date,current_alt, "Biplot", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p3)
    dev.off()
    
    p4 <- fviz_pca_biplot(pca, geom.ind = as.factor(subset_alt$Sowing_density),
                          col.ind = as.factor(subset_alt$Sowing_density), 
                          fill.ind = as.factor(subset_alt$Sowing_density),
                          palette = cbPalette, 
                          addEllipses = TRUE, col.var = "black", repel = TRUE, 
                          legend.title = "Sowing_density") +
      geom_point(aes(shape = factor(subset_alt$Genotype), colour = factor(subset_alt$Sowing_density)))+ 
      guides(shape = guide_legend(title = "Genotype")) + ggtitle(paste(current_date, 
                                                                       current_alt, "PC", PC_selected, var_explained, sep = "_"))
    png(paste(current_date,current_alt, "PCA-Biplot", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p4)
    dev.off()
    
    p5 <- fviz_eig(pca, addlabels = TRUE, ylim = c(0, 100)) + ggtitle(paste(current_date, 
                                                                            current_alt, "PC", PC_selected, var_explained, sep = "_")) 
    png(paste(current_date,current_alt, "Scree_plot", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p5)
    dev.off()
    
    # Contributions of variables to PC1
    p6 <- fviz_contrib(res.pca, choice = "var", axes = 1)+ ggtitle(paste("Contributions 1st PC ", current_date, 
                                                                         current_alt, "PC", PC_selected, var_explained, sep = "_"))
    
    png(paste(current_date,current_alt, "Contributions_PC1", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p6)
    dev.off() 
    # Contributions of variables to PC2
    p7 <- fviz_contrib(res.pca, choice = "var", axes = 2)+ ggtitle(paste("Contributions 2nd PC ", current_date, current_date, 
                                                                         current_alt, "PC", PC_selected, var_explained, sep = "_"))
    png(paste(current_date,current_alt, "Contributions_PC2", ".png", sep="_"), width = 8, height = 6, units = 'in', res = 300)
    plot(p7)
    dev.off() 
    
    # SAVE
    alt <- unique(subset_alt$Altitude)
    
    eigenval_result <- data.frame(res.pca$eig)
    eigenval_result$Date <- current_date
    eigenval_result$Altitude <- alt
    eigenval_result <- tibble::rownames_to_column(eigenval_result, "Component")
    eigenval_df <- rbind(eigenval_df, eigenval_result)
    
    contrib_result <- data.frame(res.pca$var$contrib)
    contrib_result$Date <- current_date
    contrib_result$Altitude <- alt
    contrib_result <- tibble::rownames_to_column(contrib_result, "Variable")
    contrib_df <- rbind(contrib_df, contrib_result)
    
  }
  

}

setwd(out.dir)
write.csv(eigenval_df, "Eigenvalues.csv")
write.csv(contrib_df, "VarContributions.csv")
