#==========================================
# Title:  Principal component analysis
# Author: Matthieu Rouland
# Affiliation: Institut Cochin, INSERM U1016, CNRS UMR 8104, Université de Paris, Paris, France.
# Acknowledgments: Dr. Marc Diedisheim for advice and coding help.
# License: CC-BY-SA-4.0 
# Date: 27.04.2021
#==========================================

#--Settings--#
library(ggplot2)
library(readxl)
library(writexl)
library(ggpubr)
library(purrr)
library(dplyr)
library(ggrepel)
library(corrplot)
library(Hmisc)
library(psych)
library(RcmdrMisc)
library(PerformanceAnalytics)
library(GGally)
library(FactoMineR)
library(factoextra)

setwd("")
set.seed (1234)
data <- read_excel("data.xlsx")

#------Filtering--------#
#data <- filter (data, !statut == "HD")
#data <- filter (data, !statut == "T1D Est AID")
#data <- filter (data, !statut == "T1D RO AID")
#data <- filter (data, !statut == "T1D RO")
#data <- filter (data, `n - Sexe` == "F")
#-----------------------#

#------COLOR--------#
# HD: #8aa9d6
# RO: #755a91
# LT: #cc85b1
# LT AID: #c04140
#-------------------#

group <- data[,1]
visu <- data[,1]
data_numeric_surface <- select(data, 5:6, 21:36, 63)

palette_choice <- c("#cc85b1","#8aa9d6", "#8aa9d6", "purple", "brown")

res.pca_surface <- PCA(data_numeric_surface, ncp = 9,scale.unit = TRUE,graph = FALSE)
print(res.pca_surface)

pdf(file = paste0("PCA", Sys.Date(), ".pdf"), width = 7, height = 7)
for (i in 1:9){ 
  for (j in 1:9){
    if (j != i){
      print((fviz_pca_ind(res.pca_surface,
                          geom.ind = "point",
                          col.ind = data$statut,
                          palette = palette_choice,
                          pointsize = 1.5,
                          mean.point.size = 6,
                          addEllipses = FALSE, ellipse.level = 0.95,
                          legend.title = "Groups",
                          axes = c(i, j),
                          mean.point = TRUE)))
      
      print(fviz_pca_ind(res.pca_surface,
                         geom.ind = "text",
                         col.ind = data$statut,
                         palette = palette_choice,
                         addEllipses = FALSE, ellipse.level = 0.95,
                         legend.title = "Groups",
                         axes = c(i, j)))
      
      print(fviz_pca_var(res.pca_surface, 
                         axes = c(i, j),
                         geom = c("text"),
                         col.ind = data$statut, 
                         label = "all", invisible = "none", labelsize = 2,
                         col.var = "black", alpha.var = 1, col.quanti.sup = "blue",
                         col.circle = "grey70",
                         select.var = list(name =NULL, cos2 = NULL, contrib = NULL),
                         repel = TRUE))
      
      print(fviz_pca_biplot(res.pca_surface, 
                            label="var",
                            axes = c(i, j),
                            select.var = list(contrib = 40),
                            palette = palette_choice,
                            ellipse.alpha = 0.01,
                            col.ind = data$statut,
                            col.var = "black", alpha.var = 1, col.quanti.sup = "blue",
                            labelsize = 2,
                            alpha.arrow = 0.10,
                            pointsize = 0,
                            addEllipses = TRUE, ellipse.level = 0.95, arrowsize = 0.0005,
                            repel = TRUE, 
                            alpha.ind = 0,
                            mean.point.size = 4,
                            legend.title =  "Groups"
                            
      )) 
      
      print(fviz_contrib(res.pca_surface, 
                         choice ="var", 
                         axe = c(i,j), 
                         top=60))
      
      print(i)
      print(j)     
    }
  }
}
dev.off()


