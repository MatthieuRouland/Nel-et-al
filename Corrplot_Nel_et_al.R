#==========================================
# Title:  Corrplot
# Author: Matthieu Rouland
# Affiliation: Institut Cochin, INSERM U1016, CNRS UMR 8104, Université de Paris, Paris, France.
# Acknowledgments:
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
library(shiny)
library(ggrepel)
library(corrplot)
library(Hmisc)
library(psych)
library(RcmdrMisc)
library(PerformanceAnalytics)
library(GGally)
setwd("")


cohort_select2 <- read_excel("data.xlsx")

#------Filtering--------#
# cohort_select2 <- filter (cohort_select2, `n - Sexe` == "F")
# cohort_select2 <- filter(cohort_select2, cohort_select2$statut == "T1D Est")
# cohort_select2 <- filter(cohort_select2, cohort_select2$statut == "T1D RO")
# cohort_select2 <- filter(cohort_select2, cohort_select2$statut == "HD")
# cohort_select2 <- filter(cohort_select2, cohort_select2$statut == "T1D RO AID")
# cohort_select2 <- filter(cohort_select2, cohort_select2$statut == "T1D Est AID")
# cohort_select2 <- filter(cohort_select2, cohort_select2$statut != "T1D RO AID")
# cohort_select2 <- filter(cohort_select2, cohort_select2$statut != "T1D Est AID")
#-----------------------#


##--Columns selection-##
cohort_select2 <- select(cohort_select2, 5:6, 12:14, 16, 21:36, 49, 51, 53, 55, 57, 63, 64, 65 )
summary(cohort_select2)

cohort_select_corv2 <- rcorr.adjust(cohort_select2, type = "spearman", use = "pairwise.complete.obs")
cohort_select_corv2_P <- cohort_select_corv2$R$P
cohort_select_corv2_n <- cohort_select_corv2$R$n
cohort_select_corv2_r <- cohort_select_corv2$R$r


#in case of No NA, NaN, or infinite in our matrix => replace with null values
cohort_select_corv2_r[is.na(cohort_select_corv2_r)] <- 0
cohort_select_corv2_r[is.nan(cohort_select_corv2_r)] <- 0
cohort_select_corv2_r[is.infinite(cohort_select_corv2_r)] <- 0

#in case of >1 or <-1 => correct to 1 or -1
cohort_select_corv2_r[cohort_select_corv2_r > 1 ] <- 1
cohort_select_corv2_r[cohort_select_corv2_r < sign(-1) ] <- -1

#in case of No NA, NaN, or infinite in our matrix => replace with null p-values
cohort_select_corv2_P[is.na(cohort_select_corv2_P)] <- 0.99
cohort_select_corv2_P[is.nan(cohort_select_corv2_P)] <- 0.99

coldiabetologia <- colorRampPalette(c( "#4c5988", "#8aa9d6" , "#FFFFFF", "#ed7374", "#c04140" ))

###

write_xlsx(as.data.frame(cohort_select_corv2_P), "corrplot p values.xlsx", col_names = TRUE)
write_xlsx(as.data.frame(cohort_select_corv2_n), "corrplot n values.xlsx", col_names = TRUE)
write_xlsx(as.data.frame(cohort_select_corv2_r), "corrplot r values.xlsx", col_names = TRUE)

#Size: Fig1 == 7; Fig4=10 ; Fig6 == 10
pdf(file = paste0("Only Sign - H&F - corrplot TemoinHD ALL ++++ ",Sys.Date(),".pdf"),width= 10 , height= 10)
corrplot(cohort_select_corv2_r, type = "full", na.label = "NA", p.mat = cohort_select_corv2_P,  insig = "blank", method = "square",
         sig.level = c(.05), pch.cex = 1, tl.cex = 1, pch.col = "black",
         order = "alphabet", addrect = 6, tl.col = "black", outline = "black", col = coldiabetologia(100))
dev.off()

pdf(file = paste0("H&F - corrplot TemoinHD ALL ++++ ",Sys.Date(),".pdf"),width= 10 , height= 10)
corrplot(cohort_select_corv2_r, type = "full", na.label = "NA", p.mat = cohort_select_corv2_P,  insig = "label_sig", method = "square",
         sig.level = c(.001, .01, .05), pch.cex = 1, tl.cex = 1, pch.col = "black",
         order = "alphabet", addrect = 6, tl.col = "black", outline = "black", col = coldiabetologia(100))
dev.off()




