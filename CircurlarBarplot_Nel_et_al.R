#==========================================
# Title:  CircularBarplot 
# Author: Matthieu Rouland
# Acknowledgments and adapted from: the R Graph Gallery by Yan Holtz   
# Affiliation: Institut Cochin, INSERM U1016, CNRS UMR 8104, Université de Paris, Paris, France.
# License: CC-BY-SA-4.0 
# Date: 27.04.2021
#==========================================

#--Settings--#
library(ggplot2)
library(readxl)
library(writexl)
library(tidyverse)
setwd("")

#read global data
data <- read_excel("data.xlsx")
data <- select(data,1, 3:length(data))

# Produces Mann-Withney p-values #
final <- "Pvalues Healthy_Control VS T1D_Estad"
for (i in 3:length(data)){
  
  x <- (data  %>% select(i) %>% filter(data$statut == "Healthy_Control"))
  y <- (data  %>% select(i) %>% filter(data$statut == "T1D_Estad"))
  
  print(summary(y))
  print(summary(x))
  
  pvalues <- wilcox.test(as.matrix(x),as.matrix(y), exact = T, correct = F)
  
  print(paste0(colnames(x),"___",pvalues$p.value))
  final <- append(final, paste0(colnames(x),"___",pvalues$p.value))
}
write_xlsx(as.data.frame(final), "Healthy_Control_VS_TT1D_Estad.xlsx", col_names = TRUE)


# Produces Circularbarplot #

#Read Pvalue data
data <- read_excel("Pvalue Data.xlsx")

empty_bar=0
to_add = as.data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar)
data=rbind(data, to_add)
data$id=seq(1, nrow(data))

#Label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

#Baseline
base_data=data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))

#Grid
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

p = ggplot(data, aes(x=as.factor(id), y=value, fill=group)) + 
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  
  #pvalues red dot with different linetype
  geom_segment(data=grid_data, aes(x = 0, y = 2.995732, xend = 13, yend = 3), colour = "red", alpha=0.30, size=0.4 , inherit.aes = FALSE, linetype=2) +
  geom_segment(data=grid_data, aes(x = 0, y = 4.605170, xend = 13, yend = 4.6), colour = "red", alpha=0.30, size=0.4 , inherit.aes = FALSE, linetype=4) +
  geom_segment(data=grid_data, aes(x = 0, y = 6.907755, xend = 13, yend = 6.9), colour = "red", alpha=0.30, size=0.4 , inherit.aes = FALSE, linetype=5) +
  
  annotate("text", x = 0 , y = c(0, 2, 4, 6), label = c("0", "2", "4", "6") , 
           color="grey", size=1 , angle=0, fontface="bold", hjust=2) +
  
  annotate("text", x = 0 , y = c(3.1, 4.7, 7), label = c("*", "**", "***") , 
           color="red", size=3 , angle=0, fontface="bold", hjust=2) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  scale_fill_manual(values = c("#ed7374", "#6DA567", "#755a91"))+
  
  ylim(-4,15) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4),
                       "cm"),
    text=element_text(family="serif")
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+0.5, label=variable, hjust=hjust), color="black", fontface="bold",alpha=0.6, 
            size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  
  geom_segment(data=base_data, aes(x = start, y = 0, xend = end, yend = 0), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -2, label=group), hjust=c(0.5,0.5,0.5), colour = c("#ed7374", "#6DA567", "#755a91"), 
            alpha=0.8, size=2.5, fontface="bold", inherit.aes = FALSE)


pdf(file= "circularbarplot.pdf", width = 8, height = 8)
p
dev.off()
