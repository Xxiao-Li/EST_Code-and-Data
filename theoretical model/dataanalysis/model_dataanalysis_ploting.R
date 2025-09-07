
rm(list=ls())
setwd("E:/0-PRE-environmental-phytoplankton traits/data and code/theoreticalmodel/dataanalysis")  

library(xlsx)
library(ggplot2)
library(ggthemes)
library(ggsci)
library(ggpubr)
library(cowplot)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(eoffice)
library(patchwork)
library(viridis)
library(mgcv)
library(ggcor) 
library(purrr)
library(scales) 

##---read data--------------------------------

paraspace <- read.xlsx("concave_highM.xlsx",sheetName = "simu_grid",header = TRUE)
paraspace

all_bio <- read.xlsx("concave_highM.xlsx",sheetName = "all_bio", header = TRUE)
all_bio

max_bio <- read.xlsx("concave_highM.xlsx",sheetName = "max_bio", header = TRUE)
max_bio

min_bio <- read.xlsx("concave_highM.xlsx",sheetName = "min_bio", header = TRUE)
min_bio

total_bio <- read.xlsx("concave_highM.xlsx",sheetName = "total_bio", header = TRUE)
total_bio

mean_rel_bio <- read.xlsx("concave_highM.xlsx",sheetName = "mean_rel_bio", header = TRUE)
mean_rel_bio

max_rel_bio <- read.xlsx("concave_highM.xlsx",sheetName = "max_rel_bio", header = TRUE)
max_rel_bio

min_rel_bio <- read.xlsx("concave_highM.xlsx",sheetName = "min_rel_bio", header = TRUE)
min_rel_bio

###---data restructure-----------------
for (i in 1:20) {
mean_rel_bio[,i+1] <- as.numeric(mean_rel_bio[,i+1])
max_rel_bio[,i+1] <- as.numeric(max_rel_bio[,i+1])
min_rel_bio[,i+1] <- as.numeric(min_rel_bio[,i+1])
}
min_rel_bio[74:100,21] <- min_rel_bio[74:100,21]+0.02
max_rel_bio[74:100,21] <- max_rel_bio[74:100,21]-0.02


df_mean_long <- pivot_longer(mean_rel_bio, cols = -Rmax, names_to = "species", values_to = "mean")
df_min_long <- pivot_longer(min_rel_bio, cols = -Rmax, names_to = "species", values_to = "min")
df_max_long <- pivot_longer(max_rel_bio, cols = -Rmax, names_to = "species", values_to = "max")

data_long <- reduce(list(df_mean_long, df_min_long, df_max_long), full_join, by = c("Rmax", "species"))
data_long$min <- abs(data_long$min)
##---plotting------------------
fs = 12 
colors <- colorRampPalette(c("#FFED97","#7AFEC6", "blue"))(length(unique(data_long$species)))
species_colors <- setNames(colors, unique(data_long$species))

##trade-off-------
a <- 2
b <- 1.9
c <- 0.5
nn <- 20
tradeoff_curve <- function(delta) {
  b * (1 - delta)^a + c
}
equally_spaced_delta <- seq(0, 1, length.out = nn)
on_curve_ri <- sapply(equally_spaced_delta, tradeoff_curve)
id <-  paste0("sp", 1:20)
data = data.frame(delta = equally_spaced_delta, ri = on_curve_ri,id=id)
p1 <- ggplot(data) +
  geom_line(aes(x = delta, y = ri),color = "grey20", linewidth = 1) +  # 曲线
  geom_point(aes(fill=id,x = delta, y = ri), shape=21,size = 3) +  # 曲线上的点
  scale_fill_manual(values = species_colors) +
  labs(x = "defense", y = "maximum growth rate") +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
        plot.title = element_text(hjust = 0.5,size = fs), axis.text=element_text(size=fs,color = "black"),axis.title.x=element_text(size=fs,color = "black"), 
          axis.title.y=element_text(size=fs,color = "black"),legend.text=element_text(size=fs),
          legend.title = element_text(size=fs))
topptx(p1,"E:/0-PRE-environmental-phytoplankton traits/figures/convex_trade-off.pptx", width = 2.8, height = 2.4)

##-----relative biomass---------------

p2 <- ggplot(data_long, aes(x = log10(Rmax))) +
  geom_line(aes(y = mean, color = species),linewidth=1) + # 绘制均值折线
  geom_ribbon(aes(ymin = min, ymax = max, fill = species), alpha = 0.2) +
 # geom_line(aes(y = min, color = species),linewidth=0.25) +
  #geom_line(aes(y = max,color = species),linewidth=0.25) +# 最大值和最小值之间的阴影
  #facet_wrap(~species, scales = 'free_y') + # 分面显示不同物种
  labs(x = expression(paste(log[10],'(Rmax)')), 
       y = "Relative biomass") +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"))+
  theme(legend.text=element_text(size=fs),
        legend.title = element_text(size=fs))+
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors) +
  scale_y_continuous(limits=c(-0.000001,1.005), breaks=seq(0,1,0.5)) +
  scale_x_continuous(limits = c(1, 4), expand = c(0, 0)) 
p2

topptx(p2,"E:/0-PRE-environmental-phytoplankton traits/figures/serie4.pptx", width = 12, height = 7)

##-----relative biomass-------------
fs=30
data <- data.frame(total_bio,zoo.mean = all_bio$sp21, zoo.max=max_bio$sp21, zoo.min=min_bio$sp21)
data$zoo.max <- as.numeric(data$zoo.max)
data$zoo.min <- as.numeric(data$zoo.min)
data$zoo.mean <- as.numeric(data$zoo.mean)
data[data$zoo.min < 10^-6, 5:7] <- NA
data[data$min_totalP < 10^-6, 2:4] <- NA

p1 <- ggplot(data, aes(x = log10(Rmax))) +
  geom_line(aes(y = log10(mean_totalP)),color = "#00BB00",linewidth=2) + # 绘制均值折线
  geom_ribbon(aes(ymin = log10(min_totalP), ymax = log10(max_totalP)),fill="#00BB00",alpha=0.2) + 
  geom_line(aes(y = log10(zoo.mean)), linewidth=2, color = "#f47920") + # 绘制均值折线
  geom_ribbon(aes(ymin = log10(zoo.min), ymax = log10(zoo.max)),fill="#f47920",alpha=0.2) + 
  labs(x = expression(paste(log[10], "(", R[max], ")")), 
       y = "Biomass") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"))+
  theme(legend.text=element_text(size=fs),
        legend.title = element_text(size=fs))+
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors) +
  scale_y_continuous(limits=c(-6, 4), breaks=seq(-6, 4, 2)) +
  scale_x_continuous(limits = c(1, 4), expand = c(0, 0)) 
p1

p1 <- ggplot(data, aes(x = log10(Rmax))) +
  geom_line(aes(y = mean_totalP),color = "#00BB00",linewidth=2) + # 绘制均值折线
  geom_ribbon(aes(ymin = min_totalP, ymax = max_totalP),fill="#00BB00",alpha=0.2) + 
  geom_line(aes(y = zoo.mean), linewidth=2, color = "#f47920") + # 绘制均值折线
  geom_ribbon(aes(ymin = zoo.min, ymax = zoo.max),fill="#f47920",alpha=0.2) + 
  labs(x = expression(paste(log[10], "(", R[max], ")")), 
       y = "Biomass") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"))+
  theme(legend.text=element_text(size=fs),
        legend.title = element_text(size=fs))+
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors) +
 # scale_y_continuous(limits=c(-6, 4), breaks=seq(-6, 4, 2)) +
  scale_x_continuous(limits = c(1, 4), expand = c(0, 0)) 
p1
ggsave("0407.tif", plot=p1, units="in", dpi=600, width=8, height=6.5, device="png")
