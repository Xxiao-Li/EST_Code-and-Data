
rm(list=ls())
setwd("E:/0-PRE-environmental-phytoplankton traits/data and code")        
##--carbon biomass calculations----
#long_data1 <- site_phy_abun %>%
#  pivot_longer(
#     cols = 3:27,  # replace with the actual indices of your columns
#    names_to = "point", 
#    values_to = "abundance",
#     values_drop_na = TRUE
#   )
# merged_data <- long_data1 %>%
#  left_join(phy_trait, by = "abbre.name")
# merged_data <- merged_data %>%
#  mutate(carbon_biomass = abundance * Cell.carbon / 1000)  ##unit pgC L-1

# carbon_biomass_by_point <- merged_data %>%
#  group_by(point) %>%
#  summarise(total_carbon_biomass = sum(carbon_biomass, na.rm = TRUE))
#-----------------------
library(readxl)
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
library(vegan)
library(ggrepel)
##----------read data-----------------------------------------------------------

rawdata <- read_excel("raw_data_r.xlsx",sheet = "rawdata_adjusted")
rawdata

phy_trait <- read_excel("raw_data_r.xlsx",sheet = "all_phy_trait")
phy_trait

site_phy <- read_excel("raw_data_r.xlsx",sheet = "site_phy")
site_phy

site_phy_abun <- read_excel("raw_data_r.xlsx",sheet = "site_phy_abun")
site_phy_abun

site_phy_propor <- read_excel("raw_data_r.xlsx",sheet = "site_phy_propor")
site_phy_propor

site_phy_bio <- read_excel("raw_data_r.xlsx",sheet = "site_phy_bio")
site_phy_bio

t_site_phy_bio <- read_excel("raw_data_r.xlsx",sheet = "t_site_phy_bio")
t_site_phy_bio

t_site_phy_propor <- read_excel("raw_data_r.xlsx",sheet = "t_site_phy_propor")
t_site_phy_propor
t_site_phy_propor[is.na(t_site_phy_propor)] <- 0

t_site_phy_abun <- read_excel("raw_data_r.xlsx",sheet = "t_site_phy_abun")
t_site_phy_abun

#----calculations---------------------------------
rawdata$RUE_chl_N <- with(rawdata, rawdata$chla/rawdata$DIN)
rawdata$RUE_chl_P <- with(rawdata, rawdata$chla/rawdata$PO4)
rawdata$RUE_phy_N <- with(rawdata, rawdata$phy_bio/rawdata$DIN)
rawdata$RUE_phy_P <- with(rawdata, rawdata$phy_bio/rawdata$PO4)
rawdata$RUE_zoo <- with(rawdata, rawdata$zoo_bio/rawdata$phy_bio)
rawdata$RUE_phy_Cbio_N <- with(rawdata, rawdata$phy.carbon.biomass/rawdata$DIN)
rawdata$RUE_phy_Cbio_P <- with(rawdata, rawdata$phy.carbon.biomass/rawdata$PO4)

rawdata_surface <- rawdata[rawdata$Layer == 'U', ]
rawdata_bottom <- rawdata[rawdata$Layer == 'B', ]
rawdata_04 <- rawdata[rawdata$Date == '2023.04', ]
rawdata_09 <- rawdata[rawdata$Date == '2023.09', ]


##ANOVA--------------------
result <- aov(rawdata_surface$chla ~ Date, data = rawdata_surface)

summary(result)

##-------
combined_data <- cbind(rawdata_surface,t_site_phy_propor[,-1])
combined_data <- combined_data[combined_data$Date == '2023.09', ]

plot(combined_data$Ske_cos,combined_data$phy_bio)
plot(log10(combined_data$`N/P`),combined_data$Ske_cos)
fs=20
p1 <- ggplot(data=combined_data, aes(x=Ske_cos, y=log10(RUE_phy_N)))+
  geom_point(shape=19,size = 4,alpha = 0.9,stroke = 2)+
  labs(x = expression('Relative abundance of '*italic("S. costatum")), y = expression(italic(RUE)[phy-N]))+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"))+
  theme(legend.text=element_text(size=fs),
        legend.title = element_text(size=fs))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  guides(color="none")+
  scale_x_continuous(limits = c(0,1))
p3 <- ggplot(data=combined_data, aes(x=Ske_cos, y=log10(RUE_phy_P)))+
  geom_point(shape=19,size = 4,alpha = 0.9,stroke = 2)+
  labs(x = expression('Relative abundance of '*italic("S. costatum")), y = expression(italic(RUE)[phy-P]))+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"))+
  theme(legend.text=element_text(size=fs),
        legend.title = element_text(size=fs))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  guides(color="none")+ scale_x_continuous(limits = c(0,1))
p5 <- ggplot(data=combined_data, aes(x=Ske_cos, y=log10(RUE_zoo)))+
  geom_point(shape=19,size = 4,alpha = 0.9,stroke = 2)+
  labs(x = expression('Relative abundance of '*italic("S. costatum")), y = expression(italic(RUE)[zoo]))+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"))+
  theme(legend.text=element_text(size=fs),
        legend.title = element_text(size=fs))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  guides(color="none")+ scale_x_continuous(limits = c(0,1))

p2 <- ggplot(data=combined_data, aes(x=Cos_sp., y=log10(RUE_phy_N)))+
  geom_point(shape=19,size = 4,alpha = 0.9,stroke = 2)+
  labs(x = expression('Relative abundance of '*italic("Coscinodiscus")*' sp.'),y = expression(italic(RUE)[phy-N]))+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"))+
  theme(legend.text=element_text(size=fs),
        legend.title = element_text(size=fs))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  guides(color="none")+ scale_x_continuous(limits = c(0,1))
p4 <- ggplot(data=combined_data, aes(x=Cos_sp., y=log10(RUE_phy_P)))+
  geom_point(shape=19,size = 4,alpha = 0.9,stroke = 2)+
  labs(x = expression('Relative abundance of '*italic("Coscinodiscus")*' sp.'), y = expression(italic(RUE)[phy-P]))+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"))+
  theme(legend.text=element_text(size=fs),
        legend.title = element_text(size=fs))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  guides(color="none")+ scale_x_continuous(limits = c(0,1))
p6 <- ggplot(data=combined_data, aes(x=Cos_sp., y=log10(RUE_zoo)))+
  geom_point(shape=19,size = 4,alpha = 0.9,stroke = 2)+
  labs(x = expression('Relative abundance of '*italic("Coscinodiscus")*' sp.'), y = expression(italic(RUE)[zoo])
  ) +
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"))+
  theme(legend.text=element_text(size=fs),legend.title = element_text(size=fs))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  theme(legend.position = c(0.8, 0.8)) + labs(color="")+ scale_x_continuous(limits = c(0,1))

p <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3,
               labels = c("A","B","C","D","E","F"),
               label.x = 0.02, label.y = 1.01,
               font.label = list(size = fs, face = "bold"))
p
ggsave("dominance-RUE-1027.tif", plot=p, units="in", dpi=600, width=12.5, height=15, device="png")


##---plot Figure 2--------------------------------------------------------------
rawdata_09$Date <- as.factor(rawdata_09$Date)
rawdata_09$Layer <- as.factor(rawdata_09$Layer)

fs=16

cb_palette <- c(rgb(25/255, 197/255, 202/255),rgb(249/255, 131/255, 123/255),rgb(25/255, 193/255, 76/255))
cb_palette2 <- c(rgb(25/255, 197/255, 202/255),rgb(25/255, 193/255, 76/255),rgb(249/255, 131/255, 123/255))

p1 <- ggplot(data=rawdata_09, aes(x=log10(`N/P`), y=log10(chla)))+
  geom_point(shape=21,fill="gray",color = "black",size = 4,alpha = 0.9,stroke = 0.6)+
  labs(x = expression(paste(log[10],'(N:P)')), y = expression(paste(log[10],'(Chl a)')))+
  scale_x_continuous(limits = c(0.55, 1.9), breaks= c(0.6, 0.9, 1.2, 1.5,1.8))+
  theme(axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"))+
  theme(legend.text=element_text(size=fs),legend.title = element_text(size=fs))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  geom_smooth(method="loess",color = "black", linewidth=1.5)
p1

longdata <- pivot_longer(rawdata_09, cols = c(RUE_chl_N, RUE_chl_P), names_to = "variable", values_to = "value")
p3 <- ggplot(data=longdata, aes(x=log10(`N/P`), y=log10(value), fill=variable))+
  geom_point(shape=21,color = "black",size = 4,alpha = 0.9,stroke = 0.6)+
  labs(x = expression(paste(log[10],'(N:P)')), y = expression(paste(log[10],'(RUE_Chl a)')))+
  scale_x_continuous(limits = c(0.55, 1.9), breaks= c(0.6, 0.9, 1.2, 1.5,1.8))+
  theme(plot.title = element_text(hjust = 0.5,size = fs), axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs), legend.title = element_text(size=fs),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  guides(fill = "none",color="none")  + theme(legend.background = element_blank())+
  geom_smooth(method="loess", aes(group = variable,color=variable),linewidth=1.5)+
  scale_fill_manual(values = cb_palette) + scale_color_manual(values = cb_palette)
p3

longdata <- pivot_longer(rawdata_surface, cols = c(phy_bio,zoo_bio), names_to = "variable", values_to = "value")
p2 <- ggplot(data=longdata, aes(x=log10(`N/P`), y=log10(value), fill=variable))+
  geom_point(shape=21,size = 4,alpha = 0.9,stroke = 0.6)+
  labs(x = expression(paste(log[10],'(N:P)')), y = expression(paste(log[10],'(biomass)')))+
  scale_x_continuous(limits = c(0.55, 1.9), breaks= c(0.6, 0.9, 1.2, 1.5,1.8))+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"))+
  theme(legend.text=element_text(size=fs),
        legend.title = element_text(size=fs))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  geom_smooth(method="loess", aes(group = variable,color=variable),linewidth=1.5) +
  guides(fill = "none",color="none") +
  scale_fill_manual(values = cb_palette2) + scale_color_manual(values = cb_palette2)
p2

longdata <- pivot_longer(rawdata_surface, cols = c(RUE_phy_N, RUE_phy_P, RUE_zoo), names_to = "variable", values_to = "value")
p4 <- ggplot(data=longdata, aes(x=log10(`N/P`), y=log10(value), fill=variable))+
  geom_point(shape=21,color = "black",size = 4,alpha = 0.9,stroke = 0.6)+
  labs(x = expression(paste(log[10],'(N:P)')), y = expression(paste(log[10],'(RUE)')))+
  scale_x_continuous(limits = c(0.55, 1.9), breaks= c(0.6, 0.9, 1.2, 1.5,1.8))+
   theme(plot.title = element_text(hjust = 0.5,size = fs), axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs),legend.title = element_text(size=fs),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  geom_smooth(method="loess", aes(group = variable,color=variable),linewidth=1.5)+
  guides(fill = "none",color="none") +scale_fill_manual(values = cb_palette)+scale_color_manual(values = cb_palette)
p4

p <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
p

topptx(p,"E:/0-PRE-environmental-phytoplankton traits/figures/figure2_1013.pptx", width = 10, height = 8)


##------plot 3----calcualtions site-phy-------

site_phy <- site_phy[, colSums(is.na(site_phy)) < nrow(site_phy)]
site_phy_long <- site_phy %>%
  pivot_longer(
    cols = 2:31,  
    names_to = "xx",
    values_to = "abbre.name"  
  ) %>%
  filter(abbre.name > 0)  
combined_data <- site_phy_long %>%
  left_join(phy_trait, by = "abbre.name")  

combined_data <- combined_data %>%
  left_join(rawdata_surface, by = "Site")

site_phy_bio_long <- t_site_phy_bio %>%
  pivot_longer(
    cols = 2:69, 
    names_to = "abbre.name",
    values_to = "spe_bio",
    values_drop_na = TRUE 
  ) 

site_phy_propor_long <- t_site_phy_propor %>%
  pivot_longer(
    cols = 2:69, 
    names_to = "abbre.name",
    values_to = "spe_propor",
    values_drop_na = TRUE 
  ) 

site_phy_abun_long <- t_site_phy_abun %>%
  pivot_longer(
    cols = 2:69, 
    names_to = "abbre.name",
    values_to = "spe_abun",
    values_drop_na = TRUE 
  ) 
combined_data <- cbind(combined_data,site_phy_bio_long[,3],site_phy_propor_long[,3],site_phy_abun_long[,3])
normalized_defense <- (combined_data$defense - min(combined_data$defense)) / (max(combined_data$defense) - min(combined_data$defense))
combined_data$defense <- normalized_defense

##--community composition------------
fs = 24
summary_df <- combined_data %>%
  group_by(phylum) %>%
  summarise(total = sum(spe_abun)) %>%
  mutate(percentage = total / sum(total) * 100,
         label = paste0(phylum, ": ", round(percentage, 1), "%"))

summary_df$phylum <- factor(summary_df$phylum)
p1 <- ggplot(summary_df, aes(x = "", y = total, fill = phylum)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = fs),
        legend.title = element_text(size = fs),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())

ggsave("composition_phylum.tif", plot=p1, units="in", dpi=600, width=9, height=9, device="png")

summary_df <- combined_data %>%
  group_by(order) %>%
  summarise(total = sum(spe_abun)) %>%
  mutate(percentage = total / sum(total) * 100,
         label = paste0(order, ": ", round(percentage, 1), "%"))
summary_df$order <- factor(summary_df$order)
p2 <- ggplot(summary_df, aes(x = "", y = total, fill = order)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = fs),
        legend.title = element_text(size = fs),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())
ggsave("composition_order.tif", plot=p2, units="in", dpi=600, width=13.5, height=9, device="png")

summary_df <- combined_data %>%
  group_by(class...7) %>%
  summarise(total = sum(spe_abun)) %>%
  mutate(percentage = total / sum(total) * 100,
         label = paste0(class...7, ": ", round(percentage, 1), "%"))
summary_df$class...7 <- factor(summary_df$class...7)
summary_df <- summary_df %>% rename(class = `class...7`)
p3 <- ggplot(summary_df, aes(x = "", y = total, fill = class)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = fs),
        legend.title = element_text(size = fs),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())
ggsave("composition_class.tif", plot=p3, units="in", dpi=600, width=9, height=9, device="png")


p <- ggarrange(p1, p2, ncol = 2, nrow = 1,
               labels = c("A","B"),
               label.x = 0.07, label.y = 0.9,
               font.label = list(size = fs, face = "bold"))

ggsave("composition.tif", plot=p, units="in", dpi=600, width=14, height=8, device="png")


##--plotting figure 31------------------
fs=18
fss=8
averages_and_sds <- combined_data %>%
  group_by(`N/P`) %>%
  summarise(mean = mean(maximum_growth_rate),
            min = min(maximum_growth_rate), max = max(maximum_growth_rate))

p1 <- ggplot(data=combined_data, aes(x=log10(`N/P`), y=log10(spe_propor), color=maximum_growth_rate)) +
  geom_point(shape=21, size=3, alpha=0.9, stroke=1.5, position=position_jitter(width=0.01)) +
  labs(x=expression(paste(log[10],'(N:P)')),y=expression(paste(log[10],'(Relative proportion)'))) +
  scale_x_continuous(limits=c(0.55, 1.9), breaks=seq(0.6, 1.8, 0.3)) +
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs),
        legend.title = element_text(size=fs),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  theme(legend.position = c(0.85, 0.15)) + labs(color="growth")+
  scale_color_gradient(low="yellow",high="blue")

p2 <- ggplot(data=averages_and_sds, aes(x=log10(`N/P`), y=mean)) +
  geom_point(color="blue", size=2) +  
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2) +
  #geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.01) +
  theme(plot.title = element_text(hjust = 0.5,size = fss), 
                        axis.text=element_text(size=fss,color = "black"),
                        axis.title.x=element_text(size=fss,color = "black"), 
                        axis.title.y=element_text(size=fss,color = "black"),
                        legend.text=element_text(size=fss),
                        legend.title = element_text(size=fss),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5)) +
   geom_smooth(method="loess", linewidth=1)
p1 <- p1 + inset_element(p2, left = 0.6, bottom = 0.01, right = 0.99, top = 0.34)

averages_and_sds <- combined_data %>%
  group_by(`N/P`) %>%
  summarise(mean = mean(defense), sd = sd(defense))
p3 <- ggplot(data=combined_data, aes(x=log10(`N/P`), y=log10(spe_propor), color=defense)) +
  geom_point(shape=21, size=3, alpha=0.9, stroke=1.5, position=position_jitter(width=0.01)) +
  labs(x=expression(paste(log[10],'(N:P)')), y=expression(paste(log[10],'(Relative proportion)'))) +
  scale_x_continuous(limits=c(0.55, 1.9), breaks=seq(0.6, 1.8, 0.3)) +
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs-0.5),
        legend.title = element_text(size=fs),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  theme(legend.position = c(0.85, 0.15)) + labs(size="")+
  scale_color_gradient(low="yellow",high="blue")
fss =8
p4 <- ggplot(data=averages_and_sds, aes(x=log10(`N/P`), y=mean)) +
  geom_point(color="blue", size=2) +  
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.01) +
  theme(plot.title = element_text(hjust = 0.5,size = fss), 
        axis.text=element_text(size=fss,color = "black"),
        axis.title.x=element_text(size=fss,color = "black"), 
        axis.title.y=element_text(size=fss,color = "black"),
        legend.text=element_text(size=fss),
        legend.title = element_text(size=fss),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  geom_smooth(method="loess", linewidth=1)
p3 <- p3 + inset_element(p4, left = 0.005, bottom = 0.005, right = 0.38, top = 0.32)

p <- ggarrange(p1, p3, ncol = 2, nrow = 1)
p
ggsave("NP_growth_defense.tif", plot=p, units="in", dpi=600, width=15, height=7, device="png")



##--new figure 4------------------
seed=1234
colors <- c("#BEBEBE","#FF8F59","#FFDC35","#84C1FF","#BE77FF")
fs=20
combined_data$Date <- as.factor(combined_data$Date)
p1 <- ggplot(combined_data, aes(x = defense,y = maximum_growth_rate)) +
  geom_point(aes(fill=class...7),shape=21, alpha=0.8, stroke=0.5, size = 5, position=position_jitter(width=0.03,height=0.05)) +
  labs(x = "Defense", y = "Maximum growth rate")+
  scale_color_gradient(low = "green", high = "red") +
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs-2),
        legend.title = element_text(size=fs),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.background = element_blank(), 
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  theme(legend.position = c(0.25, 0.18)) + 
  scale_x_continuous(limits=c(-0.03, 1.03), breaks=seq(0, 1, 0.25))+
  scale_fill_manual(values = colors) + labs(fill="") +
  geom_smooth(method="nls",linewidth=1.5)
p1

p2 <- ggplot(combined_data, aes(x = defense,y = maximum_growth_rate, fill= log10(`N/P`))) +
  geom_point(shape=21, alpha=0.8, stroke=0.5, size = 5, position=position_jitter(width=0.03,height=0.05)) +
  labs(x = "Defense", y = "Maximum growth rate")+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs-2),
        legend.title = element_text(size=fs-2),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.background = element_blank(),  
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  theme(legend.position = c(0.15, 0.18)) + labs(size="")+
  scale_x_continuous(limits=c(-0.03, 1.03), breaks=seq(0, 1, 0.25))+
  labs(fill = expression(paste(log[10],'(N:P)'))) +
  scale_fill_gradient(low = "#D2E9FF", high = "#0072E3") 
p2

p <- ggarrange(p1, p2, ncol = 2, nrow = 1,
               labels = c("A","B"),
               label.x = 0.07, label.y = 1.01,
               font.label = list(size = fs, face = "bold"))
p
ggsave("Figure 4.tif", plot=p, units="in", dpi=600, width=12, height=5.7, device="png")


#topptx(p1,"E:/0-PRE-environmental-phytoplankton traits/figures/tradeoff in PRE.pptx", width = 6, height = 5)
test <- subset(combined_data,combined_data$class...7 == "Bacillariophyceae" )
ggplot(test, aes(x = log10(`N/P`), y = log10(test$RUE_zoo/test$spe_bio), fill= defense)) +
  geom_point(shape=21, alpha=0.8, stroke=0.5, size = 5, position=position_jitter(width=0.03,height=0.05)) 

##---plot Figure 5------------------
combined_data$Date <- as.factor(combined_data$Date)

combined_data_04 <- combined_data[combined_data$Date == '2023.04', ]
combined_data_09 <- combined_data[combined_data$Date == '2023.09', ]
#combined_data_04 <- combined_data[log10(combined_data$N.P) <= 1, ]
#combined_data_09 <- combined_data[log10(combined_data$N.P) > 1 & log10(combined_data$N.P) <= 1.5, ]

combined_data_04$spe_RUE_phy_P <- log10(combined_data_04$spe_bio/combined_data_04$PO4)
combined_data_04$spe_RUE_zoo <- log10(combined_data_04$RUE_zoo/combined_data_04$spe_bio)
combined_data_09$spe_RUE_phy_P <- log10(combined_data_09$spe_bio/combined_data_09$PO4)
combined_data_09$spe_RUE_zoo <- log10(combined_data_09$RUE_zoo/combined_data_09$spe_bio)
combined_data_04$spe_RUE_phy_N <- log10(combined_data_04$spe_bio/combined_data_04$DIN)
combined_data_09$spe_RUE_phy_N <- log10(combined_data_09$spe_bio/combined_data_09$DIN)

combined_data_04 <- combined_data_04 %>% distinct()
combined_data_09 <- combined_data_09 %>% distinct()

normalized_fill <- (combined_data_04$spe_RUE_phy_P - min(combined_data_04$spe_RUE_phy_P,combined_data_04$spe_RUE_zoo, na.rm = TRUE)) / 
  (max(combined_data_04$spe_RUE_phy_P,combined_data_04$spe_RUE_zoo, na.rm = TRUE) - min(combined_data_04$spe_RUE_phy_P,combined_data_04$spe_RUE_zoo, na.rm = TRUE))
normalized_size <- (log10(combined_data_04$spe_bio) - min(log10(combined_data_04$spe_bio), na.rm = TRUE)) / 
  (max(log10(combined_data_04$spe_bio), na.rm = TRUE) - min(log10(combined_data_04$spe_bio), na.rm = TRUE))
combined_data_04$adjusted_size <- normalized_size
combined_data_04$adjusted_fill <- normalized_fill


fs=20
p1 <- ggplot(combined_data_04, aes(x = defense, y = maximum_growth_rate,
                                  fill = adjusted_fill, size = adjusted_size^4+3)) +
  geom_point(shape=21, alpha=0.5, stroke=0.25, position=position_jitter(width=0.03,height=0.05)) +  #
  labs(y = "maximum growth rate", x = "defense")+
  scale_x_continuous(limits=c(-0.03, 1.03), breaks=seq(0, 1, 0.25)) +
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs),
        legend.title = element_text(size=fs),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  scale_fill_viridis(limits = c(0, 1)) +
  theme(legend.position = c(0.12, 0.3)) + labs(fill = expression(RUE)) +
  guides(size ="none") + guides(fill ="none") 
p1

normalized_fill <- (combined_data_04$spe_RUE_zoo - min(combined_data_04$spe_RUE_phy_P,combined_data_04$spe_RUE_zoo, na.rm = TRUE)) / 
  (max(combined_data_04$spe_RUE_phy_P,combined_data_04$spe_RUE_zoo, na.rm = TRUE) - min(combined_data_04$spe_RUE_phy_P,combined_data_04$spe_RUE_zoo, na.rm = TRUE))
normalized_size <- (log10(combined_data_04$spe_bio) - min(log10(combined_data_04$spe_bio), na.rm = TRUE)) / 
  (max(log10(combined_data_04$spe_bio), na.rm = TRUE) - min(log10(combined_data_04$spe_bio), na.rm = TRUE))
combined_data_04$adjusted_size <- normalized_size
combined_data_04$adjusted_fill <- normalized_fill
p2 <- ggplot(combined_data_04, aes(x = defense,y = maximum_growth_rate, 
                                   fill = adjusted_fill, size = adjusted_size^4+3)) +
  geom_point(shape=21, alpha=0.5, stroke=0.25, position=position_jitter(width=0.03,height=0.05)) +
  labs(y = "maximum growth rate", x = "defense")+
  scale_x_continuous(limits=c(-0.03, 1.03), breaks=seq(0, 1, 0.25)) +
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs),
        legend.title = element_text(size=fs),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  labs(size="")+
  scale_fill_viridis(limits = c(0, 1))  + guides(size ="none") + guides(fill="none") 
 
p2

normalized_fill <- (combined_data_09$spe_RUE_phy_P - min(combined_data_09$spe_RUE_phy_P,combined_data_09$spe_RUE_zoo, na.rm = TRUE)) / 
  (max(combined_data_09$spe_RUE_phy_P,combined_data_09$spe_RUE_zoo, na.rm = TRUE) - min(combined_data_09$spe_RUE_phy_P,combined_data_09$spe_RUE_zoo, na.rm = TRUE))
normalized_size <- (log10(combined_data_09$spe_bio) - min(log10(combined_data_09$spe_bio), na.rm = TRUE)) / 
  (max(log10(combined_data_09$spe_bio), na.rm = TRUE) - min(log10(combined_data_09$spe_bio), na.rm = TRUE))
combined_data_09$adjusted_size <- normalized_size
combined_data_09$adjusted_fill <- normalized_fill

p3 <- ggplot(combined_data_09, aes(x = defense,y = maximum_growth_rate, 
                                   fill = adjusted_fill, size = adjusted_size^4+3)) +
  geom_point(shape=21, alpha=0.5, stroke=0.25, position=position_jitter(width=0.03,height=0.05)) +
  labs(y = "maximum growth rate", x = "defense")+
  scale_x_continuous(limits=c(-0.03, 1.03), breaks=seq(0, 1, 0.25)) +
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs),
        legend.title = element_text(size=fs),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  theme(legend.position = c(0.12, 0.3)) + labs(size="")+ labs(fill ="")+
  scale_fill_viridis(limits = c(0, 1))  + guides(fill ="none")  + guides(size ="none")#guides(fill="none") +
 # scale_size_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) # 设定了尺寸范围和图例的关键点
  #guides(size = guide_legend(override.aes = list(size = seq(0, 10, by = 2))))
p3

normalized_fill <- (combined_data_09$spe_RUE_zoo - min(combined_data_09$spe_RUE_phy_P,combined_data_09$spe_RUE_zoo, na.rm = TRUE)) / 
  (max(combined_data_09$spe_RUE_phy_P,combined_data_09$spe_RUE_zoo, na.rm = TRUE) - min(combined_data_09$spe_RUE_phy_P,combined_data_09$spe_RUE_zoo, na.rm = TRUE))
normalized_size <- (log10(combined_data_09$spe_bio) - min(log10(combined_data_09$spe_bio), na.rm = TRUE)) / 
  (max(log10(combined_data_09$spe_bio), na.rm = TRUE) - min(log10(combined_data_09$spe_bio), na.rm = TRUE))
combined_data_09$adjusted_size <- normalized_size
combined_data_09$adjusted_fill <- normalized_fill
p4 <- ggplot(combined_data_09, aes(x = defense,y = maximum_growth_rate, 
                                   fill = adjusted_fill, size = adjusted_size^4+3)) +
  geom_point(shape=21, alpha=0.5, stroke=0.25, position=position_jitter(width=0.03,height=0.05)) +
  labs(y = "maximum growth rate", x = "defense")+
  scale_x_continuous(limits=c(-0.03, 1.03), breaks=seq(0, 1, 0.25)) +
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs),
        legend.title = element_text(size=fs),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  scale_fill_viridis(limits = c(0, 1))+ guides(size ="none") + guides(fill="none") 
p4


p5 <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
p5
p51 <- ggarrange(p1, p2, ncol = 2, nrow = 1)
p51
my_plot <- recordPlot()
topptx(p5,"E:/0-PRE-environmental-phytoplankton traits/data and code/figure5--N.pptx", width = 9, height = 7.5)

##--N--

normalized_fill <- (combined_data_04$spe_RUE_phy_N - min(combined_data_04$spe_RUE_phy_N,combined_data_04$spe_RUE_zoo, na.rm = TRUE)) / 
  (max(combined_data_04$spe_RUE_phy_N,combined_data_04$spe_RUE_zoo, na.rm = TRUE) - min(combined_data_04$spe_RUE_phy_N,combined_data_04$spe_RUE_zoo, na.rm = TRUE))
normalized_size <- (log10(combined_data_04$spe_bio) - min(log10(combined_data_04$spe_bio), na.rm = TRUE)) / 
  (max(log10(combined_data_04$spe_bio), na.rm = TRUE) - min(log10(combined_data_04$spe_bio), na.rm = TRUE))
combined_data_04$adjusted_size <- normalized_size
combined_data_04$adjusted_fill <- normalized_fill

p1 <- ggplot(combined_data_04, aes(x = defense, y = maximum_growth_rate,
                                   fill = adjusted_fill, size = adjusted_size^4+3)) +
  geom_point(shape=21, alpha=0.5, stroke=0.25, position=position_jitter(width=0.03,height=0.05)) +
  labs(y = "maximum growth rate", x = "defense")+
  scale_x_continuous(limits=c(-0.03, 1.03), breaks=seq(0, 1, 0.25)) +
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs),
        legend.title = element_text(size=fs),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  scale_fill_viridis(limits = c(0, 1)) +
  theme(legend.position = c(0.12, 0.3)) + labs(fill = expression(RUE)) +
  guides(size ="none")  + guides(fill ="none") 
p1

normalized_fill <- (combined_data_04$spe_RUE_zoo - min(combined_data_04$spe_RUE_phy_N,combined_data_04$spe_RUE_zoo, na.rm = TRUE)) / 
  (max(combined_data_04$spe_RUE_phy_N,combined_data_04$spe_RUE_zoo, na.rm = TRUE) - min(combined_data_04$spe_RUE_phy_N,combined_data_04$spe_RUE_zoo, na.rm = TRUE))
normalized_size <- (log10(combined_data_04$spe_bio) - min(log10(combined_data_04$spe_bio), na.rm = TRUE)) / 
  (max(log10(combined_data_04$spe_bio), na.rm = TRUE) - min(log10(combined_data_04$spe_bio), na.rm = TRUE))
combined_data_04$adjusted_size <- normalized_size
combined_data_04$adjusted_fill <- normalized_fill
p2 <- ggplot(combined_data_04, aes(x = defense,y = maximum_growth_rate, 
                                   fill = adjusted_fill, size = adjusted_size^4+3)) +
  geom_point(shape=21, alpha=0.5, stroke=0.25, position=position_jitter(width=0.03,height=0.05)) +
  labs(y = "maximum growth rate", x = "defense")+
  scale_x_continuous(limits=c(-0.03, 1.03), breaks=seq(0, 1, 0.25)) +
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs),
        legend.title = element_text(size=fs),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  labs(size="")+
  scale_fill_viridis(limits = c(0, 1))  + guides(size ="none") + guides(fill="none") 

p2


normalized_fill <- (combined_data_09$spe_RUE_phy_N - min(combined_data_09$spe_RUE_phy_N,combined_data_09$spe_RUE_zoo, na.rm = TRUE)) / 
  (max(combined_data_09$spe_RUE_phy_N,combined_data_09$spe_RUE_zoo, na.rm = TRUE) - min(combined_data_09$spe_RUE_phy_N,combined_data_09$spe_RUE_zoo, na.rm = TRUE))
normalized_size <- (log10(combined_data_09$spe_bio) - min(log10(combined_data_09$spe_bio), na.rm = TRUE)) / 
  (max(log10(combined_data_09$spe_bio), na.rm = TRUE) - min(log10(combined_data_09$spe_bio), na.rm = TRUE))
combined_data_09$adjusted_size <- normalized_size
combined_data_09$adjusted_fill <- normalized_fill

p3 <- ggplot(combined_data_09, aes(x = defense,y = maximum_growth_rate, 
                                   fill = adjusted_fill, size = adjusted_size^4+3)) +
  geom_point(shape=21, alpha=0.5, stroke=0.25, position=position_jitter(width=0.03,height=0.05)) +
  labs(y = "maximum growth rate", x = "defense")+
  scale_x_continuous(limits=c(-0.03, 1.03), breaks=seq(0, 1, 0.25)) +
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs),
        legend.title = element_text(size=fs),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  theme(legend.position = c(0.12, 0.3)) + labs(size="")+ labs(fill ="")+
  scale_fill_viridis(limits = c(0, 1))  + guides(fill ="none")  +guides(size ="none")#guides(fill="none") +
# scale_size_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) # 设定了尺寸范围和图例的关键点
#guides(size = guide_legend(override.aes = list(size = seq(0, 10, by = 2))))
p3

normalized_fill <- (combined_data_09$spe_RUE_zoo - min(combined_data_09$spe_RUE_phy_N,combined_data_09$spe_RUE_zoo, na.rm = TRUE)) / 
  (max(combined_data_09$spe_RUE_phy_N,combined_data_09$spe_RUE_zoo, na.rm = TRUE) - min(combined_data_09$spe_RUE_phy_N,combined_data_09$spe_RUE_zoo, na.rm = TRUE))
normalized_size <- (log10(combined_data_09$spe_bio) - min(log10(combined_data_09$spe_bio), na.rm = TRUE)) / 
  (max(log10(combined_data_09$spe_bio), na.rm = TRUE) - min(log10(combined_data_09$spe_bio), na.rm = TRUE))
combined_data_09$adjusted_size <- normalized_size
combined_data_09$adjusted_fill <- normalized_fill
p4 <- ggplot(combined_data_09, aes(x = defense,y = maximum_growth_rate, 
                                   fill = adjusted_fill, size = adjusted_size^4+3)) +
  geom_point(shape=21, alpha=0.5, stroke=0.25, position=position_jitter(width=0.03,height=0.05)) +
  labs(y = "maximum growth rate", x = "defense")+
  scale_x_continuous(limits=c(-0.03, 1.03), breaks=seq(0, 1, 0.25)) +
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs),
        legend.title = element_text(size=fs),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  scale_fill_viridis(limits = c(0, 1))+ guides(size ="none") + guides(fill="none") 
p4


p5 <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
p5
p51 <- ggarrange(p1, p2, ncol = 2, nrow = 1)
p51
my_plot <- recordPlot()
topptx(p5,"E:/0-PRE-environmental-phytoplankton traits/data and code/figure5--N.pptx", width = 9, height = 7.5)
graphics.off()
ggsave("Fig 51.tif", plot=p4, units="in", dpi=600, width=7, height=7, device="png")

##--NMDS analysis-----------------
nmds_data <- rbind(combined_data_04,combined_data_09)
nmds_data <- rawdata
nmds_data$`N/P` <- log10(nmds_data$`N/P`)
selected_vars <- nmds_data[c("Date","phy_bio","zoo_bio","RUE_phy_P","RUE_phy_N",
                                 "RUE_zoo")]
clean_data <- na.omit(selected_vars)
nmds <- metaMDS(clean_data[,-1], distance = "bray", k = 2)
stressplot(nmds)
plot(nmds)
nmds$stress
nmds_dis_site <- data.frame(nmds$points)
merged <- cbind(nmds_dis_site,clean_data[,1])
merged$Date <- as.factor(merged$Date)
p <- ggplot(data = merged, aes(MDS1, MDS2)) +
  geom_point(size=2,aes(color =  Date)) +
  stat_ellipse(aes(fill = Date), geom = 'polygon', level = 0.95, alpha = 0.1, show.legend = FALSE) +  #添加置信椭圆，注意不是聚类
  #scale_color_manual(values =color[1:length(unique(map$group))]) +
  #scale_fill_manual(values = color[1:length(unique(map$group))]) +
  theme(panel.grid.major = element_line(color = 'gray', size = 0.2), panel.background = element_rect(color = 'black', fill = 'transparent'), 
        plot.title = element_text(hjust = 0.5),legend.title = element_blank()) +
  #, legend.position = 'none'
  geom_vline(xintercept = 0, color = 'gray', size = 0.5) +
  geom_hline(yintercept = 0, color = 'gray', size = 0.5)
  #geom_text(data = species_top10, aes(label = name), color ="royalblue4", size = 4)+
 # geom_text(data = merged, aes(x =MDS1, y = MDS2), size=4, check_overlap = TRUE)
p


plot(merged$MDS1,merged$MDS2)

###----PCA analysis----------------------
PCA_date <- 
clean_data <- na.omit(selected_vars)
library(FactoMineR)
library(factoextra)
res.pca <- PCA(clean_data[,-1])
summary(res.pca)

# 提取变量的分析结果
var <- get_pca_var(res.pca)
var
get_eigenvalue(res.pca)
fviz_eig(res.pca)
get_pca_ind(res.pca)
get_pca_var(res.pca)
fviz_pca_ind(res.pca)
fviz_pca_var(res.pca)
fviz_pca_biplot(res.pca)
p1 <- fviz_pca_ind(res.pca,
             geom.ind = "point", # 只显示点而不显示文本，默认都显示
             # 设定分类种类
             col.ind = clean_data$Date,
             # 设定颜色
            # palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,# 添加置信椭圆
             legend.title = "Groups",
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # 变量颜色
                col.ind = "#696969"  # 样本颜色
)

fviz_pca_biplot(res.pca, label ="var")
fviz_pca_biplot(res.pca, 
                col.ind = clean_data$Date, 
                addEllipses = TRUE,
                label = "var",
                col.var = "black", 
                repel = TRUE,
                legend.title = "Species")
p1 <- fviz_pca_biplot(res.pca, 
                col.ind = clean_data$Date, 
                addEllipses = TRUE,
                label = "var",
                col.var = "black", 
                repel = TRUE,
                legend.title = "Species")+
  ggpubr::fill_palette("jco")+      #样本填充颜色，使用jco期刊的风格
  ggpubr::color_palette("npg")      #变量颜色，使用npg期刊的风格

topptx(p1,"E:/0-PRE-environmental-phytoplankton traits/data and code/PCA.pptx", width = 5.5, height = 7.5)

##--MANTEL TEST---------04 09 SEPERATE-------
library(linkET)
library(cols4all)
spe_04 <- combined_data[c("spe_bio","zoo_bio","maximum_growth_rate","defense","RUE_phy_N","RUE_phy_P","RUE_zoo")]
env_04 <- combined_data[c("Depth","Velocity","WaterTemp","salinity",
                             "DO_mg","TN","DIN","TP","PO4","N/P","chla")]
spe_04_clean <- spe_04
spe_04_clean <- lapply(spe_04_clean, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
spe_04_clean <- as.data.frame(spe_04_clean)
env_04_clean <- env_04
#spe_04_clean$spe_RUE_phy_P <- 10^(spe_04_clean$spe_RUE_phy_P)
#spe_04_clean$spe_RUE_zoo <- 10^(spe_04_clean$spe_RUE_zoo)
spe_04_clean$spe_bio <- log10(spe_04_clean$spe_bio)
spe_04_clean$zoo_bio <- log10(spe_04_clean$zoo_bio)
#env_04_clean$`N/P` <- log10(env_04_clean$`N/P`)

cor2 <- correlate(env_04_clean)
corr2 <- cor2 %>% as_md_tbl()
head(corr2)
#mantel test:
mantel <- mantel_test(spe_04_clean, env_04_clean,
                      mantel_fun = "mantel",
                      spec_select = list(Bphy = 1,
                                         Bzoo = 2,
                                         growth = 3,
                                         defense = 4,
                                         RUEphyP = 5,
                                         RUEzoo = 6)) 
head(mantel)
mantel2 <- mantel %>%
  mutate(r = cut(r, breaks = c(-Inf, 0.2, 0.4,0.6, Inf),
                 labels = c("<0.2", "0.2-0.4","0.4-0.6", ">0.6")),
         p = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                 labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))
head(mantel2)

fs=16
p1 <- qcorrplot(cor2,
                grid_col = "grey50",
                grid_size = 0.2,
                type = "upper",
                diag = FALSE) +
  geom_square() +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu"),
                       limits = c(-1, 1)) +
  geom_mark(size = 4,
            only_mark = T,
            sig_level = c(0.05, 0.01, 0.001),
            sig_thres = 0.05,
            colour = 'white') +
  geom_couple(data = mantel2,
              aes(colour = p, size = r),
              curvature = nice_curvature()) +
  scale_size_manual(values = c(0.5, 1.2, 2,2.5)) + #连线粗细
  scale_colour_manual(values = c(c4a('brewer.set2',2),"grey70")) + #连线配色
  #修改图例：
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"),
                             order = 2),
         colour = guide_legend(title = "Mantel's p",
                               override.aes = list(size = 3),
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3))+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs),
        legend.title = element_text(size=fs))
  
p1

topptx(p1,"./mantel-1.pptx", width = 10, height = 16)



spe_09 <- combined_data_09[c("spe_bio","zoo_bio","maximum_growth_rate","defense","spe_RUE_phy_P","spe_RUE_zoo")]
env_09 <- combined_data_09[c("Depth","WaterTemp","salinity",
                             "DO_mg","pH","DIN","PO4","N/P","chla")]
spe_09_clean <- spe_09
env_09_clean <- env_09
spe_09_clean$spe_RUE_phy_P <- 10^spe_09_clean$spe_RUE_phy_P
spe_09_clean$spe_RUE_zoo <- 10^spe_09_clean$spe_RUE_zoo

cor2 <- correlate(env_09_clean)
corr2 <- cor2 %>% as_md_tbl()
head(corr2)
#mantel test:
mantel <- mantel_test(spe_09_clean, env_09_clean,
                      mantel_fun = 'mantel',
                      spec_select = list(Bphy = 1,
                                         Bzoo = 2,
                                         growth = 3,
                                         defense = 4,
                                         RUEphyP = 5,
                                         RUEzoo = 6 )) 
head(mantel)
#对mantel的r和P值重新赋值（设置绘图标签）：
mantel2 <- mantel %>%
  mutate(r = cut(r, breaks = c(-Inf, 0.2, 0.4, 0.6,Inf),
                 labels = c("<0.2", "0.2-0.4", "0.4-0.6",">=0.6")),
         p = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                 labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))
head(mantel2)
#首先，绘制相关性热图(和上文相同):
p4 <- qcorrplot(cor2,
                grid_col = "grey50",
                grid_size = 0.2,
                type = "upper",
                diag = FALSE) +
  geom_square() +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu"),
                       limits = c(-1, 1))
p4
p5 <- p4 +
  geom_mark(size = 4,
            only_mark = T,
            sig_level = c(0.05, 0.01, 0.001),
            sig_thres = 0.05,
            colour = 'white')
p5
p6 <- p5 +
  geom_couple(data = mantel2,
              aes(colour = p, size = r),
              curvature = nice_curvature())
p7 <- p6 +
  scale_size_manual(values = c(0.6, 1, 1.7,2.5)) + #连线粗细
  scale_colour_manual(values = c(c4a('brewer.set2',2),"grey70")) + #连线配色
  #修改图例：
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"),
                             order = 2),
         colour = guide_legend(title = "Mantel's p",
                               override.aes = list(size = 3),
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3))
p7

###----trade off curve----------------
#b_start <- max(normalized_growth_rate) - min(normalized_growth_rate)
#c_start <- min(normalized_growth_rate)
#install.packages("minpack.lm")
library(minpack.lm)
#normalized_growth_rate <- (combined_data$maximum_growth_rate - min(combined_data$maximum_growth_rate)) / (max(combined_data$maximum_growth_rate) - min(combined_data$maximum_growth_rate))
maximum_growth_rate <- combined_data$maximum_growth_rate
normalized_defense <- combined_data$defense
summary(maximum_growth_rate)
summary(normalized_defense)
plot(normalized_defense,maximum_growth_rate)

nls_fit <- nlsLM(maximum_growth_rate ~ b * (1 - normalized_defense)^a + c, 
               start = list(a = 0.2, b = 1, c = 0.8))
coef(nls_fit)
confint(nls_fit)
summary(nls_fit)
AIC_value <- AIC(nls_fit)
BIC_value <- BIC(nls_fit)

fitted_values <- fitted(nls_fit)
residuals <- residuals(nls_fit)
params <- summary(nls_fit)$parameters
print(params)
a = params["a", "Estimate"]
b = params["b", "Estimate"]
c = params["c", "Estimate"]
# You can then use these parameters to plot the fitted curve over the observed data
plotdata <- data.frame(maximum_growth_rate,normalized_defense)
fs=20
p1 <- ggplot(data=plotdata, aes(x=normalized_defense, y=maximum_growth_rate)) +
  geom_point(size=5, shape=21, alpha=0.8, fill="grey70",
             position=position_jitter(width=0.03, height=0.05)) +
  stat_function(fun = function(x) {1.6 * (1.03 - x)^0.2 + 0.8}, 
                color = "blue", linewidth = 2)+
  labs(x = "defense", y = "maximum growth rate")+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs-2),
        legend.title = element_text(size=fs-2),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.background = element_blank(),  
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  scale_x_continuous(limits=c(-0.03, 1.03), breaks=seq(0, 1, 0.25)) 
  #scale_y_continuous(limits=c(0.5, 3), breaks=seq(0.5, 2.5, 0.5)) 
p1

maximum_growth_rate_04 <- combined_data_04$maximum_growth_rate
normalized_defense_04 <- combined_data_04$defense
nls_fit <- nlsLM(maximum_growth_rate_04 ~ b * (1 - normalized_defense_04)^a + c, 
                 start = list(a = 0.05, b = 0.8, c = 0.9))
confint_nls <- confint(nls_fit)
print(confint_nls)
ssr <- sum(residuals(nls_fit)^2)
sst <- sum((maximum_growth_rate - mean(maximum_growth_rate))^2)
rsquared <- 1 - ssr/sst
print(paste("R-squared: ", round(rsquared, 4)))

params <- summary(nls_fit)$parameters
print(params)
a2 = params["a", "Estimate"]
b2 = params["b", "Estimate"]
c2 = params["c", "Estimate"]
# You can then use these parameters to plot the fitted curve over the observed data
plotdata_04 <- data.frame(maximum_growth_rate_04,normalized_defense_04)
fs=20
p2 <- ggplot(data=plotdata_04, aes(x=normalized_defense_04, y=maximum_growth_rate_04)) +
  geom_point(size=5, shape=21, alpha=0.8, fill="grey70",position=position_jitter(width=0.03, height=0.05)) +
  stat_function(fun = function(x) {b2 * (1.03 - x)^a2 + c2}, 
                color = "blue", linewidth = 2)+
  labs(x = "defense", y = "maximum growth rate")+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs-2),
        legend.title = element_text(size=fs-2),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.background = element_blank(),  
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  scale_x_continuous(limits=c(-0.03, 1.03), breaks=seq(0, 1, 0.25)) 
p2


maximum_growth_rate_09 <- combined_data_09$maximum_growth_rate
normalized_defense_09 <- combined_data_09$defense
nls_fit <- nlsLM(maximum_growth_rate_09 ~ b * (1 - normalized_defense_09)^a + c, 
                 start = list(a = 0.2, b = 1, c = 0.8))
params <- summary(nls_fit)$parameters
print(params)
a3 = params["a", "Estimate"]
b3 = params["b", "Estimate"]
c3 = params["c", "Estimate"]
# You can then use these parameters to plot the fitted curve over the observed data
plotdata_09 <- data.frame(maximum_growth_rate_09,normalized_defense_09)
fs=20
p3 <- ggplot(data=plotdata_09, aes(x=normalized_defense_09, y=maximum_growth_rate_09)) +
  geom_point(size=5, shape=21, alpha=0.8, fill="grey70",position=position_jitter(width=0.03, height=0.05)) +
  stat_function(fun = function(x) {b3 * (1 - x)^a3 + c3}, 
                color = "blue", linewidth = 2)+
  labs(x = "defense", y = "maximum growth rate")+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs-2),
        legend.title = element_text(size=fs-2),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.background = element_blank(),  
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))+
  scale_x_continuous(limits=c(-0.03, 1.03), breaks=seq(0, 1, 0.25)) 
p3

p <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
topptx(p,"E:/0-PRE-environmental-phytoplankton traits/data and code/tradeoffs.pptx", width = 13, height = 4)


##---
 
result <- cor.test(plotdata_04$maximum_growth_rate, plotdata_04$normalized_defense, method = "spearman")
summary(result)
result$statistic  # 检验统计量
result$p.value      # p 值
result$estimate         # Spearman 相关系数

##---
fs = 26
p1 <- ggplot() +
  stat_function(fun = function(x) {1.6 * (1 - x)^0.2 + 0.8}, 
                color = "blue", linewidth = 2)+
  stat_function(fun = function(x) {1.6 * (1 - x)^2.8 + 0.8}, 
                color = "red", linewidth = 2)+
  labs(x = "Defense", y = "Maximum growth rate")+
  theme(plot.title = element_text(hjust = 0.5,size = fs), 
        axis.text=element_text(size=fs,color = "black"),
        axis.title.x=element_text(size=fs,color = "black"), 
        axis.title.y=element_text(size=fs,color = "black"),
        legend.text=element_text(size=fs-2),
        legend.title = element_text(size=fs-2),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.background = element_blank(),  
        panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5))
p1
ggsave("Figure S2.tif", plot=p1, units="in", dpi=600, width=8, height=8, device="png")




