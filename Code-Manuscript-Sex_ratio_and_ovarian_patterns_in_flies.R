############################################################################################################################
# SUPPORTING INFORMATION TO

# Title: Sampling bias in sex ratio and ovarian maturation of Calliphoridae and Mesembrinellidae (Diptera): influence of trap height and bait type in the Brazilian Amazon
# Authors: Bruna L. B. Façanha1*, Rony P. S. Almeida2, José R. P. Sousa3, Leandro Juen 4, Maria C. Esposito5.
# Journal: Annals of the Brazilian Academy of Sciences 
# 1 Universidade Federal do Pará (UFPA), Belém, Pará – Brazil
# 2 Universidade Federal de Sergipe (UFS), Departamento de Biociências. Itabaiana, Sergipe - Brazil
# 3 Universidade Estadual do Maranhão (UEMA), São Luís, Maranhão - Brazil 
# 4 Universidade Federal do Pará (UFPA), Belém, Pará – Brazil
# 5 Universidade Federal do Pará (UFPA), Belém, Pará – Brazil
# *Corresponding author for this script: Rony P. S. Almeida, rony__peterson@hotmail.com

# STEPS IN THIS SCRIPT:
# 1 - Sex ratio analyses at the family and species levels, including a loop for automatic execution 
#      of species and figure generation.
# 2 - Ovarian development analyses at the family and species levels, including a loop for automatic 
#      execution of species and figure generation.
############################################################################################################################

dir()
tab1 <- read.table("Table_SexRatio_Manuscript-SexRatio&OvarianDevelopment_in_sarcosaprophagous_flies.txt", h=T, stringsAsFactors = T)

#Test----
#First row of the spreadsheet
lin_n <- 5
BD <- tab1[lin_n, 3:10]
ta <- matrix(as.numeric(BD), ncol=4)
colnames(ta) <- c("PB", "PA", "BB", "BA")
rownames(ta) <- c("F", "M")

#Test
chisq.test(as.matrix(ta))
c1 <- as.vector(chisq.test(ta))
c2 <- data.frame(cbind(X2=round(c1$statistic,3),
            df=c1$parameter,
            p_r=ifelse(c1$p.value<0.001, "<0.001", paste("=", round(c1$p.value,3), sep=""))))
#row.names(c1) <- "Total Calliphoridae"


library(chisq.posthoc.test)
chisq.posthoc.test(ta)
chisq.posthoc.test(ta, method = "none")

#Object for figure
pc1 <- data.frame(chisq.posthoc.test(ta, method = "none"))
c(pc1[c(1,3),6], pc1[c(1,3),5], pc1[c(1,3),4], pc1[c(1,3),3])
pc1[c(2,4),]

#Figure
abund <- as.numeric(BD)
trat <- as.factor(rep(c("Lung\nLow", "Lung\nHigh", "Banana\nLow", "Banana\nHigh"), each=2))
#"Bovine lung High" e  "Bovine lung Low" para "Lung High" e "Lung Low" 
sexo <- as.factor(rep(c("F", "M"), 4))
plot_sex <- data.frame(sexo, trat, abund)


library(ggmosaic)
library(ggplot2)
p1 <- ggplot(data = plot_sex) +
  geom_mosaic(aes(weight=abund, x = product(trat), fill=sexo), na.rm=F, alpha=0.8, colour=1, offset=0.02) +
  labs(x = "Tratament", y = "Gender", title = gsub("_", " ", tab1[lin_n, 2]),
       subtitle = paste("X²=", c2$X2,"; df=", c2$df, "; p", c2$p_r, sep = ""))+
  scale_fill_manual(values = c("darkseagreen2","darkslategray3"))+
  theme_classic(base_size = 14)+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black", size = .8),
        plot.title = element_text(vjust = -4, color="black"),
        plot.subtitle = element_text(size=12, hjust=0.5, vjust = -4, face="italic", color="black"))
p1

abund2 <- plot_sex[order(plot_sex$trat, decreasing = F),"abund"]

library(tidyverse)
tab_txt = 
  layer_data(p1, 1) %>% 
  select(xmin:ymax) %>% 
  mutate(m.x = (xmin + xmax)/2, m.y =  (ymin + ymax)/2) %>% 
  select(m.x, m.y)  %>% 
  mutate(string = c(abund2))

tab_txt$r2 <- c(pc1[c(1,3),6], pc1[c(1,3),5], pc1[c(1,3),4], pc1[c(1,3),3])
tab_txt$pvalue <- c(pc1[c(2,4),6], pc1[c(2,4),5], pc1[c(2,4),4], pc1[c(2,4),3])
tab_txt$signif <- ifelse(tab_txt$pvalue<0.05, T, F)
tab_txt$dir <- ifelse(tab_txt$r2>0, "↑", "↓")
tab_txt1 <- tab_txt[tab_txt$signif==T,]
tab_txt1$signif <- ifelse(c1$p.value<0.05, T, F)
tab_txt1 <- tab_txt1[tab_txt1$signif==T,]

#Inserting the values
p1 + geom_text(data = tab_txt, aes(x = m.x, y = m.y, label = string))
p1 + geom_text(data = tab_txt1, aes(x = m.x, y = m.y, label = dir))



#1 - Sex----
#Automating----
library(viridis)
rev(viridis(n = 3))
gg_sex <- function(lin_n){
  BD <- tab1[lin_n, 3:10]
  ta <- matrix(as.numeric(BD), ncol=4)
  colnames(ta) <- c("PB", "PA", "BB", "BA")
  rownames(ta) <- c("F", "M")
  
  c1 <- as.vector(chisq.test(ta))
  c2 <- data.frame(cbind(X2=round(c1$statistic,3),
                         df=c1$parameter,
                         p_r=ifelse(c1$p.value<0.001, "<0.001", paste("=", round(c1$p.value,3), sep=""))))
  library(chisq.posthoc.test)
  pc1 <- data.frame(chisq.posthoc.test(ta, method = "none"))
  #Figure
  abund <- as.numeric(BD)
  trat <- as.factor(rep(c("Lung\nLow", "Lung\nHigh", "Banana\nLow", "Banana\nHigh"), each=2))
  sexo <- as.factor(rep(c("F", "M"), 4))
  plot_sex <- data.frame(sexo, trat, abund)
  library(ggmosaic)
  p1 <- ggplot(data = plot_sex) +
    geom_mosaic(aes(weight=abund, x = product(trat), fill=sexo), na.rm=F, alpha=0.5, colour=1, offset=0.02) +
    labs(x = "Treatment", y = "Gender", title = gsub("_", " ", tab1[lin_n, 2]),
         subtitle = paste("X²=", c2$X2,"; df=", c2$df, "; p", c2$p_r, sep = ""))+
    #scale_fill_manual(values = c("darkseagreen2","darkslategray3"))+
    scale_fill_manual(values = c("#FDE725FF", "#440154FF"))+
    theme_classic(base_size = 14)+
    theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black", size = .8),
        plot.title = element_text(vjust = -4, color="black"),
        plot.subtitle = element_text(size=12, hjust=0.9, vjust = -4, face="italic", color="black"))
  #Adjustments
  abund2 <- plot_sex[order(plot_sex$trat, decreasing = F),"abund"]
  library(tidyverse)
  tab_txt = 
    layer_data(p1, 1) %>% 
    select(xmin:ymax) %>% 
    mutate(m.x = (xmin + xmax)/2, m.y =  (ymin + ymax)/2) %>% 
    select(m.x, m.y)  %>% 
    mutate(string = c(abund2))
  tab_txt$r2 <- c(pc1[c(1,3),6], pc1[c(1,3),5], pc1[c(1,3),4], pc1[c(1,3),3])
  tab_txt$pvalue <- c(pc1[c(2,4),6], pc1[c(2,4),5], pc1[c(2,4),4], pc1[c(2,4),3])
  tab_txt$signif <- ifelse(tab_txt$pvalue<0.05, T, F)
  tab_txt$dir <- ifelse(tab_txt$r2>0, "↑", "↓")
  tab_txt1 <- tab_txt[tab_txt$signif==T,]
  tab_txt1$signif <- ifelse(c1$p.value<0.05, T, F)
  tab_txt1 <- tab_txt1[tab_txt1$signif==T,]
  #Inserindo os valores
  p2 <- p1 + geom_text(data = tab_txt1, aes(x = m.x, y = m.y+0.01, label = dir), size=5)
  return(p2)
}


#__Figures----
p1 <- gg_sex(1)
p2 <- gg_sex(12)

library(cowplot)
plot_grid(p1+theme(axis.title.x = element_blank()),
          p2, ncol=1, rel_heights = c(1, 1.05))
#ggsave("fig1.png", units = "cm", width = 15, height = 22, dpi = 400)
#ggsave("fig1.tiff", units = "cm", width = 15, height = 22, dpi = 400, compression = "lzw")


#Italicize the species names.
gg_sex <- function(lin_n){
  BD <- tab1[lin_n, 3:10]
  ta <- matrix(as.numeric(BD), ncol=4)
  colnames(ta) <- c("PB", "PA", "BB", "BA")
  rownames(ta) <- c("F", "M")
  
  c1 <- as.vector(chisq.test(ta))
  c2 <- data.frame(cbind(X2=round(c1$statistic,3),
                         df=c1$parameter,
                         p_r=ifelse(c1$p.value<0.001, "<0.001", paste("=", round(c1$p.value,3), sep=""))))
  library(chisq.posthoc.test)
  pc1 <- data.frame(chisq.posthoc.test(ta, method = "none"))
  #Figure
  abund <- as.numeric(BD)
  trat <- as.factor(rep(c("Lung\nLow", "Lung\nHigh", "Banana\nLow", "Banana\nHigh"), each=2))
  sexo <- as.factor(rep(c("F", "M"), 4))
  plot_sex <- data.frame(sexo, trat, abund)
  library(ggmosaic)
  p1 <- ggplot(data = plot_sex) +
    geom_mosaic(aes(weight=abund, x = product(trat), fill=sexo), na.rm=F, alpha=0.5, colour=1, offset=0.02) +
    labs(x = "Treatment", y = "Gender", title = gsub("_", " ", tab1[lin_n, 2]),
         subtitle = paste("X²=", c2$X2,"; df=", c2$df, "; p", c2$p_r, sep = ""))+
    #scale_fill_manual(values = c("darkseagreen2","darkslategray3"))+
    scale_fill_manual(values = c("#FDE725FF", "#440154FF"))+
    theme_classic(base_size = 14)+
    theme(legend.position = "none",
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.line = element_line(color = "black", size = .8),
          plot.title = element_text(vjust = -4, color="black", face="italic"),
          plot.subtitle = element_text(size=12, hjust=0.9, vjust = -4, face="italic", color="black"))
  #Adjustments
  abund2 <- plot_sex[order(plot_sex$trat, decreasing = F),"abund"]
  library(tidyverse)
  tab_txt = 
    layer_data(p1, 1) %>% 
    select(xmin:ymax) %>% 
    mutate(m.x = (xmin + xmax)/2, m.y =  (ymin + ymax)/2) %>% 
    select(m.x, m.y)  %>% 
    mutate(string = c(abund2))
  tab_txt$r2 <- c(pc1[c(1,3),6], pc1[c(1,3),5], pc1[c(1,3),4], pc1[c(1,3),3])
  tab_txt$pvalue <- c(pc1[c(2,4),6], pc1[c(2,4),5], pc1[c(2,4),4], pc1[c(2,4),3])
  tab_txt$signif <- ifelse(tab_txt$pvalue<0.05, T, F)
  tab_txt$dir <- ifelse(tab_txt$r2>0, "↑", "↓")
  tab_txt1 <- tab_txt[tab_txt$signif==T,]
  tab_txt1$signif <- ifelse(c1$p.value<0.05, T, F)
  tab_txt1 <- tab_txt1[tab_txt1$signif==T,]
  #Inserindo os valores
  p2 <- p1 + geom_text(data = tab_txt1, aes(x = m.x, y = m.y+0.01, label = dir), size=5)
  return(p2)
}

p3 <- gg_sex(2)
p4 <- gg_sex(3)
p5 <- gg_sex(4)
p6 <- gg_sex(5)
p7 <- gg_sex(6)
p8 <- gg_sex(7) #removido, sÃ³ um registro 
p9 <- gg_sex(8)
p10 <- gg_sex(9)
p11 <- gg_sex(10) #removido, sÃ³ fÃªmeas
p12 <- gg_sex(11)

p13 <- gg_sex(13)
p14 <- gg_sex(14)
p15 <- gg_sex(15) #removido, sem registros no pulmÃ£o baixo
p16 <- gg_sex(16)


plot_grid(p3+theme(axis.title.x = element_blank()), p4+theme(axis.title.x = element_blank(), axis.title.y = element_blank()), p5+theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
          p6+theme(axis.title.x = element_blank()), p7+theme(axis.title.x = element_blank(), axis.title.y = element_blank()), p9+theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
          p10+theme(axis.title.x = element_blank()), p12+theme(axis.title.x = element_blank(), axis.title.y = element_blank()), p13+theme(axis.title.y = element_blank()),
          p14, p16+theme(axis.title.y = element_blank()), ncol=3, rel_heights = c(1, 1, 1.05), rel_widths = c(1, 1, 1, 1.05))

ggsave("fig2.png", units = "cm", width = 38, height = 42, dpi = 400)
ggsave("fig2.tiff", units = "cm", width = 38, height = 42, dpi = 400, compression = "lzw")

plot_grid(p8+theme(axis.title.x = element_blank(), plot.title = element_text(vjust = 0, color="black")) +labs(subtitle = NULL),
          p11+theme(axis.title.x = element_blank(), plot.title = element_text(vjust = 0, color="black"))+labs(subtitle = NULL),
          p15+theme(plot.title = element_text(vjust = 0, color="black"))+labs(subtitle = NULL),
          ncol=1, rel_widths = c(1, 1, 1.05))
ggsave("figS2.png", units = "cm", width = 18, height = 33, dpi = 400)
ggsave("figS2.tiff", units = "cm", width = 18, height = 33, dpi = 400, compression = "lzw")



#Estatistic----
tab_res_sex <- function(lin_n){
  BD <- tab1[lin_n, 3:10]
  ta <- matrix(as.numeric(BD), ncol=4)
  colnames(ta) <- c("PB", "PA", "BB", "BA")
  rownames(ta) <- c("F", "M")
  c1 <- as.vector(chisq.test(ta))
  c2 <- data.frame(cbind(name=as.vector(tab1[lin_n, 2]),
                         X2=round(c1$statistic,4),
                         p_r=round(c1$p.value,4)))
  library(chisq.posthoc.test)
  pc1 <- data.frame(chisq.posthoc.test(ta, method = "none"))
  pos_name <- c(paste(colnames(pc1[6]), pc1[c(1,3), 1], sep = "_"),
                paste(colnames(pc1[5]), pc1[c(1,3), 1], sep = "_"),
                paste(colnames(pc1[4]), pc1[c(1,3), 1], sep = "_"),
                paste(colnames(pc1[3]), pc1[c(1,3), 1], sep = "_"))
  r2 <- round(c(pc1[c(1,3),6], pc1[c(1,3),5], pc1[c(1,3),4], pc1[c(1,3),3]), 4)
  pvalue <- round(c(pc1[c(2,4),6], pc1[c(2,4),5], pc1[c(2,4),4], pc1[c(2,4),3]),4)
  c3 <- data.frame(name=pos_name, X2=r2, p_r=pvalue)
  c4 <- rbind(c2, c3)
  return(c4)
}


res_sex <- rbind(tab_res_sex(1),
                 tab_res_sex(2),
tab_res_sex(3),
tab_res_sex(4),
tab_res_sex(5),
tab_res_sex(6),
tab_res_sex(7), #removido, sÃ³ um registro 
tab_res_sex(8),
tab_res_sex(9),
tab_res_sex(10), #removido, sÃ³ fÃªmeas
tab_res_sex(11),
tab_res_sex(12),
tab_res_sex(13),
tab_res_sex(14),
tab_res_sex(15), #removido, sem registros no pulmÃ£o baixo
tab_res_sex(16))

#write.table(res_sex,"tab_resultado_sexo.txt", quote = F)




#2 - Ovarian development----
dir()
tab2 <- read.table("Table_OvarianDevelopment_Manuscript-SexRatio&OvarianDevelopment_in_sarcosaprophagous_flies.txt", h=T, stringsAsFactors = T)
tab3 <- tab2[1:15,] #Real 1:15, Freq 16:30
lin_n <- 1

library(viridisLite)
rev(viridis(n = 5))


gg_desC <- function(lin_n){
BD <- tab3[lin_n, -c(1,2,3,8,13,18,23)]
ta <- matrix(as.numeric(BD), ncol=4)
colnames(ta) <- c("PB", "PA", "BB", "BA")
rownames(ta) <- c("1", "2", "3", "4") #"PV", "VI", "VF", "VC"

c1 <- as.vector(chisq.test(ta))
c2 <- data.frame(cbind(X2=round(c1$statistic,3),
                       df=c1$parameter,
                       p_r=ifelse(c1$p.value<0.001, "<0.001", paste("=", round(c1$p.value,3), sep=""))))
library(chisq.posthoc.test)
pc1 <- data.frame(chisq.posthoc.test(ta, method = "none"))
#Figure
abund <- as.numeric(BD)
trat <- as.factor(rep(c("Lung\nLow", "Lung\nHigh", "Banana\nLow", "Banana\nHigh"), each=4))
desenv <- as.factor(rep(c("1", "2", "3", "4"), 4))
plot_sex <- data.frame(desenv, trat, abund)
#cor_tart <- c("gray100", "gray90", "gray80", "gray70")
cor_tart <- c("#FDE725FF", "#5DC863FF", "#21908CFF", "#3B528BFF")
library(ggmosaic)
p1 <- ggplot(data = plot_sex) +
  geom_mosaic(aes(weight=abund, x = product(trat), fill=desenv), na.rm=F, alpha=0.5, colour=1, offset=0.02) +
  labs(x = "Treatment", y = "Development stage ", title = gsub("_", " ", tab3[lin_n, 3]),
       subtitle = paste("X²=", c2$X2,"; df=", c2$df, "; p", c2$p_r, sep = ""))+
  scale_fill_manual(values = cor_tart)+
  theme_classic(base_size = 14)+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black", size = .8),
        plot.title = element_text(vjust = -4, color="black"),
        plot.subtitle = element_text(size=12, hjust=0.9, vjust = -4, face="italic", color="black"))
#Adjustments
abund2 <- plot_sex[order(plot_sex$trat, decreasing = F),"abund"]
library(tidyverse)
tab_txt = 
  layer_data(p1, 1) %>% 
  select(xmin:ymax) %>% 
  mutate(m.x = (xmin + xmax)/2, m.y =  (ymin + ymax)/2) %>% 
  select(m.x, m.y)  %>% 
  mutate(string = c(abund2))
rl <- c(1,3,5,7)
pl <- c(2,4,6,8)
tab_txt$r2 <- c(pc1[rl,6], pc1[rl,5], pc1[rl,4], pc1[rl,3])
tab_txt$pvalue <- c(pc1[pl,6], pc1[pl,5], pc1[pl,4], pc1[pl,3])
tab_txt$signif <- ifelse(tab_txt$pvalue<0.05, T, F)
tab_txt$dir <- ifelse(tab_txt$r2>0, "↑", "↓")
tab_txt1 <- tab_txt[tab_txt$signif==T,]
tab_txt1$signif <- ifelse(c1$p.value<0.05, T, F)
tab_txt1 <- tab_txt1[tab_txt1$signif==T,]
#Inserting the values
p2 <- p1 + geom_text(data = tab_txt1, aes(x = m.x, y = m.y+0.01, label = dir), size=5)
return(p2)
}

gg_desM <- function(lin_n){
BD <- tab3[lin_n, -c(1,2,3)]
ta <- matrix(as.numeric(BD), ncol=4)
colnames(ta) <- c("PB", "PA", "BB", "BA")
rownames(ta) <- c("1", "2", "3", "4", "5") #"PV", "VI", "VF", "VC", "CL

c1 <- as.vector(chisq.test(ta))
c2 <- data.frame(cbind(X2=round(c1$statistic,3),
                       df=c1$parameter,
                       p_r=ifelse(c1$p.value<0.001, "<0.001", paste("=", round(c1$p.value,3), sep=""))))
library(chisq.posthoc.test)
pc1 <- data.frame(chisq.posthoc.test(ta, method = "none"))
#Figure
abund <- as.numeric(BD)
trat <- as.factor(rep(c("Lung\nLow", "Lung\nHigh", "Banana\nLow", "Banana\nHigh"), each=5))
desenv <- as.factor(rep(c("1", "2", "3", "4", "5"), 4))
plot_sex <- data.frame(desenv, trat, abund)
#cor_tart <- c("gray100", "gray90", "gray80", "gray70", "gray60")
cor_tart <- c("#FDE725FF", "#5DC863FF", "#21908CFF", "#3B528BFF", "#440154FF")
library(ggmosaic) 
p1 <- ggplot(data = plot_sex) +
  geom_mosaic(aes(weight=abund, x = product(trat), fill=desenv), na.rm=F, alpha=0.5, colour=1, offset=0.02) +
  labs(x = "Treatment", y = "Development stage ", title = gsub("_", " ", tab3[lin_n, 3]),
       subtitle = paste("X²=", c2$X2,"; df=", c2$df, "; p", c2$p_r, sep = ""))+
  scale_fill_manual(values = cor_tart)+
  theme_classic(base_size = 14)+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black", size = .8),
        plot.title = element_text(vjust = -4, color="black"),
        plot.subtitle = element_text(size=12, hjust=0.9, vjust = -4, face="italic", color="black"))
#Adjustments
abund2 <- plot_sex[order(plot_sex$trat, decreasing = F),"abund"]
library(tidyverse)
tab_txt = 
  layer_data(p1, 1) %>% 
  select(xmin:ymax) %>% 
  mutate(m.x = (xmin + xmax)/2, m.y =  (ymin + ymax)/2) %>% 
  select(m.x, m.y)  %>% 
  mutate(string = c(abund2))
rl <- c(1,3,5,7,9)
pl <- c(2,4,6,8,10)
tab_txt$r2 <- c(pc1[rl,6], pc1[rl,5], pc1[rl,4], pc1[rl,3])
tab_txt$pvalue <- c(pc1[pl,6], pc1[pl,5], pc1[pl,4], pc1[pl,3])
tab_txt$signif <- ifelse(tab_txt$pvalue<0.05, T, F)
tab_txt$dir <- ifelse(tab_txt$r2>0, "↑", "↓")
tab_txt1 <- tab_txt[tab_txt$signif==T,]
tab_txt1$signif <- ifelse(c1$p.value<0.05, T, F)
tab_txt1 <- tab_txt1[tab_txt1$signif==T,]
#Inserting the values
p2 <- p1 + geom_text(data = tab_txt1, aes(x = m.x, y = m.y+0.01, label = dir), size=5)
return(p2)
}

library(cowplot)
library(ggplot2)

#__Figurggplot2#__Figurggplot2#__Figures----
#tab3
p1 <- gg_desC(1)
p2 <- gg_desM(11)

plot_grid(p1+theme(axis.title.x = element_blank()),
          p2, ncol=1, rel_heights = c(1, 1.05))
ggsave("fig3.png", units = "cm", width = 15, height = 22, dpi = 400)
ggsave("fig3.tiff", units = "cm", width = 15, height = 22, dpi = 400, compression = "lzw")


gg_desC <- function(lin_n){
  BD <- tab3[lin_n, -c(1,2,3,8,13,18,23)]
  ta <- matrix(as.numeric(BD), ncol=4)
  colnames(ta) <- c("PB", "PA", "BB", "BA")
  rownames(ta) <- c("1", "2", "3", "4") #"PV", "VI", "VF", "VC"
  
  c1 <- as.vector(chisq.test(ta))
  c2 <- data.frame(cbind(X2=round(c1$statistic,3),
                         df=c1$parameter,
                         p_r=ifelse(c1$p.value<0.001, "<0.001", paste("=", round(c1$p.value,3), sep=""))))
  library(chisq.posthoc.test)
  pc1 <- data.frame(chisq.posthoc.test(ta, method = "none"))
  #Figure
  abund <- as.numeric(BD)
  trat <- as.factor(rep(c("Lung\nLow", "Lung\nHigh", "Banana\nLow", "Banana\nHigh"), each=4))
  desenv <- as.factor(rep(c("1", "2", "3", "4"), 4))
  plot_sex <- data.frame(desenv, trat, abund)
  #cor_tart <- c("gray100", "gray90", "gray80", "gray70")
  cor_tart <- c("#FDE725FF", "#5DC863FF", "#21908CFF", "#3B528BFF")
  library(ggmosaic)
  p1 <- ggplot(data = plot_sex) +
    geom_mosaic(aes(weight=abund, x = product(trat), fill=desenv), na.rm=F, alpha=0.5, colour=1, offset=0.02) +
    labs(x = "Treatment", y = "Development stage ", title = gsub("_", " ", tab3[lin_n, 3]),
         subtitle = paste("X²=", c2$X2,"; df=", c2$df, "; p", c2$p_r, sep = ""))+
    scale_fill_manual(values = cor_tart)+
    theme_classic(base_size = 14)+
    theme(legend.position = "none",
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.line = element_line(color = "black", size = .8),
          plot.title = element_text(vjust = -4, color="black", face="italic"),
          plot.subtitle = element_text(size=12, hjust=0.9, vjust = -4, face="italic", color="black"))
  #Adjustments
  abund2 <- plot_sex[order(plot_sex$trat, decreasing = F),"abund"]
  library(tidyverse)
  tab_txt = 
    layer_data(p1, 1) %>% 
    select(xmin:ymax) %>% 
    mutate(m.x = (xmin + xmax)/2, m.y =  (ymin + ymax)/2) %>% 
    select(m.x, m.y)  %>% 
    mutate(string = c(abund2))
  rl <- c(1,3,5,7)
  pl <- c(2,4,6,8)
  tab_txt$r2 <- c(pc1[rl,6], pc1[rl,5], pc1[rl,4], pc1[rl,3])
  tab_txt$pvalue <- c(pc1[pl,6], pc1[pl,5], pc1[pl,4], pc1[pl,3])
  tab_txt$signif <- ifelse(tab_txt$pvalue<0.05, T, F)
  tab_txt$dir <- ifelse(tab_txt$r2>0, "↑", "↓")
  tab_txt1 <- tab_txt[tab_txt$signif==T,]
  tab_txt1$signif <- ifelse(c1$p.value<0.05, T, F)
  tab_txt1 <- tab_txt1[tab_txt1$signif==T,]
  #Inserting the values
  p2 <- p1 + geom_text(data = tab_txt1, aes(x = m.x, y = m.y+0.01, label = dir), size=5)
  return(p2)
}
gg_desM <- function(lin_n){
  BD <- tab3[lin_n, -c(1,2,3)]
  ta <- matrix(as.numeric(BD), ncol=4)
  colnames(ta) <- c("PB", "PA", "BB", "BA")
  rownames(ta) <- c("1", "2", "3", "4", "5") #"PV", "VI", "VF", "VC", "CL
  
  c1 <- as.vector(chisq.test(ta))
  c2 <- data.frame(cbind(X2=round(c1$statistic,3),
                         df=c1$parameter,
                         p_r=ifelse(c1$p.value<0.001, "<0.001", paste("=", round(c1$p.value,3), sep=""))))
  library(chisq.posthoc.test)
  pc1 <- data.frame(chisq.posthoc.test(ta, method = "none"))
  #Figure
  abund <- as.numeric(BD)
  trat <- as.factor(rep(c("Lung\nLow", "Lung\nHigh", "Banana\nLow", "Banana\nHigh"), each=5))
  desenv <- as.factor(rep(c("1", "2", "3", "4", "5"), 4))
  plot_sex <- data.frame(desenv, trat, abund)
  #cor_tart <- c("gray100", "gray90", "gray80", "gray70", "gray60")
  cor_tart <- c("#FDE725FF", "#5DC863FF", "#21908CFF", "#3B528BFF", "#440154FF")
  library(ggmosaic) 
  p1 <- ggplot(data = plot_sex) +
    geom_mosaic(aes(weight=abund, x = product(trat), fill=desenv), na.rm=F, alpha=0.5, colour=1, offset=0.02) +
    labs(x = "Treatment", y = "Development stage ", title = gsub("_", " ", tab3[lin_n, 3]),
         subtitle = paste("X²=", c2$X2,"; df=", c2$df, "; p", c2$p_r, sep = ""))+
    scale_fill_manual(values = cor_tart)+
    theme_classic(base_size = 14)+
    theme(legend.position = "none",
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.line = element_line(color = "black", size = .8),
          plot.title = element_text(vjust = -4, color="black", face="italic"),
          plot.subtitle = element_text(size=12, hjust=0.9, vjust = -4, face="italic", color="black"))
  #Adjustments
  abund2 <- plot_sex[order(plot_sex$trat, decreasing = F),"abund"]
  library(tidyverse)
  tab_txt = 
    layer_data(p1, 1) %>% 
    select(xmin:ymax) %>% 
    mutate(m.x = (xmin + xmax)/2, m.y =  (ymin + ymax)/2) %>% 
    select(m.x, m.y)  %>% 
    mutate(string = c(abund2))
  rl <- c(1,3,5,7,9)
  pl <- c(2,4,6,8,10)
  tab_txt$r2 <- c(pc1[rl,6], pc1[rl,5], pc1[rl,4], pc1[rl,3])
  tab_txt$pvalue <- c(pc1[pl,6], pc1[pl,5], pc1[pl,4], pc1[pl,3])
  tab_txt$signif <- ifelse(tab_txt$pvalue<0.05, T, F)
  tab_txt$dir <- ifelse(tab_txt$r2>0, "↑", "↓")
  tab_txt1 <- tab_txt[tab_txt$signif==T,]
  tab_txt1$signif <- ifelse(c1$p.value<0.05, T, F)
  tab_txt1 <- tab_txt1[tab_txt1$signif==T,]
  #Inserting the values
  p2 <- p1 + geom_text(data = tab_txt1, aes(x = m.x, y = m.y+0.01, label = dir), size=5)
  return(p2)
}


p3 <- gg_desC(2)
p4 <- gg_desC(3)
p5 <- gg_desC(4)
p6 <- gg_desC(5) #removido, nenhum registro no estÃ¡gio inicial
p7 <- gg_desC(6)
p8 <- gg_desC(7) #removido, nenhum registro no estÃ¡gio inicial
p9 <- gg_desC(8)
p10 <- gg_desC(9)
p11 <- gg_desC(10) #removido, nenhum registro no estÃ¡gio inicial

p13 <- gg_desM(12) #removido, nenhum registro no estÃ¡gio inicial
p14 <- gg_desM(13)
p15 <- gg_desM(14) #removido, nenhum registro no estÃ¡gio inicial
p16 <- gg_desM(15)


plot_grid(p3+theme(axis.title.x = element_blank()), p4+theme(axis.title.x = element_blank(),axis.title.y = element_blank()),
          p5+theme(axis.title.x = element_blank()), p7+theme(axis.title.x = element_blank(),axis.title.y = element_blank()),
          p9+theme(axis.title.x = element_blank()), p10+theme(axis.title.x = element_blank(),axis.title.y = element_blank()),
          p14, p16+theme(axis.title.y = element_blank()), ncol=2, rel_heights = c(1, 1, 1, 1.05), rel_widths = c(1.05, 1))
ggsave("fig4.png", units = "cm", width = 30, height = 44, dpi = 400)
ggsave("fig4.tiff", units = "cm", width = 30, height = 44, dpi = 400, compression = "lzw")


plot_grid(p6+theme(axis.title.x = element_blank(), plot.title = element_text(vjust = 0, color="black")) +labs(subtitle = NULL),
          p8+theme(axis.title.x = element_blank(), plot.title = element_text(vjust = 0, color="black")) +labs(subtitle = NULL),
          p11+theme(axis.title.x = element_blank(), plot.title = element_text(vjust = 0, color="black"))+labs(subtitle = NULL),
          p13+theme(axis.title.x = element_blank(), plot.title = element_text(vjust = 0, color="black")) +labs(subtitle = NULL),
          p15+theme(plot.title = element_text(vjust = 0, color="black"))+labs(subtitle = NULL),
          ncol=2, rel_widths = c(1, 1, 1.05))
ggsave("figS3.png", units = "cm", width = 27, height = 33, dpi = 400)
ggsave("figS3.tiff", units = "cm", width = 27, height = 33, dpi = 400, compression = "lzw")


#Estatistic----
tab_res_desC <- function(lin_n){
  BD <- tab3[lin_n, -c(1,2,3,8,13,18,23)]
  ta <- matrix(as.numeric(BD), ncol=4)
  colnames(ta) <- c("PB", "PA", "BB", "BA")
  rownames(ta) <- c("1", "2", "3", "4") #"PV", "VI", "VF", "VC"
  c1 <- as.vector(chisq.test(ta))
  c2 <- data.frame(cbind(name=as.vector(tab3[lin_n, 3]),
                         X2=round(c1$statistic,5),
                         p_r=round(c1$p.value,5)))
  library(chisq.posthoc.test)
  pc1 <- data.frame(chisq.posthoc.test(ta, method = "none"))
  pos_name <- c(paste(colnames(pc1[6]), pc1[c(1,3,5,7), 1], sep = "_"),
                paste(colnames(pc1[5]), pc1[c(1,3,5,7), 1], sep = "_"),
                paste(colnames(pc1[4]), pc1[c(1,3,5,7), 1], sep = "_"),
                paste(colnames(pc1[3]), pc1[c(1,3,5,7), 1], sep = "_"))
  r2 <- round(c(pc1[c(1,3,5,7),6], pc1[c(1,3,5,7),5], pc1[c(1,3,5,7),4], pc1[c(1,3,5,7),3]), 5)
  pvalue <- round(c(pc1[c(2,4,6,8),6], pc1[c(2,4,6,8),5], pc1[c(2,4,6,8),4], pc1[c(2,4,6,8),3]),5)
  c3 <- data.frame(name=pos_name, X2=r2, p_r=pvalue)
  c4 <- rbind(c2, c3)
  return(c4)
}
tab_res_desM <- function(lin_n){
  BD <- tab3[lin_n, -c(1,2,3)]
  ta <- matrix(as.numeric(BD), ncol=4)
  colnames(ta) <- c("PB", "PA", "BB", "BA")
  rownames(ta) <- c("1", "2", "3", "4", "5") #"PV", "VI", "VF", "VC", "CL
  c1 <- as.vector(chisq.test(ta))
  c2 <- data.frame(cbind(name=as.vector(tab3[lin_n, 3]),
                         X2=round(c1$statistic,4),
                         p_r=round(c1$p.value,4)))
  library(chisq.posthoc.test)
  pc1 <- data.frame(chisq.posthoc.test(ta, method = "none"))
  pos_name <- c(paste(colnames(pc1[6]), pc1[c(1,3,5,7,9), 1], sep = "_"),
                paste(colnames(pc1[5]), pc1[c(1,3,5,7,9), 1], sep = "_"),
                paste(colnames(pc1[4]), pc1[c(1,3,5,7,9), 1], sep = "_"),
                paste(colnames(pc1[3]), pc1[c(1,3,5,7,9), 1], sep = "_"))
  r2 <- round(c(pc1[c(1,3,5,7,9),6], pc1[c(1,3,5,7,9),5], pc1[c(1,3,5,7,9),4], pc1[c(1,3,5,7,9),3]), 4)
  pvalue <- round(c(pc1[c(2,4,6,8,10),6], pc1[c(2,4,6,8,10),5], pc1[c(2,4,6,8,10),4], pc1[c(2,4,6,8,10),3]),4)
  c3 <- data.frame(name=pos_name, X2=r2, p_r=pvalue)
  c4 <- rbind(c2, c3)
  return(c4)
}

res_des <- rbind(tab_res_desC(1),
tab_res_desC(2),
tab_res_desC(3),
tab_res_desC(4),
tab_res_desC(5), #removido, nenhum registro no estÃ¡gio inicial
tab_res_desC(6),
tab_res_desC(7), #removido, nenhum registro no estÃ¡gio inicial
tab_res_desC(8),
tab_res_desC(9),
tab_res_desC(10), #removido, nenhum registro no estÃ¡gio inicial
tab_res_desM(11),
tab_res_desM(12), #removido, nenhum registro no estÃ¡gio inicial
tab_res_desM(13),
tab_res_desM(14), #removido, nenhum registro no estÃ¡gio inicial
tab_res_desM(15))

write.table(res_des,"tab_resultado_desenv.txt", quote = F)


#References----
citation("chisq.posthoc.test")
citation("ggmosaic")
citation("cowplot")


