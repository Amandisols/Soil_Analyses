################
#Jenny Bower
#Elemental analysis
#11-9-2020
#This script was made during a global pandemic
################
library('readxl')
library('tidyverse')
library('ggplot2')
library('reshape')
library('gridExtra')
library('plyr')
library('car')
library('dunn.test')

#to do: incorporate bulk density, make applicable to soil and rock alike
setwd("~/Documents/uvm/phd/research/data/elemental")

nocsvs <- print(list.files('values_tau/hb/'))
taulist <- list()
for(a in seq_along(c(1:length(nocsvs)))){
  csvdata <- read.csv(paste0('values_tau/hb/',nocsvs[a]), header = TRUE, sep = ",")
  taulist[[a]] <- csvdata
}
taulist

#note that your deep C values are tau'ed against their average!!

taudata <- do.call(rbind, taulist)
taudatab <- cbind(taudata, "LV" = "")
taudatas <- cbind(taudatab, "Hor" = "")

i <- sapply(taudatas, is.factor)
taudatas[i] <- lapply(taudatas[i], as.character)

listy <- list()

for(a in c(1:nrow(taudatas))){
  if(as.numeric(substr(taudatas$Name[a],4,4)<4)){
    taudatas[a,grep("LV",colnames(taudatas))] <- "L"
  }
  else{
    taudatas[a,grep("LV",colnames(taudatas))] <- "V"
  }
  listy[a] <- str_extract_all(substr(taudatas$Name[a],nchar(taudatas$Pedon[a])+2,nchar(taudatas$Name[a])),"[:alpha:]")
  taudatas[a,grep("Hor",colnames(taudatas))] <- as.character(paste(listy[[a]],collapse=""))
}

prettylabs <- c("Lateral E", "Lateral Bhs", "Vertical E", "Vertical Bhs", "Deep C")

sdata <- cbind(taudatas, Cat = str_c(taudatas$LV, taudatas$Hor))

dpc <- read_csv("extra/hb_soil_alldeep.csv")
cdata <- cbind(dpc, LV = "", Hor = "", Cat = "DC")

names(cdata) <- names(sdata)

soildata <- rbind(sdata, cdata)

soilsub <- subset(soildata, soildata$Cat == "LE" | soildata$Cat == "LBhs" | soildata$Cat == "VE" | soildata$Cat == "VBhs" | soildata$Cat == "DC")

soilsub$Cat <- factor(soilsub$Cat, levels=c("LE", "LBhs", "VE", "VBhs", "DC"))

#modify this to be dynamic sel_elem <- c('Fe', 'Si', 'Ca', 'P')
#stat_box_data <- function(y, upper_limit = 1.5) {
#  return( 
#    data.frame(
#      y = 0.95 * upper_limit,
#      label = paste('count =', length(y), '\n',
#                    'mean =', round(mean(y), 1), '\n')
#    )
#  )
#}

#for mean only:
#stat_box_data <- function(y, upper_limit = 1) {
#  return( 
#    data.frame(
#      y = 0.95 * upper_limit,
#      label = paste('mean =', round(mean(y), 1), '\n')
#    )
#  )
#}

#p1 <- ggplot(soilsub, aes(x = Cat, y = tau_Al, fill = Cat)) + geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 0.5) + geom_boxplot() + geom_point() + theme_classic() + labs(title = "", x = "Horizon", y = expression(τ[Al])) + stat_summary(
#  fun.data = stat_box_data, 
#  geom = "text", 
#  hjust = 0.5,
#  vjust = 0.9
#) + theme(legend.position = "none") + ggtitle("Soil fine fraction") + theme(plot.title = element_text(size=20,face="bold")) +  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) + scale_fill_manual(values = c("#cccccc","#90593f","#cccccc","#90593f","#fcff54")) + scale_x_discrete(labels = prettylabs) + theme(axis.title.x = element_text(size=20, face="plain"), axis.title.y = element_text(size=20, face="plain"))

p1 <- ggplot(soilsub, aes(x = Cat, y = tau_Al, fill = Cat)) + geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 0.5) + geom_boxplot() + geom_point() + theme_classic() + labs(title = "", x = "Horizon", y = expression(τ[Al])) + scale_y_continuous(breaks = c(-0.5,0,0.5)) + theme(legend.position = "none") + theme(plot.title = element_text(size=20,face="bold")) +  theme(axis.text.y = element_text(size=12)) + scale_fill_manual(values = c("#cccccc","#90593f","#cccccc","#90593f","#fcff54")) + scale_x_discrete(labels = prettylabs) + theme(axis.title.x = element_blank(), axis.title.y = element_text(size=15, face="plain"))
p1
p2 <- ggplot(soilsub, aes(x = Cat, y = tau_Ca, fill = Cat)) + geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 0.5) + geom_boxplot() + geom_point() + theme_classic() + labs(title = "", x = "Horizon", y = expression(τ[Ca])) + theme(legend.position = "none") + theme(plot.title = element_text(size=20,face="bold")) +  theme(axis.text.y = element_text(size=12)) + scale_fill_manual(values = c("#cccccc","#90593f","#cccccc","#90593f","#fcff54")) + scale_x_discrete(labels = prettylabs) + theme(axis.title.x = element_blank(), axis.title.y = element_text(size=15, face="plain"))

p3 <- ggplot(soilsub, aes(x = Cat, y = tau_Fe, fill = Cat)) + geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 0.5) + geom_boxplot() + geom_point() + theme_classic() + labs(title = "", x = "Horizon", y = expression(τ[Fe])) + theme(legend.position = "none") + theme(plot.title = element_text(size=20,face="bold")) +  theme(axis.text.y = element_text(size=12)) + scale_fill_manual(values = c("#cccccc","#90593f","#cccccc","#90593f","#fcff54")) + scale_x_discrete(labels = prettylabs) + theme(axis.title.x = element_text(size=15, face="plain"), axis.title.y = element_text(size=15, face="plain"))

grid.arrange(p1, p2, p3, nrow = 3)

g <- arrangeGrob(p1, p2, p3, nrow=3)

ggsave(filename = "all3_soil_box.png", g)

 #p <- ggplot(soilsub, aes(x = LatVert, y = TiO2, fill = LatVert)) + geom_violin() + geom_point() + theme_classic() + labs(title = "", x = "Horizon", y = "TiO2") + stat_summary(
#  fun.data = stat_box_data, 
#  geom = "text", 
#  hjust = 0.5,
#  vjust = 0.9
#) + theme(legend.position = "none") + ggtitle("Soil fine fraction") + theme(plot.title = element_text(size=20,face="bold")) +  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) + ylim(0,1.5) + scale_fill_manual(values = c("#cccccc","#90593f","#cccccc","#90593f","#fcff54")) + scale_x_discrete(labels = prettylabs) + theme(axis.title.x = element_text(size=20, face="plain"), axis.title.y = element_text(size=20, face="plain"))

#(rocksub$tau_Na2O) * 1.3)
stat_box_data <- function(y, upper_limit = 1.5) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}
rocksub$LatVert <- factor(rocksub$LatVert, levels=c("LE", "LB", "VE", "VB", "C"))
#p <- ggplot(rocksub, aes(x = LatVert, y = TiO2, fill = LatVert)) + geom_violin() + geom_point() + theme_classic() + labs(title = "", x = "Horizon", y = "[TiO2]") + stat_summary(
#  fun.data = stat_box_data, 
#  geom = "text", 
#  hjust = 0.5,
#  vjust = 0.9
#) + theme(legend.position = "none") + ggtitle("Rock fragment") + theme(plot.title = element_text(size=20,face="bold")) + ylim(0,1.5) + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) + scale_fill_manual(values = c("#cccccc","#90593f","#cccccc","#90593f","#fcff54")) + scale_x_discrete(labels = prettylabs) + theme(axis.title.x = element_text(size=15, face="plain"), axis.title.y = element_text(size=15, face="plain"))
p <- ggplot(rocksub, aes(x = LatVert, y = tau_Ca, fill = LatVert)) + geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 0.5) + geom_boxplot() + geom_point() + theme_classic() + labs(title = "", x = "Horizon", y = expression(τ[Ca])) + stat_summary(
  fun.data = stat_box_data, 
  geom = "text", 
  hjust = 0.5,
  vjust = 0.9
) + theme(legend.position = "none") + ggtitle("Rock fragment") + theme(plot.title = element_text(size=20,face="bold")) + ylim(-1,1.5) + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) + scale_fill_manual(values = c("#cccccc","#90593f","#cccccc","#90593f","#fcff54")) + scale_x_discrete(labels = prettylabs) + theme(axis.title.x = element_text(size=15, face="plain"), axis.title.y = element_text(size=15, face="plain"))
p
ggsave(filename = "ca_rock_violin.png")

#qqplot
qqPlot(soilsub$tau_SiO2)

#anova
test <- aov(formula = tau_P ~ Cat, data = soilsub)
summary(test)
TukeyHSD(test)
plot(test, 1)
plot(test, 2)

le <- filter(soilsub, Cat == "LE")
lb <- filter(soilsub, Cat == "LBhs")
ve <- filter(soilsub, Cat == "VE")
vb <- filter(soilsub, Cat == "VBhs")
c <- filter(soilsub, Cat == "DC")

hist(c$Ti)

soilsub

ktest <- kruskal.test(Ti ~ Cat, data = soilsub)
ktest

dunn.test(soilsub$tau_P, soilsub$Cat, kw=TRUE, method = "bonferroni", altp = TRUE)
dunn.test(soilsub$tau_Al, soilsub$Cat, kw=TRUE, method = "bonferroni", altp = TRUE)
dunn.test(soilsub$tau_Si, soilsub$Cat, kw=TRUE, method = "bonferroni", altp = TRUE)
dunn.test(soilsub$tau_Ca, soilsub$Cat, kw=TRUE, method = "bonferroni", altp = TRUE)
dunn.test(soilsub$Ti, soilsub$Cat, kw=TRUE, method = "bonferroni", altp = TRUE)

nrow(c)
