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

nocsvs <- print(list.files('values_tau/hb/'))
taulist <- list()
for(a in seq_along(c(1:length(nocsvs)))){
  csvdata <- read.csv(paste0('values_tau/hb/',nocsvs[a]), header = TRUE, sep = ",")
  taulist[[a]] <- csvdata
}


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

#soildata <- read.csv(paste0('all_soil_taus_fu.csv'), header = TRUE, sep = ",")
#rockdata <- read.csv(paste0('all_rock_taus_fu.csv'), header = TRUE, sep = ",")
#soildata <- cbind(soildata, ID = paste0(soildata$Pit, soildata$ShortHoriz))
#rockdata <- cbind(rockdata, ID = paste0(rockdata$Pit, rockdata$ShortHoriz))
#Took out X3.1_RF1 because it was consistent outlier (and not in E podzol)
#rockdata <- as.data.frame(rockdata[rockdata$Name %in% c("X3.1_RF1") == FALSE,], stringsAsFactors = FALSE)
#rocksub <- subset(rockdata, rockdata$ShortHoriz == "E" | rockdata$ShortHoriz == "Bhs" | rockdata$ShortHoriz == "Bs" | rockdata$LatVert == "C")
#soilsub <- subset(soildata, soildata$ShortHoriz == "E" | soildata$ShortHoriz == "Bhs" | soildata$ShortHoriz == "Bs" | soildata$LatVert == "C")

#cheesed <- ddply(soilsub, c("ID","ShortHoriz","LatVert"), summarise,
#                 N    = length(ID),
#                 mean = mean(tau_TiO2),
#                 sd   = sd(tau_TiO2),
#                 se   = sd / sqrt(N),
#                 Depth = mean(Depth)
#)

#this is for demonstrating lack of relationship between depth and tau!
#p <- ggplot(cheesed, aes(x = mean, y = Depth)) + geom_point() + theme_classic() + labs(title = "", x = "Soil τ")
#p
#ggsave(filename = "soil_depth_tau.svg")

prettylabs <- c("Lateral E", "Lateral B", "Vertical E", "Vertical B", "Deep C")

stat_box_data <- function(y, upper_limit = 1.5) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}

soildata <- cbind(taudatas, Cat = str_c(taudatas$LV, taudatas$Hor))

#p <- ggplot(soilsub, aes(x = LatVert, y = TiO2, fill = LatVert)) + geom_violin() + geom_point() + theme_classic() + labs(title = "", x = "Horizon", y = "TiO2") + stat_summary(
#  fun.data = stat_box_data, 
#  geom = "text", 
#  hjust = 0.5,
#  vjust = 0.9
#) + theme(legend.position = "none") + ggtitle("Soil fine fraction") + theme(plot.title = element_text(size=20,face="bold")) +  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) + ylim(0,1.5) + scale_fill_manual(values = c("#cccccc","#90593f","#cccccc","#90593f","#fcff54")) + scale_x_discrete(labels = prettylabs) + theme(axis.title.x = element_text(size=20, face="plain"), axis.title.y = element_text(size=20, face="plain"))
p <- ggplot(soilsub, aes(x = LatVert, y = tau_P2O5, fill = LatVert)) + geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 0.5) + geom_boxplot() + geom_point() + theme_classic() + labs(title = "", x = "Horizon", y = expression(τ[P])) + stat_summary(
  fun.data = stat_box_data, 
  geom = "text", 
  hjust = 0.5,
  vjust = 0.9
) + theme(legend.position = "none") + ggtitle("Soil fine fraction") + theme(plot.title = element_text(size=20,face="bold")) +  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) + ylim(-1,1.5) + scale_fill_manual(values = c("#cccccc","#90593f","#cccccc","#90593f","#fcff54")) + scale_x_discrete(labels = prettylabs) + theme(axis.title.x = element_text(size=20, face="plain"), axis.title.y = element_text(size=20, face="plain"))
p
ggsave(filename = "p_soil_violin.png")

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
test <- aov(formula = tau_SiO2 ~ LatVert, data = rocksub)
summary(test)
TukeyHSD(test)
plot(test, 1)
plot(test, 2)

#krus
test2 <- kruskal.test(tau_SiO2 ~ LatVert, data = soilsub)
test2
pairwise.wilcox.test(soilsub$tau_SiO2, soilsub$LatVert, p.adjust.method="BH")
pairwise.wilcox.test(rocksub$tau_SiO2, soilsub$LatVert, p.adjust.method="BH")

le <- filter(rocksub, LatVert == "LE")
lb <- filter(rocksub, LatVert == "LB")
ve <- filter(rocksub, LatVert == "VE")
vb <- filter(rocksub, LatVert == "VB")
c <- filter(rocksub, LatVert == "C")

ks.test(vb$tau_CaO, c$tau_CaO)
ks.test(vb$tau_Fe2O3, ve$tau_Fe2O3)
ks.test(ve$tau_CaO, vb$tau_CaO) #can't say much about this one
ks.test(lb$tau_CaO, ve$tau_CaO)
