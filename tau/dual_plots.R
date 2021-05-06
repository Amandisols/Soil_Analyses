################
#Jenny Bower
#Elemental analysis
#05-13-2020
#This script was made during a global pandemic
################
library('readxl')
library('tidyverse')
library('ggplot2')
library('reshape')
library('gridExtra')
library('plyr')
library('svglite')

#wholeshebang <- function(pitnamez){
  pitnamez <- c("V2.1")
  setwd("/users/nqpenelope/Documents/uvm/phd/research/data/lab_data/Elemental/")
  #pick up csvs
  nocsvs <- print(list.files('taus_Ti/'))
  rockavglist <- list()
  rockonlylist <- list()
  soillist <- list()
  for(a in seq_along(c(1:length(nocsvs)))){
    csvdata <- read.csv(paste0('taus_Ti/',nocsvs[a]), header = TRUE, sep = ",")
    if(substr(nocsvs[a],1,14) == "rock_taus_avgs"){
      rockavglist[[a]] <- csvdata
    }
    else if (substr(nocsvs[a],1,14) == "rock_taus_only"){
      rockonlylist[[a]] <- csvdata
    }
    else{
      soillist[[a]] <- csvdata
    }
  }
  rockavgcsvlist <- do.call(rbind, rockavglist)  
  rocksavgcsvlist <- cbind(rockavglist, Type = c("Rock fragment"), Pit = substr(rockavglist$Name, 1, 4), stringsAsFactors = FALSE)
  rockcsvlist <- do.call(rbind, rockonlylist)  
  rockscsvlist <- cbind(rockcsvlist, Type = c("Rock fragment"), Pit = substr(rockcsvlist$Name, 1, 4), stringsAsFactors = FALSE)
  #these appear to be outliers
  rockscsvlists <- as.data.frame(rockscsvlist[rockscsvlist$Name %in% c("V3.1_RF9","X4.1_RF5","X4.1_RF6","W2.2_RF2","W3.2_RF8","W4.1_RF4") == FALSE,], stringsAsFactors = FALSE)
  rock <- sapply(rockscsvlists, is.factor)
  rockscsvlists[rock] <- lapply(rockscsvlists[rock], as.character)
  rocks <- cbind(rockscsvlists, ShortHoriz = substr(rockscsvlists$Horizon, 0, (str_locate(rockscsvlists$Horizon, "[[:digit:]N]")[,1])-1), stringsAsFactors = FALSE)
  write.csv(rocks, file = paste0("all_rock_taus.csv"))
  soilcsvlist <- do.call(rbind, soillist)
  soilcsvlist
  soilscsvlist <- cbind(soilcsvlist, Type = c("Soil fine fraction"), Horizon = substr(soilcsvlist$Name, 11, 19), Pit = substr(soilcsvlist$Name, 6, 9), stringsAsFactors = FALSE)
  #soils <- cbind(soilscsvlist, ShortHoriz = substr(soilscsvlist$Horizon, 0, (str_locate(soilscsvlist$Horizon, "[:^alpha:]")[,1])-1), stringsAsFactors = FALSE)
  write.csv(soilscsvlist, file = paste0("all_soil_taus.csv"))
  dftaus <- rbind(rockscsvlists, soilscsvlist)
  #dftaus <- soils
  pit = pitnamez
  numb <- ncol(dftaus) - 15
  taup <- cbind.data.frame(Horizon = dftaus$Horizon, TDepth = dftaus$TDepth, BDepth = dftaus$BDepth, Depth = dftaus$Depth, Name = dftaus$Name, Type = dftaus$Type, Pit = dftaus$Pit)
  #tauplot <- cbind.data.frame(taup, tau_Fe2O3 = dftaus$tau_Fe2O3, tau_CaO = dftaus$tau_CaO)
  tauplot <- cbind.data.frame(taup, tau_CaO = dftaus$tau_CaO, tau_Fe2O3 = dftaus$tau_Fe2O3, tau_Al2O3 = dftaus$tau_Al2O3)
  chees <- subset(tauplot, tauplot$Pit %in% pitnamez)
  cheese <- melt(chees, id=c("Horizon","TDepth","BDepth","Depth", "Name", "Pit", "Type"))
  cheesy <- cbind(cheese, Element = substr(cheese$variable,5,6))
  #below is for avged
  #below is for individual
  #cheesed <- ddply(cheesy, c("Name","Type","Element"), summarise,
  cheesed <- ddply(cheesy, c("Horizon","Type","Element","Pit"), summarise,
                   N    = length(Horizon),
                   mean = mean(value),
                   sd   = sd(value),
                   se   = sd / sqrt(N),
                   Depth = mean(Depth),
                   TDepth = min(TDepth),
                   BDepth = max(BDepth)
  )
  cheesr <- cheesed[rev(order(cheesed$Element)),]
  cheesedy <- cheesr[order(cheesr$Depth),]
  cheesedy
  cheesedy$Typef <- factor(cheesedy$Type, levels = c('Soil fine fraction', 'Rock fragment'))
  cbbPalette <- c("#E69F00", "#0072B2", "#D55E00", "#000000", "#F0E442", "#009E73", "#56B4E9", "#D55E00", "#CC79A7")
  #p <- ggplot(cheesedy, aes(x = mean, y = Depth, group = Pit)) + scale_y_reverse(name='Depth (cm)', limits = c(100,0)) + scale_x_continuous(name='τ') + coord_cartesian(xlim=c(0,1.5)) + geom_path(aes(color=Element)) + geom_errorbar(aes(xmin=mean-se, xmax=mean+se, color=Element), width=2, position=position_dodge())  + geom_point(aes(color=Element, shape=Element)) + theme_classic() + theme(axis.title.x = element_text(size=20, face="plain"), axis.title.y = element_text(size=20, face="plain")) + scale_color_manual(values = "#000000") + facet_wrap(~Typef,scales="free_x") + theme(strip.text.x = element_text(size=20,face="bold"), axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) + theme(legend.title = element_text(size=15,face="bold"), legend.text = element_text(size=15)) + theme(strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid")) 
  
  p <- ggplot(cheesedy, aes(x = mean, y = Depth, group = Element)) + scale_y_reverse(name='Depth (cm)', limits = c(100,0)) + scale_x_continuous(name='τ') + coord_cartesian(xlim=c(-1,1)) + geom_rect(aes(xmin= -Inf, xmax= Inf, ymin=BDepth-.1, ymax=TDepth+.1), fill="lightgray", alpha=0.2, show.legend = FALSE) + geom_vline(xintercept=0, linetype="dashed", color = "gray", size = 0.5)  + geom_path(aes(color=Element)) + geom_errorbar(aes(xmin=mean-se, xmax=mean+se, color=Element), width=2, position=position_dodge())  + geom_point(aes(color=Element, shape=Element), size=3) + theme_classic() + theme(axis.title.x = element_text(size = 30, face="plain"), axis.title.y = element_text(size = 30, face="plain")) + scale_color_manual(values = cbbPalette) + theme(strip.text.x = element_text(size=30,face="plain"), axis.text.x = element_text(size=18), axis.text.y = element_text(size=18)) + facet_wrap(~Typef,scales="free_x")  + theme(legend.title = element_text(size= 20,face="bold"), legend.text = element_text(size=20)) + theme(strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid")) 
  p <- p + ggtitle(pit) #+ theme(plot.margin = unit(c(1, 1, 1, 5), "cm"))
  p
  ggsave(filename = paste0(pitnamez,"_dual_AlCaFe.png"))
  #ggsave(filename = paste0(pitnamez,"_Ti_only.png"))
  
#}

#lapply(c("42_4_V4.1"),wholeshebang)
#lapply(c("52_4_X4.1", "42_4_V4.1", "52_3_X3.1", "42_3_V3.1", "52_2_X2.1", "42_2_V2.1", "86_4_W4.1", "86_3_W3.2", "86_2_W2.2"),wholeshebang)


