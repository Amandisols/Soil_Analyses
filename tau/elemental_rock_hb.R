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
library('aqp')

#pick up csvs from csv folder
nocsvs <- print(list.files('source/'))
csvlist <- list()
for(a in seq_along(c(1:length(nocsvs)))){
  csvdata <- read.csv(paste0('source/',nocsvs[a]), header = TRUE, sep = ",")
  csvlist[[a]] <- csvdata
}

#create data from csvs
data <- do.call(rbind, csvlist) 

#grab first column and convert to character
datan <- as.data.frame(as.character(data[,1]), stringsAsFactors = FALSE)
colnames(datan) <- c("Name")

#only add integer and numeric columns in non-oxide form (leaving out columns that have values below the detection limit, Total and LOI)
el <- which(names(data) == "Total") + 1
datae <- data[, el:ncol(data)]
dataf <- datae[, sapply(datae,is.integer) | sapply(datae,is.numeric)]

#subset to rocks
wts <- c(0.467436713, 0.529261188, 0.699430761, 0.774461846, 0.603041882, 0.714695865, 0.74186418, 0.83015022, 0.599348901, 0.436428707)
datam <- as.data.frame(mapply("*", as.data.frame(data[,3:12]), 10000*wts),stringsAsFactors = FALSE)
colnames(datam) <- c("Si","Al","Fe","Mn","Mg","Ca","Na","K","Ti","P")
datag <- as.data.frame(cbind(datan,datam,dataf))
datas <- as.data.frame(datag[c(1:3,19:20,75:124,152:164),], stringsAsFactors = FALSE)
colnames(datas)[1] <- c("Name")

#add in depths from soil data worksheet
nosheets <- print(list.files('hb_pitsheets/'))
sheetlist <- list()
for(a in seq_along(c(1:length(nosheets)))){
  esheets <- excel_sheets(paste0('hb_pitsheets/',nosheets[a]))
  dsheets <- esheets[grep("cf", esheets)]
  thesheet <- read_excel(paste0('hb_pitsheets/',nosheets[a]), sheet = which(esheets==dsheets[1]), col_names = TRUE)
  sheetlist[[a]] <- thesheet
}
biglist <- do.call(rbind, sheetlist)
bigelem <- subset(biglist, biglist$Elemental == "Y")
bigkins <- subset(bigelem, bigelem$Kinsman != "NA")
elem <- subset(bigkins, bigkins$`Bottom depth` < 100)

#this creates the name to match to the elemental rock data
joinlist <- list()
for(b in seq_along(c(1:nrow(elem)))){
  zippy <- paste0(substr(elem[b,1],6,9),"_",substr(elem[b,2],1,4))
  joinlist[[b]] <- zippy
}
zippyjoin <- cbind.data.frame(elem,do.call(rbind, joinlist), stringsAsFactors = FALSE)
zippydf <- as.data.frame(zippyjoin, stringsAsFactors = FALSE)

newlist <- list()
for(b in seq_along(c(1:nrow(elem)))){
  if(is.na(elem[b,4])){
    hor <- paste0(elem[b,3])
  }
  else {
    hor <- paste0(elem[b,3],elem[b,4])
  }
  joinlist[[b]] <- hor
}
horlist <- cbind(do.call(rbind, joinlist))

#rename column
colnames(zippydf)[17] <- "newname"

#join elemental data to horizon data
innerjoin <- inner_join(zippydf,datas, by = c("newname" = "Name"))

#export if you want to look at it
#write.csv(innerjoin, file = paste0("rockelem201214.csv"))

innerjoin

#add midpoint column !NOTE: MUST BE IN DEPTH ORDER!
mids <- numeric(length = nrow(innerjoin))
for(a in seq_along(c(1:nrow(innerjoin)))){
  mids[a] <- as.numeric((innerjoin[a,5]+innerjoin[a,6])/2)
}
master <- as.data.frame(cbind(Pedon = innerjoin[,1], Horizon = horlist[], Midpoint = mids, innerjoin[,5:6], innerjoin[,(ncol(elem)+1):ncol(innerjoin)]), stringsAsFactors = FALSE)

#this csv is the average taken of all samples greater than 1 meter in depth, already adjusted to account for oxides
avg <- read.csv('index/hb_kins_avg6.csv', header = TRUE, sep = ",")

#print out available elements
names(innerjoin[,(ncol(elem)+2):ncol(innerjoin)])

#list elements you're interested in for tau
sel_elem <- c('Fe', 'Si', 'Ca', 'P')

#subset to just the elements wanted
subs <- master[,sel_elem]
subsavg <- avg[,sel_elem]

#include ratio of ti/zr
ratio <- as.data.frame(master$Ti/master$Zr, stringsAsFactors = FALSE)
ratioavg <- avg$Ti/avg$Zr

#create data frames to work with
masters <- cbind(Pedon = master$Pedon, Horizon = master$`Horizon`, TDepth = master$`Top depth`, BDepth = master$`Bottom depth`, Depth = master$Midpoint, Name = master$newname, Ti = master$Ti, Zr = master$Zr, Y = master$Y, Ratio = ratio[,1], subs, stringsAsFactors = FALSE)
avgs <- cbind(Pedon = 'null', Horizon = 'null', TDepth = avg$Top.depth, BDepth = avg$Bottom.depth, Depth = 'null', Name = avg$newname, Ti = avg$Ti, Zr = avg$Zr, Y = avg$Y, Ratio = ratioavg, subsavg, stringsAsFactors = FALSE)

#loop for exporting and plotting tau values
peds <- unique(as.vector(masters$Pedon))

masters

for(a in c(1:length(peds))){
  
  pit <- peds[a]
  
  #subset to each pit
  mastersub <- subset(masters, as.vector(masters$Pedon) == pit)
  avgsub <- avgs
  
  mastersub
  
  #set first column of elements
  firstcol <- which(names(mastersub) == "Ratio") + 1 
  
  #set Ti values
  val <- mastersub$Ti
  index <- avgsub$Ti
  
  #function for generating tau
  taus <- function(elem){
    blp <- ((mastersub[,elem]*index)/(avgsub[1,elem]*val))-1
    return(blp)
  }
  
  #create tau dataset
  ltaus <- lapply(seq(from = firstcol, to = ncol(mastersub)), taus)

  #append tau dataset to pedon information
  dftaus <- as.data.frame(cbind(mastersub, ltaus, stringsAsFactors = FALSE))
  
  #dynamic number of element columns
  numb <- ncol(mastersub) - firstcol + 1
  
  #rename the columsn to make sense
  colnames(dftaus)[seq(from = ncol(mastersub)+1, to = ncol(mastersub)+numb)] <- colnames(dftaus[(seq(from = ncol(mastersub)-(numb-1), to = ncol(mastersub)))])
  colnames(dftaus)[seq(from = ncol(mastersub)+1, to = ncol(mastersub)+numb)] <- paste("tau", colnames(dftaus)[seq(from = ncol(mastersub)+1, to = ncol(mastersub)+numb)], sep = "_")
  
  #export the tau values so you can use them later
  write.csv(dftaus, file = paste0("values_tau/hb/taus",pit,"_rock.csv"))
  
  #select only the columns used in plotting
  tauplot <- dftaus %>% select(Horizon:Name,(firstcol+numb):(ncol(mastersub)+numb))
  
  #set color palette for the dots
  cbbPalette <- c("#531516", "#000000", "#431c53", "#034731", "#444444", "#0072B2", "#D55E00", "#CC79A7")
  
  #to work on next time: grab the soil colors from the pit database
  #set the background of the horizons to the actual soil colors
  #soilcolor <- subset(innerjoin, innerjoin$pedon == pit)
  #soilcolors <- munsell2rgb(soilcolor$hue, soilcolor$value, soilcolor$chroma)
  #soilcolors <- rep(soilcolors, length(sel_elem))
  
  cheese <- melt(tauplot, id=c("Horizon","TDepth","BDepth","Depth", "Name"))
  cheesed <- cheese[order(cheese$Depth),]

  #generate the plots
  p <- ggplot(cheesed, aes(x = value, y = Depth, group = variable)) + scale_y_reverse(name='Depth (cm)', limits = c(100,0)) + scale_x_continuous(name='τ') + coord_cartesian(xlim=c(-1,1)) + geom_rect(aes(xmin= -Inf, xmax= Inf, ymin=BDepth-.1, ymax=TDepth+.1), fill = "lavender", alpha=0.17, show.legend=FALSE) + geom_vline(xintercept=0, linetype="dashed", color = "#504c4c", size = 0.5)  + geom_path(aes(color=variable)) + geom_point(aes(color=variable)) + theme_classic() + scale_color_manual(values = cbbPalette) 
  p <- p + ggtitle(pit)
  p
  
  #save as png with custom settings
  ggsave(paste0("png/",pit,"_rock.png"), width = 5, height = 5, units = "in", dpi = "print")
  
}
  
