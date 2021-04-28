################
#Jenny Bower
#elemental analysis
#orig: 05-13-2020
#rev: 03-08-2021
#this script was made during a global pandemic
#some notes: the R-ready elemental data csvs should be in a folder called source, the horizon data and reference data should be in a folder called soil_ref
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
dataf <- datae[,sapply(datae,is.integer) | sapply(datae,is.numeric)]

#molecular weights to translate oxides to non-oxides
wts <- c(0.467436713, 0.529261188, 0.699430761, 0.774461846, 0.603041882, 0.714695865, 0.74186418, 0.83015022, 0.599348901, 0.436428707)
datam <- as.data.frame(mapply("*", as.data.frame(data[,3:12]), 10000*wts),stringsAsFactors = FALSE)

#label columns
colnames(datam) <- c("Si","Al","Fe","Mn","Mg","Ca","Na","K","Ti","P")

#put it all together--name, and all columns
datas <- as.data.frame(cbind(datan,datam,dataf))
  
#read in horizon data
list <- read.csv(paste0('horizon/regional_pedons_elemental_0321.csv'), header = TRUE, sep = ",")

#subset to elemental
elem <- subset(list, list$elemental == "Y")

#change names to fit format of source sheets
joinlist <- list()
for(b in seq_along(c(1:nrow(elem)))){
  if(is.na(elem[b,3])){
    zippy <- paste0(elem[b,1],'_',elem[b,2])
  }
  else {
    zippy <- paste0(elem[b,1],'_',elem[b,2],'.',elem[b,3])
  }
  joinlist[[b]] <- zippy
}
zippyjoin <- cbind(elem,do.call(rbind, joinlist))

#rename column
colnames(zippyjoin)[23] <- "newname"

#join elemental data to horizon data
innerjoin <- inner_join(zippyjoin, datas, by = c("newname" = "Name"))

#export so you can look at it
write.csv(innerjoin, file = paste0("values_elem/regionalelem210303.csv"))

#add midpoint column !NOTE: MUST BE IN DEPTH ORDER!
mids <- numeric(length = nrow(innerjoin))
for(a in seq_along(c(1:nrow(innerjoin)))){
  mids[a] <- as.numeric((innerjoin[a,4]+innerjoin[a,5])/2)
}
master <- as.data.frame(cbind(pedon = innerjoin[,1], mids, innerjoin[,4:5], innerjoin[,(ncol(elem)+1):ncol(innerjoin)]), stringsAsFactors = FALSE)

#this csv is the average taken of all samples greater than 1 meter in depth, already adjusted to account for oxides
avg <- read.csv('index/br_soil_avg1.csv', header = TRUE, sep = ",")

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
masters <- cbind(Pedon = master$pedon, TDepth = master$`top.cm`, BDepth = master$`base.cm`, Depth = master$mids, Name = master$newname, Ti = master$Ti, Zr = master$Zr, Y = master$Y, Ratio = ratio[,1], subs, stringsAsFactors = FALSE)
avgs <- cbind(Pedon = avg$pedon, TDepth = avg$top.cm, BDepth = avg$base.cm, Depth = avg$base.cm+((avg$top.cm-avg$base.cm)/2), Name = avg$newname, Ti = avg$Ti, Zr = avg$Zr, Y = avg$Y, Ratio = ratioavg, subsavg, stringsAsFactors = FALSE)

#function for exporting and plotting tau values
taufun <- function(pit){

#subset to each pit
mastersub <- subset(masters, masters$Pedon == pit)
avgsub <- avgs

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
write.csv(dftaus, file = paste0("values_tau/taus",pit,".csv"))

#select only the columns used in plotting
tauplot <- dftaus %>% select(TDepth:Name,(firstcol+numb):(ncol(mastersub)+numb))

#"melt" the data so you can colorize by element and plot multiple elements on the same plot
cheese <- melt(tauplot, id=c("TDepth","BDepth","Depth", "Name"))

#set color palette for the dots
cbbPalette <- c("#531516", "#000000", "#431c53", "#034731", "#444444", "#0072B2", "#D55E00", "#CC79A7")

#set the background of the horizons to the actual soil colors
soilcolor <- subset(innerjoin, innerjoin$pedon == pit)
soilcolors <- munsell2rgb(soilcolor$hue, soilcolor$value, soilcolor$chroma)
soilcolors <- rep(soilcolors, length(sel_elem))

#generate the plots
p <- ggplot(cheese, aes(x = value, y = Depth, group = variable)) + scale_y_reverse(name='Depth (cm)', limits = c(100,0)) + scale_x_continuous(name='τ') + coord_cartesian(xlim=c(-1,1)) + geom_rect(aes(xmin= -Inf, xmax= Inf, ymin=BDepth-.1, ymax=TDepth+.1), fill = soilcolors, alpha=0.17, show.legend=FALSE) + geom_vline(xintercept=0, linetype="dashed", color = "#504c4c", size = 0.5)  + geom_path(aes(color=variable)) + geom_point(aes(color=variable)) + theme_classic() + scale_color_manual(values = cbbPalette) 
p <- p + ggtitle(pit)
p

#save as png with custom settings
ggsave(paste0("png/",pit,".png"), width = 5, height = 5, units = "in", dpi = "print")
}

#run the function for looping through each pit
lapply(unique(master$pedon), taufun)

