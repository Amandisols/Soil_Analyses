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
datas <- as.data.frame(datag[c(1:5,19:20,75:124,152:164),], stringsAsFactors = FALSE)
colnames(datas)[1] <- c("Name")

write.csv(datas, file = paste0("all_elem_0311.csv"))

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
bigkins <- bigelem
#bigkins <- subset(bigelem, bigelem$Kinsman != "NA")

deepfilter <- subset(biglist, biglist$`Top depth` > 100)
deepelem <- subset(deepfilter, deepfilter$Elemental == "Y")
deepkins <- deepelem
deepkins
deepelem

#this creates the name to match to the elemental rock data
joinlist <- list()
for(b in seq_along(c(1:nrow(bigkins)))){
  zippy <- paste0(substr(bigkins[b,1],6,10),"_",substr(bigkins[b,2],1,4))
  joinlist[[b]] <- zippy
}
zippyjoin <- cbind.data.frame(bigkins,do.call(rbind, joinlist), stringsAsFactors = FALSE)
zippydf <- as.data.frame(zippyjoin, stringsAsFactors = FALSE)

joinlist <- list()
for(b in seq_along(c(1:nrow(deepkins)))){
  zippy <- paste0(substr(deepkins[b,1],6,10),"_",substr(deepkins[b,2],1,4))
  joinlist[[b]] <- zippy
}
deepjoin <- cbind.data.frame(deepkins,do.call(rbind, joinlist), stringsAsFactors = FALSE)

colnames(zippydf)[17] <- "newname"
innerjoin <- inner_join(zippydf,datas, by = c("newname" = "Name"))

deepjoin

write.csv(innerjoin, file = paste0("rockelem200311.csv"))

colnames(deepjoin)[17] <- "newname"
deepinnerjoin <- inner_join(deepjoin,datas, by = c("newname" = "Name"))

print(deepinnerjoin)
write.csv(deepinnerjoin, file = paste0("avg_rock_0311.csv"))

#6-rock average for 12 elements, 3-rock average for the rest

#input pit(s) you want here!
pit <- pitnamez
#select all matching that name from elem worksheet
edepths <- innerjoin[grep(substr(pit,6,9), substr(innerjoin$newname,1,4)),]

#select all matching that name from depths worksheet
#ddepths <- subset(bigkins, bigkins$Pit == pit)
#select all 
#edepths <- subset(ddepths, ddepths$Elemental == "Y")  

#add depth columns !NOTE: MUST BE IN DEPTH ORDER!
mids <- numeric(length = nrow(edepths))
for(a in seq_along(c(1:nrow(edepths)))){
  mids[a] <- as.numeric((edepths[a,5]+edepths[a,6])/2)
}
#mids <- as.numeric((edepths[,5]+edepths[,6])/2)
master <- data.frame()
master <- as.data.frame(cbind(paste0(edepths[,3],edepths[,4]), as.data.frame(mids, stringsAsFactors = FALSE), edepths[,5:6], edepths[,17:52]), stringsAsFactors = FALSE)
colnames(master)[2] <- c("Midpoint")
colnames(master)[1] <- c("Horizon")

#starthere
#this csv is just my kinsman sample
avg <- read.csv('avg_deeprock.csv', header = TRUE, sep = ",")
#this csv is the average taken of all deep kinsman samples
#avg <- read.csv('avg_kins.csv', header = TRUE, sep = ",")

#grep the columns including the elem you want
#sel_elem <- c('Ce', 'La', 'Pr', 'Nd', 'Sm', 'Si')
sel_elem <- c('Fe', 'Al', 'Si', 'Ca', 'P', 'Na')
sublist <- list()
for(x in seq_along(1:length(sel_elem))){
  sub <- grep(sel_elem[x], names(master), value = TRUE)
  sublist[[x]] <- sub
}
sub <- vector()
sub <- do.call(rbind, sublist)

#subset to just the elements wanted
subs <- master[,sub]
subavg <- avg[,sub]

#include ratio of ti/zr
ratio <- as.data.frame(master$TiO2/master$Zr, stringsAsFactors = FALSE)
ratioavg <- avg$TiO2/avg$Zr

#create data frames to work with
mastersub <- cbind(Horizon = master$`Horizon`, TDepth = master$`Top depth`, BDepth = master$`Bottom depth`, Depth = master$Midpoint, Name = master$newname, TiO2 = master$TiO2, Zr = master$Zr, Y = master$Y, Ratio = ratio[,1], subs, stringsAsFactors = FALSE)
avgsub <- cbind(Horizon = 'null', TDepth = avg$Top.depth, BDepth = avg$Bottom.depth, Depth = avg$Bottom.depth+((avg$Top.depth-avg$Bottom.depth)/2), Name = avg$newname, TiO2 = avg$TiO2, Zr = avg$Zr, Y = avg$Y, Ratio = ratioavg, subavg, stringsAsFactors = FALSE)

#set reference row
#bott <- mastersub[nrow(mastersub),4]
bottl <- data.frame(mastersub[nrow(mastersub),], stringsAsFactors = FALSE)
bottl[] <- avgsub
#below adjusts to lowest if C horizon present
#if(grepl("E",bott) | grepl("B",bott)){
#  bottl[] <- avgsub
#} else{
#  bottl[] <- mastersub[nrow(mastersub),]
#}

#here's where you write the damn funciton
#columns to use lapply on
seq(from = 10, to = ncol(mastersub))

index <- bottl$Zr
val <- mastersub$Zr

#below is working tau code
taus <- function(elem){
  blp <- ((mastersub[,elem]*index)/(bottl[1,elem]*val))-1
  return(blp)
}
ltaus <- lapply(seq(from = 10, to = ncol(mastersub)), taus)
dftaus <- as.data.frame(cbind(mastersub, ltaus, stringsAsFactors = FALSE))
numb <- ncol(mastersub) - 9

colnames(dftaus)[seq(from = ncol(mastersub)+1, to = ncol(mastersub)+numb)] <- colnames(dftaus[(seq(from = ncol(mastersub)-(numb-1), to = ncol(mastersub)))])
colnames(dftaus)[seq(from = ncol(mastersub)+1, to = ncol(mastersub)+numb)] <- paste("tau", colnames(dftaus)[seq(from = ncol(mastersub)+1, to = ncol(mastersub)+numb)], sep = "_")

write.csv(dftaus, file = paste0("rock_taus_only",pitnamez,".csv"))

tauplot <- dftaus[,c(1:5,(10+numb):(ncol(mastersub)+numb))]
cheese <- melt(tauplot, id=c("Horizon","TDepth","BDepth","Depth", "Name"))
cheesed <- cheese[order(cheese$Depth),]
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p <- ggplot(cheesed, aes(x = value, y = Depth, group = variable)) + scale_y_reverse(name='Depth (cm)', limits = c(NA,0)) + scale_x_continuous(name='τ') + coord_cartesian(xlim=c(-1,1)) + geom_rect(aes(xmin= -Inf, xmax= Inf, ymin=BDepth-.1, ymax=TDepth+.1), fill="lavender", alpha=0.2, show.legend = FALSE) + geom_vline(xintercept=0, linetype="dashed", color = "gray", size = 0.5)  + geom_path(aes(color=variable)) + geom_point(aes(color=variable)) + theme_classic() + scale_color_manual(values = cbbPalette) 
p <- p + ggtitle(pit)
p
}

lapply(c("52_4_X4.1", "42_4_V4.1", "52_3_X3.1", "42_3_V3.1", "52_2_X2.1", "42_2_V2.1", "86_4_W4.1", "86_3_W3.2", "86_2_W2.2"),wholeshebang)
#i don't love it the way you have it now, it would be more interesting to generate all plots in a ?loop?

#plot_data_column = function (data, column) {
#  ggplot(data, aes_string(x = column)) +
#    geom_histogram(fill = "lightgreen") +
#    xlab(column)
#}
