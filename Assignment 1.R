### Step one: reading data into R###

library(stringr)
library(dplyr)
library(lubridate)
setwd("d:\\00 Davis\\03 2015 Spring Quarter\\Stat 242\\Data_Duncan\\stat242_2015\\Assignment1\\data")

## (1) Read all data into a big list
wholeList = lapply(list.files(), function(x)(readLines(x,encoding = "UTF-8")))

## (2) Detect "=" in the files
titleDetect = function (x){
  # x means the xth element in the wholeList(big list)
  wholeList = lapply(list.files(), readLines)
  wholeList[[15]] <- str_replace(wholeList[[15]], wholeList[[15]][18], wholeList[[3]][17])
  wholeList[[15]] <- str_replace(wholeList[[15]], wholeList[[15]][17], wholeList[[3]][16])
  grep("^=", wholeList[[x]])}
# titleDect is assigned to check the = 
titleDect <- lapply(c(1:24), titleDetect)

## (3) try read.fwf to split the data according to fixed width 
count = function (x) {
  # x means the xth files in the directory
  # use for counting the fixed width of the header 
  # Add 1 to each width there is a blank after each cloumn
  nchar(x) +1
}

## (4) give the names 
filenames = function(x) {list.files()[x]}

## (5) how many lines we should skip to get rid of the big title  
skipLine = function (x) {titleDect[[x]]}

## (6) count width in the big data frame
widthC = function(x) {lapply(strsplit(wholeList[[x]][titleDect[[x]]]," "), count)}

## (7) colnames 
cherryNames <- function (x) { 
  read.fwf(filenames(x), widthC(x), skip = skipLine(x)-2, 
           fill = TRUE,  na.strings = c("","NA"), comment.char = '',
           stringsAsFactors=FALSE, blank.lines.skip = TRUE,encoding = "UTF-8")
}
cherryCol <- function (x) {tolower(cherryNames(x)[1,])}

## (8) read data by read.fwf 
cherryBlossomT <- function (x) {
  # x means the xth files in the directory
  read.fwf(filenames(x), widthC(x), skip = skipLine(x),
           col.names = cherryCol(x), check.names=FALSE, fill = TRUE, 
           na.strings = c("","NA"), comment.char = '', 
           blank.lines.skip = TRUE, encoding = "UTF-8")
}

##(10) put into a big dataframe list
cherryRun <- lapply(c(1:24),cherryBlossomT)
cherryRun[[15]] <- w01 # exceptions
cherryRun[[11]] <- m09 # exceptions

## (11) exceptions

### Encoding solved
encoding09 <- readLines("men10Mile_2009", encoding = "UTF-8")
encoding09 <- gsub("[\u00A0]"," ", encoding09 )
widthEncoding <- lapply(strsplit(encoding09[grep("^=", encoding09)]," "),count)
m09 <- read.fwf(textConnection(encoding09),c( 6,12,7,23,3,21,8,7,2,6), 
                skip = skipLine(11),  col.names =  c("place","div/tot","num","name","ag","hometown","gun time","time","id","pace"), 
                check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), 
                comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")

### file without header name 
w01 <- cherryBlossomT(15)
colnames(w01) <- cherryCol(3)

### some files have the incorrect "=" 
cherryNamesEX <- function (x) { read.fwf(filenames(x), 
                                         c(6,9,7,23,3,16,8,8,1,6,2), skip = skipLine(x)-2, fill = TRUE, 
                                         na.strings = c("","NA"), comment.char = '', stringsAsFactors=FALSE,
                                         blank.lines.skip = TRUE,encoding = "UTF-8")}
cherryColEX <- function (x) {tolower(cherryNamesEX(x)[1,])}
cherryRun[[8]]  <- read.fwf(filenames(8), c(6,9,7,23,3,16,8,8,1,6,2), 
                            skip = skipLine(8), col.names = cherryColEX(8), check.names=FALSE, 
                            fill = TRUE,  na.strings = c("","NA"), comment.char = '', 
                            blank.lines.skip = TRUE, encoding = "UTF-8")
cherryRun[[20]] <- read.fwf(filenames(20), c(6,9,7,23,3,16,8,8,1,6,2), 
                            skip = skipLine(20), col.names = cherryColEX(20), check.names=FALSE, 
                            fill = TRUE,  na.strings = c("","NA"), comment.char = '', 
                            blank.lines.skip = TRUE, encoding = "UTF-8")

## remove the irrelevent symbols like # in time variable
cherryRun[[9]] <- read.fwf(filenames(9), c( 6,12,7,23,3,19,7,1,1,6,2,8),
                           skip = skipLine(9), 
                           col.names = c("place","div/tot","num","name","ag","hometown","time","id","na","pace","s","split"), 
                           check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")
cherryRun[[21]] <- read.fwf(filenames(21), c( 6,12,7,23,3,19,7,1,1,6,2,8), 
                            skip = skipLine(21), col.names = c("place","div/tot","num","name","ag","hometown","time","id","na","pace","s","split"), 
                            check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")

cherryRun[[23]] <- read.fwf(filenames(23), c( 6,12,7,23,3,21,8,7,2,6), 
                            skip = skipLine(23), col.names = c("place","div/tot","num","name","ag","hometown","gun time","time","id","pace"), 
                            check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")

cherryRun[[12]]<-  read.fwf(filenames(12),widthC(12), 
                            skip = skipLine(12), col.names =  c("place","div/tot","num","name","ag","hometown","5 mile","time","net time","na","pace"), 
                            check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")

cherryRun[[24]]<-  read.fwf(filenames(24), 
                            c( 6,12,7,23,3,21,8,8,7,1,1,6,2), skip = skipLine(24), 
                            col.names = c("place","div/tot","num","name","ag","hometown","5 mile","gun time","time","id","na","pace","s"), check.names=FALSE,
                            fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")




# (2) Rank one problem 
placeOne <- function(x){cherryRun[[x]][1,]}
rankOne <- lapply(c(1:24), placeOne)
## Person Name
wName<-sapply(c(1:24),function(x){select(rankOne[[x]],contains("name"))})
names(wName)<- NULL
## Hometown
Home <- function(x) {select(rankOne[[x]],contains("hometown"))}
winnerHome <- as.character(unlist(sapply(c(1:24), Home)))
Kenya <- sum(str_count(winnerHome, "Kenya\\s*"))/24
Ken <- sum(str_count(winnerHome,"[K[Ee][Nn]\\s]*"))/24
Ethiopia<- sum(str_count(winnerHome, winnerHome[1]))/24
Morocco <- sum(str_count(winnerHome, winnerHome[10]))/24
Others <- (24 - kenya - ken - ethiopia - morocco)/24
pie(c(Kenya,Ken,Ethiopia,Morocco,Others),
    labels = c("Kenya","Ken","Ethiopia","Morocco","Other"),
    col = brewer.pal(5,"Purples"), main = "Hometown of Winners (1999 - 2010)")
## Age
Age <- function(x) {select(rankOne[[x]],contains("ag"))}
winnerAge <- as.numeric(unlist(sapply(c(1:24), Age)))
names(winnerAge) <- NULL
hist(winnerAge, main = "Distribution of Winner Age (1999-2010)",
     xlab = "Winner Age", col = heat.colors(5))
## Time 
Wtime <- function(x) {select(rankOne[[x]],matches("^time|^net|^ net tim\\s*$"))}
winnerTime <- lapply(c(1:24), Wtime)
timeConvert <- function(x){period_to_seconds(ms(unlist(winnerTime[[x]])))}
winnerTime[[12]] <- select(rankOne[[12]], matches("^time"))
winnerSecond <- as.numeric(sapply(c(1:24), timeConvert))
year <- as.numeric(1999:2010)
plot(winnerSecond[1:12],type = "o", axes = FALSE, pch = 22, lty = 2, ylim = range(winnerSecond), ylab = 'Time', xlab = "Year")
lines(winnerSecond[13:24],type = "o", pch =22, col = 'red')
title(main = "Winner Time", font.main = 4)
axis(1, at = 1:12, lab = as.character(c(1999:2010)))
axis(2,  las = 1, at = c(2500,2600,2700,2800,2900,3000,3100,3200,3300,3400,3500,3600))
legend("top", xjust = 0,c("Men","Women") , cex=0.8,col=c("blue","red"), pch = 21:22, lty = 1:2, bty = "n")



##Average Time??
timeConvertPlus <- function(timeframe,x) {
  ifelse(length(strsplit(as.character(unlist(timeframe[1,x],":"))))==3,
         period_to_seconds(hms(unlist(timeframe[1,x))),
         period_to_seconds(ms(unlist(timeframe[1,x]))))}

alltime <- lapply(c(1:24), function(x) {select(cherryRun[[x]],matches("^time|^net|^ net tim\\s*$"))})
alltime[[12]]<- select(cherryRun[[12]], matches("^time"))

##Hometown
Homeall <- function(x) {select(cherryRun[[x]],contains("hometown"))}
allHome <- as.character(unlist(sapply(c(1:24), Homeall)))

Kenyaall <- sum(str_count(winnerHome, "Kenya\\s*"))/113288
Kenall <- sum(str_count(winnerHome,"[K[Ee][Nn]\\s]*"))/113288
Ethiopia<- sum(str_count(winnerHome, winnerHome[1]))/113288
Morocco <- sum(str_count(winnerHome, winnerHome[10]))/113288
Others <- (113288 - kenya - ken - ethiopia - morocco)/113288
pie(c(Kenya,Ken,Ethiopia,Morocco,Others),labels = c("Kenya","Ken","Ethiopia","Morocco","Other"), col = brewer.pal(5,"Purples"), main = "Hometown of Winners (1999 - 2010)")

# (1)General Analysis 
## Age
Ageall <- function(x) {select(cherryRun[[x]],contains("ag"))}
allAge <- as.numeric(unlist(sapply(c(1:24), Ageall)))
names(allAge) <- NULL
hist(allAge, main = "Distribution of Age (1999-2010)", 
     xlab = "Age", col = heat.colors(5))

## Number of Runner 
runner <- function (x) {nrow(cherryRun[[x]])}
runnerTotal = runnerM + runnerF
runnerM = unlist(lapply(c(1:12), runner))
runnerF = unlist(lapply(c(13:24), runner))
plot(runnerM,type = "o", axes = FALSE, pch = 22, lty = 2, 
     ylim = c(2000,16000), ylab = 'Runner', xlab = "Year")
lines(runnerF,type = "o", pch =22, col = 'red')
lines(runnerTotal, type = "o", pch = 22, lty = 1, col = "blue")
title(main = "Runner Number", font.main = 4)
axis(1, at = 1:12, lab = as.character(c(1999:2010)))
axis(2,  las = 1, at = c(2000,4000,6000,8000,10000,12000,14000,16000))
legend("topleft", xjust = 0,c("Men","Women","Total"),
       cex=0.8,col=c("black","red","blue"), pch = 21:22, lty = 1:2, bty = "n")
(runnerM[12]-runnerM[1])/runnerM[1]
(runnerF[12]-runnerF[1])/runnerF[1]
(runnerTotal[12]-runnerTotal[1])/runnerTotal[1]
