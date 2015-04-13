##Step one: reading data into R##
##Get the brief idea of the data
setwd("d:\\00 Davis\\03 2015 Spring Quarter\\Stat 242\\Assignment 1\\stat242_2015\\Assignment1")
setwd("d:\\00 Davis\\03 2015 Spring Quarter\\Stat 242\\Data_Duncan\\stat242_2015\\Assignment1\\data")
list.dirs() #check the current directory
dir()
list.files()
ff <- file("women10Mile")
ff <- readLines("women10Mile_1999")
head(f)

##(1) one file
#1999
ff <- ff[-(1:6)]
ff <- read.fwf("women10Mile_1999", lapply(strsplit(ff[8], " "), count))
ff[7:13]
ff[7]
nchar(ff[7])
nchar(strsplit(ff[8], " "))

lapply(strsplit(ff[8], " "), count )
count = function (x) {
  #Add 1 to each width#
  #there is a blank after each cloumn#
  nchar(x) +1
}
strsplit(ff[8]," ")

##(2) multiple files
##Step two: EDA##
##(3) Function for 1999
###Top function

readCherryBlossom = function (filename, txt = readLines(filename, warn = FALSE)) {
 
  header = splitHeader(txt)[1]
  header
  body = splitHeader(txt)[2]
  body
} 

splitHeader = function (txt) {
  isBlank = txt ==""
  br = which(isBlank)[2]
  start = 1
  list( header = txt[start:(br-1)],
        body = txt[-(1:br)])
}

##Test read all files by using readLines
lapply(list.files(), readCherryBlossom)
##debug function
debug = function(filename, txt = readLines(filename)){
  isBlank = txt ==""
  br = which(isBlank)[2]
}
paths <- list.files(, 
                    recursive = TRUE)

lapply(paths, readCherryBlossom)
readCherryBlossom("women10Mile_2000")

##readalldata into a big list
wholeList = lapply(list.files(), function(x)(readLines(x,encoding = "UTF-8")))

##Deal with the unusual files men 2006 2008 women 2004 2008
men2006 = readLines(men10Mile_2006)
women2004 = readLines(women10Mile_2004)
men2008 = readLines(men10Mile_2008)
women2008 = readLines(women10Mile_2008)
##Though there is a waringing but it is fine. 

##Assign names to the big list ???
names(wholeList)[1] <- list.files()[1]
nameswholeList = function (filenames, wholeList = wholeList) 
{ 
  location = function (filenames) 
{ location = which (list.files() == filenames)
  location}
  names(wholeList)[location] <- list.files()[location] }
locate = function (filenames) 
{ location = which (list.files() == filenames)
  location}
lapply(list.files(), nameswholeList)


##Remove the title
##detect the blanks in the files
blanks = function (x, wholeList = lapply(list.files(), readLines)) ##could it be more efficient?
{which(wholeList[[x]] == "")}

blanksDetect <- lapply(c(1:24), blanks)

##detect "=" in the files##slow
titleDetect = function (x)
{
  library(stringr)
  wholeList = lapply(list.files(), readLines)
  wholeList[[15]] <- str_replace(wholeList[[15]], wholeList[[15]][18], wholeList[[3]][17])
  wholeList[[15]] <- str_replace(wholeList[[15]], wholeList[[15]][17], wholeList[[3]][16])
  grep("^=", wholeList[[x]])}
titleDect <- lapply(c(1:24), titleDetect)

##delect the title function
deleteTitle = function (x) 
{wholeList[[x]][-(1:(titleDect[[x]]-2))]}
wholeListNew <- lapply(c(1:24), deleteTitle)
##delete the "=="
deleteEdge = function(x)
{wholeListNew[[x]][-2]}
wholeListNew1 <- lapply(c(1:24), deleteEdge)

###speacial case for women 2001 add the header???how to sovle 
wholeList[[15]] <- str_replace(wholeList[[15]], wholeList[[15]][18], wholeList[[3]][17])
wholeList[[15]] <- str_replace(wholeList[[15]], wholeList[[15]][17], wholeList[[3]][16])
head(deleteTitle(15))
head(women2001)
wholeList[[15]]<- wholeList[[15]][-(1:grep("^=", wholeList[[15]])-2)]

##Check the title to see if there are in the same form
checkFir = function (x) 
{wholeListNew1[[x]][1]}
fir = lapply(c(1:24), checkFir)


##Turn header to the Upper?? problem
upper = function (x) 
{toupper(wholeListNew1[[x]][1])}
wholeListU = lapply(c(1:24), upper)
firU = sapply(c(1:24), function(x) (wholeListU)[[x]][1])

upperTitle = function (x) 
{library(stringr)
 wholeListNew1[[x]] <- 
   str_replace(wholeListNew1[[x]], wholeListNew1[[x]][1], firU[x])}
listCap <- lapply(c(1:24), upperTitle) #organized form 
checkTiltle <- lapply(c(1:24), function(x){listCap[[x]][1]})

##split the data
###try on a file
m99 <- as.data.frame(listCap[[1]])



##split according to ==
lapply(strsplit(wholeListNew[[1]][2], " "), count )
count = function (x) {
  #Add 1 to each width#
  #there is a blank after each cloumn#
  nchar(x) +1
}

##try read.fwf
###general cases
count = function (x) {
  #Add 1 to each width#
  #there is a blank after each cloumn#
  nchar(x) +1
}
##detect "=" in the files##slow
titleDetect = function (x)
{
  library(stringr)
  wholeList = lapply(list.files(), readLines)
  wholeList[[15]] <- str_replace(wholeList[[15]], wholeList[[15]][18], wholeList[[3]][17])
  wholeList[[15]] <- str_replace(wholeList[[15]], wholeList[[15]][17], wholeList[[3]][16])
  grep("^=", wholeList[[x]])}
titleDect <- lapply(c(1:24), titleDetect) ##a list of number where = exists 

##names 
filenames = function(x) {list.files()[x]}

#skip 
skipLine = function (x) {titleDect[[x]]}
skipN = lapply(c(1:24), skip)

#width in the big data frame
widthC = function(x) {lapply(strsplit(wholeList[[x]][titleDect[[x]]]," "), count)}

#read data amazing function 
cherryBlossom <- function (x) { read.fwf(filenames(x), widthC(x), skip = skipLine(x), comment.char = '', encoding = "UTF-8")}

##exception??Na??blank
cherryBlossomT <- function (x) { read.fwf(filenames(x), widthC(x), skip = skipLine(x), col.names = cherryCol(x), check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")}

##names ??some exception 
nameVar = function(x) { tolower(as.character(unlist(x[1,])))}
##Extract the title??problem 
cherryNames <- function (x) { read.fwf(filenames(x), widthC(x), skip = skipLine(x)-2, fill = TRUE,  na.strings = c("","NA"), comment.char = '', stringsAsFactors=FALSE, blank.lines.skip = TRUE,encoding = "UTF-8")}
cherryCol <- function (x) {tolower(cherryNames(x)[1,])}

##Encoding solved
encoding09 <- readLines("men10Mile_2009", encoding = "UTF-8")
encoding09 <- gsub("[\u00A0]"," ", encoding09 )
widthEncoding <- lapply(strsplit(encoding09[grep("^=", encoding09)]," "),count)
m09 <- read.fwf(textConnection(encoding09),c( 6,12,7,23,3,21,8,7,2,6), skip = skipLine(11),  col.names =  c("place","div/tot","num","name","ag","hometown","gun time","time","id","pace"), check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")


##name the women solved
w01 <- cherryBlossomT(15)
colnames(w01) <- cherryCol(3)

##uniform the title 
uniTitle <- sapply(c(1:24), cherryCol)

##put into a big dataframe list
cherryRun <- lapply(c(1:24),cherryBlossomT)
cherryRun[[15]]<-w01
cherryRun[[11]]<-m09
cherryRun[[8]] <- cherryRun[[8]][-(5236:5237),] ##incomplet lines
cherryRun[[10]] <- cherryRun[[10]][-(5906:5907),] ##incomplet lines
cherryRun[[22]] <- cherryRun[[22]][-6398,] ##incomplet lines

######EXCEPTIONS "="
cherryNamesEX <- function (x) { read.fwf(filenames(x), c(6,9,7,23,3,16,8,8,1,6,2), skip = skipLine(x)-2, fill = TRUE,  na.strings = c("","NA"), comment.char = '', stringsAsFactors=FALSE, blank.lines.skip = TRUE,encoding = "UTF-8")}
cherryColEX <- function (x) {tolower(cherryNamesEX(x)[1,])}
cherryRun[[8]]  <- read.fwf(filenames(8), c(6,9,7,23,3,16,8,8,1,6,2), skip = skipLine(8), col.names = cherryColEX(8), check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")
cherryRun[[20]] <- read.fwf(filenames(20), c(6,9,7,23,3,16,8,8,1,6,2), skip = skipLine(20), col.names = cherryColEX(20), check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")

######EXCEPTIONS TIME
cherryRun[[9]] <- read.fwf(filenames(9), c( 6,12,7,23,3,19,7,1,1,6,2,8), skip = skipLine(9), col.names = c("place","div/tot","num","name","ag","hometown","time","id","na","pace","s","split"), check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")

cherryRun[[21]] <- read.fwf(filenames(21), c( 6,12,7,23,3,19,7,1,1,6,2,8), skip = skipLine(21), col.names = c("place","div/tot","num","name","ag","hometown","time","id","na","pace","s","split"), check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")

cherryRun[[23]] <- read.fwf(filenames(23), c( 6,12,7,23,3,21,8,7,2,6), skip = skipLine(23), col.names = c("place","div/tot","num","name","ag","hometown","gun time","time","id","pace"), check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")


cherryRun[[12]]<-  read.fwf(filenames(12),widthC(12), skip = skipLine(12), col.names =  c("place","div/tot","num","name","ag","hometown","5 mile","time","net time","na","pace"), check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")


cherryRun[[24]]<-  read.fwf(filenames(24), c( 6,12,7,23,3,21,8,8,7,1,1,6,2), skip = skipLine(24), col.names = c("place","div/tot","num","name","ag","hometown","5 mile","gun time","time","id","na","pace","s"), check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")


##clean up the big data list 
cherryRunFinal <- lapply(c(1:24),cherryBlossomT)
cherryRunFinal[[15]]<-w01
cherryRunFinal[[11]]<-m09
cherryRunFinal[[1]] <- cherryRunFinal[[1]][,-c(2,7)]
cherryRunFinal[[13]] <- cherryRunFinal[[13]][,-c(2,7)]




##Rank one problem 
placeOne <- function(x){cherryRun[[x]][1,]}
rankOne <- lapply(c(1:24), placeOne)

##Hometown
Home <- function(x) {select(rankOne[[x]],contains("hometown"))}
winnerHome <- as.character(unlist(sapply(c(1:24), Home)))
Kenya <- sum(str_count(winnerHome, "Kenya\\s*"))/24
Ken <- sum(str_count(winnerHome,"[K[Ee][Nn]\\s]*"))/24
Ethiopia<- sum(str_count(winnerHome, winnerHome[1]))/24
Morocco <- sum(str_count(winnerHome, winnerHome[10]))/24
Others <- (24 - kenya - ken - ethiopia - morocco)/24
pie(c(Kenya,Ken,Ethiopia,Morocco,Others),labels = c("Kenya","Ken","Ethiopia","Morocco","Other"), col = brewer.pal(5,"Purples"), main = "Hometown of Winners (1999 - 2010)")

##Age
Age <- function(x) {select(rankOne[[x]],contains("ag"))}
winnerAge <- as.numeric(unlist(sapply(c(1:24), Age)))
names(winnerAge) <- NULL
hist(winnerAge, main = "Distribution of Winner Age (1999-2010)", xlab = "Winner Age", col = heat.colors(5))

###time 
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

##Age
Ageall <- function(x) {select(cherryRun[[x]],contains("ag"))}
allAge <- as.numeric(unlist(sapply(c(1:24), Ageall)))
names(allAge) <- NULL
hist(allAge, main = "Distribution of Age (1999-2010)", xlab = "Age", col = heat.colors(5))

##gender
runner <- function (x)
