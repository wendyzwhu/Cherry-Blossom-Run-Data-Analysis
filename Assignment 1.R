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
m09 <- read.fwf(textConnection(encoding09),widthC(11), skip = skipLine(11),  col.names = gsub("[\u00A0]"," ", cherryCol(11)), check.names=FALSE, fill = TRUE,  na.strings = c("","NA"), comment.char = '', blank.lines.skip = TRUE, encoding = "UTF-8")


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
