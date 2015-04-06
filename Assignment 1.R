##Step one: reading data into R##
##Get the brief idea of the data
setwd("d:\\00 Davis\\03 2015 Spring Quarter\\Stat 242\\Assignment 1\\stat242_2015\\Assignment1\\data")
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
