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
ff <- read.fwf("women10Mile_1999", widths = c( 5,9,21,2,18,7,5))
f[7:13]
f[8]
nchar(f[8])
nchar(strsplit(f[8], " "))
lapply(strsplit(f[8], " "), nchar)
strsplit(f[8]," ")

##(2) multiple files
##Step two: EDA##
##(3) Function for 1999
###Top function
readCherryBlossom = function (filename, txt = readLines(filename)) {
 
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

