##Step one: reading data into R##
##Get the brief idea of the data
setwd("d:\\00 Davis\\03 2015 Spring Quarter\\Stat 242\\Assignment 1\\stat242_2015\\Assignment1\\data")
list.dirs() #check the current directory
dir()
list.files()
f <- readLines("women10Mile_2001")
head(f)

##(1) one file

##(2) multiple files
##Step two: EDA##