getwd()
rm(allData)
allData <- read.table("ressources/gun-violence-data_01-2013_03-2018.csv", sep = ";", header=FALSE, fill=TRUE)
# allDataContient toutes les sous parties des données sur lesquelles on va faire des stats

