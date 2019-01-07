print("Projet de R")

#on reset la mémoire

rm(list = ls())

print("Déclarations de fonctions")

get_years <- function(date) {
  temp<-substr(date,1,4)
  if(is.null(temp))
  if(is.numeric(temp)&as.numeric(temp)>1900)
    return(temp)
  else return(NULL)
}

filterInterestingData <- function(allData) {
  print("filterInterestingData")
  filteredData <- matrix(ncol=4)
  colnames(filteredData) <- c("date", "nombre_morts", "nombre_blesses", "etat")
  for(row in 1:nrow(allData)){
    if(
       is.null(allData[row, "date"]) | length(allData[row, "date"])==0 |
       is.null(allData[row, "n_killed"]) | length(allData[row, "n_killed"])==0 |
       is.null(allData[row, "n_injured"]) | length(allData[row, "n_injured"])==0 |
       is.null(allData[row, "state"]) | length(allData[row, "state"])==0
       ){ 
      next
    }
      date <- allData[row, "date"] 
      #date <- substr(date, 1, 4)
      nombre_morts <- allData[row, "n_killed"]
      nombre_blesses <- allData[row, "n_injured"]
      etat <- allData[row, "state"]
      
      #nombre_de_malfaiteurs <- "NULL" # On ne peut pas obtenir cette donnée car la collection est trop hétérogène et parfois incohérente
      #age_moyen_des_malfaiteurs <- "NULL" # On ne peut pas obtenir cette donnée car la collection est trop hétérogène et parfois incohérente
      aRow <- tryCatch({
        dateAsDate <- as.Date(date, "%Y-%m-%d", "%Y/%m/%d",FALSE)
        if(is.na(dateAsDate)) next
        c(substr(paste(dateAsDate,""),1,4), nombre_morts, nombre_blesses, etat) # nombre_de_malfaiteurs, age_moyen_des_malfaiteurs retirés
      }, warning = function(w) {
        return(NULL);
      }, error = function(e) {
        return(NULL);
      })
      if(is.null(aRow)){
        print("row mal formé")
        next
      }else print("row bien formé")
      
    print(paste("date :",date,", morts :", nombre_morts, ", blesses :", nombre_blesses, "etat :", etat))
    #print(paste("Devrait etre le meme qu'au dessus : date :",aRow[1]))
   
    rbind(filteredData,aRow, NULL) -> filteredData 
  }

  return(filteredData)
}

# POINT D'ENTREE

#on se place dans l'arboresence de fichier a l'endroit ou est le projet
getwd()
setwd("/home/asus/Desktop/work/M1_MATHS/")
print("on recupere le dataset avec les entetes")

headers <- c("incident_id","date","state","city_or_county","address","n_killed","n_injured","incident_url","source_url","incident_url_fields_missing","congressional_district","gun_stolen","gun_type","incident_characteristics","latitude","location_description","longitude","n_guns_involved","notes","participant_age","participant_age_group","participant_gender","participant_name","participant_relationship","participant_status","participant_type","sources","state_house_district","state_senate_district")
allData <- fread("gun-violence-data_01-2013_03-2018.csv", sep = ",", header=TRUE, nrows = 5000)
allData <- as.data.frame(allData)
colnames(allData) <- headers

#View(allData)
filteredDataRes <- filterInterestingData(allData)
View(filteredDataRes)

dateList <- filteredDataRes[,"date"]
mortList <- filteredDataRes[,"nombre_morts"]
blessesList <-filteredDataRes[,"nombre_blesses"]
etatList <- filteredDataRes[, "etat"]

mortParEtat <- matrix(nrow=min(length(mortList), length(etatList)), ncol = 2)
mortParEtat[,0] <- mortList
mortParEtat[,1] <- etatList

print(summary(mortParEtat))


print("On établit des corrélations sur ces données")

print("Pour cela on fait des statistiques sur les échantillons obtenus :")

print("On étudie les covariances")


