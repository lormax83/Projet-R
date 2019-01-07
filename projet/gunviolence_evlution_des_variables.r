print("Projet de R")

#on reset la mémoire

rm(list = ls())

print("Déclarations de fonctions")

filterInterestingData <- function(allData) {
  filteredData <- matrix(ncol=5)
  colnames(filteredData) <- c("nombre_morts", "nombre_blesses", "nombre_de_malfaiteurs", "age_moyen_des_malfaiteurs", "etat")
  for(row in 1:nrow(allData)){
    if(length(allData[row, "date"])>0){

      #on a un row a priori renseigné
      nombre_morts <- allData[row, "n_killed"]
      nombre_blesses <- allData[row, "n_injured"]
      etat <- allData[row, "state"]
      print(paste("morts :", nombre_morts, ", blesses :", nombre_blesses, "etat :", etat, "\n"))
      nombre_de_malfaiteurs <- "todo"
      age_moyen_des_malfaiteurs <- "todo"
      
      aRow <- c(nombre_morts, nombre_blesses, nombre_de_malfaiteurs, age_moyen_des_malfaiteurs, etat)
      rbind(filteredData,aRow, NULL) -> filteredData
      
      
    }
    if(row>3) return(filteredData) # for testing
  }
  return(filteredData)
}
# POINT D'ENTREE

#on se place dans l'arboresence de fichier a l'endroit ou est le projet
getwd()
setwd("/home/asus/Desktop/work/M1_MATHS/")
print("on recupere le dataset avec les entetes")

headers <- c("incident_id","date","state","city_or_county","address","n_killed","n_injured","incident_url","source_url","incident_url_fields_missing","congressional_district","gun_stolen","gun_type","incident_characteristics","latitude","location_description","longitude","n_guns_involved","notes","participant_age","participant_age_group","participant_gender","participant_name","participant_relationship","participant_status","participant_type","sources","state_house_district","state_senate_district")
allData <- read.csv("gun-violence-data_01-2013_03-2018.csv", sep = ",", header=FALSE, skip = 1, skipNul = TRUE, fill = TRUE, quote = "", strip.white = TRUE )
colnames(allData) <- headers

#View(allData)
print("On recupere les donnes qui nous interessent depuis le dataset")

filteredDataRes <- filterInterestingData(allData)
View(filteredDataRes)

