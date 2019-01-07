print("Projet de R")

#on reset la mémoire

rm(list = ls())

print("Déclarations de fonctions")

filterInterestingData <- function(allData) {
  print("filterInterestingData")
  filteredData <- matrix(ncol=5)
  colnames(filteredData) <- c("date", "nombre_morts", "nombre_blesses", "etat", "accident")
  for(row in 1:nrow(allData)){
      date <- allData[row, "date"]
      if(allData[row, "latitude"] == "accidental")
      accident<- TRUE
      else accident <- FALSE
      #on saute les rows incohérents 
      nombre_morts <- allData[row, "n_killed"]
      nombre_blesses <- allData[row, "n_injured"]
      etat <- allData[row, "state"]
      
      #nombre_de_malfaiteurs <- "NULL" # On ne peut pas obtenir cette donnée car la collection est trop hétérogène et parfois incohérente
      #age_moyen_des_malfaiteurs <- "NULL" # On ne peut pas obtenir cette donnée car la collection est trop hétérogène et parfois incohérente
      
      aRow <- c(paste("",date), paste("",nombre_morts), paste("",nombre_blesses), paste("",etat), accident) # nombre_de_malfaiteurs, age_moyen_des_malfaiteurs retirés
      
      print(paste("date :",date,", morts :", nombre_morts, ", blesses :", nombre_blesses, "etat :", etat, ", accident: ", accident))
      #print(paste("Devrait etre le meme qu'au dessus : date :",aRow[1]))
      if (length(date)>0 &
         length(nombre_morts)>0 & is.numeric(nombre_morts) &
         length(nombre_blesses)>0 & is.numeric(nombre_blesses) &
         length(etat)>0
          )
      {
        rbind(filteredData,aRow, NULL) -> filteredData 
      }else{
        print("on evite un row incoherent")
        next;
      }
    if(row>100) return(filteredData) # faster for testing
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

print("On établit des corrélations sur ces données")

print("Pour cela on fait des statistiques sur les échantillons obtenus :")

print("On étudie les covariances")


