library("data.table") # install.packages("data.table")
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
  for(row in 0:nrow(allData)){
    date <- get_years(allData[row, "date"])
    
    if(
       is.null(date)|
       is.null(allData[row, "n_killed"]) | length(allData[row, "n_killed"])==0 |
       is.null(allData[row, "n_injured"]) | length(allData[row, "n_injured"])==0 |
       is.null(allData[row, "state"]) | length(allData[row, "state"])==0
       ){ 
      next
    }
      
      #on saute les rows incohérents 
      nombre_morts <- allData[row, "n_killed"]
      nombre_blesses <- allData[row, "n_injured"]
      etat <- allData[row, "state"]
      
      #nombre_de_malfaiteurs <- "NULL" # On ne peut pas obtenir cette donnée car la collection est trop hétérogène et parfois incohérente
      #age_moyen_des_malfaiteurs <- "NULL" # On ne peut pas obtenir cette donnée car la collection est trop hétérogène et parfois incohérente
      aRow <- tryCatch({
        c(paste("",date), nombre_morts, nombre_blesses, etat) # nombre_de_malfaiteurs, age_moyen_des_malfaiteurs retirés
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
   
    if(row>500) return(filteredData) # faster for testing
  }
  datelist <- lapply(X = filteredData[,"date"], FUN = get_years)
  print(class(datelist))
  nombre_mortslist <- lapply(X = filteredData[,"nombre_morts"], FUN = as.numeric )
  print(class(nombre_mortslist))
  nombre_blesseslist <- lapply(X = filteredData[,"nombre_blesses"], FUN = as.numeric )
  print(class(nombre_blesseslist))
  etatlist <- lapply(X = filteredData[,"etat"], FUN = as.character)
  print(class(etatlist))
  
  filteredData[,"date"] <- unlist(datelist, use.names = FALSE)
  filteredData[,"nombre_morts"] <- unlist(nombre_mortslist, use.names = FALSE)
  filteredData[,"nombre_blesses"] <- unlist(nombre_blesseslist, use.names = FALSE)
  filteredData[,"etat"] <- unlist(etatlist, use.names = FALSE)
  
  return(filteredData)
}

# POINT D'ENTREE

#on se place dans l'arboresence de fichier a l'endroit ou est le projet
getwd()
setwd("/home/asus/Desktop/work/M1_MATHS/")
print("on recupere le dataset avec les entetes")

headers <- c("incident_id","date","state","city_or_county","address","n_killed","n_injured","incident_url","source_url","incident_url_fields_missing","congressional_district","gun_stolen","gun_type","incident_characteristics","latitude","location_description","longitude","n_guns_involved","notes","participant_age","participant_age_group","participant_gender","participant_name","participant_relationship","participant_status","participant_type","sources","state_house_district","state_senate_district")
allData <- fread("gun-violence-data_01-2013_03-2018.csv", sep = ",", header=TRUE, nrows = 500)
allData <- as.data.frame(allData)
colnames(allData) <- headers

#View(allData)
print("On recupere les donnes qui nous interessent depuis le dataset")
filteredDataRes <- filterInterestingData(allData)


print(typeof(filteredDataRes))

View(filteredDataRes)


print(summary(filteredDataRes))
print("On établit des corrélations sur ces données")

print("Pour cela on fait des statistiques sur les échantillons obtenus :")

print("On étudie les covariances")


