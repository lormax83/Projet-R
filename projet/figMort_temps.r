#FIGURE
#Mort par années
barplot(vect_mort,names.arg=c("2013","2014","2015","2016","2017","2018"))
#Blésés par années
barplot(vect_blese,names.arg=c("2013","2014","2015","2016","2017","2018"))
#NB Tot blésés mort + blésés par années
barplot(vect_tot,names.arg=c("2013","2014","2015","2016","2017","2018"))
#NB mort par mois
barplot(vect_mort_mois,names.arg=c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","décembre"))
#NB blésés par mois
barplot(vect_bleses_mois,names.arg=c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","décembre"))
#NB Tot blésés mort + blésés par mois
barplot(vect_tot_mois,names.arg=c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","décembre"))
#Analyse année avec le plus de crime 2017
#Nb de mort par mois 2017
barplot(vect_mort_mois_2017,names.arg=c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","décembre"))
#Nb blésé par mois 2017
barplot(vect_bleses_mois_2017,names.arg=c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","décembre"))
#Nb blésé total : mort + blésés 2017
barplot(vect_tot_mois_2017,names.arg=c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","décembre"))


#pie
pie(vect_tot,label=c("2013","2014","2015","2016","2017","2018"))
pie(vect_tot_mois,label=c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","décembre"))