
#import data
d <- read.csv("/Users/marc/Documents/gun-violence-data_01-2013_03-2018.csv")


#import library
library(maps)
library(ggplot2)

#Diagramme baton
shooting_2013 <- sum(substr(d$date,1,4) == "2013")
shooting_2014 <- sum(substr(d$date,1,4) == "2014")
shooting_2015 <- sum(substr(d$date,1,4) == "2015")
shooting_2016 <- sum(substr(d$date,1,4) == "2016")
shooting_2017 <- sum(substr(d$date,1,4) == "2017")
shooting_2018 <- sum(substr(d$date,1,4) == "2018")

vectshooting <- c(shooting_2013,shooting_2014,shooting_2015,shooting_2016,shooting_2017,shooting_2018)

print("Nom de mort par année")
barplot(vectshooting,names.arg=c("2013","2014","2015","2016","2017","2018"))

#diagramme pour voir les etats avec le plus d'incidents liés au armes
bystate <- d %>% group_by(state) %>% summarize(n = n()) %>% arrange(n)
bystate$state <- factor(bystate$state, levels = bystate$state)

ggplot(bystate, aes(x = state, y = n)) + geom_bar(stat = "identity") + coord_flip() + theme_bw()

#incidents repartie sur la carte des Etats-Unis
global <- map_data("state")
ggplot(global, aes(x = long, y = lat)) + geom_polygon(aes(group = group), fill = "white", col = "black") + coord_fixed(1.3, xlim = c(-130,-60), ylim = c(20,50)) +
  geom_point(data = d, aes(x = longitude, y = latitude, col = n_killed), size = 0.001, alpha = .1) +  
  theme_void() + 
  theme(legend.position = "none")

#incidents répartie sur les jours 
#tableau
topdatearrange <- d %>% group_by(substr(d$date,6,10)) %>% summarize(n = n()) %>% arrange(-n)
topdatearrange

#graph
topdate <- d %>% group_by(substr(d$date,6,10)) %>% summarize(n = n())
topdate
barplot(topdate$n)