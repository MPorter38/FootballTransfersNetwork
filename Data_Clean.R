# This script takes the data from 'RawData0021.csv', transfer involving unwanted leagues, 
# and formats the data into graph objects for each season. The edges are the 
# (combined) transfers and the vertices are the leagues 

library(igraph)

# Read in the data and setup variable of season names
Data<- read.csv(file = 'RawData0021.csv')
season<-c("00-01","01-02","02-03","03-04","04-05","05-06", "06-07", "07-08", 
          "08-09", "09-10","10-11", "11-12", "12-13", "13-14", "14-15", "15-16",
          "16-17", "17-18" ,"18-19","19-20","20-21")


# League Names that we want to have (e.g. only top divisions)
league_names<- c("Vysheyshaya Liga","Virsliga","UAE Gulf League","Super League",
                 "Stars League","Serie A Segunda Etapa","Serie A","Super Lig",
                 "Chilean Primera","Colombia Primera","Premiership","Premier Liga",
                 "Premier League","NB I.","MLS","Ligue 1","Ligat ha'Al","Liga NOS",
                 "LaLiga","K League 1","Jupiler Pro League","J1 League","Fortuna Liga",
                 "Eredivisie","Eliteserien","Ekstraklasa","Bundesliga","Botola Pro",
                 "Allsvenskan","1.HNL","1.Bundesliga","Primera División","SuperLigaen",
                 "Liga 1","Liga MX","Brasileiro","Professional League","Ligue I Pro",
                 "UPL", "GSL","SSL","Veikkausliiga","A-League","Gulf Pro League",
                 "Mol Nat Div","Paraguay Primera","Peru Liga","Uruguay Primera",
                 "Parva Liga","Super liga", "Slovak Liga", "SA Prem" )

# Remove unwanted leagues from League_from 
Data<- Data[which(Data[,2] %in% league_names),]
# and from League_to
Data<- Data[which(Data[,3] %in% league_names),]
# Remove first column - Player Names
Data<- Data[,-1]

# Setup a graph object for each season
yearly.graphs<- list()
for (i in 1:21){
  year<- Data[which(Data[,4]==levels(factor(Data[,4]))[i]),][c(1,2,3)]
  year.edj<- graph_from_edgelist(as.matrix(year[1:2]))
  E(year.edj)$weight<- year[,3] 
  yearly.graphs[[i]]<- simplify(year.edj)
  E(yearly.graphs[[i]])$width<- 0.06*(E(yearly.graphs[[i]])$weight)
}
rm(year, year.edj)