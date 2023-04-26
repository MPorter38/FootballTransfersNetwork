# Script to perform some exploratory analysis after Data_Clean.R

# Run Functions.R file for functions
source("Functions.R")

# Plots of how Size and Order of the networks change over time
Nv<-rep(NA,21)
Ne<-rep(NA,21)
for(i in 1:21){
  Nv[i]<-vcount(yearly.graphs[[i]])
  Ne[i]<-ecount(yearly.graphs[[i]])
}
plot(Ne,xlab = "Season", ylab="Size", main="The Size of each Season's Network")
plot(Nv,xlab = "Season", ylab="Order", main="The Order of each Season's Network")

# Plot circular graph of network for each season
for(i in 1:21){
  plot(yearly.graphs[[i]], layout=layout_in_circle, edge.arrow.size=0.5)
}

# Heatmap plot of each season
for (i in 1:21){
  year.adj<- as.matrix(get.adjacency(yearly.graphs[[i]]))
  heatmap(year.adj, Rowv = NA, Colv = NA)
  title(xlab="League From", ylab= "League To")
}

# Focusing on Top5 Leagues (Prem, La Liga, Ligue Un, Bundesliga, Serie A)
top5Leagues<- c("Premier League", "Serie A", "Ligue 1","LaLiga","Bundesliga")

# Plotting betweeness centrality of Top 5 Leagues
top5bet<- matrix(nrow=21, ncol=5)
colnames(top5bet)<- top5Leagues
rownames(top5bet)<- rep("",21)
for (i in 1:21){
  top5bet[i,]<- betweenness(yearly.graphs[[i]], v=top5Leagues)
}
mosaicplot(cbind(top5bet[,5],top5bet[,c(4,3,2,1)]), col=c(5,4,3,2,1), main= "A Plot of Betweeness from 00/01 to 20/21")

# Plotting closeness centrality for top 5 leagues
top5close<- matrix(nrow=21, ncol=5)
colnames(top5close)<- top5Leagues
rownames(top5close)<- rep("",21)
for (i in 1:21){
  #only looks at largest connected component 
  top5close[i,]<- closeness(decompose(yearly.graphs[[i]])[[1]], v=top5Leagues)
}
mosaicplot(top5close, col=c(1,2,3,4,5), main="A Plot of Closeness from 00/01 to 20/21")

# Plotting eigenvector centrality for top 5 leagues
top5ev<- matrix(nrow=21, ncol=5)
colnames(top5ev)<- top5Leagues
rownames(top5ev)<- rep("",21)
for (i in 1:21){
  ev<- eigen_centrality(yearly.graphs[[i]])$vector
  for (j in 1:5){
    top5ev[i,j]<- ev[top5Leagues[j]]
  }
}
mosaicplot(top5ev, col=c(1:5), main="A Plot of Eigen Centrality from 00/01 to 20/21")

# Look at the leagues that are in every graph 
V.list<- list()
Leagues.mat <- rep(0,52)
for(i in 1:19){
  V.list[[i]]<- labels(V(yearly.graphs[[i]]))
  Leagues.mat <- Leagues.mat + (league_names %in% V.list[[i]])
}
topLeagues<- league_names[which(Leagues.mat==19)]
print("Leagues in every network are")
print(topLeagues)

