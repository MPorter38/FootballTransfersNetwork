# Script uses a linear regression approach to predict the edges in 2019-20 season
# using the previous season's data (edge weights)

# Set up adjacency matrix that is consistent across season
# i.e. introduce zero entries for when league is not in a season's network
adj<- matrix(0L, nrow=52, ncol=52)
rownames(adj)<- league_names
colnames(adj)<- league_names
yearly.adjs<- list()
for (i in 1:21){
  yearly.adjs[[i]]<- adj
  year<- Data[which(Data$Season==levels(factor(Data[,4]))[i]),][c(1,2,3)]
  year.graph<- graph_from_data_frame(year, directed=TRUE)
  E(year.graph)$weight<- E(year.graph)$Fee
  year.graph<- simplify(year.graph) 
  year.adj<- as.matrix(get.adjacency(year.graph, attr = "weight")) 
  yearnames<- rownames(year.adj)
  for(j in 1:length(yearnames)){
    for(k in 1:length(yearnames)){
      if(year.adj[yearnames[k],yearnames[j]]>0){
        yearly.adjs[[i]][yearnames[k],yearnames[j]]<-year.adj[yearnames[k],yearnames[j]] 
      }
    }
  }
}
rm(year, year.graph, year.adj, adj, yearnames)

# Create graph objects from these standardised adjacency matrices (can plot if desired)
yearly.graphs.reg<- list()
for(i in 1:21){
  graph<- graph_from_adjacency_matrix(yearly.adjs[[i]], weighted = TRUE)
  E(graph)$width<- 0.1*E(graph)$weight
  # plot(graph, layout=layout_in_circle, arrow.size=0.5)
  yearly.graphs.reg[[i]]<- graph
}
rm(graph)

# How does total edge weight (transfer fees) change over time?
TotalFees<- rep(0,21)
for (i in 1:21){
  TotalFees[i]<- sum(yearly.adjs[[i]])
}
plot(1:19, TotalFees[1:19], xlab = "Season", ylab="Total Transfer Fees (millions of £)", col= c(rep(1,19)), type="b", main="Change in Transfer Fees over Time")

# Look at just Serie A and Premier League edge to see linear model method 
PremSerieA<- rep(0,19)
for (i in 1:19){
  PremSerieA[i]<- yearly.adjs[[i]]["Premier League","Serie A"]
}
Index<-1:19
data<- rbind(Index, PremSerieA)
LM<- lm(PremSerieA~Index, data = as.data.frame(data)) 
plot(Index, PremSerieA, xlab = "Season", ylab = "Edge Weight", main="Premier League to Serie A")
abline(LM, col=2)

# Predict edge weights like this for all edges of all graphs for 20th season
# Prediction matrix is essentially a adjacency matrix
Prediction<- matrix(0L, nrow=52, ncol = 52)
Season<-1:19
for (i in 1:52){
  for(j in 1:52){
    A<- league_names[i]
    B<- league_names[j]
    Weights<- rep(0,19)
    for (k in 1:19){
      Weights[k]<- yearly.adjs[[k]][A,B]
    }
    data<- rbind(Season, Weights)
    LM<- lm(Weights~Season, data = as.data.frame(data)) 
    P<- LM$coefficients[1]+20*LM$coefficients[2]
    if(P>0){
      Prediction[i,j]<- round(P, digits = 2)
    }
  }
}

# Graphs from these predictions
Pred.graph<-graph_from_adjacency_matrix(Prediction, weighted = TRUE)
V(Pred.graph)$label<- league_names
E(Pred.graph)$width<-0.1*E(Pred.graph)$weight

# Label and plot predicted 20th season graph and actual graph for 20th season
V(Pred.graph)$label.cex <- rep(0.9, 52)
V(Pred.graph)$size <- rep(10, 52)
V(Pred.graph)$label <- c("Vysheyshaya Liga","Virsliga","UAE Gulf League","Super League","Stars League","Serie A Segunda Etapa","Serie A","Super Lig","Chilean Primera","Colombia Primera","Premiership","P.Liga","Prem","NB I.","MLS","Ligue 1","Ligat ha'Al","Liga NOS","LaLiga","K League 1","Jupiler Pro League","J1 League","Fortuna Liga","Eredivisie","Eliteserien","Ekstraklasa","Bundesliga","Botola Pro","Allsvenskan","1.HNL","1.Bundesliga","Primera División","SuperLigaen","Liga 1","Liga MX","Brasileiro","ProLeague","Ligue I Pro","UPL", "GSL","SSL","Veikkausliiga","A-League","Gulf Pro League","Mol Nat Div","Paraguay Primera","Peru Liga","Uruguay Primera","Parva Liga","Super liga", "Slovak Liga", "SA Prem" )
Actual.graph <- yearly.graphs.reg[[20]]
V(Actual.graph)$size <- rep(10, 52)
plot(Actual.graph, layout=layout_in_circle, edge.arrow.size=0.5, vertex.label=NA)
plot(Pred.graph, layout=layout_in_circle, edge.arrow.size=0.5, vertex.label=NA)
# Predicted graph has more smaller edges, possibly because it does not have cutoff
# of only top 200 transfers per season

# Hence looking at minimum transfer fee to meet this threshold each season
yearly.min<- rep(0,19)
for (i in 1:19){
  yearly.min[i]<- min(yearly.adjs[[i]][yearly.adjs[[i]]>0])
}
yearly.minLM<- lm(yearly.min~Season, data= as.data.frame(rbind(Season, yearly.min)))
plot(1:19, yearly.min, xlab = "Season", ylab = "Minimum Value", main="Minimum Value of Edge Weights for each Season")
abline(yearly.minLM, col=2)
# Can use this predicted minimum transfer fee to raise the cutoff for edges to be 
# included in the predicted graph 
newmin <- yearly.minLM$coefficients[1] + 20* yearly.minLM$coefficients[2]
for (i in 1:52){
  for (j in 1:52){
    if(Prediction[i,j]<newmin){
      Prediction[i,j]<- 0
    }
  }
}

# Plot predicted and actual graph again
Pred.graph<-graph_from_adjacency_matrix(Prediction, weighted = TRUE)
V(Pred.graph)$label<- league_names
E(Pred.graph)$width<-0.1*E(Pred.graph)$weight
V(Pred.graph)$size <- rep(10, 52)
plot(Pred.graph, layout=layout_in_circle, vertex.label=NA, edge.arrow.size=0.5)
plot(yearly.graphs.reg[[20]], layout=layout_in_circle)

# Rescale edge weights so total transfers fee match, only 10 most recent seasons
# to reflect how transfer fees have increased over time
TotalFees.LM<- lm(TotalFees[9:19]~Season[9:19], data= as.data.frame(rbind(Season[1:19], TotalFees[1:19])))
plot(c(Season), TotalFees[1:19], xlab = "Season", ylab="Total Transfer Fees (millions of £)", col= c(rep(1,19)))
abline(TotalFees.LM, col=2)
predicted.total<- TotalFees.LM$coefficients[1]+20*TotalFees.LM$coefficients[2]
Prediction<- (predicted.total/sum(Prediction)) * Prediction

# Plot new predicted graph 
Pred.graph<-graph_from_adjacency_matrix(Prediction, weighted = TRUE)
V(Pred.graph)$label<- NA
E(Pred.graph)$width<-0.1*E(Pred.graph)$weight
V(Pred.graph)$size <- rep(10, 52)
plot(Pred.graph, layout=layout_in_circle, edge.arrow.size=0.5)
# Now closer to actual graph