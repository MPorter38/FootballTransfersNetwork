# This script uses the methods from Regression_Model.R to predict what the 
# 20-21 season's graphs could have looked like without the impact of the 
# COVID19 Pandemic 

# Predicted Adjacency matrix 
Prediction21<- matrix(0L, nrow=52, ncol = 52)
Season<-1:20
for (i in 1:52){
  for(j in 1:52){
    A<- league_names[i]
    B<- league_names[j]
    Weights<- rep(0,20)
    for (k in 1:20){
      Weights[k]<- yearly.adjs[[k]][A,B]
    }
    data<- rbind(Season, Weights)
    LM<- lm(Weights~Season, data = as.data.frame(data)) 
    P<- LM$coefficients[1]+21*LM$coefficients[2]
    if(P>0){
      Prediction21[i,j]<- round(P, digits = 2)
    }
  }
}

# Impose predicted minimum fee
yearly.min<- rep(0,20)
for (i in 1:20){
  yearly.min[i]<- min(yearly.adjs[[i]][yearly.adjs[[i]]>0])
}
yearly.minLM<- lm(yearly.min~Season, data= as.data.frame(rbind(Season, yearly.min)))

newmin <- yearly.minLM$coefficients[1] + 21* yearly.minLM$coefficients[2]

for (i in 1:52){
  for (j in 1:52){
    if(Prediction21[i,j]<newmin){
      Prediction21[i,j]<- 0
    }
  }
}

# Rescale based on predicted total transfer fees
TotalFees<- rep(0,21)
for (i in 1:21){
  TotalFees[i]<- sum(yearly.adjs[[i]])
}
TotalFees.LM<- lm(TotalFees[9:20]~Season[9:20], data= as.data.frame(rbind(Season[1:20], TotalFees[1:20])))

predicted.total<- TotalFees.LM$coefficients[1]+21*TotalFees.LM$coefficients[2]
Prediction21<- (predicted.total/sum(Prediction21)) * Prediction21

# Plot predicted graph
Pred.graph21<-graph_from_adjacency_matrix(Prediction21, weighted = TRUE)
E(Pred.graph21)$width<-0.1*E(Pred.graph21)$weight
V(Pred.graph21)$size <- rep(10, 52)
plot(Pred.graph21, layout=layout_in_circle, edge.arrow.size=0.5, vertex.label=NA)
# Actual graph
plot(yearly.graphs.reg[[21]], layout=layout_in_circle, edge.arrow.size=0.5, vertex.label=NA)

# Which edge is the most different between predicted and actual 
which(Prediction21-yearly.adjs[[21]]==max(Prediction21-yearly.adjs[[21]]), arr.ind=T)
# Ligue Un and La Liga