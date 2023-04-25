# This scripts produces fitted Random Graphs for the first 19 seasons 
# Then analyses the 'small world' properties of these graphs 

set.seed(12)
# Run Functions.R file for functions
source("Functions.R")

#Generate fitted p parameter 
p.hat<-rep(0,19)
for(i in 1:19){
  p.hat[i]<- calculate_p(ecount(yearly.graphs[[i]]), vcount(yearly.graphs[[i]]))
}

# Looking at the small world properties 

# Diameter and Clustering Coefficient of the actual graphs (yearly graphs)
cl.yg<- diam.yg<-l.yg<-rep(NA,19)
for (i in 1:19){
  cl.yg[i]<-transitivity(yearly.graphs[[i]])
  diam.yg[i]<-diameter(yearly.graphs[[i]], weights = NA)
  l.yg[i]<- mean_distance(yearly.graphs[[i]])
}

#Generate ntrials of random graphs and take small world properties
cl.rg.lower<-cl.rg.upper<-cl.rg.mean<-rep(NA,19)
diam.rg.lower<-diam.rg.upper<-diam.rg.mean<- l.rg.mean<-rep(NA,19)
ntrials<-1000
# Do for 19 seasons
for (j in 1:19){
  nv<- vcount(yearly.graphs[[j]])
  
  cl.rg<-rep(NA,ntrials)
  diam.rg<-rep(NA,ntrials)
  l.rg<-rep(NA,ntrials)
  
  # Sample random graphs ntrails times
  for (i in 1:ntrials){
    g.rg<- sample_gnp(nv, p.hat[j])
    cl.rg[i]<-transitivity(g.rg)
    diam.rg[i]<-diameter(g.rg, weights = NULL)
    l.rg[i]<- mean_distance(g.rg)
  }
  
  # Take upper, lower bounds and mean
  cl.rg.lower[j]<-summary(cl.rg)[2]
  cl.rg.upper[j]<-summary(cl.rg)[5]
  diam.rg.lower[j]<-summary(diam.rg)[2]
  diam.rg.upper[j]<-summary(diam.rg)[5]
  cl.rg.mean[j]<- mean(cl.rg)
  l.rg.mean[j]<-mean(l.rg)
}

# Plot Clustering Coefficient 
plot(cl.yg,ylim=c(0,0.6),ylab = "Clustering Coefficient", xlab = "Season", main="The Clustering of each Graph compared to the Random Graphs")
points(cl.rg.lower, col=2 ,pch=20)
points(cl.rg.upper, col=2 ,pch=20)
segments(1:19,cl.rg.lower,x1=1:19,y1=cl.rg.upper,col=2)

#Plot Diameters
plot(diam.yg, ylab = "Diameter",xlab = "Season",main="The Diameter of each Graph compared to the Random Graphs")
points(diam.rg.lower, col=2 ,pch=20)
points(diam.rg.upper, col=2 ,pch=20)
segments(1:19,diam.rg.lower,x1=1:19,y1=diam.rg.upper,col=2)