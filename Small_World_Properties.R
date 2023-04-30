# This script looks at the small world properties of our season by season graphs 
# We then fit a Watts-Strogatz model to the data

# Diameter and clustering coefficient (small world properties) 
# of undirected season graphs
cl.yg<-rep(NA,19)
diam.yg<-rep(NA,19)
deg.yg<-rep(NA,19)
for (i in 1:19){
  cl.yg[i]<-transitivity(yearly.graphs.undirected[[i]])
  diam.yg[i]<-diameter(yearly.graphs.undirected[[i]])
  deg.yg[i]<-mean(degree(yearly.graphs.undirected[[i]]))
}

# Same metrics for comparable random graphs 
cl.rg.lower<-cl.rg.upper<-rep(NA,19)
diam.rg.lower<-diam.rg.upper<-rep(NA,19)
ntrials<-100
for (j in 1:19){
  nv<- vcount(yearly.graphs.undirected[[j]])
  ne<- ecount(yearly.graphs.undirected[[j]])
  cl.rg<-rep(NA,ntrials)
  diam.rg<-rep(NA,ntrials)
  for (i in 1:ntrials){
    g.rg<- sample_gnm(nv, ne, directed = TRUE)
    cl.rg[i]<-transitivity(g.rg)
    diam.rg[i]<-diameter(g.rg)
  }
  cl.rg.lower[j]<-summary(cl.rg)[2]
  cl.rg.upper[j]<-summary(cl.rg)[5]
  diam.rg.lower[j]<-summary(diam.rg)[2]
  diam.rg.upper[j]<-summary(diam.rg)[5]
}

plot(cl.yg,ylim=c(0,max(cl.yg,cl.rg.upper)),ylab = "Clustering Coefficient", xlab = "Season", main="How Clustered are the Networks compared to Random Graphs?")
segments(1:19,cl.rg.lower,x1=1:19,y1=cl.rg.upper,col=2)
# Error in plot.new() : figure margins too large 
plot(diam.yg,ylim=c(0,max(diam.yg,diam.rg.upper)), ylab = "Diameter",xlab = "Season",main="How does the Diameter of the Networks compare to Random Graphs?")
segments(1:19,diam.rg.lower,x1=1:19,y1=diam.rg.upper,col=2)

# Sampling small world models with different p values 
# in order to fit a small world model to our actual graphs 
p<-c(seq(-5,0,length.out = 20)) 
p<-exp(p)
diam.sw<-cl.sw<-rep(0,length(p))
for (i in 1:length(p)){
  sw<-sample_smallworld(1,27,3,p[i])
  diam.sw[i]<-diameter(sw)
  cl.sw[i]<-transitivity(sw)
}

# Plotting comparisons between mean diameter and clustering coefficient of 
# actual graphs and small world graphs 
plot(log(p),diam.sw, ylim=c(3,8))
abline(mean(diam.yg),0)

plot(log(p),cl.sw)
abline(mean(cl.yg),0)

# Based off the clustering coefficent, we can refine the p value 
p.finer<-c(seq(log(p[10]),log(p[12]),length.out = 20)) 
p.finer<-exp(p.finer)
cl.sw.finer<-rep(0,length(p.finer))
for (i in 1:length(p.finer)){
  sw<-sample_smallworld(1,27,3,p.finer[i])
  cl.sw.finer[i]<-transitivity(sw)
}

plot(log(p.finer),cl.sw.finer)
abline(mean(cl.yg),0)

# Refine even further (make more precise)
p.vfiner<-c(seq(-2.4,-2.3,length.out = 20)) 
p.vfiner<-exp(p.vfiner)
cl.sw.vfiner<-rep(0,length(p.vfiner))
for (i in 1:length(p.vfiner)){
  sw<-sample_smallworld(1,27,3,p.vfiner[i])
  cl.sw.vfiner[i]<-transitivity(sw)
}

plot(log(p.vfiner),cl.sw.vfiner)
abline(mean(cl.yg),0)

# look at an example small world graph with this p 
sample_sw_cc <- transitivity(sample_smallworld(1,27,3,exp(log(p.vfiner[13]))))
plot(sample_smallworld(1,27,3,exp(log(p.vfiner[13]))), layout=layout_in_circle)