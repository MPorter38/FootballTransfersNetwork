# Fit stochastic block models to the data for the first season 
# Then hypothesis test if Portugese league has unusual betweeness

# Take adjacency matrix and make symmetrical (ie make graph undirected)
nv <- vcount(yearly.graphs[[19]])
A.19 <- as.matrix( as_adjacency_matrix(yearly.graphs[[19]], type="both") )
A.19 <- make_adj_sym(A.19, nv) 
# Fit stochastic block model (sbm)
sbm.19 <- BM_bernoulli("SBM_sym", A.19, 
                       verbosity=0, plotting='')
sbm.19$estimate() 

# How many classes should we chose using the integrated conditional likelihood 
ICL <- sbm.19$ICL
Q <- which.max(ICL)
# This suggests two classes and we can see this in a plot
plot(sbm.19$ICL,xlab="Q",ylab="ICL",type="b", main = "The ICL for various Q")
lines(c(Q,Q),c(min(ICL),max(ICL)),col="red",lty=2)

#Which class is each vertex in? In the form of probability 
Z <- sbm.19$memberships[[Q]]$Z

# Label classes based on the class that each vertex has the highest probability
# of being in
cl.labs <- apply(Z,1,which.max)
summary(Z[cbind(1:nv,cl.labs)])
# Summary shows that all vertex has a high probability of being in a particular class

# What percent of vertices are in each class?
cl.cnts <- as.vector(table(cl.labs))
alpha <- cl.cnts/nv
alpha
# Probability of edge appearing between/within each class  
Pi.mat <- sbm.19$model_parameters[[Q]]$pi
Pi.mat

# Plot the leagues that are in the second class
plot(yearly.graphs[[19]], layout=layout_in_circle, vertex.color= apply(Z,1,which.max), edge.arrow.size=0.5)
# Approximately seems to be the top European leagues in one class and the rest in the other class

# As a goodness of fit measure look at how the degree of the sbm model
# compares to the actual network 
ntrials <- 1000
Pi.mat <- (t(Pi.mat)+Pi.mat)/2
deg.summ <- list(ntrials)
for(i in (1:ntrials)){
  blk.sz <- rmultinom(1,nv,alpha)
  g.sbm <- sample_sbm(nv,pref.matrix=Pi.mat,
                      block.sizes=blk.sz,
                      directed=FALSE)
  deg.summ[[i]] <- summary(degree(g.sbm))
}
Reduce('+',deg.summ)/ntrials

summary(degree(yearly.graphs[[19]]))
# Median and mean are comparable but the 3rd Qu and max are notably different

# Hypothesis Testing 
# Despite having high betweeness in the actual network (the second most) Liga Nos
# (Portugal_ is in the "other" class for the fitted sbm model. How often does the sbm model
# produce a graph where one of the top two most "between" vertices is in this 
# "Other" class (called class one)

# Looking at radial plot of betweeness centrality for actual network 
bet<-as.vector(betweenness(yearly.graphs[[19]]))
library(network)
g <- network::as.network.matrix(as.matrix(as_adjacency_matrix(yearly.graphs[[19]])), directed=TRUE)
library(sna)
sna::gplot.target(g, bet, 
                  main="Betweeness for 2018-19 Season", circ.lab = FALSE, 
                  circ.col="skyblue", usearrows = FALSE,
                  edge.col="darkgray")
detach("package:sna")
detach("package:network")

# Test hypothesis by Monte Carlo Method
ntrials <- 10000
Pi.mat <- (t(Pi.mat)+Pi.mat)/2
results.vec<- rep(0,ntrials)
for(i in (1:ntrials)){
  # sample block size from normal based % of vertices in each class
  blk.sz <- rmultinom(1,nv,alpha)
  # sample graphs from sbm model
  g.sbm <- sample_sbm(nv,pref.matrix=Pi.mat,
                      block.sizes=blk.sz,
                      directed=FALSE)
  B <- betweenness(g.sbm)
  # take index of two most between vertices
  first <- min(which.max(B)) # min in case joints firsts
  second <- min(which(B==sort(B, decreasing = TRUE)[2]))
  # check if in first block/class
  if(first<(blk.sz[1]+1) | second<(blk.sz[1]+1)){
    results.vec[i] <- 1 
  }
}
sum(results.vec)/length(results.vec) * 100
# Very rare for vertex in first class to be one of the top two most between vertices
