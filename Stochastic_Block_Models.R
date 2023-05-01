# Fit stochastic block models to the data then hypothesis test if the Premier League
# has a unexpectedly high degree centrality

# Demonstrated method first on in Stochastic_Block_Models_19.R
# for just 19th season's data 

# Degree Centrality of the leagues that are in each graph
# Which leagues are in every graph?
V.list<- list()
Leagues.mat <- rep(0,52)
for(i in 1:19){
  V.list[[i]]<- labels(V(yearly.graphs[[i]]))
  Leagues.mat <- Leagues.mat + (league_names %in% V.list[[i]])
}
Leagues.mat
topLeagues<- league_names[which(Leagues.mat==19)]
# What is the degree centrality of these leagues? 
deg<- matrix(nrow=19, ncol=length(topLeagues))
colnames(deg)<- topLeagues
rownames(deg)<- rep("",19)
for (i in 1:19){
  tdeg<- degree(yearly.graphs[[i]])
  for (j in 1:length(topLeagues)){
    deg[i,j]<- tdeg[topLeagues[j]]
  }
}
colours <- c(0:8,"purple")
plot(1:19,deg[,1], type="l", ylim=c(0,21), lty=2, ylab = "Degree Centrality", xlab = "Season", main = "Degree Centrality of Selected Leagues", bty="L")
for (i in 2:length(topLeagues)){
  lines(1:19, deg[,i], col=colours[i])
}
#Legend 
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", topLeagues, lty=c(2,rep(1,10)), col=c(1,colours[2:10]))

# Hypothesis Testing if Premier league has higher degree than expected
# Where "Expected" refers to the behaviour of comparable SBM graphs
ntrials <- 10000
seasonlydeg<- ldeg<-udeg<- overprem<-rep(0,19)
for(i in 1:19){
  A <- as.matrix( as_adjacency_matrix(yearly.graphs[[i]], type="both") )
  A <- make_adj_sym(A, vcount(yearly.graphs[[i]])) 
  sbm <- BM_bernoulli("SBM_sym", A, 
                      verbosity=0, plotting='')
  sbm$estimate() 
  
  Z <- sbm$memberships[[2]]$Z
  cl.labs <- apply(Z,1,which.max)
  cl.cnts <- as.vector(table(cl.labs))
  
  nv <- vcount(yearly.graphs[[i]])
  alpha <- cl.cnts/nv
  Pi.mat <- sbm$model_parameters[[2]]$pi
  Pi.mat <- (t(Pi.mat)+Pi.mat)/2
  
  maxdeg<- rep(0,ntrials)
  for(j in (1:ntrials)){
    blk.sz <- rmultinom(1,nv,alpha)
    g.sbm <- sample_sbm(nv,pref.matrix=Pi.mat,
                        block.sizes=blk.sz,
                        directed=FALSE)
    maxdeg[j] <- max(degree(g.sbm)) 
  }
  overprem[i]<- length(which(maxdeg>deg[i]))
  seasonlydeg[i]<- mean(maxdeg)
  ldeg[i]<- min(maxdeg)
  udeg[i]<- max(maxdeg)
}

# Plot Premier League's degree and average, upper bound, lower bound of predicted maximum degree
plot(1:19,deg[,"Premier League"], ylim=c(0,22), type="l", xlab = "Season", ylab = "Maximum Degree", main = "Predicted Maximum Degree and the Premier League's Degree")
lines(1:19,seasonlydeg, col=2)
lines(1:19, ldeg, col=3, lty=2)
lines(1:19, udeg, col=4, lty=2)
# Legend 
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", c("Premier League","Mean","Minimum","Maximum"), lty=c(1,1,2,2), col=c(1,2,3,4))
# Premier League's degree measure seems to be higher than in most of the SBM graphs, most of the time
# Showing the league's importance in the international transfer market