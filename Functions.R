#Script with utility functions in 

library(igraph)
library(ggplot2)
library(graphics)
library(blockmodels)

set.seed(12)

#Change a weighted adj matrix to an unweighted adj matrix 
unweighted<- function(adj.matrix,n){
  for (i in 1:n) {
    for (j in i:n) {
      if (adj.matrix[i,j]>0|adj.matrix[j,i]>0){
        adj.matrix[i,j]=1
        adj.matrix[j,i]=1
      }
      else{
        adj.matrix[i,j]=0
        adj.matrix[j,i]=0
      }
    }
  }
  return(adj.matrix)
}

#Make an undirected adjacency matrix symmetric 
make_adj_sym<- function(adj.matrix,n){
  for(i in 1:n){
    for(j in 1:n){
      if(adj.matrix[i,j]>0 && adj.matrix[j,i]==0){
        adj.matrix[j,i]=adj.matrix[i,j]
      }
    }
  }
  return(adj.matrix)
}

#Generate a suitable parameter for random graphs
calculate_p<- function(M,n){
  N <- (n*(n-1))/2
  return(M/N)
}

#Generated a weighted random graph of size n with probability p 
WRG <- function(n,p){
  A<-matrix(0L,nrow=n,ncol=n)
  for(i in 1:n-1){
    for(j in (i+1):n){
      A[i,j]<-rgeom(1,1-p)
    }
  }
  return(A)
}

#Small World propensity of a graph 
SWPropensity <- function(cl_OBS, cl_LAT, cl_RG, L_OBS, L_LAT, L_RG){
  delta_C<- (cl_LAT-cl_OBS)/(cl_LAT-cl_RG)
  delta_D<- (L_OBS-L_RG)/ (L_LAT-L_RG)
  if (delta_D<0){
    delta_D=0
  }
  if(delta_C<0){
    delta_C=0
  }
  if(delta_D>1){
    delta_D=1
  }
  if(delta_C>1){
    delta_C=1 
  }
  phi <- 1 - ((delta_D^2+delta_C^2)/2)^0.5
  return(phi)
}


# Generate directed Watts Strogratz 
Generate_DirectedSW<- function(Size, p){
  adj.matrix<- diag(0, nrow= Size, ncol=Size)
  adj.matrix[row(adj.matrix)-col(adj.matrix)==1]<- adj.matrix[row(adj.matrix)-col(adj.matrix)==-1]<- 1
  adj.matrix[row(adj.matrix)-col(adj.matrix)==2]<- 1 #matches neighbourhood size of 3
  adj.matrix[Size,1]<- 1 #makes circular
  adj.matrix[1,Size]<- 1
  graph<- graph_from_adjacency_matrix(adj.matrix, mode=c("directed"))
  rewired_graph<- rewire(graph,with = each_edge(p))
  return(rewired_graph)
} 

