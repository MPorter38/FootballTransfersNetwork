# An Analysis of Networks of Football Transfers
In this repository, we analyse a group of networks based on men's association football transfers. The collection consists of twenty one graphs, one for every season starting with the 2000-01 season and ending with the 2020-21 season. 

In our graph for each network, the vertices represent leagues, specifically leagues that are the highest level of professional men's football in their country. The edges then represent transfers that have taken place between clubs from these leagues. In particular, transfers that rank in the top 200 most expensive for the given season. 

These edges are weighted by the value of fee for the transfer that they represent. In order to avoid creating multi graphs in cases where there could be multiple edges, we have combine these into a single edge weighted by the sum of all the constituent transfer fees. Also, we have not allowed loops in the networks, which would be transfers that start and end in the same league.   

In these scripts, we first do some exploratory analysis, before fitting a Erdos-Renyi Random Graph to our data and then a Watts-Strogatz Graph. 

- Stochastic Block Models, Portgual betweeness, & Premier League Degree
- Linear Models for prediction
- Purpose of this 
