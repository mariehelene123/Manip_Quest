library(qgraph)
library(glasso)
library(mgm)
library(igraph)
library(bootnet)
library(psych)
library(GPArotation)
library(ggplot2)
library(ggcorrplot)
library(corpcor)
library(parcor)
library(Matrix)



Data <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Analyses/Scale_fornetwork2.csv")
#str(Data)
Data$X = NULL

setwd("C:/Users/lisa/Documents/-These/MANIP_Quest/Analyses/Network")


############################
############################ METHODO CONSTANTINI 
#Exploratory factor analysis
fa.parallel(Data)
fa(r=Data, nfactors=11, rotate="Varimax")

#Exploratory factor analysis
fa.parallel(Data)
fa(r=Data, nfactors=11, rotate="Varimax")

#martrice de corr?lation 

corr_mat0=cor(Data,method = "spearman",use="pairwise.complete.obs")
corr_mat1=cor(Data,method = "spearman",use="na.or.complete")
corr_mat2=cor(Data,method = "spearman",use="complete.obs")

#correlation matrix plot 
ggcorrplot(corr_mat0)
ggcorrplot(corr_mat1)
ggcorrplot(corr_mat2)


# enl?ve des corr?lation 1 dans la diagonale
corr_mat0_diag=corr_mat0-diag(NCOL(Data))
corr_mat1_diag=corr_mat1-diag(NCOL(Data))
corr_mat2_diag=corr_mat2-diag(NCOL(Data))

#correlation network

qgraph(corr_mat0_diag, layout = "spring", labels =
         colnames(Data))
qgraph(corr_mat1_diag, layout = "spring", labels =
         colnames(Data))
qgraph(corr_mat2_diag, layout = "spring", labels =
         colnames(Data))

#partial correlation matrix
pcorr_mat0=cor2pcor(corr_mat0)
pcorr_mat1=cor2pcor(corr_mat1)
pcorr_mat2=cor2pcor(corr_mat2)

#partial correlation matrix plot 
ggcorrplot(pcorr_mat0)
ggcorrplot(pcorr_mat1)
ggcorrplot(pcorr_mat2)

# enl?ve des corr?lation 1 
pcorr_mat0_diag=pcorr_mat0-diag(NCOL(Data))
pcorr_mat1_diag=pcorr_mat1-diag(NCOL(Data))
pcorr_mat2_diag=pcorr_mat2-diag(NCOL(Data))

# Partial correlation networks
qgraph(pcorr_mat0_diag, layout = "spring", labels =
           colnames(Data))
qgraph(pcorr_mat1_diag, layout = "spring", labels =
         colnames(Data))
qgraph(pcorr_mat2_diag, layout = "spring", labels =
         colnames(Data))

#> library(parcor)

set.seed(100)
adls <- adalasso.net(Data)
network <-
  as.matrix(forceSymmetric(adls$pcor.adalasso))
qgraph(network, layout = ??spring??, labels =
         colnames(Data), groups = groups)

#2.1. Descriptive statistics sur pcorr_mat2_diag

ew <- pcorr_mat2_diag[upper.tri(pcorr_mat2_diag)]

nb_ed=sum(ew != 0) # the number of edges
nb_ed_pos=sum(ew > 0) # the number of positive edges
nb_ed_neg=sum(ew < 0) # the number of negative edges

t.test(abs (ew [ew > 0]), abs(ew [ew < 0]), var.equal
       = TRUE)

centrality <- centrality_auto(network)
nc <- centrality$node.centrality
ebc <- centrality$edge.betweenness.centrality

#2.2. Centrality measures