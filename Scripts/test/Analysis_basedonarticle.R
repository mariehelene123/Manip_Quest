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
library(Matrix)
library(foreign)

Data <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Analyses/Scale_fornetwork2.csv")
#str(Data)
Data$X = NULL

setwd("C:/Users/lisa/Documents/-These/MANIP_Quest/Analyses/Network")


############################
############################ METHODO CONSTANTINI 
#Exploratory factor analysis
fa.parallel(Data)
fa(r=Data, nfactors=11, rotate="Varimax")

#martrice de corrélation 
corr_mat0=cor(Data,method = "spearman",use="pairwise.complete.obs")
corr_mat1=cor(Data,method = "spearman",use="na.or.complete")
corr_mat2=cor(Data,method = "spearman",use="complete.obs")

#correlation matrix plot 
ggcorrplot(corr_mat0)
ggcorrplot(corr_mat1)
ggcorrplot(corr_mat2)


# enlève des corrélation 1 dans la diagonale
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

# enlève des corrélation 1 
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
qgraph(network, layout = ‘‘spring’’, labels =
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

###########################
############################
#avec espamk 2018 : Regularized partial correlation network with gLASOO
#library(foreign)
#library(qgraph)
#library(bootnet)

###!!!!! Check help(estimateNetwork) to set the parameters 

pdf("glasso1.pdf")
results <- estimateNetwork(
  Data,
  default = "EBICglasso",
  tuning = 0.5)
results$graph
plot(results)
dev.off()
cent=centrality(results)
cent$OutDegree
cent$InDegree
centralityPlot(results)

pdf("glasso2.pdf")
results <- estimateNetwork(
  Data,
  default = "EBICglasso",
  corMethod = "cor_auto",
  tuning = 0.5)
plot(results)
dev.off()

pdf("glasso3(0.25).pdf")
results <- estimateNetwork(
  Data,
  default = "EBICglasso",
  corMethod = "cor_auto",
  tuning = 0.25)
results$graph
plot(results)
dev.off()


results <- estimateNetwork(
  Data,
  default = "EBICglasso",
  corMethod = "cor_auto",
  tuning = 0)
results$graph
plot(results)


#for several other network
#estimation methodologies such as the Ising Model and the
#Mixed Graphical Model) is implemented in the bootnet package
#(see Epskamp et al. 2017 Estimating psychological network and their accuracy 

#To compare two networks, one should constrain
#the layout to be equal for both networks. One way to
#do so is by using averageLayout from the qgraph package,




##############################################
##############################################
#avec methodo Armour 2017



data.cor<-cor_auto(Data) #data including covariates
graph.m <-EBICglasso(data.cor, n = nrow(Data))


pdf("Fig1.pdf")
graph.g<-qgraph(data.cor, graph="glasso", layout="spring", 
                vsize=7, cut=0, maximum=.45, sampleSize = nrow(Data),
                border.width=1.5, border.color="black", minimum=.03)
dev.off()

pdf("Fig2.pdf")
centralityPlot(graph.g)
dev.off()

m1<-bootnet(Data, nBoots=1000, default="EBICglasso", type="nonparametric")
plot(m1, labels = FALSE, order = "sample"
     dev.off()
     
     
     