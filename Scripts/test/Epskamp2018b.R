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

##centrality 
cent=centrality(results)
cent$OutDegree
cent$InDegree
centralityPlot(results)


### A priori sample size analysis 

network <- estimateNetwork(
  Data,
  default = "EBICglasso",
  corMethod = "cor_auto",
  tuning = 0.5,
  refit = TRUE)

simRes <- netSimulator(network$graph,
                         dataGenerator = ggmGenerator(
                           ordinal = TRUE, nLevels = 5),
                         default = "EBICglasso",
                         nCases = c(100,250,500,1000,2500),
                         tuning = 0.5,
                         nReps =28  )
# the dataGenerator argument
#specifies the data generating process (can be ignored for nonordinaldata), 
#nCases encodes the sample size conditions,
#nReps the number of repetitions per condition, and 
#nCores the number of computer cores to use. 

simRes
plot(simRes)
plot(simRes,
     yvar = c("strength","closeness","betweenness"))


###### Post-hoc stability analysis

boot1 <- bootnet(results, nCores = 8,
                 nBoots = 1000, type = "nonparametric")
boot2 <- bootnet(results, nCores = 8,
                 nBoots = 1000, type = "case")
     