library("foreign")
library("qgraph")
library("bootnet")
library("dplyr")
library("igraph")




#Data <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Analyses/Scale_fornetwork2.csv")
Data <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_fornetwork2.csv")

#str(Data)
Data$X = NULL

setwd("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Network")



###########################
############################
#avec espamk 2018 : Regularized partial correlation network with gLASOO
#library(foreign)
#library(qgraph)
#library(bootnet)

###!!!!! Check help(estimateNetwork) to set the parameters 


##### 3 ESTIMATE & PLOT NETWORKS (Figure 3)



pdf("glasso1_Ep.pdf")
graph1 <- estimateNetwork(
  Data,
  default = "EBICglasso",
  corMethod = "cor_auto",
  tuning = 0)
graph1$graph
plot(graph1)
dev.off()


pdf("glasso2_Ep.pdf")
graph2 <- estimateNetwork(
  Data,
  default = "EBICglasso",
  corMethod = "cor_auto",
  tuning = 0.25)
plot(graph2)
dev.off()

pdf("glasso3_Ep.pdf")
graph3 <- estimateNetwork(
  Data,
  default = "EBICglasso",
  corMethod = "cor_auto",
  tuning = 0.5)
graph3$graph
plot(graph3)
dev.off()


graph4 <- estimateNetwork(
  Data,
  default = "EBICglasso",
  tuning = 0.5)
graph4$graph
plot(graph4)


# Compute average layout:
L <- averageLayout(graph1,graph2, graph3,graph4)

pdf("Fig3_Ep.pdf")
plot(graph1, layout=L, maximum = 0.54, cut = 0) # maximum = 0.54 so it is consistent with later graphs 
plot(graph2, layout=L, maximum = 0.54, cut = 0)
plot(graph3, layout=L, maximum = 0.54, cut = 0)
plot(graph4, layout=L, maximum = 0.54, cut = 0)
dev.off()


wm1 <- getWmat(graph1)
wm2 <- getWmat(graph2)
wm3 <- getWmat(graph3)
wm4 <- getWmat(graph4)


sum(wm1[upper.tri(wm1, diag=F)]!=0)  #447
sum(wm2[upper.tri(wm2, diag=F)]!=0)  #193
sum(wm3[upper.tri(wm3, diag=F)]!=0)  #193
sum(wm4[upper.tri(wm4, diag=F)]!=0)  #436


sum(abs(wm1[upper.tri(wm1, diag=F)])) /2 #13.53912
sum(abs(wm2[upper.tri(wm2, diag=F)])) /2 #7.506301
sum(abs(wm3[upper.tri(wm3, diag=F)])) /2 #7.506301
sum(abs(wm4[upper.tri(wm4, diag=F)])) /2 #12.72398





#for several other network
#estimation methodologies such as the Ising Model and the
#Mixed Graphical Model) is implemented in the bootnet package
#(see Epskamp et al. 2017 Estimating psychological network and their accuracy 

#To compare two networks, one should constrain
#the layout to be equal for both networks. One way to
#do so is by using averageLayout from the qgraph package,


##### 4 CENTRALITY (Figure 4)

pdf("Fig4_Ep.pdf")
centralityPlot(list(EBIC0=graph1,EBIC0.25=graph2,EBIC0.5=graph3,EBIC0.5_pcor=graph4),include="all")
dev.off()
pdf("Fig5_Ep.pdf")
centralityPlot(list(EBIC0.25=graph2))
dev.off()


##centrality 
cent=centrality(results)
cent$OutDegree
cent$InDegree




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

boot1 <- bootnet(Data, nCores = 8,
                 nBoots = 1000, type = "nonparametric")
boot2 <- bootnet(results, nCores = 8,
                 nBoots = 1000, type = "case")
     