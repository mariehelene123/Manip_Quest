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
     
     
     