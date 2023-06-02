################################################################
#####                                                      #####
#####              Armour, Fried et al. 2016               #####
#####      DSM-5 PTSD symptoms network analysis paper      #####
#####                                                      #####
################################################################

library(foreign)
library(qgraph)
library(bootnet)
library(glasso)
library(mgm)  #for mixed model networks 
library(igraph)
library(psych)
library(GPArotation)
library(ggplot2)
library(ggcorrplot)
library(corpcor)
library(Matrix)
library(psych)

setwd("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Network")


##### 1 DATA 
data_full <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_bysub_imputed.csv")

cols_to_keep1<-c("A1_sexe","A2_diff","A3_age_m","A6_IMC", 
                  "C_AP", 
                  "D1_priv","D2_pub", "D3_anx_soc",
                  "E_IRI",  
                  "F2_reseauquant",
                  "H_estime_corp",  
                  "I_objectifi", 
                  "II_press_soc",
                  "K1_notice", "K2_nodistract", "K3_notworry","K4_emotion","K5_listing","K6_trust",
                  "M1_douleurs_nb",
                  "N_influence_pairs", 
                  "sit_eco")

data_selec1<- data_full[,cols_to_keep1]

data_selec1.cor<-cor_auto(data_selec1) 

gr3 <- list(c(1:4), 
           c(5),
           c(6:8),
           c(9),
           c(10),
           c(11),
           c(12),
           c(13),
           c(14:19), 
           c(20),c(21),
           c(22))   


pdf("TOT_Scale_select1_Network.pdf")
graph.g1<-qgraph(data_selec1.cor, graph="glasso", layout="spring", 
                vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_selec1),
                border.width=1.5, border.color="black", minimum=.03, 
                groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                             '#00ffff','#3300ff','#660066','#6666ff',
                                             '#cc00ff','#ffcc00','#ccff00', '#ff8b00'))
dev.off()
                                             

pdf("TOT_Scale_select1_Centrality1.pdf")
centralityPlot(graph.g1,include="All")
dev.off()


pdf("TOT_Scale_select1_Centrality2.pdf")
centralityPlot(graph.g1,include="All",orderBy = "Strength")
dev.off()


pdf("TOT_Scale_select1_Centrality3.pdf")
centralityPlot(graph.g1,include="All",orderBy = "Betweenness")
dev.off()
                                                                                           
                                                                                           
                                                                                           
                                                                                           
                                                                                           