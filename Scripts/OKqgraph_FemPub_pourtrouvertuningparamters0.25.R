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
data_FemPub1_full <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_bysub_imputed_FemPub1.csv")
data_FemPub2_full <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_bysub_imputed_FemPub2.csv")



cols_Subscale1 <-c("A1_sexe", "A2_diff", "A6_IMC", 
                   "B_puberte", 
                   "C_AP", 
                   "D1_priv",   "D2_pub",  "D3_anx_soc",
                   "E1_perspec","E2_empat", 
                   "G1_comp",   "G2_cog",    "G3_affec",  
                   "H1_appa",   "H2_attri",  "H3_poids",  "I_objectifi", 
                   "II1_fam","II2_pairs", "II3_reseau",
                   "N_influence_pairs",
                   "K1_notice", "K2_nodistract","K3_notworry",  "K4_emotion","K5_listing","K6_trust",
                   "M1_douleurs_nb" ,    
                   "sit_eco", 
                   "L2_tact_soc")  
data_FemPub1 <- data_FemPub1_full[, cols_Subscale1]
data_FemPub2 <- data_FemPub2_full[, cols_Subscale1]


names_full <-c("Sexe", "Cisgender", "Body Mass Index", 
                          "Puberty", 
                          "Physical Activity", 
                          "Private Self-Consciousness", "Public Self-Consciousness", "Social Anxiety", 
                          "Perspective Taking", "Empathic Concern", 
                          "Behavioral engagement in Social Media", "Cognitive engagement in Social Media", "Affective engagement in Social Media", 
                          "Body Esteem for Appearance "," Body Esteem Attribution", "Body Esteem for Weight satisfaction", "Body Objectification",
                          "Family Pressure", "Peers Pressure", "Media Pressure", 
                          "Resistance to Peer Influence", 
                          "Noticing body sensations", "Not-Distracting","Not-Worrying", "Emotional Awareness", " Body Listening","Body Trusting", 
                          "Chronic Pain", "Economic Status","Social touch")

names(data_FemPub1) <- names_full
names(data_FemPub2) <- names_full


#str(Data)
data_FemPub1$X = NULL
data_FemPub2$X = NULL

data_FemPub1$Sexe = NULL
data_FemPub2$Sexe = NULL

data_FemPub1$Puberty = NULL
data_FemPub2$Puberty = NULL

data_FemPub1$Cisgender = NULL
data_FemPub2$Cisgender = NULL



### 1.1 Make correlation matrix
data_FemPub1.cor<-cor_auto(data_FemPub1) #data including covariates
ggcorrplot(data_FemPub1.cor)

if (!is.positive.definite(data_FemPub1.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_FemPub1.cor <- nearPD(data_FemPub1.cor)$mat
}

data_FemPub2.cor<-cor_auto(data_FemPub2) #data including covariates
ggcorrplot(data_FemPub2.cor)
if (!is.positive.definite(data_FemPub2.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_FemPub2.cor <- nearPD(data_FemPub2.cor)$mat
}




gr3 <- list(c(1:2), c(3),c(4:6),
            c(7:8),c(9:11), 
            c(12:15), c(16:18),c(19),c(20:25),c(26),c(27),c(28))           


##### 3. Network 1 

### 3.1 Figure 1

shortnames_nodes<-c( "Cis", "BMI", 
                    "PhyAc", 
                    "PriSC", "PubSC", "SocAnx", 
                    "PerTak", "EmpCon", 
                    "BehSM", "CogSM", "AffSM", 
                    "ApBE","AtBE", "WeiBE", "BoObj",
                    "FamPree", "PeePres", "MedPres", 
                    "ResPeer", 
                    "NotiB", "NotDiB","NotWoB", "EmoAwaB", "ListB","TrustB", 
                    "CPain", 
                    "Eco",
                    "SoTou")


pdf("FemPub1_Subscale_Network.pdf")
graph.g1<-qgraph(data_FemPub1.cor, graph="glasso", layout="spring",
                 labels=shortnames_nodes,
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_FemPub1),
                 border.width=0.1, border.color="black", minimum=.03, 
                 groups=gr3, color=c('#a8e6cf',  '#ffd3b6', '#ff8b94',
                                                 '#00ffff','#3300ff','#660066','#6666ff',
                                                 '#ffcc00','#cc00ff','#ccff00','#99ccff','#88ffcc'),title= "Self and adolescence",
                                                 legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1.2,
                 nodeNames=names_full)
dev.off()
                                              

pdf("FemPub2_Subscale_Network.pdf")
graph.g2<-qgraph(data_FemPub2.cor, graph="glasso", layout="spring",
                 labels=shortnames_nodes,
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_FemPub2),
                 border.width=0.1, border.color="black", minimum=.03, 
                 groups=gr3, color=c('#a8e6cf',  '#ffd3b6', '#ff8b94',
                                              '#00ffff','#3300ff','#660066','#6666ff',
                                              '#ffcc00','#cc00ff','#ccff00','#99ccff','#88ffcc'),title= "Self and adolescence",
                                              legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1.2,
                 nodeNames=names_full)
dev.off()
                                              

### 3.2 Centrality 

###FemPub1                                                                                     
pdf("FemPub1_Subscale_Centrality1.pdf")
centralityPlot(graph.g1,include="All")
dev.off()

pdf("FemPub1_Subscale_Centrality2.pdf")
centralityPlot(graph.g1,include="All",orderBy = "Strength")
dev.off()

pdf("FemPub1_Subscale_Centrality3.pdf")
centralityPlot(graph.g1,include="All",orderBy = "Betweenness")
dev.off()

pdf("FemPub1_Subscale_Clustering.pdf")
clusteringPlot(graph.g1)
dev.off()

###FemPub2

pdf("FemPub2_Subscale_Centrality1.pdf")
centralityPlot(graph.g2,include="All")
dev.off()

pdf("FemPub2_Subscale_Centrality2.pdf")
centralityPlot(graph.g2,include="All",orderBy = "Strength")
dev.off()

pdf("FemPub2_Subscale_Centrality3.pdf")
centralityPlot(graph.g2,include="All",orderBy = "Betweenness")
dev.off()

pdf("FemPub2_Subscale_Clustering.pdf")
clusteringPlot(graph.g2)
dev.off()

cent_FemPub1=centralityTable(graph.g1)
cent_FemPub2=centralityTable(graph.g2)


###### Post-hoc  analysis

###############
#################Pub1
EBIC_list<-c(0,0.01,0.02,0.05,0.10,0.15,0.20,0.25,0.5)

list_boot4_FemPub1<-list()
CSbetweeness_FemPub1<-list()
CSstrenght_FemPub1<-list()

list_boot4_FemPub2<-list()
CSbetweeness_FemPub2<-list()
CSstrenght_FemPub2<-list()


i=0
for (param in EBIC_list){
  i=i+1
network_FemPub1 <- estimateNetwork(
  data_FemPub1,
  default = "EBICglasso",
  corMethod = "cor_auto",
  tuning = 0.25,
  refit = TRUE)
#plot(network_FemPub1)

z<- bootnet(network_FemPub1, default = "EBICglasso", nBoots=2500, type = "case", statistics = "betweenness")
list_boot4_FemPub1[[i]]<-z
#plot(boot4_FemPub2[[param]], statistics = "betweenness")
CSbetweeness_FemPub1[i]<-corStability(z)


z<- bootnet(network_FemPub1, default = "EBICglasso", nBoots=1000, type = "case", statistics = "strength")
list_boot4_FemPub1[[i]]<-z
#plot(boot4_FemPub2[[param]], statistics = "betweenness")
CSstrenght_FemPub1[i]<-corStability(z)

network_FemPub2 <- estimateNetwork(
  data_FemPub2,
  default = "EBICglasso",
  corMethod = "cor_auto",
  tuning = param,
  refit = TRUE)

z<- bootnet(network_FemPub2, default = "EBICglasso", nBoots=1000, type = "case", statistics = "betweenness")
list_boot4_FemPub2[[i]]<-z
#plot(boot4_FemPub2[[param]], statistics = "betweenness")
CSbetweeness_FemPub2[i]<-corStability(z)


z<- bootnet(network_FemPub2, default = "EBICglasso", nBoots=1000, type = "case", statistics = "strength")
list_boot4_FemPub2[[i]]<-z
#plot(boot4_FemPub2[[param]], statistics = "betweenness")
CSstrenght_FemPub2[i]<-corStability(z)

}

"""

Coefficient de stabilité (de la betweeness et de la strenght) pour les deux network en faisant  varier les paramètre tunings. 
###première valeure avec des nCase max et min, 2eme valeure sans nCase max et Min 

CSbetweeness_FemPub1 0 = 0.0829 // 0.0497
CSbetweeness_FemPub1 0.01 = 0.0829 // 0.0497
CSbetweeness_FemPub1 0.02 = 0.0829 // 0.0497
CSbetweeness_FemPub1 0.05 = 0.0994 // 0.127 ++++
CSbetweeness_FemPub1 0.1 = 0.127 // 0.0497
CSbetweeness_FemPub1 0.15 =0.0994 // 0.127 ++++
CSbetweeness_FemPub1 0.20 = 0.11 // 0.127 ++++
CSbetweeness_FemPub1 0.25 =0.127// 0.127  ++++
CSbetweeness_FemPub1 0.50 =0.042 // 0

CSbetweeness_FemPub2 0 = 0.283 // 0.283
CSbetweeness_FemPub2 0.01 = 0.283 // 0.283 
CSbetweeness_FemPub2 0.02 = 0.283 // 0.283
CSbetweeness_FemPub2 0.05 = 0.283 // 0.283
CSbetweeness_FemPub2 0.1 = 0.283 // 0.283
CSbetweeness_FemPub2 0.15 =0.283 // 0.283
CSbetweeness_FemPub2 0.20 = 0.283 // 0.283
CSbetweeness_FemPub2 0.25 =0.362 // 0.362 ++++
CSbetweeness_FemPub2 0.50 =0.283 // 0.283

CSstrenght_FemPub1 0 = 0.127 // 0.436
CSstrenght_FemPub1 0.01 = 0.127 // 0.519
CSstrenght_FemPub1 0.02 = 0.127 // 0.519
CSstrenght_FemPub1 0.05 = 0.127 // 0.597
CSstrenght_FemPub1 0.1 = 0.127 // 0.597
CSstrenght_FemPub1 0.15 =0.127 // 0.597
CSstrenght_FemPub1 0.20 = 0.127 // 0.597
CSstrenght_FemPub1 0.25 =0.127 // 0.519 ++++
CSstrenght_FemPub1 0.50 =0.127 // 0.359

CSstrenght_FemPub2 0 = 0.125 // 0.516
CSstrenght_FemPub2 0.01 = 0.125 // 0.516
CSstrenght_FemPub2 0.02 = 0.125 // 0.516
CSstrenght_FemPub2 0.05 = 0.125 // 0.595
CSstrenght_FemPub2 0.1 = 0.125 // 0.595
CSstrenght_FemPub2 0.15 = 0.125 // 0.595
CSstrenght_FemPub2 0.20 = 0.125 // 0.595
CSstrenght_FemPub2 0.25 = 0.125 // 0.595 ++++
CSstrenght_FemPub2 0.50 = 0.125 // 0.437


==> DONC ON FIXE LE paramètre tuning à 0.25 





"""


simRes <- netSimulator(network_FemPub1$graph,
                       dataGenerator = ggmGenerator(
                         ordinal = TRUE, nLevels = 5),
                       default = "EBICglasso",
                       nCases = c(100,200,300,500),
                       tuning = 0.25,
                       nReps =28  )
plot(simRes)
plot(simRes,yvar = c("strength","closeness","betweenness"))


### edge weightaccuracy bootstrap ==> non-parametric (resampling rows from the data

boot1_FemPub1<-bootnet(data_FemPub1, nBoots=2500, default="EBICglasso",  type="nonparametric", statistics='edge')
summary(boot1_FemPub1)
pdf("boot1_FemPub1", useDingbats=FALSE) 
plot(boot1_FemPub1, labels = FALSE, order = "sample")
dev.off()

### 4.2 node dropping bootstrap
boot2_FemPub1<- bootnet(network_FemPub1, default = "EBICglasso", nBoots=2500, type = "node",computeCentrality=TRUE, statistics = "betweenness")
pdf("boot2_FemPub1.pdf", useDingbats=FALSE) 
plot(boot2_FemPub1, statistics = "betweenness")
plot(boot2_FemPub1, statistics = "betweenness", perNode = TRUE)
dev.off()


boot3_FemPub1<- bootnet(network_FemPub1, default = "EBICglasso", nBoots=2500, type = "case", statistics = "betweenness")
plot(boot3_FemPub1, statistics = "betweenness")
corStability(boot3_FemPub1)


boot4_FemPub1<- bootnet(network_FemPub1, default = "EBICglasso", nBoots=2500, type = "case", statistics = "strength")
plot(boot4_FemPub1, statistics = "strength")
corStability(boot4_FemPub1)


###############
#################Pub2


EBIC_list<-c(0.01,0.02,0.05,0.10,0.20,0.25,0.5)

list_boot4_FemPub2<-list()
CSbetweeness_FemPub2<-list()

i=4
for (param in EBIC_list){
i=i+1
network_FemPub2 <- estimateNetwork(
  data_FemPub2,
  default = "EBICglasso",
  corMethod = "cor_auto",
  tuning = param,
  refit = TRUE)

z<- bootnet(network_FemPub2, default = "EBICglasso", nBoots=2500, type = "case", statistics = "betweenness")
list_boot4_FemPub2[[i]]<-z
#plot(boot4_FemPub2[[param]], statistics = "betweenness")
CSbetweeness_FemPub2[i]<-corStability(z)
}

"""
CSEBIC 0.01 = 0.204
CSEBIC 0.02 = 0.283
CSEBIC 0.05 = 0.283
CSEBIC 0.1 = 0.283
CSEBIC 0.5= 0.283
"""





simRes <- netSimulator(network_FemPub2$graph,
                       dataGenerator = ggmGenerator(
                         ordinal = TRUE, nLevels = 5),
                       default = "EBICglasso",
                       nCases = c(100,200,300,500),
                       tuning = 0.5,
                       nReps =28  )

plot(simRes)
plot(simRes,yvar = c("strength","closeness","betweenness"))


### edge weightaccuracy bootstrap ==> non-parametric (resampling rows from the data

boot1_FemPub2<-bootnet(data_FemPub2, nBoots=2500, default="EBICglasso",  type="nonparametric", statistics='edge')
summary(boot1_FemPub2)
pdf("boot1_FemPub2", useDingbats=FALSE) 
plot(boot1_FemPub2, labels = FALSE, order = "sample")
dev.off()

### 4.2 node dropping bootstrap
boot2_FemPub2<- bootnet(network_FemPub2, default = "EBICglasso", nBoots=2500, type = "node",computeCentrality=TRUE, statistics = "betweenness")
pdf("boot2_FemPub2.pdf", useDingbats=FALSE) 
plot(boot2_FemPub2, statistics = "betweenness")
plot(boot2_FemPub2, statistics = "betweenness", perNode = TRUE)
dev.off()

boot3_FemPub2<- bootnet(network_FemPub2, default = "EBICglasso", nBoots=2500, type = "node",computeCentrality=TRUE, statistics = "strength")
pdf("boot3_FemPub2", useDingbats=FALSE) 
plot(boot3_FemPub2, statistics = "strength")
plot(boot3_FemPub2, statistics = "strength", perNode = TRUE)
dev.off()


boot4_FemPub2<- bootnet(network_FemPub2, default = "EBICglasso", nBoots=2500, type = "case", statistics = "betweenness")
plot(boot4_FemPub2, statistics = "betweenness")
corStability(boot4_FemPub2)


boot5_FemPub2<- bootnet(network_FemPub2, default = "EBICglasso", nBoots=2500, type = "case", statistics = "strength")
plot(boot5_FemPub2, statistics = "strength")
corStability(boot5_FemPub2)






















##########################
########################      NetworkComparisonTest: 
##########################
##########################




library("IsingSampler")
library("IsingFit")
library("NetworkComparisonTest" )

NCTFemPub1vsFemPub2<-NCT(data_FemPub1, data_FemPub2, 
                   it = 1000, # The number of iterations (permutations).
                   binary.data=FALSE, 
                   paired=FALSE, 
                   weighted=TRUE, 
                   abs=TRUE,
                   test.edges=TRUE, 
                   edges="all", 
                   progressbar=TRUE, 
                   make.positive.definite=TRUE,
                   p.adjust.methods= c("none"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                   test.centrality=TRUE, 
                   centrality=c("betweenness"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                   nodes="all",
                   communities=gr3,
                   useCommunities="all",
                   #estimator,
                   #estimatorArgs = list(), 
                   verbose = TRUE)
summary(NCTFemPub1vsFemPub2)


"""
 NETWORK INVARIANCE TEST
      Test statistic M:  0.2189397 
 p-value 0.104 

 GLOBAL STRENGTH INVARIANCE TEST
      Global strength per group:  4.587907 8.055244 
 Test statistic S:  3.467337 
 p-value 0.074
 
 
 """



NCTFemPub1vsFemPub2$glstrinv.pval ## pval The p value resulting from the permutation test concerning difference in global strength.

NCTFemPub1vsFemPub2$nwinv.pval  ##The p value resulting from the permutation test concerning the maximum difference in edge weights.
##network invariance test 

NCTFemPub1vsFemPub2$einv.pvals	### p-values (corrected for multiple testing or not according to ’p.adjust.methods’) per edge from the permutation test concerning differences in edges weights. Only returned if test.edges = TRUE.


NCTFemPub1vsFemPub2$einv.pvals$index <- seq_len(nrow(NCTFemPub1vsFemPub2$einv.pvals)) 

NCTFemPub1vsFemPub2$diffcen.pval

NCTFemPub1vsFemPub2$diffcen.real



"000000000000000000000000000000000000000000000000000000000000000000000000000000A""Bonferroni 

2_diff                 1.000
A6_IMC                  1.000
C_AP                    1.000
D1_priv                 0.558
D2_pub                  0.746
D3_anx_soc              0.652
E1_perspec              0.190
E2_empat                1.000
G1_comp                 0.193
G2_cog                  0.907
G3_affec                0.855
H1_appa                 0.074
H2_attri                1.000
H3_poids                0.872
I_objectifi             0.493
II1_fam                 0.671
II2_pairs               1.000
II3_reseau              0.472
J_cons_corps            0.899
K1_notice               0.341
K2_nodistract           1.000
K3_notworry             0.068
K4_emotion              0.782
K5_listing              0.019
K6_trust                0.004
M1_douleurs_nb          0.029
N_influence_pairs       0.818
sit_eco                 0.020