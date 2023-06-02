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

setwd("../Analyses/Network")


##### 1 DATA 
data_MalPub1 <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_MalPub1_fornetwork2.csv")
data_MalPub2 <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_MalPub2_fornetwork2.csv")

#str(Data)
data_MalPub1$X = NULL
data_MalPub2$X = NULL

data_MalPub1$A1_sexe = NULL
data_MalPub2$A1_sexe = NULL

data_MalPub1$A3_age_m = NULL
data_MalPub2$A3_age_m = NULL




### 1.1 Make correlation matrix
data_MalPub1.cor<-cor_auto(data_MalPub1) #data including covariates
ggcorrplot(data_MalPub1.cor)

if (!is.positive.definite(data_MalPub1.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_MalPub1.cor <- nearPD(data_MalPub1.cor)$mat
}

data_MalPub2.cor<-cor_auto(data_MalPub2) #data including covariates
ggcorrplot(data_MalPub2.cor)
if (!is.positive.definite(data_MalPub2.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_MalPub2.cor <- nearPD(data_MalPub2.cor)$mat
}

gp_A<-c("A1_sexe","A2_diff","A6_IMC")
gp_C<-c("C_AP")
gp_D<-c("D1_priv","D2_pub","D3_anx_soc")
gp_E<-c("E1_perspec","E2_empat")
gp_F<-c("G1_comp","G2_cog","G3_affec")
gp_HI<-c("H1_appa","H2_attri","H3_poids","I_objectifi")
gp_II<-c("II1_fam","II2_pairs","II3_reseau")
gp_JK<-c("J_cons_corps","K1_notice","K2_nodistract","K3_notworry","K4_emotion","K5_listing","K6_trust")
gp_L<-c("M1_douleurs_nb")
gp_M<-c("N_influence_pairs")
gp_N<-c("sit_eco")

gr3<-list(gp_A,gp_C,gp_D,gp_E,gp_F,gp_HI,gp_II,gp_JK,gp_L,gp_M,gp_N)

gr3 <- list(c(1:3), c(4),c(5:7),
            c(8:9),c(10:12), 
            c(13:16), c(17:19),c(20:26),c(27),c(28),c(29))           


##### 3. Network 1 

### 3.1 Figure 1

pdf("MalPub1_Subscale_Network.pdf")
graph.g1<-qgraph(data_MalPub1.cor, graph="glasso", layout="spring", 
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_MalPub1),
                 border.width=1.5, border.color="black", minimum=.03, 
                 groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                              '#00ffff','#3300ff','#660066','#6666ff',
                                              '#cc00ff','#ffcc00','#ccff00'))
dev.off()
                                              

pdf("MalPub2_Subscale_Network.pdf")
graph.g2<-qgraph(data_MalPub2.cor, graph="glasso", layout="spring", 
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_MalPub2),
                 border.width=1.5, border.color="black", minimum=.03, 
                 groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                              '#00ffff','#3300ff','#660066','#6666ff',
                                              '#cc00ff','#ffcc00','#ccff00'))
dev.off()
                                              

### 3.2 Centrality 

###MalPub1                                                                                     
pdf("MalPub1_Subscale_Centrality1.pdf")
centralityPlot(graph.g1,include="All")
dev.off()

pdf("MalPub1_Subscale_Centrality2.pdf")
centralityPlot(graph.g1,include="All",orderBy = "Strength")
dev.off()

pdf("MalPub1_Subscale_Centrality3.pdf")
centralityPlot(graph.g1,include="All",orderBy = "Betweenness")
dev.off()

pdf("MalPub1_Subscale_Clustering.pdf")
clusteringPlot(graph.g1)
dev.off()

###MalPub2

pdf("MalPub2_Subscale_Centrality1.pdf")
centralityPlot(graph.g2,include="All")
dev.off()

pdf("MalPub2_Subscale_Centrality2.pdf")
centralityPlot(graph.g2,include="All",orderBy = "Strength")
dev.off()

pdf("MalPub2_Subscale_Centrality3.pdf")
centralityPlot(graph.g2,include="All",orderBy = "Betweenness")
dev.off()

pdf("MalPub2_Subscale_Clustering.pdf")
clusteringPlot(graph.g2)
dev.off()

##########################
########################      NetworkComparisonTest: 
##########################
##########################


library("IsingSampler")
library("IsingFit")
library("NetworkComparisonTest" )

NCTMalPub1vsMalPub2<-NCT(data_MalPub1, data_MalPub2, 
                   it = 100, # The number of iterations (permutations).
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
summary(NCTMalPub1vsMalPub2)

NCTMalPub1vsMalPub2$glstrinv.pval ## pval The p value resulting from the permutation test concerning difference in global strength.

NCTMalPub1vsMalPub2$nwinv.pval  ##The p value resulting from the permutation test concerning the maximum difference in edge weights.
##network invariance test 

NCTMalPub1vsMalPub2$einv.pvals	### p-values (corrected for multiple testing or not according to ’p.adjust.methods’) per edge from the permutation test concerning differences in edges weights. Only returned if test.edges = TRUE.


NCTMalPub1vsMalPub2$einv.pvals$index <- seq_len(nrow(NCTMalPub1vsMalPub2$einv.pvals)) 

NCTMalPub1vsMalPub2$diffcen.pval
A2_diff                  1.00
A6_IMC                   1.00
C_AP                     1.00
D1_priv                  0.63
D2_pub                   0.78
D3_anx_soc               0.57
E1_perspec               0.17
E2_empat                 1.00
G1_comp                  0.19
G2_cog                   0.96
G3_affec                 0.83
H1_appa                  0.08
H2_attri                 1.00
H3_poids                 0.88
I_objectifi              0.56
II1_fam                  0.59
II2_pairs                1.00
II3_reseau               0.47
J_cons_corps             0.92
K1_notice                0.31
K2_nodistract            1.00
K3_notworry              0.05
K4_emotion               0.79
K5_listing               0.02
K6_trust                 0.01
M1_douleurs_nb           0.01
N_influence_pairs        0.79
sit_eco                  0.04