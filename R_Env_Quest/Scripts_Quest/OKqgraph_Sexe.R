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


data_Fem <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_Fem_fornetwork2.csv")
data_Mal <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_Mal_fornetwork2.csv")


data_FemPub1 <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_FemPub1_fornetwork2.csv")
data_MalPub1 <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_MalPub1_fornetwork2.csv")


#str(Data)
data_Fem$X = NULL
data_Mal$X= NULL

data_Fem$A1_sexe = NULL
data_Mal$A1_sexe= NULL

data_FemPub1$X = NULL
data_MalPub1$X = NULL

data_FemPub1$A1_sexe = NULL
data_MalPub1$A1_sexe = NULL

data_FemPub1$A3_age_m = NULL
data_MalPub1$A3_age_m = NULL


### 1.1 Make correlation matrix

data_Fem.cor<-cor_auto(data_Fem) #data including covariates
ggcorrplot(data_Fem.cor)

if (!is.positive.definite(data_Fem.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_Fem.cor <- nearPD(data_Fem.cor)$mat
}

data_Mal.cor<-cor_auto(data_Mal) #data including covariates
ggcorrplot(data_Mal.cor)
if (!is.positive.definite(data_Mal.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_Mal.cor <- nearPD(data_Mal.cor)$mat
}



data_FemPub1.cor<-cor_auto(data_FemPub1) #data including covariates
ggcorrplot(data_FemPub1.cor)

if (!is.positive.definite(data_FemPub1.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_FemPub1.cor <- nearPD(data_FemPub1.cor)$mat
}

data_MalPub1.cor<-cor_auto(data_MalPub1) #data including covariates
ggcorrplot(data_MalPub1.cor)
if (!is.positive.definite(data_MalPub1.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_MalPub1.cor <- nearPD(data_MalPub1.cor)$mat
}



gp_A<-c("A3_age_m","A2_diff","A6_IMC")
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

pdf("Fem_Subscale_Network.pdf")
graph.fem<-qgraph(data_Fem.cor, graph="glasso", layout="spring", 
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Fem),
                 border.width=1.5, border.color="black", minimum=.03, 
                 groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                              '#00ffff','#3300ff','#660066','#6666ff',
                                              '#cc00ff','#ffcc00','#ccff00'))
dev.off()
                                              

pdf("Mal_Subscale_Network.pdf")
graph.mal<-qgraph(data_Mal.cor, graph="glasso", layout="spring", 
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Mal),
                 border.width=1.5, border.color="black", minimum=.03, 
                 groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                              '#00ffff','#3300ff','#660066','#6666ff',
                                              '#cc00ff','#ffcc00','#ccff00'))
dev.off()


### 3.2 Centrality 

###Fem                                                                                     
pdf("Fem_Subscale_Centrality1.pdf")
centralityPlot(graph.fem,include="All")
dev.off()

pdf("Fem_Subscale_Centrality2.pdf")
centralityPlot(graph.fem,include="All",orderBy = "Strength")
dev.off()

pdf("Fem_Subscale_Centrality3.pdf")
centralityPlot(graph.fem,include="All",orderBy = "Betweenness")
dev.off()

pdf("Fem_Subscale_Clustering.pdf")
clusteringPlot(graph.fem)
dev.off()

###Mal
pdf("Mal_Subscale_Centrality1.pdf")
centralityPlot(graph.mal,include="All")
dev.off()

pdf("Mal_Subscale_Centrality2.pdf")
centralityPlot(graph.mal,include="All",orderBy = "Strength")
dev.off()

pdf("Mal_Subscale_Centrality3.pdf")
centralityPlot(graph.mal,include="All",orderBy = "Betweenness")
dev.off()

pdf("Mal_Subscale_Clustering.pdf")
clusteringPlot(graph.mal)
dev.off()



##########################
########################      NetworkComparisonTest: 
##########################
##########################


library("IsingSampler")
library("IsingFit")
library("NetworkComparisonTest" )

NCTFemvsMal<-NCT(data_Fem, data_Mal, 
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
summary(NCTFemvsMal)

NCTFemvsMal$glstrinv.pval
NCTFemvsMal$einv.pvals	
NCTFemvsMal$einv.pvals$index <- seq_len(nrow(NCTFemvsMal$einv.pvals)) 

filtered_rows <- NCTFemvsMal$einv.pvals[NCTFemvsMal$einv.pvals$`p-value` < 0.05, ]

filtered_rows

"""
119      A6_IMC           D1_priv    0.00     9
205    A3_age_m        E1_perspec    0.00    23
207        C_AP        E1_perspec    0.03    25
323        C_AP          G3_affec    0.04    59
327  E1_perspec          G3_affec    0.03    63
381        C_AP          H2_attri    0.04    82
443  E1_perspec       I_objectifi    0.00   113
449    H2_attri       I_objectifi    0.04   119
467      A6_IMC           II1_fam    0.02   123
610     A2_diff     K2_nodistract    0.00   211
650    G3_affec       K3_notworry    0.02   243
681    H2_attri        K4_emotion    0.03   267
686  II3_reseau        K4_emotion    0.02   272
717   K1_notice        K5_listing    0.02   297
738     H1_appa          K6_trust    0.03   313
749  K4_emotion          K6_trust    0.01   324
769    H3_poids    M1_douleurs_nb    0.01   340
780    K6_trust    M1_douleurs_nb    0.03   351
794      G2_cog N_influence_pairs    0.04   362
799 I_objectifi N_influence_pairs    0.00   367
804   K1_notice N_influence_pairs    0.03   372
809    K6_trust N_influence_pairs    0.04   377
827    H3_poids           sit_eco    0.00   393
 
"""
                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                      


plot(NCTFemvsMal, what="network")


plot(NCTFemvsMal, what="strength")


filtered_rows2 <- NCTFemvsMal$diffcen.pval[NCTFemvsMal$diffcen.pval < 0.05, ]

filtered_rows2  #  D2_pub0.041 ==>-62       K4_emotion0.047  =>-54          K6_trust 0.019 =>  75     N_influence_pairs 0.015 =>28         sit_eco  0.015 => 27

NCTFemvsMal$diffcen.real

"""
> NCTFemvsMal$diffcen.pval
betweenness
A2_diff                 1.000
A3_age_m                0.804
A6_IMC                  0.800
C_AP                    1.000
D1_priv                 0.147
D2_pub                  0.034  <========
D3_anx_soc              0.943
E1_perspec              0.992
E2_empat                1.000
G1_comp                 0.476
G2_cog                  0.184
G3_affec                0.259
H1_appa                 0.942
H2_attri                0.090
H3_poids                0.475
I_objectifi             0.631
II1_fam                 0.834
II2_pairs               0.084
II3_reseau              0.984
J_cons_corps            0.769
K1_notice               0.371
K2_nodistract           1.000
K3_notworry             0.166
K4_emotion              0.054
K5_listing              0.955
K6_trust                0.025 <========
M1_douleurs_nb          1.000
N_influence_pairs       0.013  <========
sit_eco                 0.013  <========

"""


NCTFemPub1vsMalPub1$einv.pvals$index <- seq_len(nrow(NCTFemPub1vsMalPub1$einv.pvals)) 
filtered_rows <- NCTFemPub1vsMalPub1$einv.pvals[NCTFemPub1vsMalPub1$einv.pvals$`p-value` < 0.05, ]


NCTFemPub1vsMalPub1<-NCT(data_FemPub1, data_MalPub1, 
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
summary(NCTFemPub1vsMalPub1)
NCTFemPub1vsMalPub1$diffcen.pval
