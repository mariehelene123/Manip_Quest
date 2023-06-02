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


data_K6low <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_K6low_fornetwork2.csv")
data_K6high <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_K6high_fornetwork2.csv")

#str(Data)
data_K6low$X = NULL
data_K6high$X= NULL

data_K6low$K6_trust = NULL
data_K6high$K6_trust= NULL

### 1.1 Make correlation matrix

data_K6low.cor<-cor_auto(data_K6low) #data including covariates
ggcorrplot(data_K6low.cor)

if (!is.positive.definite(data_K6low.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_K6low.cor <- nearPD(data_K6low.cor)$mat
}

data_K6high.cor<-cor_auto(data_K6high) #data including covariates
ggcorrplot(data_K6high.cor)
if (!is.positive.definite(data_K6high.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_K6high.cor <- nearPD(data_K6high.cor)$mat
}


gp_A<-c("A3_age_m","A1_sexe","A2_diff","A6_IMC")
gp_C<-c("C_AP")
gp_D<-c("D1_priv","D2_pub","D3_anx_soc")
gp_E<-c("E1_perspec","E2_empat")
gp_F<-c("G1_comp","G2_cog","G3_affec")
gp_HI<-c("H1_appa","H2_attri","H3_poids","I_objectifi")
gp_II<-c("II1_fam","II2_pairs","II3_reseau")
gp_JK<-c("J_cons_corps","K1_notice","K2_nodistract","K3_notworry","K4_emotion","K5_listing")
gp_L<-c("M1_douleurs_nb")
gp_M<-c("N_influence_pairs")
gp_N<-c("sit_eco")

gr3<-list(gp_A,gp_C,gp_D,gp_E,gp_F,gp_HI,gp_II,gp_JK,gp_L,gp_M,gp_N)

gr3 <- list(c(1:4), c(5),c(6:8),
            c(9:10),c(11:13), 
            c(14:17), c(18:20),c(21:26),c(27),c(28),c(29))           


##### 3. Network 1 

### 3.1 Figure 1

pdf("K6low_Subscale_Network.pdf")
graph.K6low<-qgraph(data_K6low.cor, graph="glasso", layout="spring", 
                  vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_K6low),
                  border.width=1.5, border.color="black", minimum=.03, 
                  groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                               '#00ffff','#3300ff','#660066','#6666ff',
                                               '#cc00ff','#ffcc00','#ccff00'))
dev.off()
                                               

pdf("K6high_Subscale_Network.pdf")
graph.K6high<-qgraph(data_K6high.cor, graph="glasso", layout="spring", 
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_K6high),
                 border.width=1.5, border.color="black", minimum=.03, 
                 groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                              '#00ffff','#3300ff','#660066','#6666ff',
                                              '#cc00ff','#ffcc00','#ccff00'))
dev.off()
                                              
                                                                                              
### 3.2 Centrality 

###K6low                                                                                     
pdf("K6low_Subscale_Centrality1.pdf")
centralityPlot(graph.K6low,include="All")
dev.off()

pdf("K6low_Subscale_Centrality2.pdf")
centralityPlot(graph.K6low,include="All",orderBy = "Strength")
dev.off()

pdf("K6low_Subscale_Centrality3.pdf")
centralityPlot(graph.K6low,include="All",orderBy = "Betweenness")
dev.off()

pdf("K6low_Subscale_Clustering.pdf")
clusteringPlot(graph.K6low)
dev.off()

###K6high
pdf("K6high_Subscale_Centrality1.pdf")
centralityPlot(graph.K6high,include="All")
dev.off()

pdf("K6high_Subscale_Centrality2.pdf")
centralityPlot(graph.K6high,include="All",orderBy = "Strength")
dev.off()

pdf("K6high_Subscale_Centrality3.pdf")
centralityPlot(graph.K6high,include="All",orderBy = "Betweenness")
dev.off()

pdf("K6high_Subscale_Clustering.pdf")
clusteringPlot(graph.K6high)
dev.off()



##########################
########################      NetworkComparisonTest: 
##########################
##########################


library("IsingSampler")
library("IsingFit")
library("NetworkComparisonTest" )

NCTK6lowvsK6high<-NCT(data_K6low, data_K6high, 
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
summary(NCTK6lowvsK6high)

NCTK6lowvsK6high$glstrinv.pval
NCTK6lowvsK6high$einv.pvals	
NCTK6lowvsK6high$einv.pvals$index <- seq_len(nrow(NCTK6lowvsK6high$einv.pvals)) 

filtered_rows <- NCTK6lowvsK6high$einv.pvals[NCTK6lowvsK6high$einv.pvals$`p-value` < 0.05, ]

filtered_rows

"""
146     A1_sexe           D1_priv    0.01    11
291     A1_sexe           G1_comp    0.01    46
418      G2_cog          H2_attri    0.03   103
449     H1_appa          H3_poids    0.03   119
479    H2_attri       I_objectifi    0.00   135
499     D1_priv           II1_fam    0.01   142
566    H2_attri        II3_reseau    0.00   186
568 I_objectifi        II3_reseau    0.02   188
588  D3_anx_soc      J_cons_corps    0.00   198
591     G1_comp      J_cons_corps    0.02   201
640     A2_diff     K2_nodistract    0.00   233
716  II3_reseau        K4_emotion    0.01   296
720 K3_notworry        K4_emotion    0.02   300
735    E2_empat        K5_listing    0.03   310
765     G1_comp    M1_douleurs_nb    0.04   336
779  K4_emotion    M1_douleurs_nb    0.01   350
796    G3_affec N_influence_pairs    0.01   364
 
 """
                                                                                              
                                                                                              
                                                                                              
                                                                                              
plot(NCTK6lowvsK6high, what="network")


plot(NCTK6lowvsK6high, what="strength")


NCTK6lowvsK6high$diffcen.pval
                                                                                              
                                                                                              """
> NCTK6lowvsK6high$diffcen.pval
A1_sexe                  0.46
A2_diff                  1.00
A3_age_m                 0.37
A6_IMC                   0.63
C_AP                     1.00
D1_priv                  0.75
D2_pub                   0.69
D3_anx_soc               0.88
E1_perspec               0.22
E2_empat                 0.06
G1_comp                  0.10
G2_cog                   0.90
G3_affec                 0.20
H1_appa                  0.48
H2_attri                 1.00
H3_poids                 0.17
I_objectifi              0.25
II1_fam                  0.84
II2_pairs                0.84
II3_reseau               0.93
J_cons_corps             0.31
K1_notice                0.72
K2_nodistract            0.00 ==>>>
K3_notworry              0.00==>
K4_emotion               0.25
K5_listing               1.00
K6_trust                 0.29
M1_douleurs_nb           1.00
N_influence_pairs        0.14
sit_eco                  1.00

"""
                                                                                              