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


data_D1low <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_D1low_fornetwork2.csv")
data_D1high <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_D1high_fornetwork2.csv")

#str(Data)
data_D1low$X = NULL
data_D1high$X= NULL

data_D1low$D1_priv = NULL
data_D1high$D1_priv= NULL

### 1.1 Make correlation matrix

data_D1low.cor<-cor_auto(data_D1low) #data including covariates
ggcorrplot(data_D1low.cor)

if (!is.positive.definite(data_D1low.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_D1low.cor <- nearPD(data_D1low.cor)$mat
}

data_D1high.cor<-cor_auto(data_D1high) #data including covariates
ggcorrplot(data_D1high.cor)
if (!is.positive.definite(data_D1high.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_D1high.cor <- nearPD(data_D1high.cor)$mat
}


gp_A<-c("A3_age_m","A1_sexe","A2_diff","A6_IMC")
gp_C<-c("C_AP")
gp_D<-c("D2_pub","D3_anx_soc")
gp_E<-c("E1_perspec","E2_empat")
gp_F<-c("G1_comp","G2_cog","G3_affec")
gp_HI<-c("H1_appa","H2_attri","H3_poids","I_objectifi")
gp_II<-c("II1_fam","II2_pairs","II3_reseau")
gp_JK<-c("J_cons_corps","K1_notice","K2_nodistract","K3_notworry","K4_emotion","K5_listing","K6_trust")
gp_L<-c("M1_douleurs_nb")
gp_M<-c("N_influence_pairs")
gp_N<-c("sit_eco")

gr3<-list(gp_A,gp_C,gp_D,gp_E,gp_F,gp_HI,gp_II,gp_JK,gp_L,gp_M,gp_N)

gr3 <- list(c(1:4), c(5),c(6:7),
            c(8:9),c(10:12), 
            c(13:16), c(17:19),c(20:25),c(26),c(27),c(28))           


##### 3. Network 1 

### 3.1 Figure 1

pdf("D1low_Subscale_Network.pdf")
graph.D1low<-qgraph(data_D1low.cor, graph="glasso", layout="spring", 
                  vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_D1low),
                  border.width=1.5, border.color="black", minimum=.03, 
                  groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                               '#00ffff','#3300ff','#660066','#6666ff',
                                               '#cc00ff','#ffcc00','#ccff00'))
dev.off()
                                               

pdf("D1high_Subscale_Network.pdf")
graph.D1high<-qgraph(data_D1high.cor, graph="glasso", layout="spring", 
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_D1high),
                 border.width=1.5, border.color="black", minimum=.03, 
                 groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                              '#00ffff','#3300ff','#660066','#6666ff',
                                              '#cc00ff','#ffcc00','#ccff00'))
dev.off()
                                              
                                                                                              
### 3.2 Centrality 

###D1low                                                                                     
pdf("D1low_Subscale_Centrality1.pdf")
centralityPlot(graph.D1low,include="All")
dev.off()

pdf("D1low_Subscale_Centrality2.pdf")
centralityPlot(graph.D1low,include="All",orderBy = "Strength")
dev.off()

pdf("D1low_Subscale_Centrality3.pdf")
centralityPlot(graph.D1low,include="All",orderBy = "Betweenness")
dev.off()

pdf("D1low_Subscale_Clustering.pdf")
clusteringPlot(graph.D1low)
dev.off()

###D1high
pdf("D1high_Subscale_Centrality1.pdf")
centralityPlot(graph.D1high,include="All")
dev.off()

pdf("D1high_Subscale_Centrality2.pdf")
centralityPlot(graph.D1high,include="All",orderBy = "Strength")
dev.off()

pdf("D1high_Subscale_Centrality3.pdf")
centralityPlot(graph.D1high,include="All",orderBy = "Betweenness")
dev.off()

pdf("D1high_Subscale_Clustering.pdf")
clusteringPlot(graph.D1high)
dev.off()



##########################
########################      NetworkComparisonTest: 
##########################
##########################


library("IsingSampler")
library("IsingFit")
library("NetworkComparisonTest" )

NCTD1lowvsD1high<-NCT(data_D1low, data_D1high, 
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
summary(NCTD1lowvsD1high)

NCTD1lowvsD1high$glstrinv.pval
NCTD1lowvsD1high$einv.pvals	
NCTD1lowvsD1high$einv.pvals$index <- seq_len(nrow(NCTD1lowvsD1high$einv.pvals)) 

filtered_rows <- NCTD1lowvsD1high$einv.pvals[NCTD1lowvsD1high$einv.pvals$`p-value` < 0.05, ]

filtered_rows

"""

             Var1              Var2 p-value index
264      A3_age_m           G1_comp    0.04    39
266          C_AP           G1_comp    0.04    41
327    E1_perspec          G3_affec    0.00    63
409      A3_age_m          H3_poids    0.01    94
413    D3_anx_soc          H3_poids    0.04    98
450      H3_poids       I_objectifi    0.02   120
532       G1_comp        II3_reseau    0.01   163
540     II2_pairs        II3_reseau    0.02   171
660 K2_nodistract       K3_notworry    0.00   253
726       A1_sexe          K6_trust    0.02   301
735       G1_comp          K6_trust    0.00   310
750    K5_listing          K6_trust    0.01   325
780      K6_trust    M1_douleurs_nb    0.01   351
789        D2_pub N_influence_pairs    0.00   357
790    D3_anx_soc N_influence_pairs    0.00   358
804     K1_notice N_influence_pairs    0.02   372


 
 """
                                                                                              
                                                                                              
                                                                                              
                                                                                              
plot(NCTD1lowvsD1high, what="network")


plot(NCTD1lowvsD1high, what="strength")


NCTD1lowvsD1high$diffcen.pval
                                                                                              
 """
> NCTD1lowvsD1high$diffcen.pval
A1_sexe                  0.01
A2_diff                  1.00
A3_age_m                 0.47
A6_IMC                   0.94
C_AP                     1.00
D2_pub                   0.57
D3_anx_soc               0.69
E1_perspec               0.17
E2_empat                 0.92
G1_comp                  0.64
G2_cog                   0.60
G3_affec                 0.00
H1_appa                  0.49
H2_attri                 1.00
H3_poids                 0.27
I_objectifi              0.88
II1_fam                  0.75
II2_pairs                0.30
II3_reseau               0.38
J_cons_corps             0.79
K1_notice                0.00
K2_nodistract            1.00
K3_notworry              0.29
K4_emotion               0.40
K5_listing               0.88
K6_trust                 0.91
M1_douleurs_nb           1.00
N_influence_pairs        0.00
sit_eco                  1.00

"""
                                                                                              