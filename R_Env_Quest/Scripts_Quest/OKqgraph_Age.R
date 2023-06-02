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
data_Age1 <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_Age1_fornetwork2.csv")
data_Age2 <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_Age2_fornetwork2.csv")
data_Age3 <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_Age3_fornetwork2.csv")

data_Pub1 <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_Pub1_fornetwork2.csv")
data_Pub2 <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_Pub2_fornetwork2.csv")

#str(Data)
data_Age1$X = NULL
data_Age2$X = NULL
data_Age3$X = NULL
data_Pub1$X = NULL
data_Pub2$X = NULL
data_Pub1$A3_age_m = NULL
data_Pub2$A3_age_m= NULL

#data_Age1$A3_age_m = NULL
#data_Age2$A3_age_m = NULL
#data_Age3$A3_age_m = NULL



### 1.1 Make correlation matrix
data_Age1.cor<-cor_auto(data_Age1) #data including covariates
ggcorrplot(data_Age1.cor)

if (!is.positive.definite(data_Age1.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_Age1.cor <- nearPD(data_Age1.cor)$mat
}

data_Age2.cor<-cor_auto(data_Age2) #data including covariates
ggcorrplot(data_Age2.cor)
if (!is.positive.definite(data_Age2.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_Age2.cor <- nearPD(data_Age2.cor)$mat
}


data_Age3.cor<-cor_auto(data_Age3) #data including covariates
ggcorrplot(data_Age3.cor)
if (!is.positive.definite(data_Age3.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_Age3.cor <- nearPD(data_Age3.cor)$mat
}


data_Pub1.cor<-cor_auto(data_Pub1) #data including covariates
ggcorrplot(data_Pub1.cor)

if (!is.positive.definite(data_Pub1.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_Pub1.cor <- nearPD(data_Pub1.cor)$mat
}

data_Pub2.cor<-cor_auto(data_Pub2) #data including covariates
ggcorrplot(data_Pub2.cor)
if (!is.positive.definite(data_Pub2.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_Pub2.cor <- nearPD(data_Pub2.cor)$mat
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

pdf("Age1_Subscale_Network.pdf")
graph.g1<-qgraph(data_Age1.cor, graph="glasso", layout="spring", 
                     vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Age1),
                     border.width=1.5, border.color="black", minimum=.03, 
                     groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                          '#00ffff','#3300ff','#660066','#6666ff',
                                          '#cc00ff','#ffcc00','#ccff00'))
dev.off()


pdf("Age2_Subscale_Network.pdf")
graph.g2<-qgraph(data_Age2.cor, graph="glasso", layout="spring", 
                vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Age2),
                border.width=1.5, border.color="black", minimum=.03, 
                groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                             '#00ffff','#3300ff','#660066','#6666ff',
                                             '#cc00ff','#ffcc00','#ccff00'))
dev.off()



pdf("Age3_Subscale_Network.pdf")
graph.g3<-qgraph(data_Age3.cor, graph="glasso", layout="spring", 
                vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Age3),
                border.width=1.5, border.color="black", minimum=.03, 
                groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                             '#00ffff','#3300ff','#660066','#6666ff',
                                             '#cc00ff','#ffcc00','#ccff00'))
dev.off()

pdf("Pub1_Subscale_Network.pdf")
graph.Pub1<-qgraph(data_Pub1.cor, graph="glasso", layout="spring", 
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Pub1),
                 border.width=1.5, border.color="black", minimum=.03, 
                 groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                              '#00ffff','#3300ff','#660066','#6666ff',
                                              '#cc00ff','#ffcc00','#ccff00'))
dev.off()
                                              
                                              
pdf("Pub2_Subscale_Network.pdf")
graph.Pub2<-qgraph(data_Pub2.cor, graph="glasso", layout="spring", 
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Pub2),
                 border.width=1.5, border.color="black", minimum=.03, 
                 groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                              '#00ffff','#3300ff','#660066','#6666ff',
                                              '#cc00ff','#ffcc00','#ccff00'))
dev.off()


### 3.2 Centrality 

###Age1                                                                                     
 pdf("Age1_Subscale_Centrality1.pdf")
 centralityPlot(graph.g1,include="All")
 dev.off()
 
 pdf("Age1_Subscale_Centrality2.pdf")
 centralityPlot(graph.g1,include="All",orderBy = "Strength")
 dev.off()
 
 pdf("Age1_Subscale_Centrality3.pdf")
 centralityPlot(graph.g1,include="All",orderBy = "Betweenness")
 dev.off()
 
 pdf("Age1_Subscale_Clustering.pdf")
 clusteringPlot(graph.g1)
 dev.off()
 
###Age2
 
 pdf("Age2_Subscale_Centrality1.pdf")
 centralityPlot(graph.g2,include="All")
 dev.off()
 
 pdf("Age2_Subscale_Centrality2.pdf")
 centralityPlot(graph.g2,include="All",orderBy = "Strength")
 dev.off()
 
 pdf("Age2_Subscale_Centrality3.pdf")
 centralityPlot(graph.g2,include="All",orderBy = "Betweenness")
 dev.off()
 
 pdf("Age2_Subscale_Clustering.pdf")
 clusteringPlot(graph.g2)
 dev.off()

 ###Age3                                                                                     
 pdf("Age3_Subscale_Centrality1.pdf")
 centralityPlot(graph.g3,include="All")
 dev.off()
 
 pdf("Age3_Subscale_Centrality2.pdf")
 centralityPlot(graph.g3,include="All",orderBy = "Strength")
 dev.off()
 
 pdf("Age3_Subscale_Centrality3.pdf")
 centralityPlot(graph.g3,include="All",orderBy = "Betweenness")
 dev.off()
 
 pdf("Age3_Subscale_Clustering.pdf")
 clusteringPlot(graph.g3)
 dev.off()


 
 ###Pub1                                                                                     
 pdf("Pub1_Subscale_Centrality1.pdf")
 centralityPlot(graph.Pub1,include="All")
 dev.off()
 
 pdf("Pub1_Subscale_Centrality2.pdf")
 centralityPlot(graph.Pub1,include="All",orderBy = "Strength")
 dev.off()
 
 pdf("Pub1_Subscale_Centrality3.pdf")
 centralityPlot(graph.Pub1,include="All",orderBy = "Betweenness")
 dev.off()
 
 pdf("Pub1_Subscale_Clustering.pdf")
 clusteringPlot(graph.Pub1)
 dev.off()
 
 ###Pub2
 
 pdf("Pub2_Subscale_Centrality1.pdf")
 centralityPlot(graph.Pub2,include="All")
 dev.off()
 
 pdf("Pub2_Subscale_Centrality2.pdf")
 centralityPlot(graph.Pub2,include="All",orderBy = "Strength")
 dev.off()
 
 pdf("Pub2_Subscale_Centrality3.pdf")
 centralityPlot(graph.Pub2,include="All",orderBy = "Betweenness")
 dev.off()
 
 pdf("Pub2_Subscale_Clustering.pdf")
 clusteringPlot(graph.Pub2)
 dev.off()
 
 
 
 

##########################
 ########################      NetworkComparisonTest: 
 ##########################
 ##########################
 
 
 library("IsingSampler")
 library("IsingFit")
 library("NetworkComparisonTest" )
 
 NCTPub1vsPub2<-NCT(data_Pub1, data_Pub2, 
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
 summary(NCTPub1vsPub2)
 
 NCTPub1vsPub2$glstrinv.pval ## pval The p value resulting from the permutation test concerning difference in global strength.
 
 NCTPub1vsPub2$nwinv.pval  ##The p value resulting from the permutation test concerning the maximum difference in edge weights.
 ##network invariance test 
 
 NCTPub1vsPub2$einv.pvals	### p-values (corrected for multiple testing or not according to ’p.adjust.methods’) per edge from the permutation test concerning differences in edges weights. Only returned if test.edges = TRUE.
 
 
 NCTPub1vsPub2$einv.pvals$index <- seq_len(nrow(NCTPub1vsPub2$einv.pvals)) 
 filtered_rows <- NCTPub1vsPub2$einv.pvals[NCTPub1vsPub2$einv.pvals$`p-value` < 0.05, ]
 
 """
 155           D1_priv            D2_pub    0.03    15
 184              C_AP        D3_anx_soc    0.03    19
 246            D2_pub          E2_empat    0.04    34
 277        D3_anx_soc    F2_reseauquant    0.00    43
 364              C_AP          G3_affec    0.01    70
 368        E1_perspec          G3_affec    0.03    74
 422           A2_diff          H2_attri    0.04    93
 459          E2_empat          H3_poids    0.00   114
 486            D2_pub       I_objectifi    0.01   126
 495          H2_attri       I_objectifi    0.00   135
 511           A1_sexe           II1_fam    0.00   137
 524           H1_appa           II1_fam    0.03   150
 576            D2_pub        II3_reseau    0.01   177
 638        E1_perspec         K1_notice    0.01   218
 651      J_cons_corps         K1_notice    0.00   231
 667        D3_anx_soc     K2_nodistract    0.01   238
 682         K1_notice     K2_nodistract    0.00   253
 713     K2_nodistract       K3_notworry    0.00   276
 755           D1_priv        K5_listing    0.02   305
 773     K2_nodistract        K5_listing    0.00   323
 774       K3_notworry        K5_listing    0.00   324
 791           G1_comp          K6_trust    0.01   336
 795          H2_attri          K6_trust    0.01   340
 812           A2_diff    M1_douleurs_nb    0.02   353
 846            D2_pub N_influence_pairs    0.00   384
 847        D3_anx_soc N_influence_pairs    0.03   385
 857       I_objectifi N_influence_pairs    0.00   395
 865        K4_emotion N_influence_pairs    0.00   403
 872           A2_diff           sit_eco    0.00   408
 898    M1_douleurs_nb           sit_eco    0.01   434
 899 N_influence_pairs           sit_eco    0.03   435
 
 """
 
 
 plot(NCTPub1vsPub2, what="network")
 
 # Plot results of global strength invariance test (not reliable with only 10 permutations!):
 plot(NCTPub1vsPub2, what="strength")
 
 plot(NCTPub1vsPub2, what="centrality",nodes="K6_trust")
 
 # Plot results of the edge invariance test (not reliable with only 10 permutations!):
 # Note that two distributions are plotted
 #plot(NCTPub1vsPub2, what="edge")

 
 NCTPub1vsPub2$diffcen.pval

 """
                   betweenness
A1_sexe                 0.352
A2_diff                 1.000
A6_IMC                  1.000
C_AP                    1.000
D1_priv                 0.367
D2_pub                  0.534
D3_anx_soc              0.751
E1_perspec              0.287
E2_empat                0.099
G1_comp                 0.825
G2_cog                  0.990
G3_affec                0.907
H1_appa                 0.633
H2_attri                1.000
H3_poids                0.555
I_objectifi             0.672
II1_fam                 0.075
II2_pairs               0.216
II3_reseau              0.589
J_cons_corps            0.874
K1_notice               1.000
K2_nodistract           1.000
K3_notworry             0.394
K4_emotion              0.781
K5_listing              0.151
K6_trust                0.001
M1_douleurs_nb          0.008
N_influence_pairs       0.449
sit_eco                 0.006
"""
 
 
 
 
  NCT1vs2<-NCT(data_Age1, data_Age2, 
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
     centrality=c("betweenness"),
     nodes="all",
     communities=NULL,useCommunities="all",
     #estimator,
     #estimatorArgs = list(), 
     verbose = TRUE)
 
  summary(NCT1vs2)
  NCT1vs2$diffcen.pval #====>influence des pairs
  
 NCT1vs2$glstrinv.pval	
 NCT1vs2$einv.pvals$`p-value`
 NCT1vs2$diffcen.pval
 
 
 NCT1vs3<-NCT(data_Age1, data_Age3, 
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
              centrality=c("betweenness"),
              nodes="all",
              communities=NULL,useCommunities="all",
              #estimator,
              #estimatorArgs = list(), 
              verbose = TRUE)
 summary(NCT1vs3)
 NCT1vs3$diffcen.pval
 
 
 
 NCT2vs3<-NCT(data_Age2, data_Age3, 
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
              centrality=c("betweenness"),
              nodes="all",
              communities=NULL,useCommunities="all",
              #estimator,
              #estimatorArgs = list(), 
              verbose = TRUE)
 
 summary(NCT2vs3)
 NCT2vs3$diffcen.pval # ==>Influence des pairs G1 comp
 
 
 
 
 
 
 
 
 
 
 
### 3.3 intercorrelation of different centrality measures
cor(centrality$InDegree, centrality$Closeness, method="spearman")     #0.7253529 spearman=0.65
cor(centrality$InDegree, centrality$Betweenness, method="spearman")   #0.80 spearman=0.72
cor(centrality$Betweenness, centrality$Closeness, method="spearman")  #0.81 spearman=0.82



##### 4. Robustness Network 1


### A priori sample size analysis 

network <- estimateNetwork(
  data,
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





### 4.1 edge weight bootstrap

m1<-bootnet(data, nBoots=1000, default="EBICglasso", labels=names, type="nonparametric")
# save(m1, file = "boot_ptsd_edgeweights.Rdata")
# load(file = "boot_ptsd_edgeweights.Rdata")
plot(m1, labels = FALSE, order = "sample")

pdf("boot_edges_Ar.pdf", useDingbats=FALSE) 
plot(m1, labels = FALSE, order = "sample")
dev.off()

### 4.2 node dropping bootstrap

m2<- bootnet(data, default = "EBICglasso", nBoots=1000, type = "node", labels=names)
# save(m2, file = "boot_ptsd_nodedropping.Rdata")
# load(file = "boot_ptsd_nodedropping.Rdata")

plot(m2)
plot(m2, "strength", perNode = TRUE, legend = FALSE)
plot(m2, "betweenness", perNode = TRUE, legend = FALSE)
plot(m2, "closeness", perNode = TRUE, legend = FALSE)

pdf("boot_nodedropping_Ar.pdf", useDingbats=FALSE) 
plot(m2)
plot(m2, "strength", perNode = TRUE, legend = FALSE)
plot(m2, "betweenness", perNode = TRUE, legend = FALSE)
plot(m2, "closeness", perNode = TRUE, legend = FALSE)
dev.off()

### person dropping
m3<- bootnet(data, default = "EBICglasso", nBoots=1000, type = "person", labels=names)
save(m3, file = "boo_persondropping.Rdata")

plot(m3)
plot(m3, "strength", perNode = TRUE, legend = FALSE)
plot(m3, "betweenness", perNode = TRUE, legend = FALSE)
plot(m3, "closeness", perNode = TRUE, legend = FALSE)

pdf("boot_persondropping_Ar.pdf", useDingbats=FALSE) 
plot(m3)
plot(m3, "strength", perNode = TRUE, legend = FALSE)
plot(m3, "betweenness", perNode = TRUE, legend = FALSE)
plot(m3, "closeness", perNode = TRUE, legend = FALSE)
dev.off()



















##### 5. Network 2: 20 PTSD symptoms + 7 covariates
graph.m <-EBICglasso(data.cor, n = nrow(data))


pdf("Fig3_Ar.pdf")
qgraph(data.cor, labels=names1, layout="spring", vsize=7, groups=gr2, graph="glasso", 
       sampleSize = nrow(data), color=c('#bbbbbb','#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94'), cut=0, maximum=0.45, minimum=0.03,
       border.width=1.5, border.color="black")
dev.off()



##### 6. Robustness Network 2: 20 PTSD symptoms + 7 covariates

### 6.1 edge weight
m4<-bootnet(data, nBoots=1000, default="EBICglasso", labels=names, type="nonparametric")
save(m4, file = "boot_ptsd27full_edgeweights.Rdata")
load(file = "boot_ptsd27full_edgeweights.Rdata")

plot(m4, labels = FALSE, order = "sample")

pdf("boot_PTSDFULL_edges_Ar.pdf", useDingbats=FALSE) 
plot(m4, labels = FALSE, order = "sample")
dev.off()

"""

##### 7. Examine changes of symptoms once covariare are added

graph.m2<-graph.m[-c(21:27),-c(21:27)] # delete covariates from full network with covariates

delta<-graph_ptsd.m-graph.m2 #delta network
max(delta)
mean(abs(delta[upper.tri(delta,diag=FALSE)]))

L<-averageLayout(graph_ptsd.m) # use same layout as Figure 1
#pdf("Fig4_Ar.pdf")
#qgraph(delta, labels=names_ptsd1, layout=L, vsize=7, cut=0, maximum=.45, 
           border.width=1.5, border.color="black", minimum=.03, 
           groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94'))
dev.off()

#cor(as.vector(graph.m2),as.vector(graph_ptsd.m), method="spearman") #0.97

### 7.1 mean edge weight among symptoms=0.04; adding covariates reduces sum of edges among symptoms by 11.5%
mean(abs(delta[upper.tri(delta,diag=FALSE)]))
sum1<-sum(abs(graph_ptsd.m[upper.tri(graph_ptsd.m,diag=FALSE)])); sum1     # 8.68; sum/2 of ptsd network matrix
mean1<-mean(abs(graph_ptsd.m[upper.tri(graph_ptsd.m,diag=FALSE)])); mean1  # 0.05; mean edge strength
sum2<-sum(abs(graph.m2[upper.tri(graph.m2,diag=FALSE)])); sum2             # 7.78; sum/2 of ptsd + covariates matrix, with covariate cells deleted (ptsd network accounting for covariates but without them)
mean2<-mean(abs(graph.m2[upper.tri(graph.m2,diag=FALSE)])); mean2          # 0.04; mean edge strength
1-sum2/sum1 # change in connectivity of PTSD network once symptoms are added 11.5%

### 7.2 mean edge weight among covariates=0.09
graph.m3<-graph.m[-c(1:20),-c(1:20)]   # just covariate connections of full network
sum3<-sum(abs(graph.m3[upper.tri(graph.m3,diag=FALSE)])); sum3            # 1.91
mean3<-mean(abs(graph.m3[upper.tri(graph.m3,diag=FALSE)])); mean3         # 0.09

### 7.3 mean edge weight symptoms-covaraites=0.02
graph.m4<-graph.m[-c(1:20),-c(21:27)]  # just connection betw covariates and symptoms
sum4<-sum(abs(graph.m4[upper.tri(graph.m4,diag=FALSE)])); sum4            # 2.13
mean4<-mean(abs(graph.m4[upper.tri(graph.m3,diag=FALSE)])); mean4         # 0.02

