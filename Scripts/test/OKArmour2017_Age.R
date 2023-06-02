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

#str(Data)
data_Age1$X = NULL
data_Age2$X = NULL
data_Age3$X = NULL

data_Age1$A3_age_m = NULL
data_Age2$A3_age_m = NULL
data_Age3$A3_age_m = NULL

### 1.1 Make correlation matrix
data_Age1.cor<-cor_auto(data_Age1) #data including covariates
ggcorrplot(data_Age1.cor)

data_Age2.cor<-cor_auto(data_Age2) #data including covariates
ggcorrplot(data_Age2.cor)

data_Age3.cor<-cor_auto(data_Age3) #data including covariates
ggcorrplot(data_Age3.cor)

data_Age1.glasso<-glasso(data_Age1.cor,0.25)
data_Age2.glasso<-glasso(data_Age2.cor,0.25)
data_Age3.glasso<-glasso(data_Age3.cor,0.25)


### 1.2 Create names object & group object for graph
#names<-c("IntT","NgtM", "FshB", "EcuR", "PcuR", "AvT", "AvR", "Amn", "NBlf", "BSO", "NTrE", "Lsit", "Dtch", "ResAf", "Arrt", "SDB", "HpV", "ExSR", "DfCon", "sDis", "Anx", "Dep", "SI", "PFunc", "Mfunc", "Age", "Sex")
#names1<-c("B1","B2", "B3", "B4", "B5", "C1", "C2", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "E1", "E2", "E3", "E4", "E5", "E6", "Anx", "Dep", "SI", "PFunc", "Mfunc", "Age", "Sex")
#names_ptsd<-c("IntT","NgtM", "FshB", "EcuR", "PcuR", "AvT", "AvR", "Amn", "NBlf", "BSO", "NTrE", "Lsit", "Dtch", "ResAf", "Arrt", "SDB", "HpV", "ExSR", "DfCon", "sDis")
#names_ptsd1<-c("B1","B2", "B3", "B4", "B5", "C1", "C2", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "E1", "E2", "E3", "E4", "E5", "E6")
#gr1 <- list(c(21:27), c(1:20))                         #first covariates, second group ptsd symptoms
#gr2 <- list(c(21:27), c(1:5), c(6:7),c(8:14),c(15:20)) #first covariates, then PTSD symptoms categories B C D E
#gr3 <- list(c(1:5), c(6:7),c(8:14),c(15:20))           #PTSD symptoms categories B C D E

names=names(data)
gp_A<-c("A6_IMC")
gp_C<-c("C_AP")
gp_D<-c("D1_priv","D2_pub","D3_anx_soc")
gp_E<-c("E1_perspec","E2_empat")
gp_F<-c("F2_reseauquant","G1_comp","G2_cog","G3_affec")
gp_HI<-c("H1_appa","H2_attri","H3_poids","I_objectifi")
gp_II<-c("II1_fam","II2_pairs","II3_reseau")
gp_JK<-c("J_cons_corps","K1_notice","K2_nodistract","K3_notworry","K4_emotion","K5_listing","K6_trust")
gp_M<-c("M1_douleurs_nb")
gp_N<-c("N_influence_pairs")

gr3<-list(gp_A,gp_C,gp_D,gp_E,gp_F,gp_HI,gp_II,gp_JK,gp_M,gp_N)

gr3 <- list(c(1), c(2),c(3:5),
            c(6:7),c(8:11), 
            c(12:15), c(16:18),c(19:25),c(26),c(27))           


##### 3. Network 1 

### 3.1 Figure 1

pdf("Age1_Network_Ar.pdf")
graph.g1<-qgraph(data_Age1.cor, graph="glasso", layout="spring", 
                     vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Age1),
                     border.width=1.5, border.color="black", minimum=.03, 
                     groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                          '#00ffff','#3300ff','#660066','#6666ff',
                                          '#cc00ff','#ffcc00','#ccff00'))
dev.off()


pdf("Age2_Network_Ar.pdf")
graph.g2<-qgraph(data_Age2.cor, graph="glasso", layout="spring", 
                vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Age2),
                border.width=1.5, border.color="black", minimum=.03, 
                groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                             '#00ffff','#3300ff','#660066','#6666ff',
                                             '#cc00ff','#ffcc00','#ccff00'))
dev.off()



pdf("Age3_Network_Ar.pdf")
graph.g3<-qgraph(data_Age3.cor, graph="glasso", layout="spring", 
                vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Age3),
                border.width=1.5, border.color="black", minimum=.03, 
                groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                             '#00ffff','#3300ff','#660066','#6666ff',
                                             '#cc00ff','#ffcc00','#ccff00'))
                                             dev.off()


### 3.2 Centrality 


pdf("Age1_centrality_Ar.pdf")
centrality=centrality(graph.g1)
centralityPlot(graph.g1,include="All")
clusteringPlot(graph.g1)
dev.off()


pdf("Age2_centrality_Ar.pdf")
centrality=centrality(graph.g2)
centralityPlot(graph.g2,include="All")
clusteringPlot(graph.g2)
dev.off()


pdf("Age3_centrality_Ar.pdf")
centrality=centrality(graph.g3)
centralityPlot(graph.g3,include="All")
clusteringPlot(graph.g3)
dev.off()


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

