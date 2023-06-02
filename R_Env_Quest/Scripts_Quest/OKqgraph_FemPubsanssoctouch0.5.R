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
library(graphics)

setwd("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Network/femPubsanssoctouch")


##### 1 DATA 
data_FemPub1_full <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_bysub_imputed_FemPub1.csv")
data_FemPub2_full <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_bysub_imputed_FemPub2.csv")



cols_Subscale1 <-c("A2_diff", "A6_IMC", 
                   "C_AP", 
                   "D1_priv",   "D2_pub",  "D3_anx_soc",
                   "E1_perspec","E2_empat", 
                   "G1_comp",   "G2_cog",    "G3_affec",  
                   "H1_appa",   "H2_attri",  "H3_poids",  "I_objectifi", 
                   "II1_fam","II2_pairs", "II3_reseau",
                   "N_influence_pairs",
                   "K1_notice", "K2_nodistract","K3_notworry",  "K4_emotion","K5_listing","K6_trust",
                   "M1_douleurs_nb" ,    
                   "sit_eco" )  

data_FemPub1 <- data_FemPub1_full[, cols_Subscale1]
data_FemPub2 <- data_FemPub2_full[, cols_Subscale1]


names_full <-c("Cisgender", "Body Mass Index", 
                "Physical Activity", 
                "Private SC", "Public SC", "Social Anxiety", 
                "Perspective Taking", "Empathic Concern", 
                "Behavioral Eng.", "Cognitive Eng. ", "Affective Eng. ", 
                "BE for Appearance "," BE Attribution", "BE for Weight satisfaction", "Body Objectification",
                "Family Pressure", "Peers Pressure", "Media Pressure", 
                "Resistance to Peer Influence", 
                "Noticing body sensations", "Not-Distracting","Not-Worrying", "Emotional Awareness", " Body Listening","Body Trusting", 
                "Chronic Pain", 
               "Economic Status")

names(data_FemPub1) <- names_full
names(data_FemPub2) <- names_full



gr3 <- list(c(1:2), c(3),c(4:6),
            c(7:8),c(9:11), 
            c(12:15), c(16:18),c(19),c(20:25),c(26),c(27))           


names(gr3) <- c("General", "Physical Activity Level", 
                "Self Consciousness Scale",
                "Interpersonal Reactivity Index", 
                "Social Media Engagement Scale for Adolescents", 
                "Body Esteem Scale ", 
                "Sociocultural Attitudes Towards Appearance Quest", 
                "Resistance to Peer Influence", 
                "Multidimensional Assessment of Interoceptive Awareness", 
                "Chronic Pain", "Economic Status")



### 1.1 Make correlation matrix
data_FemPub1.cor<-cor_auto(data_FemPub1) 
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
                    "Eco")


pdf("FemPub1_Subscale_Network.pdf", width=18, height=14)
graph.g1<-qgraph(data_FemPub1.cor, graph="glasso", layout="spring",tuning=0.5,
                 labels=shortnames_nodes,
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_FemPub1),
                 border.width=0.1, border.color="white", minimum=.03, 
                 groups=gr3, color=c('#a8e6cf',  '#ffd3b6', '#ff8b94',
                                                 '#00ffff','#3300ff','#660066','#6666ff',
                                                 '#ffcc00','#cc00ff','#ccff00','#99ccff'),title= "In pubertal group network",
                                                 legend=TRUE,legend.mode='style2',GLratio=2.5,layoutScale=1,
                 nodeNames=names_full)
dev.off()
                                              

pdf("FemPub2_Subscale_Network.pdf", width=18, height=14)
graph.g2<-qgraph(data_FemPub2.cor, graph="glasso", layout="spring",tuning=0.5,
                 labels=shortnames_nodes,
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_FemPub2),
                 border.width=0.1, border.color="white", minimum=.03, 
                 groups=gr3, color=c('#a8e6cf',  '#ffd3b6', '#ff8b94',
                                              '#00ffff','#3300ff','#660066','#6666ff',
                                              '#ffcc00','#cc00ff','#ccff00','#99ccff'),title= "Post-pubertal group network",
                                              legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,
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


###### Post-hoc  analysis

###############
#################Pub1
network_FemPub1 <- estimateNetwork(
  data_FemPub1,
  default = "EBICglasso",
  #corMethod = "cor_auto",
  tuning = 0.5,
  refit = TRUE)

simRes <- netSimulator(network_FemPub1$graph,
                       dataGenerator = ggmGenerator(
                         ordinal = TRUE, nLevels = 5),
                       default = "EBICglasso",
                       nCases = c(100,200,300,500),
                       tuning = 0.5,
                       nReps =28  )

pdf("simRes_FemPub1.pdf", useDingbats=FALSE) 
plot(simRes)
plot(simRes,yvar = c("strength","betweenness"))
dev.off()


### edge weight accuracy bootstrap ==> non-parametric (resampling rows from the data)

boot0_FemPub1<-bootnet(data_FemPub1, nBoots=2500, default="EBICglasso", tuning = 0.5, type="nonparametric", statistics='edge')
summary(boot0_FemPub1)
pdf("boot0_FemPub1.pdf", useDingbats=FALSE) 
plot(boot0_FemPub1, labels = FALSE, order = "sample")
dev.off()


boot1_FemPub1<-bootnet(network_FemPub1, nBoots=2500)
pdf("boot1_FemPub1.pdf", useDingbats=FALSE) 
#Plot bootstrapped edge CIs:
plot(boot1_FemPub1, labels = FALSE, order = "sample")
# Plot significant differences (alpha = 0.05) of edges:
plot(boot1_FemPub1, "edge", plot = "difference",onlyNonZero = TRUE,order = "sample",alpha = 0.05)
# Plot significant differences (alpha = 0.05) of node strength:
plot(boot1_FemPub1, "strength", plot = "difference",alpha = 0.05)
dev.off()

### 4.2 node dropping bootstrap
boot2_FemPub1<- bootnet(network_FemPub1, default = "EBICglasso", nBoots=2500, type = "node",computeCentrality=TRUE, statistics = c("betweenness","strength"))
pdf("boot2_FemPub1.pdf", useDingbats=FALSE) 
plot(boot2_FemPub1, statistics = c("betweenness","strength"))
plot(boot2_FemPub1, statistics = c("betweenness"), perNode = TRUE)
plot(boot2_FemPub1, statistics = c("strength"), perNode = TRUE)
dev.off()


### 4.2 case or person dropping bootstrap
boot3_FemPub1<- bootnet(network_FemPub1, default = "EBICglasso", nBoots=2500, type = "person", statistics =c("betweenness","strength"))
pdf("boot3_FemPub1.pdf", useDingbats=FALSE) 
plot(boot3_FemPub1, statistics = c("betweenness","strength"))
#plot(boot3_FemPub1, statistics = c("strength"))
dev.off()
corStability(boot3_FemPub1)#   betweeness = 0.127, strenght = 0.519

#boot4_FemPub1<- bootnet(network_FemPub1, default = "EBICglasso", nBoots=2500, type = "case", statistics = c("betweenness","strength"))
#plot(boot4_FemPub1,statistics = c("betweenness","strength"))
#corStability(boot4_FemPub1)#0.127



###############
#################Pub2
network_FemPub2 <- estimateNetwork(
  data_FemPub2,
  default = "EBICglasso",
  #corMethod = "cor_auto",
  tuning = 0.5,
  refit = TRUE)

simRes <- netSimulator(network_FemPub2$graph,
                       dataGenerator = ggmGenerator(
                         ordinal = TRUE, nLevels = 5),
                       default = "EBICglasso",
                       nCases = c(100,200,300,500),
                       tuning = 0.5,
                       nReps =28  )

pdf("simRes_FemPub2.pdf", useDingbats=FALSE) 
plot(simRes)
plot(simRes,yvar = c("strength","betweenness"))
dev.off()

### edge weight accuracy bootstrap ==> non-parametric (resampling rows from the data)
boot0_FemPub2<-bootnet(data_FemPub2, nBoots=2500, default="EBICglasso", tuning = 0.5,type="nonparametric", statistics='edge')
summary(boot0_FemPub2)
pdf("boot0_FemPub2.pdf", useDingbats=FALSE) 
plot(boot0_FemPub2, labels = FALSE, order = "sample")
dev.off()

boot1_FemPub2<-bootnet(network_FemPub2, nBoots=2500)
pdf("boot1_FemPub2.pdf", useDingbats=FALSE) 
#Plot bootstrapped edge CIs:
plot(boot1_FemPub2, labels = FALSE, order = "sample")
# Plot significant differences (alpha = 0.05) of edges:
plot(boot1_FemPub2, "edge", plot = "difference",onlyNonZero = TRUE,order = "sample",alpha = 0.05)
# Plot significant differences (alpha = 0.05) of node strength:
plot(boot1_FemPub2, "strength", plot = "difference",alpha = 0.05)
dev.off()


### 4.2 node dropping bootstrap
boot2_FemPub2<- bootnet(network_FemPub2, default = "EBICglasso", nBoots=2500, type = "node",computeCentrality=TRUE, statistics = c("betweenness","strength"))
pdf("boot2_FemPub2.pdf", useDingbats=FALSE) 
plot(boot2_FemPub2, statistics = c("betweenness","strength"))
plot(boot2_FemPub2, statistics = c("betweenness"), perNode = TRUE)
plot(boot2_FemPub2, statistics = c("strength"), perNode = TRUE)
dev.off()


### 4.2 case or person dropping bootstrap
boot3_FemPub2<- bootnet(network_FemPub2, default = "EBICglasso", nBoots=2500, type = "person", statistics =c("betweenness","strength"))
CS<-corStability(boot3_FemPub2)
pdf("boot3_FemPub2.pdf", useDingbats=FALSE) 
plot(boot3_FemPub2, statistics = c("betweenness","strength"))
dev.off()
#mtext(paste0("CS betweenness = ", CS[1], ", CS strength = ", CS[2]), side=4, line=3)

#boot4_FemPub1<- bootnet(network_FemPub1, default = "EBICglasso", nBoots=2500, type = "case", statistics = c("betweenness","strength"))
  #plot(boot4_FemPub1,statistics = c("betweenness","strength"))
  #corStability(boot4_FemPub1)#0.127
  
  




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
                   p.adjust.methods= c("holm"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                   test.centrality=TRUE, 
                   centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                   nodes="all",
                   communities=gr3,
                   useCommunities="all",
                   #estimator,
                   #estimatorArgs = list(), 
                   verbose = TRUE)
summary<-summary(NCTFemPub1vsFemPub2)
write.table(capture.output(summary(NCTFemPub1vsFemPub2)), file = "summary(NCTFemPub1vsFemPub2).txt")


NCTFemPub1vsFemPub2$glstrinv.pval ## pval The p value resulting from the permutation test concerning difference in global strength.
#0.498

NCTFemPub1vsFemPub2$nwinv.pval  ##The p value resulting from the permutation test concerning the maximum difference in edge weights.
##network invariance test  0.371


EDGEINVARIANCETEST<-NCTFemPub1vsFemPub2$einv.pvals	### p-values (corrected for multiple testing or not according to ’p.adjust.methods’) per edge from the permutation test concerning differences in edges weights. Only returned if test.edges = TRUE.
einv.real<-NCTFemPub1vsFemPub2$einv.real	### p-values (corrected for multiple testing or not according to ’p.adjust.methods’) per edge from the permutation test concerning differences in edges weights. Only returned if test.edges = TRUE.
write.csv(EDGEINVARIANCETEST, "einv.csv", row.names=TRUE)


diffcen <- NCTFemPub1vsFemPub2$diffcen.real
pval <- NCTFemPub1vsFemPub2$diffcen.pval
CENTRALITYINVARIANCETEST <- cbind(diffcen, pval)
write.csv(CENTRALITYINVARIANCETEST, "diffcen_pval.csv", row.names=TRUE)

plot(NCTFemPub1vsFemPub2,what="network")
plot(NCTFemPub1vsFemPub2,what="strength")
plot(NCTFemPub1vsFemPub2,what="edge")