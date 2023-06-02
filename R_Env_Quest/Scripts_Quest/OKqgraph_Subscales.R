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
library(stats)
library(graphics)
library(reticulate) # pour lire des package R dans python
#setwd("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Network")

#py_run_file("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/P_Env_Quest/myScripts/Item_andScale_bysub.py")


##### 1 DATA 
data_full <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/POURANALYSE_imputed.csv")
data_raw <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/POURANALYSE.csv") 


cols_Subscale1 <-c("A1_sexe", "A2_diff", "A6_IMC", 
                  "B_puberte", 
                  "C_AP", 
                  "D1_priv",   "D2_pub",  "D3_anx_soc",
                  "E1_perspec","E2_empat", 
                  "G1_comp",   "G2_cog",    "G3_affec",  
                  "H1_appa",   "H2_attri",  "H3_poids",  "I_objectifi", 
                  "II1_fam","II2_pairs", "II3_reseau",
                  'J_cons_corps',
                  "K1_notice", "K2_nodistract","K3_notworry",  "K4_emotion","K5_listing","K6_trust",
                  "L_mouv_activite" ,                   
                  "N_influence_pairs",
                  "M1_douleurs_nb" ,    
                  "sit_eco", 
                  "L2_tact_soc")

data_Subscale1 <- data_full[, cols_Subscale1]
data_Raw1 <- data_raw[, cols_Subscale1]

names_full <-c("Sexe", "Cisgender", "Body Mass Index", "Puberty", 
                          "Physical Activity", 
                          "Private Self-Consciousness", "Public Self-Consciousness", "Social Anxiety", 
                          "Perspective Taking", "Empathic Concern", 
                          "Behavioral engagement in Social Media", "Cognitive engagement in Social Media", "Affective engagement in Social Media", 
                          "Body Esteem for Appearance "," Body Esteem Attribution", "Body Esteem for Weight satisfaction", "Body Objectification",
                          "Family Pressure", "Peers Pressure", "Media Pressure", 
                          'Private Body consciousness',
                          "Noticing body sensations", "Not-Distracting","Not-Worrying", "Emotional Awareness", " Body Listening","Body Trusting", 
                          "Mouvement Recherche",
                          "Resistance to Peer Influence", 
                          "Chronic Pain", 
                          "Economic Status",
                          "Social touch")
names(data_Subscale1)<- names_full
names(data_Raw1) <- names_full
        




grSub1 <- list(c(1:4), c(5),c(6:8),
            c(9:10),c(11:13), 
            c(14:17), c(18:20),c(21:26),c(27),c(28),c(29),c(30),c(31))           


names(grSub1) <- c("General", "Physical Activity Level", 
                "Self Consciousness Scale",
                "Interpersonal Reactivity Index", 
                "Social Media Engagement Scale for Adolescents", 
                "Body Esteem Scale ", 
                "Sociocultural Attitudes Towards Appearance Quest", 
                "Multidimensional Assessment of Interoceptive Awareness", 
                "Mouvement Recherche",
                "Resistance to Peer Influence", 
                "Chronic Pain", "Economic Status", "Social Touch Quest")





cor_tot<-cor(data_Subscale1)
cor_raw_tot<-cor(data_Raw1)



##

library(networktools)
gb_dataset<-goldbricker(
  data_Subscale1,
  p = 0.05,
  method = "hittner2003",
  threshold = 0.25,
  corMin = 0.5,
  progressbar = TRUE)
#Suggested reductions: Less than 25 % of correlations are significantly different for the following pairs:  J_cons_corps & K4_emotion 
#0.2068966 

#reduced network based on  gb

red_nodes1 <- net_reduce(data=data_Subscale1, badpairs=gb_dataset)

gb_red_nodes1<-goldbricker(
  red_nodes1,
  p = 0.05,
  method = "hittner2003",
  threshold = 0.25,
  corMin = 0.5,
  progressbar = TRUE)





### 1.1 Make correlation matrix
data_Subscale1.cor<-cor_auto(data_Subscale1) 
data_Raw1.cor<-cor_auto(data_Raw1) 


##### 3. Network 1 

### 3.1 Figure 1

shortnames_nodes<-c("Sex", "Cis", "BMI", "Pub", 
               "PhyAc", 
               "PriSC", "PubSC", "SocAnx", 
               "PerTak", "Emp", 
               "BehSM", "CogSM", "AffSM", 
               "ApBE","AtBE", "WeiBE", "BoObj",
               "FamPree", "PeePres", "MedPres", 
               "NotiB", "NoDiB","NoWoB", "EmoB", "ListB","TrustB", 
               "Mouv",
               "ResPeer",
               "CPain", "Eco","SoTou")

png("TOT_Subscale1_Network.png", width=2000, height=1400)
graph.g1<-qgraph(data_Subscale1.cor, graph="glasso", layout="spring",labels=shortnames_nodes,
                     vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Subscale1),
                     border.width=0.1, border.color="#a8a8a8", minimum=.03, 
                     groups=grSub1, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                                     "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                    legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                    nodeNames=names_full)
dev.off()
centralityPlot(graph.g1,include="All",orderBy = "Betweenness")

pdf("TOT_Subscale1_rawdata_Network.pdf")
graph.raw1<-qgraph(data_Raw1.cor, graph="glasso", layout="spring",
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Raw1),
                 border.width=1.5, border.color="black", minimum=.10, 
                 groups=grSub1, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                                 '#00ffff','#3300ff','#660066','#6666ff',
                                                 '#ffcc00','#cc00ff','#ccff00','#99ccff','#88ffcc'))
centralityPlot(graph.raw1,include="All",orderBy = "Betweenness")
                                                 


### 3.2 Centrality 


#pdf("TOT_Subscale_Centrality1.pdf")
#centralityPlot(graph.g1,include="All")
#dev.off()


#pdf("TOT_Subscale_Centrality2.pdf")
#centralityPlot(graph.g1,include="All",orderBy = "Strength")
#dev.off()



pdf("TOT_Subscale1_Betweenness.pdf")
centralityPlot(graph.g1,include="All",orderBy = "Betweenness")
dev.off()


pdf("TOT_Subscale1_Clustering.pdf")
clusteringPlot(graph.g1)
dev.off()

cluster1=clusteringTable(graph.g1)
central1=centralityTable(graph.g1)



##### 4. Robustness Network 1


### A priori sample size analysis 

#recreate network with estimateNetwork function from package bootnet 
network <- estimateNetwork(
  data_Subscale1,
  default = "EBICglasso",
  corMethod = "cor_auto",
  tuning = 0.5,
  refit = TRUE)
plot(network)


class(network)
simRes <- netSimulator(network$graph,
                       dataGenerator = ggmGenerator(
                         ordinal = TRUE, nLevels = 5),
                       default = "EBICglasso",
                       nCases = c(100,300,600,1000),
                       tuning = 0.5,
                       nReps =28  )


plot(simRes)
plot(simRes,
     yvar = c("strength","closeness","betweenness"))


###### Post-hoc  analysis
### edge weightaccuracy bootstrap ==> non-parametric (resampling rows from the data
#with replacement) bootstrap can be used to assess the accuracy
#of network estimation, by investigating the sampling
#variability in edge-weights, as well as to test if edge-weights
#and centrality indices significantly differ from one-another
#using the bootstrapped difference test

m0<-bootnet(data_Subscale1, nBoots=1000, default="EBICglasso",  type="nonparametric", statistics='edge')
summary(m0)
file_name <- paste0("TOT_Subscale1_bootstrapedge.pdf")
pdf(file_name, width = 5, height = 15, useDingbats=FALSE)
plot(m1, labels = FALSE, order = "sample")
dev.off()

boot0<-bootnet(network, nBoots=2500, nCores=8,type="nonparametric")
summary(boot0)
plot(boot0, order = "sample")

boot1<-bootnet(network, nBoots=2500, nCores=8,type="nonparametric",statistics = "rspbc")
summary(boot1)
plot(boot1,statistics = "rspbc")

boot3<-bootnet(network, nBoots=2500, nCores=8,type="nonparametric",statistics = "betweenness")
summary(boot1)
plot(boot1, labels = FALSE,statistics = "rspbc")
plot(boot1, "betweeness")

boot4 <- bootnet(data_Subscale1, 100, default = "pcor", type = "parametric")
plot(boot4, statistics = c("strength", "closeness", "betweenness"),
     CIstyle = "quantiles")

boot4 <- bootnet(network, 100, default = "pcor", type = "parametric")
plot(boot4, statistics = c("strength", "closeness", "betweenness"),
     CIstyle = "quantiles")



## Testing for significant differences  ==> compare edge-weights and centralities using the bootstrapped difference test
differenceTest(boot1, 3, 17, "strength")  # tests if Node 3 and Node 17 differ in node strength centrality

plot(boot1, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample") ## plots the difference tests of node strength between all pairs of edge-weights



#######centrality stability ==> casedropping ==> to know how well the order of centralities are retained after observing only a subset of the data
#quantify with the CS-coefficient 

boot2 <- bootnet(network, nBoots = 2500,
                  type = "case", nCores = 8)
plot(boot2)
corStability(boot2)



### 4.2 node dropping bootstrap

m2<- bootnet(network, default = "EBICglasso", nBoots=25000, type = "node",computeCentrality=TRUE, statistics = "betweenness")
pdf("TOT_Subscale1_bootstrapnodes.pdf", useDingbats=FALSE) 
plot(m2, statistics = "betweenness")
plot(m2, statistics = "betweenness", perNode = TRUE)
dev.off()


m4<- bootnet(network, default = "EBICglasso", nBoots=2500, type = "case",computeCentrality=TRUE, statistics = "betweenness")
plot(m4, statistics = "betweenness")
corStability(m4)


### person dropping
m3<- bootnet(data_Subscale1, default = "EBICglasso", nBoots=1000, type = "person")
pdf("TOT_Subscale1_bootstrapnodes_pearson .pdf", useDingbats=FALSE) 
plot(m3)
plot(m3, "strength", perNode = TRUE, legend = FALSE)
plot(m3, "betweenness", perNode = TRUE, legend = FALSE)
plot(m3, "closeness", perNode = TRUE, legend = FALSE)
dev.off()





cols_Scale0<-c( "A1_sexe",  "A2_diff",   "A6_IMC", 
                "B_puberte", 
                "C_AP", 
                "D_cons_soi",
                "E_IRI",
                "G_reseau",  
                "H_estime_corp", "I_objectifi",  
                "II_press_soc", 
                "N_influence_pairs",
                "J_cons_corps", "K_intero",  
                "M1_douleurs_nb" ,
                "sit_eco")  

data_Scale0 <- data_full[, cols_Scale0]

grSc0<-list(c(1:3), c(4),c(5),c(6),c(7),c(8),
            c(9:10),c(11),c(12), c(13:14), 
            c(15),c(16))      

data_Scale0.cor<-cor_auto(data_Scale0) 

pdf("TOT_Scale0_Network.pdf")
graph.g0<-qgraph(data_Scale0.cor, graph="glasso", layout="spring", 
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Scale0),
                 border.width=1.5, border.color="black", minimum=.03, 
                 groups=grSc0, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                                '#00ffff','#3300ff','#660066','#6666ff',
                                                '#ffcc00','#cc00ff','#ccff00','#99ccff'))
                                                dev.off()



pdf("TOT_Scale0_Centrality1.pdf")
centralityPlot(graph.g0,include="All")
dev.off()
cluster0=clusteringTable(graph.g0)
central0=centralityTable(graph.g0)

                                                
                                                
                                                
                                                

######
######
######
###### Analyse en enlevant certains nodes




cols_to_keep1<-c ( "A1_sexe","A2_diff","A3_age_m","A6_IMC", 
                   "C_AP", 
                   "D1_priv","D2_pub", "D3_anx_soc",
                   "E1_perspec","E2_empat",  
                   "F2_reseauquant",
                   "H1_appa","H2_attri",  "H3_poids",  "I_objectifi", 
                   "K1_notice", "K2_nodistract", "K3_notworry","K4_emotion","K5_listing","K6_trust",
                   "M1_douleurs_nb",
                   "N_influence_pairs", 
                   "sit_eco")

data_selec1<- data_full[, cols_to_keep1]

data_selec1.cor<-cor_auto(data_selec1) 

gr3 <- list(c(1:4), 
            c(5),
            c(6:8),
            c(9:10),
            c(11), 
            c(12:15), 
            c(16:21),
            c(22),c(23),c(24))   


pdf("TOT_Subscale_select1_Network.pdf")
graph.g1<-qgraph(data_selec1.cor, graph="glasso", layout="spring", 
                vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_selec1),
                border.width=1.5, border.color="black", minimum=.03, 
                groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                             '#00ffff','#3300ff','#660066','#6666ff',
                                             '#cc00ff','#ffcc00','#ccff00'))
                                             

dev.off()


pdf("TOT_Subscale_select1_Centrality1.pdf")
centralityPlot(graph.g1,include="All")
dev.off()


pdf("TOT_Subscale_select1_Centrality2.pdf")
centralityPlot(graph.g1,include="All",orderBy = "Strength")
dev.off()


pdf("TOT_Subscale_select1_Centrality3.pdf")
centralityPlot(graph.g1,include="All",orderBy = "Betweenness")
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

