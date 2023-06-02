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

setwd("C:/Users/lisa/Documents/-These/MANIP_Comp/Share/Analyse/Results_PS_Quest")


##### 1 DATA 
data_full <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Comp/Share/Analyse/python/Result_PS_Quest.csv")

cols_Subscale1_Comp <-c("A2_diff", "A6_IMC", 
                   "B_puberte", 
                   "C_AP", 
                   "D1_priv",   "D2_pub",  "D3_anx_soc",
                   "E1_perspec","E2_empat", 
                   "G1_comp",   "G2_cog",    "G3_affec",  
                   "H1_appa",   "H2_attri",  "H3_poids",  "I_objectifi", 
                   "II1_fam","II2_pairs", "II3_reseau",
                   "N_influence_pairs",
                   "J_cons_corps",  "K1_notice", "K2_nodistract","K3_notworry",  "K4_emotion","K5_listing","K6_trust",
                   "M1_douleurs_nb" ,    
                   "sit_eco"  )  
data_Subscale1_Comp <- data_full[, cols_Subscale1_Comp]

grSub1<-list(c(1:2), c(3),c(4),c(5:7),
             c(8:9),c(10:12), c(13:16), 
             c(17:19), c(20),c(21:27),c(28),c(29)) 


cols_Subscale_PS_Comp<-c(  "A2_diff", "A6_IMC", 
                "B_puberte", 
                "C_AP", 
                "D1_priv",   "D2_pub",  "D3_anx_soc",
                "E1_perspec","E2_empat", 
                "G1_comp",   "G2_cog",    "G3_affec",  
                "H1_appa",   "H2_attri",  "H3_poids",  "I_objectifi", 
                "II1_fam","II2_pairs", "II3_reseau",
                "N_influence_pairs",
                "J_cons_corps",  "K1_notice", "K2_nodistract","K3_notworry",  "K4_emotion","K5_listing","K6_trust",
                "M1_douleurs_nb" ,    
                "sit_eco",
                "tactile","HBDTzam","ET_er_sig1__F20","moy_er_abs_F20",
                "RHIdrift_SmoinsA","RHIQuestTot_SmoinsA")  

data_Subscale_PS_Comp <- data_full[, cols_Subscale_PS_Comp]

grSub_PS<-list(c(1:2), c(3),c(4),c(5:7),
               c(8:9),c(10:12), c(13:16), 
               c(17:19), c(20),c(21:27),c(28),c(29),c(30:33),c(34:35))      



### 1.1 Make correlation matrix
data_Subscale1_Comp.cor<-cor_auto(data_Subscale1_Comp) 
if (!is.positive.definite(data_Subscale1_Comp.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_Subscale1_Comp.cor <- nearPD(data_Subscale1_Comp.cor)$mat
}

data_Subscale_PS_Comp.cor<-cor_auto(data_Subscale_PS_Comp) 
if (!is.positive.definite(data_Subscale_PS_Comp.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data_Subscale_PS_Comp.cor <- nearPD(data_Subscale_PS_Comp.cor)$mat
}

library(stats)
library(corrplot)
cor_matrix <- cor(data_full, method = "pearson",use = "pairwise.complete.obs")
# Calculer les p-values pour chaque corrélation
p_values <- cor_pmat(data_full, method = "pearson",use = "pairwise.complete.obs")
# Appliquer le seuil pour ne garder que les corrélations significatives
cor_matrix[p_values >= 0.05] <- NaN

# Créer un graphique de la matrice de corrélation
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 45)
data.glasso<-glasso(data.cor,0.25)


##### 3. Network 1 

### 3.1 Figure 1

pdf("PS_Quest_Network.pdf")
graph.g<-qgraph(data_Subscale1_Comp.cor, graph="glasso", layout="spring", 
                vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Subscale1_Comp),
                border.width=1.5, border.color="black", minimum=.03, 
                groups=grSub1, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                               '#00ffff','#3300ff','#660066','#6666ff',
                                               '#ffcc00','#cc00ff','#ccff00','#99ccff'))
dev.off()

pdf("PS_Quest_Network.pdf")
graph.g<-qgraph(data_Subscale_PS_Comp.cor, graph="glasso", layout="spring", 
                vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_Subscale_PS_Comp),
                border.width=1.5, border.color="black", minimum=.03, 
                groups=grSub_PS, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                                '#00ffff','#3300ff','#660066','#6666ff',
                                                '#ffcc00','#cc00ff','#ccff00','#99ccff'))
dev.off()
                                                
                                             
                                             
 
 ### 3.2 Centrality 
 
 pdf("PS_Quest_Centrality2_Ar.pdf")
 centralityPlot(graph.g)
 dev.off()
 
 
 pdf("PS_Quest_Centrality_Ar.pdf")
 centralityPlot(graph.g,include="All")
 dev.off()
 
 pdf("PS_Quest_Centrality3_Ar.pdf")
 clusteringPlot(graph.g)
 dev.off()