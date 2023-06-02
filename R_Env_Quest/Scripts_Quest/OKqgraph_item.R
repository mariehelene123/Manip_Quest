##############################################################


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

library(mice, warn.conflicts = FALSE)
#library(qgraph)
library(lavaan)



setwd("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Network")

Item_bysub <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Item_bysub.csv")

cols_to_keep<-c("A1_sexe","A2_diff",  "A3_age_m","A6_IMC",   
                "B01",  "B02","B03","B04.07",   "B05.08", 
                "C_AP",     
                "Dcons01",  "Dcons02",  "Dcons03",  "Dcons04",  "Dcons05",  "Dcons06","Dcons07",  "Dcons08",  "Dcons09",  "Dcons10",  "Dcons11",  "Dcons12",  "Dcons13",  "Dcons14",  
                "Dcons15",  "Dcons16",  "Dcons17",  "Dcons18","Dcons19",  "Dcons20",  "Dcons21",  "Dcons22",  "Dcons23",  
                "EIRI01",   "EIRI02",   "EIRI03",   "EIRI04",   "EIRI05",   "EIRI06",   "EIRI07",  "EIRI08",   "EIRI09",   "EIRI10",   "EIRI11",   "EIRI12",   "EIRI13",   "EIRI14",
                "F_reseau", "F02","F03","Greseau01","Greseau02","Greseau03","Greseau04","Greseau05","Greseau06","Greseau07","Greseau08","Greseau09","Greseau10","Greseau11",
                "Hest01",   "Hest02",   "Hest03", "Hest04",   "Hest05",   "Hest06",   "Hest07",   "Hest08",   "Hest09",   "Hest10",   "Hest11",   "Hest12",   "Hest13",   "Hest14",
                "Hest15", "Hest16",   "Hest17",   "Hest18",   "Hest19",   "Hest20",   "Hest21",   "Hest22",   "Hest23",   
                "Iobj01",   "Iobj02",   "Iobj03",   "Iobj04", "Iobj05",   "Iobj06",   "Iobj07", "Iobj08", 
                "IIpress01","IIpress02","IIpress03","IIpress04","IIpress05","IIpress06","IIpress07","IIpress08" ,"IIpress09","IIpress10","IIpress11","IIpress12",
                "Jcons01",  "Jcons02",  "Jcons03",  "Jcons04",  "Jcons05",  "Jcons06",  "Jcons07",  
                "Kinter01","Kinter02", "Kinter03", "Kinter04", "Kinter05", "Kinter06", "Kinter07", "Kinter08", "Kinter09", "Kinter10", "Kinter11", "Kinter12", "Kinter13",
                "Kinter14", "Kinter15", "Kinter16", "Kinter17", "Kinter18", "Kinter19", "Kinter20", "Kinter21", 
                "L1tact01", "L1tact02", "L1tact03", "L1tact04","L1tact05", "L1tact06", "L1tact07", 
                "L2tact01", "L2tact02", "L2tact03", "L2tact04", "L2tact05", "L2tact06", "L2tact07", "L2tact08", 
                "L3mouv01","L3mouv02", "L3mouv03", "L3mouv04", "L3mouv05", "L3mouv06", "L3mouv07", "L3mouv08", 
                "L4act01",  "L4act02",  "L4act03",  "L4act04",  "L4act05", "L4act06",  "L4act07",  "L4act08",  "L4act09",  "L4act10",  
                "M1_douleurs_nb" ,
                "NLpairs01","NLpairs02","NLpairs03","NLpairs04","NLpairs05","NLpairs06","NLpairs07","NLpairs08","NLpairs09","NLpairs10",
                "sit_eco")  

data_tri <- Item_bysub[, cols_to_keep]



############################################# 1MIssing data

estimer si aléatoire ou non 
library(naniar)
miss<-miss_var_summary(Item_bysub)
MCAR<-mcar_test(Item_bysub)

#En résumé, le test MCAR suggère que les données sont manquantes de manière aléatoire, car 
#la p-valeur est supérieure à 0,05 (valeur de référence souvent utilisée pour rejeter ou ne pas rejeter l'hypothèse nulle).


# Utiliser FIML pour remplacer les valeurs manquantes
library(lavaan)
mydata_imputed <- lavOptions(Item_bysub, missing = "fiml")

# Afficher le dataframe avec les valeurs manquantes remplacées
mydata_imputed

# Spécifier le nombre d'imputations (ici, 5)
n_imputations <- 5
# Imputer les valeurs manquantes avec la fonction "mice"
imputed <- mice(Item_bysub, m= n_imputations, method = NULL, , ignore = NULL, where = NULL, , visitSequence = NULL, blots = NULL,
            post = NULL, maxit = 5, printFlag = TRUE,seed = NA, data.init = NULL)

# Accéder aux données imputées pour la première imputation
imputed_data1 <- complete(imp, 1)
imputed_data2 <- complete(imp, 2)


# Vérifier s'il y a des valeurs manquantes dans les données imputées
any(is.na(imputed_data2))
"""

###########################Item selection 


##########################
#########################




### 1.1 Make correlation matrix
library(stats)
data.cor<-cor_auto(data_tri) 
#ggcorrplot(data.cor)

#si pb de 
# Calculer la matrice de corrélation
#data.cor <- cor(data_tri, method = "pearson", use = "pairwise.complete.obs")

# Vérifier si la matrice est positive définie
if (!is.positive.definite(data.cor)) {
  # Effectuer une correction pour rendre la matrice positive définie
  data.cor <- nearPD(data.cor)$mat
}

"""
# Appliquer la méthode EBICglasso à la matrice de corrélation
resultat <- EBICglasso(S = data.cor, n = nrow(data_tri))
data.glasso<-glasso(data.cor,0.25)
"""




### 1.2 Create names object & group object for graph
gp_A<-c("A1_sexe","A2_diff",  "A3_age_m","A6_IMC") 
gp_B<-c("B01",  "B02","B03","B04.07",   "B05.08")
gp_C<-c("C_AP")     
gp_D<-c(  "Dcons01",  "Dcons02",  "Dcons03",  "Dcons04",  "Dcons05",  "Dcons06","Dcons07",  "Dcons08",  "Dcons09",  "Dcons10",  "Dcons11",  "Dcons12",  "Dcons13",  "Dcons14",  
          "Dcons15",  "Dcons16",  "Dcons17",  "Dcons18","Dcons19",  "Dcons20",  "Dcons21",  "Dcons22",  "Dcons23")  
gp_E<-c("EIRI01",   "EIRI02",   "EIRI03",   "EIRI04",   "EIRI05",   "EIRI06",   "EIRI07",  "EIRI08",   "EIRI09",   "EIRI10",   "EIRI11",   "EIRI12",   "EIRI13",   "EIRI14")
gp_F<-c(  "F_reseau", "F02","F03","Greseau01","Greseau02","Greseau03","Greseau04","Greseau05","Greseau06","Greseau07","Greseau08","Greseau09","Greseau10","Greseau11")

gp_G<-c(  "Hest01",   "Hest02",   "Hest03", "Hest04",   "Hest05",   "Hest06",   "Hest07",   "Hest08",   "Hest09",   "Hest10",   "Hest11",   "Hest12",   "Hest13",   "Hest14",
          "Hest15", "Hest16",   "Hest17",   "Hest18",   "Hest19",   "Hest20",   "Hest21",   "Hest22",   "Hest23",   
          "Iobj01",   "Iobj02",   "Iobj03",   "Iobj04", "Iobj05",   "Iobj06",   "Iobj07","Iobj08")  
gp_H<-c("IIpress01","IIpress02","IIpress03","IIpress04","IIpress05","IIpress06","IIpress07","IIpress08" ,"IIpress09","IIpress10","IIpress11","IIpress12")
gp_I<-c("Jcons01",  "Jcons02",  "Jcons03",  "Jcons04",  "Jcons05",  "Jcons06",  "Jcons07",  
  "Kinter01","Kinter02", "Kinter03", "Kinter04", "Kinter05", "Kinter06", "Kinter07", "Kinter08", "Kinter09", "Kinter10", "Kinter11", "Kinter12", "Kinter13",
  "Kinter14", "Kinter15", "Kinter16", "Kinter17", "Kinter18", "Kinter19", "Kinter20", "Kinter21")
gp_J<-c("L1tact01", "L1tact02", "L1tact03", "L1tact04","L1tact05", "L1tact06", "L1tact07")
gp_K<-c( "L2tact01", "L2tact02", "L2tact03", "L2tact04", "L2tact05", "L2tact06", "L2tact07", "L2tact08")
gp_L<-c(  "L3mouv01","L3mouv02", "L3mouv03", "L3mouv04", "L3mouv05", "L3mouv06", "L3mouv07", "L3mouv08")
gp_M<-c(  "L4act01",  "L4act02",  "L4act03",  "L4act04",  "L4act05", "L4act06",  "L4act07",  "L4act08",  "L4act09",  "L4act10") 
gp_N<-c(  "M1_douleurs_nb")
gp_O<-c(  "NLpairs01","NLpairs02","NLpairs03","NLpairs04","NLpairs05","NLpairs06","NLpairs07","NLpairs08","NLpairs09","NLpairs10")
gp_P<-c(  "sit_eco")  

gr3<-list(gp_A,gp_B,gp_C,gp_D,gp_E,gp_F,gp_G,gp_H,gp_I,gp_J,gp_K,gp_L,gp_M,gp_N,gp_O,gp_P)

gr3 <- list(c(1:4), #A
            c(5:9),  #B
            c(10),   #C
            c(11:33),  #D
            c(34:47),  #E
            c(48:61), #F
            c(62:93),  #G
            c(94:105),  #H
            c(106:132),  #I
            c(133:139),  #J
            c(140:147),   #K
            c(148:155),  #L
            c(156:165),  #M
            c(166),  #N
            c(167:176), #O
            c(177))  #P


##### 3. Network 1 

### 3.1 Figure 1

pdf("TOT_Item_Network.pdf")
graph.g<-qgraph(data.cor, graph="glasso", layout="spring", 
                vsize=7, cut=0, maximum=.45, sampleSize = nrow(data_tri),
                border.width=1.5, border.color="black", minimum=.03, 
                groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                             '#00ffff','#3300ff','#660066','#6666ff',
                                             '#cc00ff','#ffcc00','#ccff00','#ffff99'))
dev.off()


pdf("TOT_Item_Centrality1.pdf",height=20)
centralityPlot(graph.g,include="All")
dev.off()


pdf("TOT_Item_ClusteringPlot.pdf",height=20)
clusteringPlot(graph.g)
dev.off()





clusteringTable(graph.g)


