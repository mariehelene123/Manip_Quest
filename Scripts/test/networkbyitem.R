##############################################################

library(foreign)
#library(qgraph)
#library(bootnet)
library(glasso)
#library(mgm)  #for mixed model networks 
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
install.packages("psychonetrics")



setwd("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/validity_consistency")

Item_bysub <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Item_bysub.csv")

Item_bysub [ , c("X","numquest","A2_genre","A3_age_y","A4_taille","A5_poids","Règles",
"L1tact01","L1tact02","L1tact03","L1tact04","L1tact05","L1tact06",     
"L1tact07","L2tact01","L2tact02","L2tact03","L2tact04","L2tact05","L2tact06","L2tact07","L2tact08","L3mouv01","L3mouv02","L3mouv03",      
"L3mouv04","L3mouv05","L3mouv06","L3mouv07","L3mouv08","L4act01","L4act02","L4act03","L4act04","L4act05","L4act06","L4act07",       
"L4act08","L4act09","L4act10","classe","filiere","diplome_parent","langue","main","departement" )] <- list(NULL)


############################################# 1MIssing data

#estimer si aléatoire ou non 
library(naniar)
miss<-miss_var_summary(Item_bysub)
MCAR<-mcar_test(Item_bysub)
"""
En résumé, le test MCAR suggère que les données sont manquantes de manière aléatoire, car 
la p-valeur est supérieure à 0,05 (valeur de référence souvent utilisée pour rejeter ou ne pas rejeter l'hypothèse nulle).
"""

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


###########################Item selection 


##########################
#########################
library("psychonetrics")
library("dplyr")

