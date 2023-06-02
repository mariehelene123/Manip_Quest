renv::activate()
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
library(visdat)
library(naniar) ##lille MCAR test
library(mice, warn.conflicts = FALSE)
#library(qgraph)
library(lavaan)

##https://naniar.njtierney.com/articles/getting-started-w-naniar.html



#save.image("../R_Env_Quest/missing.RData")


#Item_bysub <- read.csv("../../Share/Analyses/Item_bysub_num.csv")
#Scale_bysub <- read.csv("../../Share/Analyses/Scale_bysub_num.csv")
#SubScale_bysub <- read.csv("../../Share/Analyses/SubScale_bysub_num.csv")


###VIsualiser les données
vis_dat(Measures_bysub)
gg_miss_var(Measures_bysub)
gg_miss_fct(Measures_bysub, A1_sexe) 

#sub <- subset(SubScale_bysub, !(A1_sexe == 2 & is.na(K6_trust)))
#sub <- subset(sub, !(A1_sexe == 1 & is.na(K6_trust)))
#sub <- subset(sub, !(A1_sexe == 2 & is.na(C_AP)))
#sub <- subset(sub, !(A1_sexe == 1 & is.na(C_AP)))
#gg_miss_fct(sub, A1_sexe) 



#estimer si aléatoire ou non aveec Naniar => These leads to either the decision to use 
##to use simple random => appropriate if one can be certain that the means are not different for different missingness patterns
##to model the missingness mechanism and use that model for imputation 
#En résumé, le test MCAR suggère que les données sont manquantes de manière aléatoire, car 
#la p-valeur est supérieure à 0,05 (valeur de référence souvent utilisée pour rejeter ou ne pas rejeter l'hypothèse nulle).

MCAR_measure<-mcar_test(Measures_bysub)

#MCAR_Item_bysub<-mcar_test(Item_bysub)
#MCAR_Scale_bysub<-mcar_test(Scale_bysub)
#MCAR_SubScale_bysub<-mcar_test(SubScale_bysub)


### test missing at random 
# Créer une variable "missingness" qui prend une valeur de 1 si des données sont manquantes pour un participant et une valeur de 0 sinon
Measures_bysub$missingness <- apply(Measures_bysub, 1, function(x) sum(is.na(x)) > 0)

# Effectuer la régression logistique en utilisant "missingness" comme variable dépendante et "sexe" et "A3_age_m" comme prédicteurs pour savoir si missing dépend de quelque chose
logistic_model <- glm(missingness ~ A1_sexe + A3_age_m, data = Measures_bysub, family = "binomial")

# Examinez les p-values associées aux coefficients de l'âge et du sexe dans la régression logistique
summary(logistic_model)


# Utiliser FIML pour remplacer les valeurs manquantes
library(lavaan)
mydata_imputed <- lavOptions(Measures_bysub)
model <- ' # Spécifiez votre modèle en utilisant la syntaxe du package lavaan '
# Utiliser la fonction fiml() pour imputer les données manquantes
imputed_data <- fiml(model, data, missing = c("nom_variable1", "nom_variable2", "nom_variable3"))
# Extraire les données imputées à l'aide de la fonction complete()
imputed_data <- complete(imputed_data)
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

save.image("../R_Env_Quest/missing.RData")
