library(sjPlot)
library(ggcorrplot)
library(corpcor)
library(glasso)
#library(mgm)  #for mixed model networks 
library(igraph)
library(psych)
library(GPArotation)
library(Matrix)
library(psych)

library(mice, warn.conflicts = FALSE)
#library(qgraph)
library(lavaan)



setwd("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/validity_consistency")

Scale_bysub <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_bysub.csv")

Scale_bysub [ , c( "X" ,"numquest" ,"date_nais","A2_genre", "A3_age_y", "A4_taille","A5_poids" ,"B_puberte","B4_pub_gon","B4_pub_adre","B2_regles","B_pub_group","D_cons_soi",       
"E_IRI","G_reseau","H_estime_corp" ,"II_press_soc","K_intero","L1a_tact_dis_rech","L1b_tact_dis_seuil", "L2_tact_soc","L3_mouv_rech" ,"L3_mouv_habil","L4_act_rech" ,
"L4_act_seuil", "classe","filiere","diplome_parent","langue","main","departement","connu_par" )] <- list(NULL)



#Convertir les colonnes spécifiées en variables ordinales
library(dplyr)
Scale_bysub <- Scale_bysub %>% mutate(A1_sexe = as.factor(A1_sexe),
                                       A2_diff = as.factor(A2_diff),
                                       C_AP = as.ordered(C_AP),
                                       F_reseau = as.ordered(F_reseau),
                                       M_douleurs = as.ordered(M_douleurs),
                                       M1_douleurs_nb = as.ordered(M1_douleurs_nb),
                                       sit_eco = as.ordered(sit_eco))

renv::activate()

############################################# 
############################################# 1MIssing data
############################################# 
#estimer si aléatoire ou non 
library(naniar)
miss<-miss_var_summary(Scale_bysub)
MCAR<-mcar_test(Scale_bysub)



############################################# 
############################################# Network structure estimation
############################################# 
library(foreign)
library(qgraph)
library(bootnet)
### 1.1 Make correlation matrix
Scale_bysub.cor<-cor_auto(Scale_bysub) #data including covariates
ggcorrplot(Scale_bysub.cor)

data.glasso<-glasso(Scale_bysub.cor,0.25)



### 1.2 Create names object & group object for graph

names=names(Scale_bysub)
gp_A<-c("A3_age_m","A4_taille","A5_poids","A6_IMC","B_puberte" )
gp_C<-c("C_AP")
gp_D<-c("D1_priv","D2_pub","D3_anx_soc")
gp_E<-c("E1_perspec","E2_empat")
gp_F<-c("F2_reseauquant","G1_comp","G2_cog","G3_affec")
gp_HI<-c("H1_appa","H2_attri","H3_poids","I_objectifi")
gp_II<-c("II1_fam","II2_pairs","II3_reseau")
gp_JK<-c("J_cons_corps","K1_notice","K2_nodistract","K3_notworry","K4_emotion","K5_listing","K6_trust")
gp_L<-c("L1a_tact_dis_rech","L1b_tact_dis_seuil","L2_tact_soc","L3_mouv_rech","L3_mouv_habil","L4_act_rech","L4_act_seuil")
gp_M<-c("M1_douleurs_nb")
gp_N<-c("N_influence_pairs")

gr3<-list(gp_A,gp_C,gp_D,gp_E,gp_F,gp_HI,gp_II,gp_JK,gp_L,gp_M,gp_N)

gr3 <- list(c(1:5), c(6),c(7:9),
            c(10:11),c(12:15), 
            c(16:19), c(20:22),c(23:29),
            c(30:36),c(37),c(38))           


##### 2. Demographics
mean(data$A3_age_m)                      # 200.0565
sd(data$A3_age_m)                        #40.10271
range(data$A3_age_m)                     #121 303

##### 3. Network 1 
graph.m <-help(EBICglasso(data.cor, n = nrow(data),gamma = 0.5,threshold=TRUE) 
               
               ### 3.1 Figure 1
               pdf("Fig1_Ar.pdf")
               graph.g<-qgraph(data.cor, graph="glasso", layout="spring", 
                               vsize=7, cut=0, maximum=.45, sampleSize = nrow(data),
                               border.width=1.5, border.color="black", minimum=.03, 
                               groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                                            '#00ffff','#3300ff','#660066','#6666ff',
                                                            '#cc00ff','#ffcc00','#ccff00'))
                                                            dev.off()
                                                            