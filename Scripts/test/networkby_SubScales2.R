
Scale_bysub <- read.csv("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Scale_bysub.csv")

Scale_bysub [ , c( "X" ,"numquest" ,"date_nais","A2_diff","A2_genre", "A3_age_y", "A4_taille","A5_poids" ,"B_puberte","B4_pub_gon","B4_pub_adre","B2_regles","B_pub_group","D_cons_soi",       
"E_IRI","G_reseau","H_estime_corp" ,"II_press_soc","K_intero","L1a_tact_dis_rech","L1b_tact_dis_seuil", "L2_tact_soc","L3_mouv_rech" ,"L3_mouv_habil","L4_act_rech" ,
"L4_act_seuil", "M_douleurs","classe","filiere","diplome_parent","langue","main","departement","connu_par" )] <- list(NULL)



#Convertir les colonnes spécifiées en variables ordinales
library(dplyr)
Scale_bysub <- Scale_bysub %>% mutate(A1_sexe = as.ordered(A1_sexe),
                                       A2_diff = as.ordered(A2_diff),
                                       C_AP = as.ordered(C_AP),
                                       F_reseau = as.ordered(F_reseau),
                                       M_douleurs = as.ordered(M_douleurs),
                                       M1_douleurs_nb = as.ordered(M1_douleurs_nb),
                                       sit_eco = as.ordered(sit_eco))

Scale_bysub <- sapply(Scale_bysub, as.numeric)

#renv::activate()
setwd("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/Network")




############################################# 
############################################# MIssing data
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

### Make correlation matrix
Scale_bysub.cor<-cor_auto(Scale_bysub)

library(ggcorrplot)
ggcorrplot(Scale_bysub.cor)

#data.glasso<-glasso(Scale_bysub.cor,0.25)



### 1.2 Create names object & group object for graph

names=names(Scale_bysub)
gp_1<-c("A1_sexe","A3_age_m","A6_IMC")
gp_2<-c("C_AP")
gp_3<-c("D1_priv","D2_pub","D3_anx_soc")
gp_4<-c("E1_perspec","E2_empat")
gp_5<-c("F2_reseauquant","G1_comp","G2_cog","G3_affec")
gp_6<-c("H1_appa","H2_attri","H3_poids","I_objectifi")
gp_7<-c("II1_fam","II2_pairs","II3_reseau")
gp_8<-c("J_cons_corps","K1_notice","K2_nodistract","K3_notworry","K4_emotion","K5_listing","K6_trust")
gp_9<-c("M1_douleurs_nb")
gp_10<-c("N_influence_pairs")
gp_11<-c("sit_eco")

gr3<-list(gp_1,gp_2,gp_3,gp_4,gp_5,gp_6,gp_7,gp_8,gp_9,gp_10,gp_11)

gr3 <- list(c(1:3),
            c(4),
            c(5:7),
            c(8:9),
            c(10:13), 
            c(14:17),
            c(18:20),
            c(21:27),
            c(28),
            c(29),
            c(30))           



library(Matrix)
Scale_bysub2.cor <- nearPD(Scale_bysub.cor)
Scale_bysub2.cor$mat
Scale_bysub2.cor$error_message



### 3.1 Figure 1
pdf("Subscale_qgraph.pdf")
graph.g<-qgraph(Scale_bysub.cor, graph="glasso", layout="spring", 
   vsize=7, cut=0, maximum=.45, sampleSize = nrow(Scale_bysub),
   border.width=1.5, border.color="black", minimum=.03, 
   groups=gr3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ff8b94',
                                '#00ffff','#3300ff','#660066','#6666ff',
                                '#cc00ff','#ffcc00','#ccff00'))
dev.off()


pdf("glasso2_Ep.pdf")
graph2 <- estimateNetwork(
  Scale_bysub,
  default = "EBICglasso",
  corMethod = "cor_auto",
  tuning = 0.25)
plot(graph2)
dev.off()
