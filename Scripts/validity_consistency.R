################################################################
#####                                                      #####
#####         ANALYSE ADO BODY CONSCIOUSNESS               #####
#####                       network analysis SCRIPTS       #####
#####                                                      #####
################################################################
renv::activate()
library(psych)
#library(foreign)
#library(qgraph)
#library(bootnet)
#library(glasso)
#library(mgm)  #for mixed model networks 
#library(igraph)
#library(psych)
#library(GPArotation)
#library(ggplot2)
#library(ggcorrplot)
#library(corpcor)
#library(Matrix)

#setwd("C:/Users/lisa/Documents/-These/MANIP_Quest/Share/Analyses/validity_consistency")
#save.image("../R_Env_Quest/validity.RData")


##### 1 DATA 
Item_bysub <- read.csv("../../Share/Analyses/Item_bysub_num.csv")
dataglobal <- read.csv("../../Share/Analyses/exporterdansR.csv")


# Sélectionner les colonnes que vous souhaitez inclure dans le calcul de l'alpha de Cronbach
Dcons_col <- c("Dcons01", "Dcons02", "Dcons04", "Dcons05", "Dcons06", "Dcons07", "Dcons08", "Dcons09", "Dcons10", "Dcons11", 
          "Dcons12", "Dcons13", "Dcons14", "Dcons15", "Dcons16", "Dcons17", "Dcons18", "Dcons19", "Dcons20", "Dcons21", "Dcons22", "Dcons23")
Dcons <- Item_bysub[,Dcons_col]
# Calculer l'alpha de Cronbach pour les données sélectionnées
alpha_Dcons0<-alpha(Dcons)
alpha_Dcons<-round(alpha_Dcons0$total$raw_alpha,digit=2)
#Some items ( Dcons03 ) were negatively correlated with the total scale and =>> donc je supprime le D3 dans le script spyder
#raw alpha 0.86

Dpriv_col<- c("Dcons01","Dcons05", "Dcons07", "Dcons09", "Dcons13", "Dcons15", "Dcons18","Dcons20","Dcons22")
Dpub_col<- c("Dcons02","Dcons06", "Dcons11", "Dcons14", "Dcons17", "Dcons19", "Dcons21")
Danx_col<- c("Dcons04","Dcons08", "Dcons10", "Dcons12", "Dcons16", "Dcons23")
Dpriv <- Item_bysub[,Dpriv_col]
Dpub <- Item_bysub[,Dpub_col]
Danx <- Item_bysub[,Danx_col]
alpha_Dpriv<-round(alpha(Dpriv)$total$raw_alpha,digit=2)
alpha_Dpub<-round(alpha(Dpub)$total$raw_alpha,digit=2)
alpha_Danx<-round(alpha(Danx)$total$raw_alpha,digit=2)


EIRI_col<-c('EIRI01','EIRI02','EIRI03','EIRI04','EIRI05','EIRI06','EIRI07','EIRI08','EIRI09',
            'EIRI10','EIRI11','EIRI12','EIRI13','EIRI14')
EIRI<- Item_bysub[,EIRI_col]
alpha_EIRI0<-alpha(EIRI)
alpha_EIRI<-round(alpha_EIRI0$total$raw_alpha,digit=2)
#raw alpha 0.79
EPT_col<-c('EIRI02','EIRI04','EIRI06','EIRI08','EIRI11','EIRI13')
EEC_col<-c('EIRI01','EIRI03','EIRI05','EIRI07','EIRI09','EIRI10','EIRI12','EIRI14')
EPT <- Item_bysub[,EPT_col]
EEC <- Item_bysub[,EEC_col]
alpha_EPT<-round(alpha(EPT)$total$raw_alpha,digit=2)
alpha_EEC<-round(alpha(EEC)$total$raw_alpha,digit=2)


Greseau_col<-c('Greseau01', 'Greseau02','Greseau03','Greseau04', 'Greseau05','Greseau06',
            'Greseau07', 'Greseau08','Greseau09','Greseau10', 'Greseau11')
Greseau<- Item_bysub[,Greseau_col]
alpha_Greseau0<-alpha(Greseau)
alpha_Greseau<-round(alpha_Greseau0$total$raw_alpha,digit=2)
#raw alpha 0.89
Gcomp_col<-c('Greseau01', 'Greseau02','Greseau03','Greseau04')
Gcog_col<-c('Greseau05','Greseau06','Greseau07')
Gaffec_col<-c( 'Greseau08','Greseau09','Greseau10', 'Greseau11')
Gcomp <- Item_bysub[,Gcomp_col]
Gcog <- Item_bysub[,Gcog_col]
Gaffec <- Item_bysub[,Gaffec_col]
alpha_Gcomp<-round(alpha(Gcomp)$total$raw_alpha,digit=2)
alpha_Gcog<-round(alpha(Gcog)$total$raw_alpha,digit=2)
alpha_Gaffec<-round(alpha(Gaffec)$total$raw_alpha,digit=2)


Hest_col<-c( 'Hest01','Hest02','Hest03','Hest04','Hest05','Hest06','Hest07','Hest08','Hest09','Hest10',
                'Hest11','Hest12','Hest13','Hest14','Hest15','Hest16','Hest17','Hest18','Hest19','Hest20','Hest21','Hest22','Hest23')
Hest<- Item_bysub[,Hest_col]
alpha_Hest0<-alpha(Hest)
alpha_Hest<-round(alpha_Hest0$total$raw_alpha,digit=2)
##Some items ( Hest05 ) were negatively correlated with the total scale and probably should be reversed.
#raw alpha 0.85
Happa_col<-c('Hest01','Hest06','Hest07','Hest09','Hest11','Hest13','Hest15','Hest17','Hest21','Hest23')
Hwei_col<-c('Hest03','Hest04','Hest08','Hest10','Hest16','Hest18','Hest19','Hest22')
Hattri_col<-c('Hest02','Hest05','Hest12','Hest14','Hest20')
Happa <- Item_bysub[,Happa_col]
Hwei <- Item_bysub[,Hwei_col]
Hattri <- Item_bysub[,Hattri_col]
alpha_Happa<-round(alpha(Happa)$total$raw_alpha,digit=2)
alpha_Hwei<-round(alpha(Hwei)$total$raw_alpha,digit=2)
alpha_Hattri<-round(alpha(Hattri)$total$raw_alpha,digit=2)

Iobj_col<-c('Iobj01','Iobj02','Iobj03','Iobj04','Iobj05','Iobj06','Iobj07','Iobj08')
Iobj<- Item_bysub[,Iobj_col]
alpha_Iobj0<-alpha(Iobj)
alpha_Iobj<-round(alpha_Iobj0$total$raw_alpha,digit=2)



IIpress_col<-c('IIpress01','IIpress02','IIpress03','IIpress04','IIpress05','IIpress06','IIpress07','IIpress08','IIpress09','IIpress10','IIpress11','IIpress12')
IIpress<- Item_bysub[,IIpress_col]
alpha_IIpress0<-alpha(IIpress)
alpha_IIpress<-round(alpha_IIpress0$total$raw_alpha,digit=2)
IIfam_col<-c('IIpress01','IIpress02','IIpress03','IIpress04')
IIpee_col<- c('IIpress05','IIpress06','IIpress07','IIpress08')
IImed_col<- c('IIpress09','IIpress10','IIpress11','IIpress12')
IIfam <- Item_bysub[,IIfam_col]
IIpee <- Item_bysub[,IIpee_col]
IImed <- Item_bysub[,IImed_col]
alpha_IIfam<-round(alpha(IIfam)$total$raw_alpha,digit=2)
alpha_IIpee<-round(alpha(IIpee)$total$raw_alpha,digit=2)
alpha_IImed<-round(alpha(IImed)$total$raw_alpha,digit=2)


Jcons_col<-c('Jcons01','Jcons02','Jcons03','Jcons04','Jcons05','Jcons06','Jcons07')
Jcons<- Item_bysub[,Jcons_col]
alpha(Jcons)
#raw alpha 0.74

Kinter_col<-c('Kinter01','Kinter02','Kinter03','Kinter04','Kinter05','Kinter06','Kinter07','Kinter08','Kinter09','Kinter10',
             'Kinter11','Kinter12','Kinter13','Kinter14','Kinter15','Kinter16','Kinter17','Kinter18','Kinter19','Kinter20','Kinter21')
Kinter<- Item_bysub[,Kinter_col]
alpha_Kinter0<-alpha(Kinter)
alpha_Kinter<-round(alpha_Kinter0$total$raw_alpha,digit=2)
#Some items ( Kinter06 Kinter07 Kinter08 Kinter09 ) were negatively correlated with the total scale and 
#raw alpha 0.76

K1inter_col<-c('Kinter01','Kinter02','Kinter03','Kinter04')
K1inter<- Item_bysub[,K1inter_col]
alpha_K1inter0<-alpha(K1inter)
alpha_Knoticing<-round(alpha(K1inter)$total$raw_alpha,digit=2) #raw alpha 0.67

K2inter_col<-c('Kinter05','Kinter06','Kinter07')
K2inter<- Item_bysub[,K2inter_col]
alpha_K2inter0<-alpha(K2inter)
alpha_Knotdistact<-round(alpha(K2inter)$total$raw_alpha,digit=2) #raw alpha 0.66

K3inter_col<-c('Kinter08','Kinter09','Kinter10')
K3inter<- Item_bysub[,K3inter_col]
alpha(K3inter)
alpha_K3inter0<-alpha(K3inter)
alpha_Knotworry<-round(alpha(K3inter)$total$raw_alpha,digit=2) #raw alpha 0.55

K4inter_col<-c('Kinter11','Kinter12','Kinter13','Kinter14','Kinter15')
K4inter<- Item_bysub[,K4inter_col]
alpha(K4inter)
alpha_K4inter0<-alpha(K4inter)
alpha_Kemotion<-round(alpha(K4inter)$total$raw_alpha,digit=2) #raw alpha 0.78

K5inter_col<-c('Kinter16','Kinter17','Kinter18')
K5inter<- Item_bysub[,K5inter_col]
alpha(K5inter)
alpha_K5inter0<-alpha(K5inter)
alpha_Klistening<-round(alpha(K5inter)$total$raw_alpha,digit=2) #raw alpha 0.77

K6inter_col<-c('Kinter19','Kinter20','Kinter21')
K6inter<- Item_bysub[,K6inter_col]
alpha(K6inter)
alpha_K6inter0<-alpha(K6inter)
alpha_Ktrusting<-round(alpha(K6inter)$total$raw_alpha,digit=2) #raw alpha 0.79



NLpairs_col<-c('NLpairs01','NLpairs02','NLpairs03','NLpairs04','NLpairs05','NLpairs06','NLpairs07','NLpairs08','NLpairs09','NLpairs10')
NLpairs<- Item_bysub[,NLpairs_col]
alpha_NLpairs0<-alpha(NLpairs)
alpha_NLpairs<-round(alpha(NLpairs)$total$raw_alpha,digit=2) 
#raw alpha 0.67



#############################SOCIAL 

L2tact_soc_col<-c('L2tact01','L2tact02','L2tact03','L2tact04','L2tact05','L2tact06','L2tact07','L2tact08')
L2tact_soc<- Item_bysub[,L2tact_soc_col]
alpha(L2tact_soc)
#raw alpha 0.68
L2tact_soc_col<-c('L2tact01','L2tact02','L2tact03','L2tact04','L2tact05','L2tact06','L2tact08') #supprimer le 07
L2tact_soc<- Item_bysub[,L2tact_soc_col]
alpha_L2tact_soc0<-alpha(L2tact_soc)#raw alpha 0.73
alpha_L2tact_soc<-round(alpha(L2tact_soc)$total$raw_alpha,digit=2) 


##################BASE SUR PCA

RC1_col<-c('L2tact01','L2tact02','L2tact03','L2tact04','L2tact05','L2tact06','L2tact08','L4act07','L4act04') #supprimer le 07
RC1<- Item_bysub[,RC1_col]
alpha(RC1)#raw alpha 0.77

RC2_col<-c('L2tact01','L2tact04','L2tact05','L3mouv02','L3mouv06','L4act08','L1tact01') #supprimer le 07
RC2<- Item_bysub[,RC2_col]
alpha(RC2)#raw alpha 0.54


RC3_col<-c('L4act05','L4act09','L4act06','L4act01','L4act02') 
RC3<- Item_bysub[,RC3_col]
alpha(RC3)#raw alpha 0.52

RC4_col<-c('L4act01','L3mouv04','L1tact07','L3mouv08','L4act03') 
RC4<- Item_bysub[,RC4_col]
alpha(RC4,check.keys=TRUE)#raw alpha 0.54











##################BASE SUR 
LL_col<-c('L3mouv01','L3mouv02','L3mouv03','L3mouv05','L3mouv06','L3mouv04','L3mouv07','L3mouv08',
      'L4act01','L4act02','L4act03','L4act04','L4act05','L4act06','L4act07','L4act08','L4act09','L4act10')
LL<- Item_bysub[,LL_col]
alpha(LL,check.keys=TRUE) #0.69

L_mouv_activite_col<-c('L3mouv01','L3mouv03','L3mouv04','L3mouv05','L3mouv06','L3mouv07','L3mouv08', #'L3mouv02'
                       'L4act02','L4act04','L4act05','L4act06','L4act07','L4act09','L4act10') #'L4act01',,'L4act03''L4act08',
L_mouv_activite<- Item_bysub[,L_mouv_activite_col]
alpha(L_mouv_activite,check.keys=TRUE) #0.72



#####ANALYSE DUNN 
recherche_col<-c('L1tact01','L1tact03','L4act01','L4act03','L4act08','L3mouv02','L3mouv06','L2tact01')
sensibilitite_col<-c('L3mouv01','L3mouv05','L3mouv08','L1tact02','L1tact04','L1tact05','L4act09')
evitement_col<-c('L3mouv03','L4act08','L4act04','L4act07','L2tact02','L2tact03')#'L3mouv03','L4act08',
enregistrementfaible_col<-c('L3mouv04','L3mouv07','L1tact06','L1tact07','L4act02','L4act05','L4act06')

recherche<- Item_bysub[,recherche_col]
sensibilitite<- Item_bysub[,sensibilitite_col]
evitement<- Item_bysub[,evitement_col]
enregistrementfaible<- Item_bysub[,enregistrementfaible_col]

alpha(recherche) # 0.52
alpha(sensibilitite) #0.57
alpha(evitement) #0.61 # peut monter à0.73 si on enlève L4act08 et L3mouv03
alpha(enregistrementfaible,check.keys=TRUE)  #0.54 peut monter à 0.56


####################Citherlet 2021 

#high threshold component combines ‘‘low registration” and ‘‘sensation seeking” items
highthreshold_col<-c('L1tact03','L1tact06','L1tact07','L3mouv02','L3mouv04','L3mouv07','L4act01','L4act02','L4act03','L4act05','L4act06')#,'L1tact01','L4act08''L2tact01','L3mouv06',
highthreshold<- Item_bysub[,highthreshold_col]
alpha(highthreshold,check.keys=TRUE) # 0.46 passage à 0.54 en suprrimant

#the low threshold component combines ‘‘sensory sensitivity” and ‘‘sensation avoiding”
lowthreshold_col<-c('L1tact02','L1tact04','L1tact05','L3mouv01','L3mouv03','L3mouv05','L3mouv08','L4act04','L4act07','L4act09','L4act08','L2tact02','L2tact03')
lowthreshold<- Item_bysub[,lowthreshold_col]
alpha(lowthreshold,check.keys=TRUE) # 0.7

#passive behavior component combines ‘‘sensory sensitivity” and ‘‘low registration” 
passive_col<-c('L3mouv01','L3mouv05','L3mouv08','L1tact02','L1tact04','L1tact05','L4act09','L3mouv04','L3mouv07','L1tact06','L1tact07','L4act02','L4act05','L4act06')
passive<- Item_bysub[,passive_col]
alpha(passive,check.keys=TRUE) # 0.69

#and the active behavior component combines ‘‘sensations avoiding” and ‘‘sensation seeking” 
active_col<-c('L1tact01','L1tact03','L4act01','L4act03','L4act08','L3mouv02','L3mouv06','L2tact01','L3mouv03','L4act04','L4act07','L4act08','L2tact02','L2tact03')
active<- Item_bysub[,active_col]
alpha(active,check.keys=TRUE) # 0.61


####################Citherlet 2021 en utilisant que les L3 mouv et L4act

#high threshold component combines ‘‘low registration” and ‘‘sensation seeking” items
highthreshold_col<-c('L3mouv02','L3mouv04','L3mouv06','L3mouv07','L4act01','L4act02','L4act03','L4act05','L4act06','L4act08')#,'L1tact01','L2tact01','L1tact03','L1tact06','L1tact07',
highthreshold<- Item_bysub[,highthreshold_col]
alpha(highthreshold,check.keys=TRUE) # 0.52

#the low threshold component combines ‘‘sensory sensitivity” and ‘‘sensation avoiding”
lowthreshold_col<-c('L3mouv01','L3mouv03','L3mouv05','L3mouv08','L4act04','L4act07','L4act09','L4act08') #'L1tact02','L1tact04','L1tact05',,'L2tact02','L2tact03'
lowthreshold<- Item_bysub[,lowthreshold_col]
alpha(lowthreshold,check.keys=TRUE) # 0.54

#passive behavior component combines ‘‘sensory sensitivity” and ‘‘low registration” 
passive_col<-c('L3mouv01','L3mouv05','L3mouv08','L3mouv04','L3mouv07','L4act09','L4act02','L4act05','L4act06')#'L1tact02','L1tact04','L1tact05','L1tact06','L1tact07',
passive<- Item_bysub[,passive_col]
alpha(passive,check.keys=TRUE) # 0.65

#and the active behavior component combines ‘‘sensations avoiding” and ‘‘sensation seeking” 
active_col<-c('L4act01','L4act03','L4act08','L3mouv02','L3mouv06','L3mouv03','L4act04','L4act07','L4act08')#'L1tact01','L1tact03','L2tact01','L2tact02','L2tact03'
active<- Item_bysub[,active_col]
alpha(active,check.keys=TRUE) # 0.49

####################Citherlet 2021 en rajoutant L1

#high threshold component combines ‘‘low registration” and ‘‘sensation seeking” items
highthreshold_col<-c('L3mouv02','L3mouv04','L3mouv06','L3mouv07','L4act01','L4act02','L4act03','L4act05','L4act06','L4act08','L1tact01','L1tact03','L1tact06','L1tact07')#,'L1tact01','L2tact01','L1tact03','L1tact06','L1tact07',
highthreshold<- Item_bysub[,highthreshold_col]
alpha(highthreshold,check.keys=TRUE) # 0.52 passage à 0.48 avec L1

#the low threshold component combines ‘‘sensory sensitivity” and ‘‘sensation avoiding”
lowthreshold_col<-c('L3mouv01','L3mouv03','L3mouv05','L3mouv08','L4act04','L4act07','L4act09','L4act08','L1tact02','L1tact04','L1tact05') #,,'L2tact02','L2tact03'
lowthreshold<- Item_bysub[,lowthreshold_col]
alpha(lowthreshold,check.keys=TRUE) # 0.54 passage à 0.63 avec L1

#passive behavior component combines ‘‘sensory sensitivity” and ‘‘low registration” 
passive_col<-c('L3mouv01','L3mouv05','L3mouv08','L3mouv04','L3mouv07','L4act09','L4act02','L4act05','L4act06','L1tact02','L1tact04','L1tact05','L1tact06','L1tact07')#
passive<- Item_bysub[,passive_col]
alpha(passive,check.keys=TRUE) # 0.65 passage à 0.69 avec L1

#and the active behavior component combines ‘‘sensations avoiding” and ‘‘sensation seeking” 
active_col<-c('L4act01','L4act03','L4act08','L3mouv02','L3mouv06','L3mouv03','L4act04','L4act07','L4act08','L1tact01','L1tact03')#'L2tact01','L2tact02','L2tact03'
active<- Item_bysub[,active_col]
alpha(active,check.keys=TRUE) # 0.49 passage à  0.49 avec L1

save.image("../R_Env_Quest/validity.RData")
