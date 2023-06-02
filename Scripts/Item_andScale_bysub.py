# -*- coding: utf-8 -*-
"""
Created on Mon Jan 31 13:59:31 2022

@author: lisa


script pour traiter le questionnaire et calculer chaque construit par participant
ici les non réponses "je ne souhaite pas répondre", "je ne sais ou ne comprends pas la question' sont toutess remplacées par nan
"""

import random
import numpy as np
import scipy 
from scipy import *
import csv
import sys
import pandas as pd
from pathlib import Path
import os
from datetime import datetime
import operator
from dateutil.relativedelta import *
from datetime import date
import seaborn as sns
import matplotlib.pyplot as plt


#!!!set workingdiretory here
wd='C:\\Users\\lisa\Documents\\-These\\MANIP_Quest\\Share'
os.chdir(wd)

#load dataset
dataset=pd.read_csv('.\\Data\\results-survey961915.csv')


#%%

date=datetime.today().strftime('%Y%m%d')

construits=['numquest',
            'date_nais',
            'A1_sexe',
            'A2_genre',
            'A2_diff',
            'A3_age_m',
            'A3_age_y',
            'age_groupe',
            'A4_taille',
            'A5_poids',
            'A6_IMC',
            'B_puberte',
            'B4_pub_gon',
            'B4_pub_adre',
            'B2_regles',
            'B_pub_group',
            'C_AP',
            'D_cons_soi',
            'D1_priv',
            'D2_pub',
            'D3_anx_soc',
            'E_IRI',
            'E1_perspec',
            'E2_empat',
            'F_reseau',
            'F2_reseauquant',
            'G_reseau',
            'G1_comp',
            'G2_cog',
            'G3_affec',
            'H_estime_corp',
            'H1_appa',
            'H2_attri',
            'H3_poids',
            'I_objectifi',
            'II_press_soc',
            'II1_fam',
            'II2_pairs',
            'II3_reseau',
            'J_cons_corps',
            'K_intero',
            'K1_notice',
            'K2_nodistract',
            'K3_notworry',
            'K4_emotion',
            'K5_listing',
            'K6_trust',
            'L1a_tact_dis_rech',
            'L1b_tact_dis_seuil',
            'L2_tact_soc',
            'L3_mouv_rech',
            'L3_mouv_habil',
            'L4_act_rech',
            'L4_act_seuil',
            'L_mouv_activite',            
            'M_douleurs',
            'M1_douleurs_nb',
            'N_influence_pairs',
            'classe',
            'filiere',
            'diplome_parent',
            'sit_eco',
            'langue',
            'main',
            'departement',
            'connu_par']

Final=pd.DataFrame(columns=construits)

item_col=['numquest',
            'A1_sexe',
            'A2_genre',
            'A2_diff',
            'A3_age_m',
            'A3_age_y',
            'age_groupe',
            'A4_taille',
            'A5_poids',
            'A6_IMC',
            'B01',
            'B02',
            'B03',
            'B04/07',
            'B05/08',
            'Règles',
            'C_AP',
            'Dcons01','Dcons02','Dcons03','Dcons04','Dcons05','Dcons06','Dcons07','Dcons08','Dcons09','Dcons10',
            'Dcons11','Dcons12','Dcons13','Dcons14','Dcons15','Dcons16','Dcons17','Dcons18','Dcons19','Dcons20','Dcons21','Dcons22','Dcons23',
            'EIRI01','EIRI02','EIRI03','EIRI04','EIRI05','EIRI06','EIRI07','EIRI08','EIRI09',
            'EIRI10','EIRI11','EIRI12','EIRI13','EIRI14',
            'F_reseau',
            'F02',
            'F03',
            'Greseau01', 'Greseau02','Greseau03','Greseau04', 'Greseau05','Greseau06',
            'Greseau07', 'Greseau08','Greseau09','Greseau10', 'Greseau11',
            'Hest01','Hest02','Hest03','Hest04','Hest05','Hest06','Hest07','Hest08','Hest09','Hest10',
            'Hest11','Hest12','Hest13','Hest14','Hest15','Hest16','Hest17','Hest18','Hest19','Hest20','Hest21','Hest22','Hest23',
            'Iobj01','Iobj02','Iobj03','Iobj04','Iobj05','Iobj06','Iobj07','Iobj08',
            'IIpress01','IIpress02','IIpress03','IIpress04','IIpress05','IIpress06','IIpress07','IIpress08','IIpress09','IIpress10','IIpress11','IIpress12',
            'Jcons01','Jcons02','Jcons03','Jcons04','Jcons05','Jcons06','Jcons07',
            'Kinter01','Kinter02','Kinter03','Kinter04','Kinter05','Kinter06','Kinter07','Kinter08','Kinter09','Kinter10',
            'Kinter11','Kinter12','Kinter13','Kinter14','Kinter15','Kinter16','Kinter17','Kinter18','Kinter19','Kinter20','Kinter21',
            'L1tact01','L1tact02','L1tact03','L1tact04','L1tact05','L1tact06','L1tact07',
            'L2tact01','L2tact02','L2tact03','L2tact04','L2tact05','L2tact06','L2tact07','L2tact08',
            'L3mouv01','L3mouv02','L3mouv03','L3mouv04','L3mouv05','L3mouv06','L3mouv07','L3mouv08',
            'L4act01','L4act02','L4act03','L4act04','L4act05','L4act06','L4act07','L4act08','L4act09','L4act10',
            'M1_douleurs_nb',
            'NLpairs01','NLpairs02','NLpairs03','NLpairs04','NLpairs05','NLpairs06','NLpairs07','NLpairs08','NLpairs09','NLpairs10', 
            'classe',
            'filiere',
            'diplome_parent',
            'sit_eco',
            'langue',
            'main',
            'departement']


Final_item=pd.DataFrame(columns=item_col)


#%%
a_supprimer=[764,765,766,797,799,800,
             1078,1148,
                656,
                1228,
                1226,
                289,
                1094,
                791,
                776,
                782,
                794,
                796,
                760,
                789,
                763,
                302,
                1050,
                1207,
                574,
                744,
                762,
                1051,
                597,
                746,353,536,1227,546,759,1013,801,565,1210,1208,980] #  980 à supprimer mais utile pour la manip comp 


dataset = dataset[~dataset.id.isin(a_supprimer)]

#%%Pour ne garderque les complets

index_a_supp = dataset[dataset['lastpage'] < 13 ].index# Delete these row indexes from dataFrame
dataset.drop(index_a_supp , inplace=True)

#%% analyse des valeurs manquantes

miss_A10 = {}
miss_A11 = {}
miss_emp = {}

# Pour chaque colonne du DataFrame
for col in dataset.columns:
    # Sélectionner les lignes qui contiennent ' ', 'A11' ou 'A10'
    selected_rows = dataset[(dataset[col] == 'A10')]
    # Calculer le pourcentage de lignes sélectionnées par rapport au nombre total de lignes
    percentage = len(selected_rows) / len(dataset)
    # Ajouter le pourcentage à la liste des résultats
    miss_A10[col] = percentage
    
    selected_rows = dataset[(dataset[col] == 'A11')]
    # Calculer le pourcentage de lignes sélectionnées par rapport au nombre total de lignes
    percentage = len(selected_rows) / len(dataset)
    # Ajouter le pourcentage à la liste des résultats
    miss_A11[col] = percentage
    
    selected_rows = dataset[(dataset[col] == '') | (dataset[col] == ' ')]
    # Calculer le pourcentage de lignes sélectionnées par rapport au nombre total de lignes
    percentage = len(selected_rows) / len(dataset)
    # Ajouter le pourcentage à la liste des résultats
    miss_emp[col] = percentage
# Afficher les résultats


#%% analyse des valeurs manquantes par suj

miss_parsuj10 = {}
miss_parsuj11 = {}

# Pour chaque ligne du DataFrame
for index, row in dataset.iterrows():
    # Récupérer le numéro de sujet
    subject_number = row["id"]
    # Compter le nombre d'occurrences de 'A10'
    count10 = (row == 'A10').sum()
    count11 = (row == 'A11').sum()
    # Ajouter le nombre d'occurrences et le numéro de sujet au dictionnaire des résultats
    miss_parsuj10[subject_number] = count10
    miss_parsuj11[subject_number] = count11

new_dict = {}
# Pour chaque entrée dans le dictionnaire original
for key, value in miss_parsuj10.items():
    # Si la valeur n'est pas égale à 0
    if value != 0:
        # Ajouter l'entrée au nouveau dictionnaire
        new_dict[key] = value
# Remplacer le dictionnaire original par le nouveau dictionnaire
miss_parsuj10 = new_dict

new_dict = {}
# Pour chaque entrée dans le dictionnaire original
for key, value in miss_parsuj11.items():
    # Si la valeur n'est pas égale à 0
    if value != 0:
        # Ajouter l'entrée au nouveau dictionnaire
        new_dict[key] = value
# Remplacer le dictionnaire original par le nouveau dictionnaire
miss_parsuj11 = new_dict

# Convertir le dictionnaire en DataFrame
plot_miss10 = pd.DataFrame.from_dict(miss_parsuj10,orient='index',columns=['miss10'])
# Créer le countplot
sns.countplot(x='miss10', data=plot_miss10)
plt.xlabel("nb de miss10")
plt.ylabel("nb de suj")
plt.title("nb de suj ayant mis je ne sais pas")
plt.savefig(wd+'\\Analyses\\Plot_Datasbrutes\\missingA10.png')
plt.close()

plot_miss11 = pd.DataFrame.from_dict(miss_parsuj11,orient='index',columns=['miss11'])
# Créer le countplot
sns.countplot(x='miss11', data=plot_miss11)
plt.xlabel("nb de miss11")
plt.ylabel("nb de suj")
plt.title("nb de suj ayant mis je ne souhaite pas répondre")
plt.savefig(wd+'\\Analyses\\Plot_Datasbrutes\\missingA11.png')



#%%

dataset=dataset.replace(np.nan,'',regex=True)

a = len(dataset)


##Va chercher chaque valeur pour chaque construit pour chaque sujet
for i in dataset.index:

    numquest=dataset.loc[i,'id']
          
    
################sexe&genre
    if dataset.loc[i,'A02']=='A3'or dataset.loc[i,'A02']=='-oth-':
        A1_sexe=0
    if dataset.loc[i,'A02']=='A1': 
        A1_sexe=1   ###filles
    if dataset.loc[i,'A02']=='A2': 
        A1_sexe=2  ###garçon
    if dataset.loc[i,'A022']=='A3'or dataset.loc[i,'A022']=='-oth-':
        A2_genre=0
    if dataset.loc[i,'A022']=='A1': 
        A2_genre=1 
    if dataset.loc[i,'A022']=='A2': 
        A2_genre=2
    
    A2_diff=1
    if A2_genre==1 and A1_sexe==1:
        A2_diff=2
    if A2_genre==2 and A1_sexe==2:
        A2_diff=2

##############age
    submitdate=dataset.loc[i,'startdate']
    submitdate=submitdate[0:10]
    submitdate= datetime.strptime(submitdate, '%Y-%m-%d')
    datebirth=dataset.loc[i,'A01']
    datebirth=datebirth[0:10]
    datebirth=datetime.strptime(datebirth, '%Y-%m-%d')
    A3_age_d=submitdate-datebirth
    A3_age_y=int(A3_age_d.days/365.25)
    A3_age_m=int(A3_age_d.days/365.25*12)
    #age = relativedelta(submitdate, datebirth)
    ###((submitdate.month, submitdate.day) < (datebirth.month, datebirth.day))=> donne un bool True ou False. Si true alors =1 si false alors = 0
    #A3_age_y=submitdate.year - datebirth.year - ((submitdate.month, submitdate.day) < (datebirth.month, datebirth.day))
    #A3_age_m=A3_age_y*12+(submitdate.month - datebirth.month) - ((submitdate.day) < ( datebirth.day))

##############taille&poids
    A4_taille=dataset.loc[i,'A03']
    A5_poids=dataset.loc[i,'A04']
    A6_IMC=A5_poids/(A4_taille/100)**2
    
#######PUBERTE###################################"""

    if dataset.loc[i,'B01']=='A1':
        B01=1
    if dataset.loc[i,'B01']=='A2':
        B01=2
    if dataset.loc[i,'B01']=='A3':
        B01=3
    if dataset.loc[i,'B01']=='A4':
        B01=4
    if dataset.loc[i,'B01']=='A10'or dataset.loc[i,'B01']=='A11':
        B01=np.nan
    
    if dataset.loc[i,'B02']=='A1':
        B02=1
    if dataset.loc[i,'B02']=='A2':
        B02=2
    if dataset.loc[i,'B02']=='A3':
        B02=3
    if dataset.loc[i,'B02']=='A4':
        B02=4
    if dataset.loc[i,'B02']=='A10' or dataset.loc[i,'B02']=='A11':
        B02=np.nan
    
    if dataset.loc[i,'B03']=='A1':
        B03=1
    if dataset.loc[i,'B03']=='A2':
        B03=2
    if dataset.loc[i,'B03']=='A3':
        B03=3
    if dataset.loc[i,'B03']=='A4':
        B03=4
    if dataset.loc[i,'B03']=='A10' or dataset.loc[i,'B03']=='A11':
        B03=np.nan

    #####pour les filles 
    if A1_sexe==1 or A1_sexe==2:
        if dataset.loc[i,'B04']=='A2':
            B04=1
        if dataset.loc[i,'B04']=='A3':
            B04=2
        if dataset.loc[i,'B04']=='A4':
            B04=3
        if dataset.loc[i,'B04']=='A5':
            B04=4
        if dataset.loc[i,'B04']=='A10'or dataset.loc[i,'B04']=='A11' or dataset.loc[i,'B04']=='':
            B04=np.nan

        
        if dataset.loc[i,'B05']=='A1':
            B05=4
        if dataset.loc[i,'B05']=='A2':
            B05=1
        if dataset.loc[i,'B05']=='A10'or dataset.loc[i,'B05']=='A11' or dataset.loc[i,'B05']=='':
            B05=np.nan
            
        B_puberte=pd.DataFrame([B01,B02,B03,B04,B05])
        B_puberte=B_puberte.mean()
        B_puberte=B_puberte.loc[0]
        
        
        
        ###############pour les stades 
        
       # B3_pub_Tanner=pd.DataFrame([B02,B04,B05])
        #B3_pub_Tanner=B3_pub_Tanner.mean()
        #B3_pub_Tanner=B3_pub_Tanner.loc[0]
        #B3_pub_Tanner=(B3_pub_Tanner-1)/3*5
        
        B4_pub_gon=pd.DataFrame([B01,B04,B05])
        B4_pub_gon=B4_pub_gon.mean()
        B4_pub_gon=B4_pub_gon.loc[0]
        B4_pub_gon=(B4_pub_gon-1)/3*5      
 
        B4_pub_adre=pd.DataFrame([B02,B03])
        B4_pub_adre=B4_pub_adre.mean()
        B4_pub_adre=B4_pub_adre.loc[0]
        B4_pub_adre=(B4_pub_adre-1)/3*5
     
             
    
        ###########RèGLES 
        if dataset.loc[i,'B06']=='A1':
            B2_regles=8
        if dataset.loc[i,'B06']=='A2':
            B2_regles=9
        if dataset.loc[i,'B06']=='A3':
            B2_regles=10
        if dataset.loc[i,'B06']=='A4':
            B2_regles=11
        if dataset.loc[i,'B06']=='A5':
            B2_regles=12
        if dataset.loc[i,'B06']=='A6':
            B2_regles=13
        if dataset.loc[i,'B06']=='A7':
            B2_regles=14
        if dataset.loc[i,'B06']=='A8':
            B2_regles=15
        if dataset.loc[i,'B06']=='A9':
            B2_regles=16
        if dataset.loc[i,'B06']=='A10':
            B2_regles=17
        if dataset.loc[i,'B06']=='A11':
            B2_regles=18
        if dataset.loc[i,'B06']=='A12':
            B2_regles=19
        if dataset.loc[i,'B06']=='A13':
            B2_regles=20
        if dataset.loc[i,'B06']=='A14':
            B2_regles=21
        if dataset.loc[i,'B06']=='A15' or dataset.loc[i,'B06']==' 'or dataset.loc[i,'B06']=='':
            B2_regles=np.nan
        

        Pub_list=[B01,B02,B03,B04,B05,B2_regles]
     
    #####pour les garçons
    if A1_sexe==2:
        B2_regles=np.nan
        if dataset.loc[i,'B07']=='A1':
            B07=1
        if dataset.loc[i,'B07']=='A2':
            B07=2
        if dataset.loc[i,'B07']=='A3':
            B07=3
        if dataset.loc[i,'B07']=='A4':
            B07=4
        if dataset.loc[i,'B07']=='A10'or dataset.loc[i,'B07']=='A11'or dataset.loc[i,'B07']=='':
            B07=np.nan


        if dataset.loc[i,'B08']=='A1':
            B08=1
        if dataset.loc[i,'B08']=='A2':
            B08=2
        if dataset.loc[i,'B08']=='A3':
            B08=3
        if dataset.loc[i,'B08']=='A4':
            B08=4
        if dataset.loc[i,'B08']=='A10'or dataset.loc[i,'B08']=='A11'or dataset.loc[i,'B08']=='':
            B08=np.nan
            
        B_puberte=pd.DataFrame([B01,B02,B03,B07,B08])
        B_puberte=B_puberte.mean()
        B_puberte=B_puberte.loc[0]
        
        #B3_pub_Tanner=pd.DataFrame([B02,B07,B08])
        #B3_pub_Tanner=B3_pub_Tanner.mean()
        #B3_pub_Tanner=B3_pub_Tanner.loc[0]
        #B3_pub_Tanner=(B3_pub_Tanner-1)/3*5
        
        B4_pub_gon=pd.DataFrame([B01,B07,B08])
        B4_pub_gon=B4_pub_gon.mean()
        B4_pub_gon=B4_pub_gon.loc[0]
        B4_pub_gon=(B4_pub_gon-1)/3*5   
 
        B4_pub_adre=pd.DataFrame([B02,B03])
        B4_pub_adre=B4_pub_adre.mean()
        B4_pub_adre=B4_pub_adre.loc[0]
        B4_pub_adre=(B4_pub_adre-1)/3*5 
        
        Pub_list=[B01,B02,B03,B07,B08,np.nan]
        
#######Activité physique ###################################"""
    if dataset.loc[i,'C01']=='A1':
        C_AP=1
    if dataset.loc[i,'C01']=='A2':
        C_AP=2
    if dataset.loc[i,'C01']=='A3':
        C_AP=3
    if dataset.loc[i,'C01']=='A4':
        C_AP=4
    if dataset.loc[i,'C01']=='A10'or dataset.loc[i,'C01']=='A11'or dataset.loc[i,'C01']=='':
        C_AP=np.nan
    
    


#######Conscience de soi ###################################"""
    
    Dcons=[np.nan for i in range(0,24)] ## crée une liste vide qu'il s'agira ensuite de remplir 
    Dendroit=[1,2,4,5,6,7,8,10,11,13,14,15,16,17,18,19,20,21,22,23]
    Dinverse=[3,9,12]
    
    for j in Dendroit :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'D01[SQ0'+k+']']=='A1':
            Dcons[j]=1
        if dataset.loc[i,'D01[SQ0'+k+']']=='A2':
            Dcons[j]=2
        if dataset.loc[i,'D01[SQ0'+k+']']=='A3':
            Dcons[j]=3
        if dataset.loc[i,'D01[SQ0'+k+']']=='A4':
            Dcons[j]=4
        if dataset.loc[i,'D01[SQ0'+k+']']=='A5':
            Dcons[j]=5
        if dataset.loc[i,'D01[SQ0'+k+']']=='A10':
            Dcons[j]=np.nan 
        if dataset.loc[i,'D01[SQ0'+k+']']=='A11':
            Dcons[j]=np.nan 
    for j in Dinverse :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'D01[SQ0'+k+']']=='A1':
            Dcons[j]=5
        if dataset.loc[i,'D01[SQ0'+k+']']=='A2':
            Dcons[j]=4
        if dataset.loc[i,'D01[SQ0'+k+']']=='A3':
            Dcons[j]=3
        if dataset.loc[i,'D01[SQ0'+k+']']=='A4':
            Dcons[j]=2
        if dataset.loc[i,'D01[SQ0'+k+']']=='A5':
            Dcons[j]=1
        if dataset.loc[i,'D01[SQ0'+k+']']=='A10':
            Dcons[j]=np.nan 
        if dataset.loc[i,'D01[SQ0'+k+']']=='A11':
            Dcons[j]=np.nan 

    D1_privlist=pd.DataFrame(operator.itemgetter(1,3,5,7,9,13,15,18,20,22)(Dcons))
    D1_priv=D1_privlist.mean()
    D1_priv=D1_priv.loc[0]
    
    D2_publist=pd.DataFrame(operator.itemgetter(2,6,11,14,17,19,21)(Dcons))
    D2_pub=D2_publist.mean()
    D2_pub=D2_pub.loc[0]
    
    D3_anx_soclist=pd.DataFrame(operator.itemgetter(4,8,10,12,16,23)(Dcons))
    D3_anx_soc=D3_anx_soclist.mean()
    D3_anx_soc=D3_anx_soc.loc[0]
    
    del Dcons[0] #car ne correspond à rien 
    D_cons_soilist=pd.DataFrame(Dcons)
    D_cons_soi=D_cons_soilist.mean()
    D_cons_soi=D_cons_soi.loc[0]
    
#######Empathie#####################################################################
    EIRI=[np.nan for i in range(0,15)] ## crée une liste vide qu'il s'agira ensuite de remplir 
    EIRIendroit=[1,4,5,6,10,11,12,13,14]
    EIRIinverse=[2,3,7,8,9]
    
    for j in EIRIendroit :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'E01[SQ0'+k+']']=='A1':
            EIRI[j]=1
        if dataset.loc[i,'E01[SQ0'+k+']']=='A2':
            EIRI[j]=2
        if dataset.loc[i,'E01[SQ0'+k+']']=='A3':
            EIRI[j]=3
        if dataset.loc[i,'E01[SQ0'+k+']']=='A4':
            EIRI[j]=4
        if dataset.loc[i,'E01[SQ0'+k+']']=='A5':
            EIRI[j]=5
        if dataset.loc[i,'E01[SQ0'+k+']']=='A10':
            EIRI[j]=np.nan 
        if dataset.loc[i,'E01[SQ0'+k+']']=='A11':
            EIRI[j]=np.nan 
    for j in EIRIinverse :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'E01[SQ0'+k+']']=='A1':
            EIRI[j]=5
        if dataset.loc[i,'E01[SQ0'+k+']']=='A2':
            EIRI[j]=4
        if dataset.loc[i,'E01[SQ0'+k+']']=='A3':
            EIRI[j]=3
        if dataset.loc[i,'E01[SQ0'+k+']']=='A4':
            EIRI[j]=2
        if dataset.loc[i,'E01[SQ0'+k+']']=='A5':
            EIRI[j]=1
        if dataset.loc[i,'E01[SQ0'+k+']']=='A10':
            EIRI[j]=np.nan 
        if dataset.loc[i,'E01[SQ0'+k+']']=='A11':
            EIRI[j]=np.nan 
    
    
    E1_perspeclist=pd.DataFrame(operator.itemgetter(2,4,6,8,11,13)(EIRI))
    E1_perspec=E1_perspeclist.mean()
    E1_perspec=E1_perspec.loc[0]

    E2_empatlist=pd.DataFrame(operator.itemgetter(1,3,5,7,9,10,12)(EIRI))
    E2_empat=E2_empatlist.mean()
    E2_empat=E2_empat.loc[0]

    del EIRI[0] #car ne correspond à rien 
    E_IRIlist=pd.DataFrame(EIRI)
    E_IRI=E_IRIlist.mean()
    E_IRI=E_IRI.loc[0]
 


############################################reseau sociaux objectifs ###################
    if dataset.loc[i,'F01']=='A11':
        F_reseau=3 
        F2_reseauquant=np.nan
        F02=np.nan
        F03=np.nan
    if dataset.loc[i,'F01']=='A2':
        F_reseau=2
        F2_reseauquant=0
        F02=0
        F03=0
    if dataset.loc[i,'F01']=='A1':
        F_reseau=1 
        if dataset.loc[i,'F02']=='A1':
            F02=1
        if dataset.loc[i,'F02']=='A2':
            F02=2
        if dataset.loc[i,'F02']=='A3':
            F02=3
        if dataset.loc[i,'F02']=='A4':
            F02=4
        if dataset.loc[i,'F02']=='A5':
            F02=5
        if dataset.loc[i,'F02']=='A10':
            F02=np.nan 
        if dataset.loc[i,'F02']=='A11' or dataset.loc[i,'F02']=='':
            F02=np.nan 
        if dataset.loc[i,'F03']=='A1':
            F03=1
        if dataset.loc[i,'F03']=='A2':
            F03=2
        if dataset.loc[i,'F03']=='A3':
            F03=3
        if dataset.loc[i,'F03']=='A4':
            F03=4
        if dataset.loc[i,'F03']=='A5':
            F03=5
        if dataset.loc[i,'F03']=='A10':
            F03=np.nan 
        if dataset.loc[i,'F03']=='A11' or dataset.loc[i,'F03']=='':
            F03=np.nan 
        F2_reseauquant=(F02+F03)/2



############ reseau sociaux subjectifs échelle################################
    Greseau=[np.nan for i in range(0,12)] ## crée une liste vide qu'il s'agira ensuite de remplir 
    Greseauendroit=[1,2,3,4,5,6,7,8,9,10,11]
    Greseauinverse=[]
    
    for j in Greseauendroit :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'G01[SQ0'+k+']']=='A1':
            Greseau[j]=1
        if dataset.loc[i,'G01[SQ0'+k+']']=='A2':
            Greseau[j]=2
        if dataset.loc[i,'G01[SQ0'+k+']']=='A3':
            Greseau[j]=3
        if dataset.loc[i,'G01[SQ0'+k+']']=='A4':
            Greseau[j]=4
        if dataset.loc[i,'G01[SQ0'+k+']']=='A5':
            Greseau[j]=5
        if dataset.loc[i,'G01[SQ0'+k+']']=='A10':
            Greseau[j]=np.nan 
        if dataset.loc[i,'G01[SQ0'+k+']']=='A11':
            Greseau[j]=np.nan 

    G1_complist=pd.DataFrame(operator.itemgetter(1,2,3,4)(Greseau))
    G1_comp=G1_complist.mean()
    G1_comp=G1_comp.loc[0]

    G2_coglist=pd.DataFrame(operator.itemgetter(5,6,7)(Greseau))
    G2_cog=G2_coglist.mean()
    G2_cog=G2_cog.loc[0]

    G3_affeclist=pd.DataFrame(operator.itemgetter(8,9,10,11)(Greseau))
    G3_affec=G3_affeclist.mean()
    G3_affec=G3_affec.loc[0]

    del Greseau[0] #car ne correspond à rien 
    G_reseaulist=pd.DataFrame(Greseau)
    G_reseau=G_reseaulist.mean()
    G_reseau=G_reseau.loc[0]

######################################################### o	H01 Estime de soi corporelle
######estime de soi corporelle
    Hest=[np.nan for i in range(0,24)] ## crée une liste vide qu'il s'agira ensuite de remplir 
    Hestendroit=[1,2,3,5,6,8,10,12,14,15,16,20,22,23]
    Hestinverse=[4,7,9,11,13,17,18,19,21]
    
    for j in Hestendroit :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'H01[SQ0'+k+']']=='A1':
            Hest[j]=1
        if dataset.loc[i,'H01[SQ0'+k+']']=='A2':
            Hest[j]=2
        if dataset.loc[i,'H01[SQ0'+k+']']=='A3':
            Hest[j]=3
        if dataset.loc[i,'H01[SQ0'+k+']']=='A4':
            Hest[j]=4
        if dataset.loc[i,'H01[SQ0'+k+']']=='A5':
            Hest[j]=5
        if dataset.loc[i,'H01[SQ0'+k+']']=='A10':
            Hest[j]=np.nan 
        if dataset.loc[i,'H01[SQ0'+k+']']=='A11':
            Hest[j]=np.nan 
    for j in Hestinverse :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'H01[SQ0'+k+']']=='A1':
            Hest[j]=5
        if dataset.loc[i,'H01[SQ0'+k+']']=='A2':
            Hest[j]=4
        if dataset.loc[i,'H01[SQ0'+k+']']=='A3':
            Hest[j]=3
        if dataset.loc[i,'H01[SQ0'+k+']']=='A4':
            Hest[j]=2
        if dataset.loc[i,'H01[SQ0'+k+']']=='A5':
            Hest[j]=1
        if dataset.loc[i,'H01[SQ0'+k+']']=='A10':
            Hest[j]=np.nan 
        if dataset.loc[i,'H01[SQ0'+k+']']=='A11':
            Hest[j]=np.nan 
    
    
    H1_appalist=pd.DataFrame(operator.itemgetter(1,6,7,9,11,13,15,17,21,23)(Hest))
    H1_appa=H1_appalist.mean()
    H1_appa=H1_appa.loc[0]

    H2_attrilist=pd.DataFrame(operator.itemgetter(2,5,12,14,20)(Hest))
    H2_attri=H2_attrilist.mean()
    H2_attri=H2_attri.loc[0]
    
    
    H3_poidslist=pd.DataFrame(operator.itemgetter(3,4,8,10,16,18,19,22)(Hest))
    H3_poids=H3_poidslist.mean()
    H3_poids=H3_poids.loc[0]
    
    del Hest[0] #car ne correspond à rien 
    H_estime_corplist=pd.DataFrame(Hest)
    H_estime_corp=H_estime_corplist.mean()
    H_estime_corp=H_estime_corp.loc[0]
    
    
    
######################################################### I01 Objectified body consciousness scale
######################
    
    Iobj=[np.nan for i in range(0,9)] ## crée une liste vide qu'il s'agira ensuite de remplir 
    Iobjendroit=[5,6]
    Iobjinverse=[1,2,3,4,7,8]
    
    for j in Iobjendroit :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'I01[SQ0'+k+']']=='A1':
            Iobj[j]=1
        if dataset.loc[i,'I01[SQ0'+k+']']=='A2':
            Iobj[j]=2
        if dataset.loc[i,'I01[SQ0'+k+']']=='A3':
            Iobj[j]=3
        if dataset.loc[i,'I01[SQ0'+k+']']=='A4':
            Iobj[j]=4
        if dataset.loc[i,'I01[SQ0'+k+']']=='A5':
            Iobj[j]=5
        if dataset.loc[i,'I01[SQ0'+k+']']=='A10':
            Iobj[j]=np.nan 
        if dataset.loc[i,'I01[SQ0'+k+']']=='A11'or dataset.loc[i,'I01[SQ0'+k+']']=='':
            Iobj[j]=np.nan 
    for j in Iobjinverse :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'I01[SQ0'+k+']']=='A1':
            Iobj[j]=5
        if dataset.loc[i,'I01[SQ0'+k+']']=='A2':
            Iobj[j]=4
        if dataset.loc[i,'I01[SQ0'+k+']']=='A3':
            Iobj[j]=3
        if dataset.loc[i,'I01[SQ0'+k+']']=='A4':
            Iobj[j]=2
        if dataset.loc[i,'I01[SQ0'+k+']']=='A5':
            Iobj[j]=1
        if dataset.loc[i,'I01[SQ0'+k+']']=='A10':
            Iobj[j]=np.nan 
        if dataset.loc[i,'I01[SQ0'+k+']']=='A11'or dataset.loc[i,'I01[SQ0'+k+']']=='':
            Iobj[j]=np.nan 
    
    del Iobj[0]#car ne correspond à rien  
    I_objectifilist=pd.DataFrame(Iobj)
    I_objectifi=I_objectifilist.mean()
    I_objectifi=I_objectifi.loc[0]
    
    
    
######################################################### 	II01 Pression socio-culturelle envers l’apparence
    IIpress=[np.nan for i in range(0,13)] ## crée une liste vide qu'il s'agira ensuite de remplir 
    IIpressendroit=[1,2,3,5,6,7,9,10,12]
    IIpressinverse=[4,8,11]
    
    for j in IIpressendroit :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'II01[SQ0'+k+']']=='A1':
            IIpress[j]=1
        if dataset.loc[i,'II01[SQ0'+k+']']=='A2':
            IIpress[j]=2
        if dataset.loc[i,'II01[SQ0'+k+']']=='A3':
            IIpress[j]=3
        if dataset.loc[i,'II01[SQ0'+k+']']=='A4':
            IIpress[j]=4
        if dataset.loc[i,'II01[SQ0'+k+']']=='A5':
            IIpress[j]=5
        if dataset.loc[i,'II01[SQ0'+k+']']=='A10':
            IIpress[j]=np.nan 
        if dataset.loc[i,'II01[SQ0'+k+']']=='A11':
            IIpress[j]=np.nan 
    for j in IIpressinverse :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'II01[SQ0'+k+']']=='A1':
            IIpress[j]=5
        if dataset.loc[i,'II01[SQ0'+k+']']=='A2':
            IIpress[j]=4
        if dataset.loc[i,'II01[SQ0'+k+']']=='A3':
            IIpress[j]=3
        if dataset.loc[i,'II01[SQ0'+k+']']=='A4':
            IIpress[j]=2
        if dataset.loc[i,'II01[SQ0'+k+']']=='A5':
            IIpress[j]=1
        if dataset.loc[i,'II01[SQ0'+k+']']=='A10':
            IIpress[j]=np.nan 
        if dataset.loc[i,'II01[SQ0'+k+']']=='A11':
            IIpress[j]=np.nan 
    
    
    II1_famlist=pd.DataFrame(operator.itemgetter(1,2,3,4)(IIpress))
    II1_fam=II1_famlist.mean()
    II1_fam=II1_fam.loc[0]

    II2_pairslist=pd.DataFrame(operator.itemgetter(5,6,7,8)(IIpress))
    II2_pairs=II2_pairslist.mean()
    II2_pairs=II2_pairs.loc[0]
    
    
    II3_reseaulist=pd.DataFrame(operator.itemgetter(9,10,11,12)(IIpress))
    II3_reseau=II3_reseaulist.mean()
    II3_reseau=II3_reseau.loc[0]
    
    del IIpress[0] #car ne correspond à rien 
    II_press_soclist=pd.DataFrame(IIpress)
    II_press_soc=II_press_soclist.mean()
    II_press_soc=II_press_soc.loc[0]
    
###################################### J01 Body consciousness questionnaire 
    
    Jcons=[np.nan for i in range(0,8)] ## crée une liste vide qu'il s'agira ensuite de remplir 
    Jconsendroit=[1,2,3,4,5,6,7]
    Jconsinverse=[]
    
    for j in Jconsendroit :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'J01[SQ0'+k+']']=='A1':
            Jcons[j]=1
        if dataset.loc[i,'J01[SQ0'+k+']']=='A2':
            Jcons[j]=2
        if dataset.loc[i,'J01[SQ0'+k+']']=='A3':
            Jcons[j]=3
        if dataset.loc[i,'J01[SQ0'+k+']']=='A4':
            Jcons[j]=4
        if dataset.loc[i,'J01[SQ0'+k+']']=='A5':
            Jcons[j]=5
        if dataset.loc[i,'J01[SQ0'+k+']']=='A10':
            Jcons[j]=np.nan 
        if dataset.loc[i,'J01[SQ0'+k+']']=='A11':
            Jcons[j]=np.nan 
    
    del Jcons[0] #car ne correspond à rien 
    J_cons_corpslist=pd.DataFrame(Jcons)
    J_cons_corps=J_cons_corpslist.mean()
    J_cons_corps=J_cons_corps.loc[0]
    
###################################### o	K01 Multidimensional interoceptive awareness 
    
    Kinter=[np.nan for i in range(0,22)] ## crée une liste vide qu'il s'agira ensuite de remplir 
    Kinterendroit=[1,2,3,4,10,11,12,13,14,15,16,17,18,19,20,21]
    Kinterinverse=[5,6,7,8,9]
    
    for j in Kinterendroit :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'K01[SQ0'+k+']']=='A1':
            Kinter[j]=1
        if dataset.loc[i,'K01[SQ0'+k+']']=='A2':
            Kinter[j]=2
        if dataset.loc[i,'K01[SQ0'+k+']']=='A3':
            Kinter[j]=3
        if dataset.loc[i,'K01[SQ0'+k+']']=='A4':
            Kinter[j]=4
        if dataset.loc[i,'K01[SQ0'+k+']']=='A5':
            Kinter[j]=5
        if dataset.loc[i,'K01[SQ0'+k+']']=='A10':
            Kinter[j]=np.nan 
        if dataset.loc[i,'K01[SQ0'+k+']']=='A11':
            Kinter[j]=np.nan 
    for j in Kinterinverse :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'K01[SQ0'+k+']']=='A1':
            Kinter[j]=5
        if dataset.loc[i,'K01[SQ0'+k+']']=='A2':
            Kinter[j]=4
        if dataset.loc[i,'K01[SQ0'+k+']']=='A3':
            Kinter[j]=3
        if dataset.loc[i,'K01[SQ0'+k+']']=='A4':
            Kinter[j]=2
        if dataset.loc[i,'K01[SQ0'+k+']']=='A5':
            Kinter[j]=1
        if dataset.loc[i,'K01[SQ0'+k+']']=='A10':
            Kinter[j]=np.nan 
        if dataset.loc[i,'K01[SQ0'+k+']']=='A11':
            Kinter[j]=np.nan 
    
    
    K1_noticelist=pd.DataFrame(operator.itemgetter(1,2,3,4)(Kinter))
    K1_notice=K1_noticelist.mean()
    K1_notice=K1_notice.loc[0]
    
    K2_nodistractlist=pd.DataFrame(operator.itemgetter(5,6,7)(Kinter))
    K2_nodistract=K2_nodistractlist.mean()
    K2_nodistract=K2_nodistract.loc[0]
    
    K3_notworrylist=pd.DataFrame(operator.itemgetter(8,9,10)(Kinter))
    K3_notworry=K3_notworrylist.mean()
    K3_notworry=K3_notworry.loc[0]

    K4_emotionlist=pd.DataFrame(operator.itemgetter(11,12,13,14,15)(Kinter))
    K4_emotion=K4_emotionlist.mean()
    K4_emotion=K4_emotion.loc[0]
    
    K5_listinglist=pd.DataFrame(operator.itemgetter(16,17,18)(Kinter))
    K5_listing=K5_listinglist.mean()
    K5_listing=K5_listing.loc[0]

    K6_trustlist=pd.DataFrame(operator.itemgetter(19,20,21)(Kinter))
    K6_trust=K6_trustlist.mean()
    K6_trust=K6_trust.loc[0]


    del Kinter[0] #car ne correspond à rien 
    K_interolist=pd.DataFrame(Kinter)
    K_intero=K_interolist.mean()
    K_intero=K_intero.loc[0]
    

###################################### L Profil sensoriel
########################L Profil sensoriel : L01 Tactil discriminatif
    L1tact=[np.nan for i in range(0,8)] ## crée une liste vide qu'il s'agira ensuite de remplir 
    L1tactendroit=[1,3,6,7]
    L1tactinverse=[2,4,5]
    
    for j in L1tactendroit :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'L01[SQ0'+k+']']=='A1':
            L1tact[j]=1
        if dataset.loc[i,'L01[SQ0'+k+']']=='A2':
            L1tact[j]=2
        if dataset.loc[i,'L01[SQ0'+k+']']=='A3':
            L1tact[j]=3
        if dataset.loc[i,'L01[SQ0'+k+']']=='A4':
            L1tact[j]=4
        if dataset.loc[i,'L01[SQ0'+k+']']=='A5':
            L1tact[j]=5
        if dataset.loc[i,'L01[SQ0'+k+']']=='A10':
            L1tact[j]=np.nan 
        if dataset.loc[i,'L01[SQ0'+k+']']=='A11':
            L1tact[j]=np.nan 
    for j in L1tactinverse :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'L01[SQ0'+k+']']=='A1':
            L1tact[j]=5
        if dataset.loc[i,'L01[SQ0'+k+']']=='A2':
            L1tact[j]=4
        if dataset.loc[i,'L01[SQ0'+k+']']=='A3':
            L1tact[j]=3
        if dataset.loc[i,'L01[SQ0'+k+']']=='A4':
            L1tact[j]=2
        if dataset.loc[i,'L01[SQ0'+k+']']=='A5':
            L1tact[j]=1
        if dataset.loc[i,'L01[SQ0'+k+']']=='A10':
            L1tact[j]=np.nan 
        if dataset.loc[i,'L01[SQ0'+k+']']=='A11':
            L1tact[j]=np.nan
    
        
    
    L1a_tact_dis_rechlist=pd.DataFrame(operator.itemgetter(1,2,3,4,5)(L1tact))
    L1a_tact_dis_rech=L1a_tact_dis_rechlist.mean()
    L1a_tact_dis_rech=L1a_tact_dis_rech.loc[0]
    
    L1b_tact_dis_seuillist=pd.DataFrame(operator.itemgetter(6,7)(L1tact))
    L1b_tact_dis_seuil=L1b_tact_dis_seuillist.mean()
    L1b_tact_dis_seuil=L1b_tact_dis_seuil.loc[0]
    
    del L1tact[0]
    
######################## L Profil sensoriel : L02 Tactil social 
    
    L2tact=[np.nan for i in range(0,9)] ## crée une liste vide qu'il s'agira ensuite de remplir 
    L2tactendroit=[1,4,7] ## on supprime le 7 car mauvaise consistence dans les analyse consistency 
    L2tactinverse=[2,3,5,6,8]
    
    for j in L2tactendroit :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'L02[SQ0'+k+']']=='A1':
            L2tact[j]=1
        if dataset.loc[i,'L02[SQ0'+k+']']=='A2':
            L2tact[j]=2
        if dataset.loc[i,'L02[SQ0'+k+']']=='A3':
            L2tact[j]=3
        if dataset.loc[i,'L02[SQ0'+k+']']=='A4':
            L2tact[j]=4
        if dataset.loc[i,'L02[SQ0'+k+']']=='A5':
            L2tact[j]=5
        if dataset.loc[i,'L02[SQ0'+k+']']=='A10':
            L2tact[j]=np.nan 
        if dataset.loc[i,'L02[SQ0'+k+']']=='A11':
            L2tact[j]=np.nan 
    for j in L2tactinverse :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'L02[SQ0'+k+']']=='A1':
            L2tact[j]=5
        if dataset.loc[i,'L02[SQ0'+k+']']=='A2':
            L2tact[j]=4
        if dataset.loc[i,'L02[SQ0'+k+']']=='A3':
            L2tact[j]=3
        if dataset.loc[i,'L02[SQ0'+k+']']=='A4':
            L2tact[j]=2
        if dataset.loc[i,'L02[SQ0'+k+']']=='A5':
            L2tact[j]=1
        if dataset.loc[i,'L02[SQ0'+k+']']=='A10':
            L2tact[j]=np.nan 
        if dataset.loc[i,'L02[SQ0'+k+']']=='A11':
            L2tact[j]=np.nan
        
     #car ne correspond à rien 
    L2_tact_soclist=pd.DataFrame(operator.itemgetter(1,2,3,4,5,6,8)(L2tact)) ## le 7 est supprimé car enlève de la puissance
    L2_tact_soc=L2_tact_soclist.mean()
    L2_tact_soc=L2_tact_soc.loc[0]
    
    del L2tact[0]
######################## L Profil sensoriel : L03 mouvement 

    L3mouv=[np.nan for i in range(0,9)] ## crée une liste vide qu'il s'agira ensuite de remplir 
    L3mouvendroit=[2,6]
    L3mouvinverse=[1,3,4,5,7,8]
    
    for j in L3mouvendroit :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'L03[SQ0'+k+']']=='A1':
            L3mouv[j]=1
        if dataset.loc[i,'L03[SQ0'+k+']']=='A2':
            L3mouv[j]=2
        if dataset.loc[i,'L03[SQ0'+k+']']=='A3':
            L3mouv[j]=3
        if dataset.loc[i,'L03[SQ0'+k+']']=='A4':
            L3mouv[j]=4
        if dataset.loc[i,'L03[SQ0'+k+']']=='A5':
            L3mouv[j]=5
        if dataset.loc[i,'L03[SQ0'+k+']']=='A10':
            L3mouv[j]=np.nan 
        if dataset.loc[i,'L03[SQ0'+k+']']=='A11':
            L3mouv[j]=np.nan 
    for j in L3mouvinverse :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'L03[SQ0'+k+']']=='A1':
            L3mouv[j]=5
        if dataset.loc[i,'L03[SQ0'+k+']']=='A2':
            L3mouv[j]=4
        if dataset.loc[i,'L03[SQ0'+k+']']=='A3':
            L3mouv[j]=3
        if dataset.loc[i,'L03[SQ0'+k+']']=='A4':
            L3mouv[j]=2
        if dataset.loc[i,'L03[SQ0'+k+']']=='A5':
            L3mouv[j]=1
        if dataset.loc[i,'L03[SQ0'+k+']']=='A10':
            L3mouv[j]=np.nan 
        if dataset.loc[i,'L03[SQ0'+k+']']=='A11':
            L3mouv[j]=np.nan
        
    L3_mouv_rechlist=pd.DataFrame(operator.itemgetter(1,2,3,5,6)(L3mouv))
    L3_mouv_rech=L3_mouv_rechlist.mean()
    L3_mouv_rech=L3_mouv_rech.loc[0]
    
    L3_mouv_habillist=pd.DataFrame(operator.itemgetter(4,7,8)(L3mouv))
    L3_mouv_habil=L3_mouv_habillist.mean()
    L3_mouv_habil=L3_mouv_habil.loc[0]
    
    L3mouvB=L3mouv.copy()
    
    del L3mouv[0]
    
######################## L Profil sensoriel : L04 niveau d'activité
    
    L4act=[np.nan for i in range(0,11)] ## crée une liste vide qu'il s'agira ensuite de remplir 
    L4actendroit=[1,3,8]
    L4actinverse=[2,4,5,6,7,9,10]
    
    for j in L4actendroit :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'L04[SQ0'+k+']']=='A1':
            L4act[j]=1
        if dataset.loc[i,'L04[SQ0'+k+']']=='A2':
            L4act[j]=2
        if dataset.loc[i,'L04[SQ0'+k+']']=='A3':
            L4act[j]=3
        if dataset.loc[i,'L04[SQ0'+k+']']=='A4':
            L4act[j]=4
        if dataset.loc[i,'L04[SQ0'+k+']']=='A5':
            L4act[j]=5
        if dataset.loc[i,'L04[SQ0'+k+']']=='A10':
            L4act[j]=np.nan 
        if dataset.loc[i,'L04[SQ0'+k+']']=='A11':
            L4act[j]=np.nan 
    for j in L4actinverse :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'L04[SQ0'+k+']']=='A1':
            L4act[j]=5
        if dataset.loc[i,'L04[SQ0'+k+']']=='A2':
            L4act[j]=4
        if dataset.loc[i,'L04[SQ0'+k+']']=='A3':
            L4act[j]=3
        if dataset.loc[i,'L04[SQ0'+k+']']=='A4':
            L4act[j]=2
        if dataset.loc[i,'L04[SQ0'+k+']']=='A5':
            L4act[j]=1
        if dataset.loc[i,'L04[SQ0'+k+']']=='A10':
            L4act[j]=np.nan 
        if dataset.loc[i,'L04[SQ0'+k+']']=='A11':
            L4act[j]=np.nan
        
    L4_act_rechlist=pd.DataFrame(operator.itemgetter(1,3,4,7,8,10)(L4act))
    L4_act_rech=L4_act_rechlist.mean()
    L4_act_rech=L4_act_rech.loc[0]
    
    L4_act_seuillist=pd.DataFrame(operator.itemgetter(2,5,6,9)(L4act))
    L4_act_seuil=L4_act_seuillist.mean()
    L4_act_seuil=L4_act_seuil.loc[0]
    

    
    L4actB=L4act.copy()
    del L4act[0]
    
        
    
    
    L_mouv_activitelist1=pd.DataFrame(operator.itemgetter(1,3,4,5,6,7,8)(L3mouvB)) ##à partir de notre profil
    L_mouv_activitelist2=pd.DataFrame(operator.itemgetter(2,4,5,6,7,9,10)(L4actB)) ##à partir de notre profil
    L_mouv_activitelist=pd.concat([L_mouv_activitelist1,L_mouv_activitelist2],ignore_index=True)
    L_mouv_activite=L_mouv_activitelist.mean()
    L_mouv_activite=L_mouv_activite.loc[0]
    
    
    
###################################

    if dataset.loc[i,'M01']=='A10'or dataset.loc[i,'M01']=='A11':
        M_douleurs=np.nan
        M1_douleurs_nb=np.nan
    if dataset.loc[i,'M01']=='A2':
        M_douleurs=0
        M1_douleurs_nb=0
    if dataset.loc[i,'M01']=='A1':
        M_douleurs=1
        
    if dataset.loc[i,'M02']=='A1':
        M1_douleurs_nb=1
    if dataset.loc[i,'M02']=='A2':
        M1_douleurs_nb=2
    if dataset.loc[i,'M02']=='A3':
        M1_douleurs_nb=3
    if dataset.loc[i,'M02']=='A4':
        M1_douleurs_nb=4
    #if dataset.loc[i,'M02']=='A10' or dataset.loc[i,'M02']=='A11':
     #   M1_douleurs_nb=np.nan
        
        ##### M2_douleurs_intmoy=!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ######################INTENSITE DE DOULEUR PAS ANALYSE
        
    
###################################
    
    NLpairs=[np.nan for i in range(0,11)]
    NLpairsendroit=[1,3,4,5,7,8,9]
    NLpairsinverse=[2,6,10]
    
    for j in NLpairsendroit :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'LL'+k+'[SQ001]']=='A1' and dataset.loc[i,'LL'+k+'[SQ002]']=='':
            NLpairs[j]=1
        if dataset.loc[i,'LL'+k+'[SQ001]']=='A2' and dataset.loc[i,'LL'+k+'[SQ002]']=='':
            NLpairs[j]=2
        if dataset.loc[i,'LL'+k+'[SQ001]']=='' and dataset.loc[i,'LL'+k+'[SQ002]']=='A1':
            NLpairs[j]=4
        if dataset.loc[i,'LL'+k+'[SQ001]']=='' and dataset.loc[i,'LL'+k+'[SQ002]']=='A2':
            NLpairs[j]=3           
            
    for j in NLpairsinverse :
        if j<10 : k='0'+str(j)
        if j>=10 : k=str(j)
        if dataset.loc[i,'LL'+k+'[SQ001]']=='A1' and dataset.loc[i,'LL'+k+'[SQ002]']=='':
            NLpairs[j]=4
        if dataset.loc[i,'LL'+k+'[SQ001]']=='A2' and dataset.loc[i,'LL'+k+'[SQ002]']=='':
            NLpairs[j]=3
        if dataset.loc[i,'LL'+k+'[SQ001]']=='' and dataset.loc[i,'LL'+k+'[SQ002]']=='A1':
            NLpairs[j]=1
        if dataset.loc[i,'LL'+k+'[SQ001]']=='' and dataset.loc[i,'LL'+k+'[SQ002]']=='A2':
            NLpairs[j]=2        

            
    del NLpairs[0] #car ne correspond à rien 
    NL_influence_pairslist=pd.DataFrame(NLpairs)
    NL_influence_pairs=NL_influence_pairslist.mean()
    NL_influence_pairs=NL_influence_pairs.loc[0]
    
    
############################################ A06 classe
    if dataset.loc[i,'A06']==' ':
        classe=np.nan
    if dataset.loc[i,'A06']=='A11':
        classe=np.nan
    if dataset.loc[i,'A07']==' ':
        classe=np.nan
    if dataset.loc[i,'A06']=='A1':
        classe='8'
    if dataset.loc[i,'A06']=='A2':
        classe='7'
    if dataset.loc[i,'A06']=='A3':
        classe='6'
    if dataset.loc[i,'A06']=='A4':
        classe='5'
    if dataset.loc[i,'A06']=='A5':
        classe='4'
    if dataset.loc[i,'A06']=='A6':
        classe='3'
    if dataset.loc[i,'A06']=='A7':
        classe='2'
    if dataset.loc[i,'A06']=='A8':
        classe='1'
    if dataset.loc[i,'A06']=='A9':
        classe='0'
    if dataset.loc[i,'A07']=='A2':
        classe='-1'
    if dataset.loc[i,'A07']=='A3':
        classe='-2'
    if dataset.loc[i,'A07']=='A4':
        classe='-3'
    if dataset.loc[i,'A07']=='A5':
        classe='-4'
    if dataset.loc[i,'A07']=='A6':
        classe='-5'
    if dataset.loc[i,'A07']=='A7':
        classe='-6'
    if dataset.loc[i,'A07']=='A8':
        classe='-7'
    if dataset.loc[i,'A07']=='A9':
        classe='-8'


#############################################################   a09 filiere
    
    filiere=np.nan
    if dataset.loc[i,'A05']=='A2':
        if dataset.loc[i,'A09']=='A1':
            filiere='art lettres langues'
        if dataset.loc[i,'A09']=='A2':
            filiere='droit eco gestion'
        if dataset.loc[i,'A09']=='A3':
            filiere='commerce'
        if dataset.loc[i,'A09']=='A4':
            filiere='santé'
        if dataset.loc[i,'A09']=='A5':
            filiere='sciences et techno'
        if dataset.loc[i,'A09']=='A6':
            filiere='STAPS'
        if dataset.loc[i,'A09']=='A7':
            filiere='SHS'
        if dataset.loc[i,'A09']=='A8':
            filiere=np.nan
        if dataset.loc[i,'A09']=='oth-':
            filiere=dataset.loc[i,'A09[other]']
            
    if dataset.loc[i,'A05']=='A3':
        filiere='travail'
        
        
        
#################################### N01 & N02

    diplome_parent=np.nan 


#################################### N03
    if dataset.loc[i,'N03']=='A1':
        sit_eco='5'
    if dataset.loc[i,'N03']=='A2':
        sit_eco='4'
    if dataset.loc[i,'N03']=='A3':
        sit_eco='3'
    if dataset.loc[i,'N03']=='A4':
        sit_eco='2'
    if dataset.loc[i,'N03']=='A5':
        sit_eco='1'
    if dataset.loc[i,'N03']=='A6':
        sit_eco=np.nan
    if dataset.loc[i,'N03']=='A7':
        sit_eco=np.nan

######################################################################## N 032 est-ce que français langue mat
    langue=dataset.loc[i,'N032']

################################N04
    if dataset.loc[i,'N04']=='A1':
        main=1
    if dataset.loc[i,'N04']=='A2':
        main=2
    if dataset.loc[i,'N04']=='A3':
        main=0
################################N08
    codepost=dataset.loc[i,'N08']
    if codepost=='':
        departement=np.nan
    else :
        departement=codepost[0]+codepost[1]

################ A08 connaissance du questionnaire 
    
    if dataset.loc[i,'A08']=='A11':
        connu_par=np.nan
    if dataset.loc[i,'A08']=='A1':
        connu_par='etab'
    if dataset.loc[i,'A08']=='A2':
        connu_par='pairs'
    if dataset.loc[i,'A08']=='A3':
        connu_par='sport'
    if dataset.loc[i,'A08']=='A4':
        connu_par='réseau'
    if dataset.loc[i,'A08']=='A5':
        connu_par='adult'
    if dataset.loc[i,'A08']=='A6':
        connu_par='autre'
    if dataset.loc[i,'A08']=='-oth-':
        connu_par='autre'
    
    puberte_groupe=np.nan
    if B_puberte >= 3.0 : 
        puberte_groupe=2
    if B_puberte > 3.7 : 
        puberte_groupe=3
    if B_puberte < 3.0 : 
        puberte_groupe=1
        
    age_groupe=np.nan
    if A3_age_y > 13.99 : 
        age_groupe=2
    if A3_age_y > 17.00 : 
        age_groupe=3
    if A3_age_y <= 13.99 : 
        age_groupe=1
    
   
    
################################### 
    Final_suj= pd.DataFrame([[numquest,
                              datebirth,
                              A1_sexe,
                                A2_genre,
                                A2_diff,
                                A3_age_m,
                                A3_age_y,
                                age_groupe,
                                A4_taille,
                                A5_poids,
                                A6_IMC,
                                B_puberte,
                                B4_pub_gon,
                                B4_pub_adre,
                                B2_regles,
                                puberte_groupe,
                                C_AP,
                                D_cons_soi,
                                D1_priv,
                                D2_pub,
                                D3_anx_soc,
                                E_IRI,
                                E1_perspec,
                                E2_empat,
                                F_reseau,
                                F2_reseauquant,
                                G_reseau,
                                G1_comp,
                                G2_cog,
                                G3_affec,
                                H_estime_corp,
                                H1_appa,
                                H2_attri,
                                H3_poids,
                                I_objectifi,
                                II_press_soc,
                                II1_fam,
                                II2_pairs,
                                II3_reseau,
                                J_cons_corps,
                                K_intero,
                                K1_notice,
                                K2_nodistract,
                                K3_notworry,
                                K4_emotion,
                                K5_listing,
                                K6_trust,
                                L1a_tact_dis_rech,
                                L1b_tact_dis_seuil,
                                L2_tact_soc,
                                L3_mouv_rech,
                                L3_mouv_habil,
                                L4_act_rech,
                                L4_act_seuil,
                                L_mouv_activite,
                                M_douleurs,
                                M1_douleurs_nb,
                                NL_influence_pairs,
                                classe,
                                filiere,
                                diplome_parent,
                                sit_eco,
                                langue,
                                main,
                                departement,
                                connu_par]],
                                index=None, columns=construits)

    Final=pd.concat([Final,Final_suj], ignore_index=True)
    

    item_suj=[numquest,A1_sexe,A2_genre,A2_diff,A3_age_m,A3_age_y,age_groupe,A4_taille,A5_poids,A6_IMC]
    item_suj.extend(Pub_list) 
    item_suj=item_suj+[C_AP]
    item_suj.extend(Dcons)
    item_suj.extend(EIRI)
    item_suj=item_suj+[F_reseau]
    item_suj=item_suj+[F02]
    item_suj=item_suj+[F03]
    item_suj.extend(Greseau) 
    item_suj.extend(Hest)
    item_suj.extend(Iobj)
    item_suj.extend(IIpress)
    item_suj.extend(Jcons)
    item_suj.extend(Kinter)
    item_suj.extend(L1tact)
    item_suj.extend(L2tact)
    item_suj.extend(L3mouv)
    item_suj.extend(L4act)
    item_suj=item_suj+[M1_douleurs_nb]
    item_suj.extend(NLpairs)
    item_suj=item_suj+[classe,filiere,diplome_parent,sit_eco,langue,main,departement]
    
    item_suj= pd.DataFrame(item_suj).transpose()
    item_suj.columns=item_col
    
    Final_item=pd.concat([Final_item,item_suj], ignore_index=True)
    print(f'ok for {numquest}')
    
Scale_bysub_all=Final
Scale_bysub_all.to_csv(wd+'\\Analyses\\Scale_bysub_all.csv')


Item_bysub_all=Final_item
Item_bysub_all.to_csv(wd+'\\Analyses\\Item_bysub_all.csv')


#%%ne garder que les valeures qui nous interessent sur lesquelles faires des stats numériques 

non_numeric_columns = Scale_bysub_all.select_dtypes(exclude=['number']).columns

Scale_bysub_num=Scale_bysub_all.drop(['date_nais', 'M_douleurs','classe', 'filiere','langue', 'main', 'departement','diplome_parent', 'connu_par'],axis=1)
Item_bysub_num=Item_bysub_all.drop([ 'classe', 'filiere','langue', 'main', 'departement','diplome_parent'],axis=1)
SubScale_bysub_num=Scale_bysub_num.drop(['B4_pub_gon', 'B4_pub_adre', 'B2_regles', 'B_pub_group',
                                         'D_cons_soi', 'E_IRI', 'F_reseau', 'F2_reseauquant','G_reseau',
                                         'H_estime_corp', 'II_press_soc', 'K_intero',
                                         'L1a_tact_dis_rech', 'L1b_tact_dis_seuil','L3_mouv_rech',
                                         'L3_mouv_habil','L4_act_rech','L4_act_seuil'],axis=1)


Scale_bysub_num.to_csv(wd+'\\Analyses\\Scale_bysub_num.csv')
Item_bysub_num.to_csv(wd+'\\Analyses\\Item_bysub_num.csv')
SubScale_bysub_num.to_csv(wd+'\\Analyses\\SubScale_bysub_num.csv')

#%%imputer les données manquantes


import fancyimpute
from fancyimpute import IterativeImputer as MICE

##############################remplacer les données manquantes dans chacune des échelles et sous échelle par la méthode MICE 
Scale_bysub_imputed=MICE().fit_transform(Scale_bysub_num)
Scale_bysub_imputed=pd.DataFrame(Scale_bysub_imputed)

##renommer les colonnes de Scale_bysub_imputed
num_col=list(Scale_bysub_num.columns)
imputed_col=list(Scale_bysub_imputed.columns)
mapping=dict(zip(imputed_col, num_col))
Scale_bysub_imputed = Scale_bysub_imputed.rename(columns=mapping)
Scale_bysub_imputed.to_csv(wd+'\\Analyses\\Scale_bysub_imputed.csv')


#################################remplacer les données manquantes dans chacune des sous échelle par la méthode MICE 
SubScale_bysub_num_imputed=MICE().fit_transform(SubScale_bysub_num)
SubScale_bysub_num_imputed=pd.DataFrame(SubScale_bysub_num_imputed)

##renommer les colonnes de Scale_bysub_imputed
num_col=list(SubScale_bysub_num.columns)
imputed_col=list(SubScale_bysub_num_imputed.columns)
mapping=dict(zip(imputed_col, num_col))
SubScale_bysub_num_imputed = SubScale_bysub_num_imputed.rename(columns=mapping)
SubScale_bysub_num_imputed.to_csv(wd+'\\Analyses\\SubScale_bysub_num_imputed.csv')




##############################remplacer les données manquantes dans chacun des item par la méthode MICE 

Item_bysub_imputed=MICE().fit_transform(Item_bysub_num)
Item_bysub_imputed=pd.DataFrame(Item_bysub_imputed)

##renommer les colonnes de Item_bysub_imputed
num_col=list(Item_bysub_num.columns)
imputed_col=list(Item_bysub_imputed.columns)
mapping=dict(zip(imputed_col, num_col))
Item_bysub_imputed = Item_bysub_imputed.rename(columns=mapping)
Item_bysub_imputed.to_csv(wd+'\\Analyses\\Item_bysub_imputed.csv')

#%% Pour regrouper item et subscale et scale dans un seul data 


# Obtenir les noms de colonnes uniques de b
cols_item = set(Item_bysub_imputed)
cols_Scale = set(Scale_bysub_imputed)
cols_commun = cols_item & cols_Scale


# Trouver les index des colonnes à conserver dans a
cols_to_keep = [col for col in Item_bysub_imputed.columns if col not in cols_commun]
# Copier le tableau en supprimant les colonnes
Item_bysub_copie = Item_bysub_imputed[cols_to_keep].copy()

ItemandScale_imp_bySub=pd.concat([Scale_bysub_imputed,Item_bysub_copie], axis=1)
ItemandScale_imp_bySub.to_csv(wd+'\\Analyses\\ItemandScale_imp_bySub.csv')

#%% Pour savoir le nb de données manquantes quand on fait les subscale avant de remplacer les données manquantes 

miss_final = {}

# Pour chaque colonne du DataFrame
for col in Scale_bysub_num.columns:
    # Sélectionner les lignes qui contiennent ' ', 'A11' ou 'A10'
    selected_rows = Scale_bysub_num[Scale_bysub_num[col].isna()]
    # Calculer le pourcentage de lignes sélectionnées par rapport au nombre total de lignes
    percentage = len(selected_rows) / len(dataset)
    # Ajouter le pourcentage à la liste des résultats
    miss_final[col] = percentage
    

miss_final_parsuj= {}

# Pour chaque ligne du DataFrame
for index, row in Scale_bysub_num.iterrows():
    # Récupérer le numéro de sujet
    subject_number = row["numquest"]
    # Compter le nombre d'occurrences de 'A10'
    count = (row.isna()).sum()
    # Ajouter le nombre d'occurrences et le numéro de sujet au dictionnaire des résultats
    miss_final_parsuj[subject_number] = count

new_dict = {}
# Pour chaque entrée dans le dictionnaire original
for key, value in miss_final_parsuj.items():
    # Si la valeur n'est pas égale à 0
    if value != 0:
        # Ajouter l'entrée au nouveau dictionnaire
        new_dict[key] = value
# Remplacer le dictionnaire original par le nouveau dictionnaire
miss_final_parsuj = new_dict


# Convertir le dictionnaire en DataFrame
miss_final_parsuj = pd.DataFrame.from_dict(miss_final_parsuj,orient='index',columns=['miss'])
# Créer le countplot
sns.countplot(x='miss', data=miss_final_parsuj)
plt.xlabel("nb de miss")
plt.ylabel("nb de suj")
plt.title("nb de suj ayant mis je ne sais pas")
plt.savefig(wd+'\\Analyses\\Plot_Datasbrutes\\missing_final.png')
plt.close()

nan_count = Scale_bysub_num.isna().sum().sum()


# Afficher le résultat
print("Nombre de valeurs manquantes Scale :", nan_count)

#%% Pour savoir le nb de données manquantes dans les items donc data brutes Item_bysub_num=Item_bysub_all.drop([ 'M_douleurs','classe', 'filiere','langue', 'main', 'departement','diplome_parent'],axis=1)
Item_bysub_test=Item_bysub_all.drop(['Iobj08','classe', 'filiere','langue', 'main', 'departement','diplome_parent'],axis=1)

miss_item = {}

# Pour chaque colonne du DataFrame
for col in Item_bysub_test.columns:
    # Sélectionner les lignes qui contiennent ' ', 'A11' ou 'A10'
    selected_rows = Item_bysub_test[Item_bysub_test[col].isna()]
    # Calculer le pourcentage de lignes sélectionnées par rapport au nombre total de lignes
    percentage = len(selected_rows) / len(dataset)
    # Ajouter le pourcentage à la liste des résultats
    miss_item[col] = percentage
    
    
pourcentage_nan_paritem = Item_bysub_test.isna().mean().round(2)

pourcentage_nan_paritem = Item_bysub_test.isna().mean().mean() * 100


print(pourcentage_nan_paritem)