# -*- coding: utf-8 -*-
"""
Created on Sun Jan 15 21:25:46 2023

@author: lisa
"""


#import random
#import numpy as np
#import scipy 
#from scipy import *
#import csv
#import sys
import pandas as pd
from pathlib import Path
import os
from datetime import datetime
#import operator
from dateutil.relativedelta import *
from datetime import date
import seaborn as sns
import matplotlib
import matplotlib.pyplot as plt


#!!!set workingdiretory here
wd='C:\\Users\\lisa\Documents\\-These\\MANIP_Quest\\Share'
os.chdir(wd)

#load dataset
dataset=pd.read_csv('.\\Analyses\\SubScale_bysub_num_imputed.csv')


Age1=dataset[dataset['A3_age_y']<=13.5]
Age2=dataset[dataset['A3_age_y']>13.5]
Age2=Age2[Age2['A3_age_y']<=17]
Age3=dataset[dataset['A3_age_y']>17]


Pub1=dataset[dataset['B_puberte']<3.4]
Pub2=dataset[dataset['B_puberte']>=3.4]

Fem=dataset[dataset['A1_sexe']==1]
Mal=dataset[dataset['A1_sexe']==2]


FemPub1=Pub1[Pub1['A1_sexe']==1]
FemPub2=Pub2[Pub2['A1_sexe']==1]
FemPub1.to_csv(wd+'\\Analyses\SubScale_bysub_num_imputed_FemPub1.csv')
FemPub2.to_csv(wd+'\\Analyses\\SubScale_bysub_num_imputed_FemPub2.csv')


MalPub1=Pub1[Pub1['A1_sexe']==2]
MalPub2=Pub2[Pub2['A1_sexe']==2]


K6low=dataset[dataset['K6_trust']<dataset['K6_trust'].mean()]
K6high=dataset[dataset['K6_trust']>dataset['K6_trust'].mean()]

D1low=dataset[dataset['D1_priv']<dataset['D1_priv'].mean()]
D1high=dataset[dataset['D1_priv']>dataset['D1_priv'].mean()]



#%%

col_network=['A1_sexe',
             'A2_diff',
                 'A3_age_m',
                 'A6_IMC',
                 'B_puberte',
                 'B2_regles',
                 'B_pub_group'
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
                 'M1_douleurs_nb',
                 'N_influence_pairs']
    
col_network_subscale=['A1_sexe',
                      'A2_diff',
                 'A3_age_m',
                 'A6_IMC',
                 'B_puberte',
                 'C_AP',
                 'D1_priv',
                 'D2_pub',
                 'D3_anx_soc',
                 'E1_perspec',
                 'E2_empat',
                 'F2_reseauquant',
                 'G1_comp',
                 'G2_cog',
                 'G3_affec',
                 'H1_appa',
                 'H2_attri',
                 'H3_poids',
                 'I_objectifi',
                 'II1_fam',
                 'II2_pairs',
                 'II3_reseau',
                 'J_cons_corps',
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
                 'M1_douleurs_nb',
                 'N_influence_pairs',
                 'sit_eco']


col_network_mean=['A1_sexe',
                  'A2_diff',
                 'A2_genre',
                 'A3_age_m',
                 'A4_taille',
                 'A5_poids',
                 'A6_IMC',
                 'B_puberte',
                 'B2_regles',
                 'C_AP',
                 'D_cons_soi',
                 'E_IRI',
                 'F_reseau',
                 'F2_reseauquant',
                 'G_reseau',
                 'H_estime_corp',
                 'I_objectifi',
                 'II_press_soc',
                 'J_cons_corps',
                 'K_intero',
                 'L1a_tact_dis_rech',
                 'L1b_tact_dis_seuil',
                 'L2_tact_soc',
                 'L3_mouv_rech',
                 'L3_mouv_habil',
                 'L4_act_rech',
                 'L4_act_seuil',
                 'M1_douleurs_nb',
                 'N_influence_pairs',
                 'sit_eco']



Scale_fornetwork_subscale=dataset.copy()
Scale_fornetwork_subscale=Scale_fornetwork_subscale[col_network_subscale]
Scale_fornetwork_subscale.to_csv(wd+'\\Analyses\\Scale_fornetwork_subscale.csv')

Scale_fornetwork_mean=dataset.copy()
Scale_fornetwork_mean=Scale_fornetwork_mean[col_network_mean]
Scale_fornetwork_mean.to_csv(wd+'\\Analyses\\Scale_fornetwork_mean.csv')

#%%

col_binary=['A1_sexe',
                 'A2_genre',
                 'F_reseau',]

col_nonbinaire=[]
for i in col_network_subscale : 
    if i not in col_binary:
        col_nonbinaire.append(i) 

col_asupp=['B_puberte','L1a_tact_dis_rech',
'L1b_tact_dis_seuil',
'L3_mouv_rech','J_cons_corps',
'L3_mouv_habil',
'L4_act_rech',
'L4_act_seuil','M_douleurs','F2_reseauquant']

col_analyses=[]
for i in col_network_subscale : 
    if i not in col_asupp:
        col_analyses.append(i)
        
Scale_fornetwork2=dataset.copy()
Scale_fornetwork2=Scale_fornetwork_subscale[col_analyses]
Scale_fornetwork2.to_csv(wd+'\\Analyses\\Scale_fornetwork2.csv')

        
Scale_Age1_fornetwork2=Age1.copy()
Scale_Age1_fornetwork2=Scale_Age1_fornetwork2[col_analyses]
Scale_Age1_fornetwork2.to_csv(wd+'\\Analyses\\Scale_Age1_fornetwork2.csv')

Scale_Age2_fornetwork2=Age2.copy()
Scale_Age2_fornetwork2=Scale_Age2_fornetwork2[col_analyses]
Scale_Age2_fornetwork2.to_csv(wd+'\\Analyses\\Scale_Age2_fornetwork2.csv')

Scale_Age3_fornetwork2=Age3.copy()
Scale_Age3_fornetwork2=Scale_Age3_fornetwork2[col_analyses]
Scale_Age3_fornetwork2.to_csv(wd+'\\Analyses\\Scale_Age3_fornetwork2.csv')

       
Scale_Pub1_fornetwork2=Pub1.copy()
Scale_Pub1_fornetwork2=Scale_Pub1_fornetwork2[col_analyses]
Scale_Pub1_fornetwork2.to_csv(wd+'\\Analyses\\Scale_Pub1_fornetwork2.csv')

Scale_Pub2_fornetwork2=Pub2.copy()
Scale_Pub2_fornetwork2=Scale_Pub2_fornetwork2[col_analyses]
Scale_Pub2_fornetwork2.to_csv(wd+'\\Analyses\\Scale_Pub2_fornetwork2.csv')

Scale_Fem_fornetwork2=Fem.copy()
Scale_Fem_fornetwork2=Scale_Fem_fornetwork2[col_analyses]
Scale_Fem_fornetwork2.to_csv(wd+'\\Analyses\\Scale_Fem_fornetwork2.csv')

Scale_Mal_fornetwork2=Mal.copy()
Scale_Mal_fornetwork2=Scale_Mal_fornetwork2[col_analyses]
Scale_Mal_fornetwork2.to_csv(wd+'\\Analyses\\Scale_Mal_fornetwork2.csv')

       
Scale_FemPub1_fornetwork2=FemPub1.copy()
Scale_FemPub1_fornetwork2=Scale_FemPub1_fornetwork2[col_analyses]
Scale_FemPub1_fornetwork2.to_csv(wd+'\\Analyses\\Scale_FemPub1_fornetwork2.csv')

Scale_FemPub2_fornetwork2=FemPub2.copy()
Scale_FemPub2_fornetwork2=Scale_FemPub2_fornetwork2[col_analyses]
Scale_FemPub2_fornetwork2.to_csv(wd+'\\Analyses\\Scale_FemPub1_fornetwork2.csv')

Scale_MalPub1_fornetwork2=MalPub1.copy()
Scale_MalPub1_fornetwork2=Scale_MalPub1_fornetwork2[col_analyses]
Scale_MalPub1_fornetwork2.to_csv(wd+'\\Analyses\\Scale_MalPub1_fornetwork2.csv')

Scale_MalPub2_fornetwork2=MalPub2.copy()
Scale_MalPub2_fornetwork2=Scale_MalPub2_fornetwork2[col_analyses]
Scale_MalPub2_fornetwork2.to_csv(wd+'\\Analyses\\Scale_MalPub2_fornetwork2.csv')



Scale_K6low_fornetwork2=K6low.copy()
Scale_K6low_fornetwork2=Scale_K6low_fornetwork2[col_analyses]
Scale_K6low_fornetwork2.to_csv(wd+'\\Analyses\\Scale_K6low_fornetwork2.csv')

Scale_K6high_fornetwork2=K6high.copy()
Scale_K6high_fornetwork2=Scale_K6high_fornetwork2[col_analyses]
Scale_K6high_fornetwork2.to_csv(wd+'\\Analyses\\Scale_K6high_fornetwork2.csv')

Scale_D1low_fornetwork2=D1low.copy()
Scale_D1low_fornetwork2=Scale_D1low_fornetwork2[col_analyses]
Scale_D1low_fornetwork2.to_csv(wd+'\\Analyses\\Scale_D1low_fornetwork2.csv')

Scale_D1high_fornetwork2=D1high.copy()
Scale_D1high_fornetwork2=Scale_D1high_fornetwork2[col_analyses]
Scale_D1high_fornetwork2.to_csv(wd+'\\Analyses\\Scale_D1high_fornetwork2.csv')


#%%# Calculer le nombre d'occurrences de chaque valeur dans la colonne A1_sexe

total = Pub2['A1_sexe'].count()
counts = Pub2['A1_sexe'].value_counts()
pourcentage_2 = (counts[2] / total) * 100


total = Pub1['A1_sexe'].count()
counts = Pub1['A1_sexe'].value_counts()
pourcentage_2 = (counts[2] / total) * 100

