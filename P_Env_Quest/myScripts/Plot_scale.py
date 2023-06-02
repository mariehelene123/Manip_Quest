# -*- coding: utf-8 -*-
"""
Created on Fri Dec 16 14:55:54 2022

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
from scipy.stats import shapiro
from scipy.stats import anderson
from scipy.stats import kstest


#!!!set workingdiretory here
wd='C:\\Users\\lisa\Documents\\-These\\MANIP_Quest\\Share'
os.chdir(wd)

#load dataset
dataset=pd.read_csv('.\\Analyses\\Measures_bysub.csv')
dataset_item=pd.read_csv('.\\Analyses\\Item_bysub_all.csv')


col_notraited=['diplome_parent']


#%%
z=0
for i in dataset.index:
    if dataset.loc[i,'A1_sexe'] != dataset.loc[i, 'A2_diff']:
        z=z+1


mask = dataset['A1_sexe'].ne(dataset['A2_diff'])
filtered_data = dataset[mask]


#%%
col_binary=['A1_sexe',
                 'A2_diff']


for col in col_binary:
    sns.countplot(x=col, data=dataset)
    plt.title(f'Count of values in {col}')
    plt.savefig(wd+'\\Analyses\\Plot_Datasbrutes\\'+col+'.png')
    plt.show()

col_disc=['A3_age_m','C_AP','M1_douleurs_nb','classe',
'filiere',
'sit_eco',
'langue']

for col in col_disc:
    sns.countplot(x=col, data=dataset)
    plt.title(f'Count of values in {col}')
    plt.savefig(wd+'\\Analyses\\Plot_Datasbrutes\\'+col+'.png')
    plt.show()

#%%

col_toplot=list(dataset.columns)
col_toplot=col_toplot[5:60]
col_toplot = [col for col in col_toplot if col not in col_disc+col_binary+col_notraited]


for ele in col_toplot: 
    sns.displot(
        dataset, x=ele, 
        kind="kde")
    plt.title(f'KDE plot of {ele}')
    plt.savefig(wd+'\\Analyses\\Plot_Datasbrutes\\'+ele+'.png')
    plt.show()
    
for ele in col_toplot: 
    ele_clean=dataset[ele].dropna()
    stat, p = shapiro(ele_clean)
    # interpret test results
    alpha = 0.05
    if p > alpha:
        print(f'{ele}  looks Gaussian (fail to reject H0) with p-value = {p}')
    else:
        pass
        #print(f'{ele} does not look Gaussian (reject H0) with p-value = {p}')

for ele in col_toplot:
    ele_clean=dataset[ele].dropna()
    anderson_results = anderson(ele_clean)
    print('A-squared:', anderson_results.statistic)
    print('Critical values:', anderson_results.critical_values)
    if anderson_results.statistic < anderson_results.critical_values[2]:
        print(f'{ele} looks Gaussian (fail to reject H0)')
    else:
        pass
        #print(f'{ele} does not look Gaussian (reject H0)')

ks_results = kstest(ele_clean, 'norm')

#%%
#Plotbysub=sns.catplot(data=dataset_item, x="suj", y="ownership",aspect=3, height=10, kind='box')