# -*- coding: utf-8 -*-
"""
Created on Sun Jan 22 09:40:21 2023

@author: lisa
"""
import pandas as pd
from pathlib import Path
import os
from datetime import datetime
from dateutil.relativedelta import *
from datetime import date
import seaborn as sns
import matplotlib
import matplotlib.pyplot as plt
from scipy.stats import shapiro
from scipy.stats import anderson
from scipy.stats import kstest


for ele in datacsv.columns: 
    sns.countplot(x=ele, data=datacsv)
    plt.title(f'Count of values in {ele}')
    #plt.savefig(wd+'\\Analyses\\Plot_Datasbrutes\\'+ele+'.png')
    plt.show()
    