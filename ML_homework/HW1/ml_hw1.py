# -*- coding: utf-8 -*-
"""
Created on Sun Jan 19 20:05:51 2020

@author: aravind
"""

import pandas as pd
import numpy as np
traindata = pd.read_csv("train.csv")
testdata = pd.read_csv("test.csv")
combine = [traindata,testdata]
combinedata = pd.concat(combine)
combinedata['Age'] = combinedata['Age'].fillna(combinedata['Age'].mean()).round(2)
combinedata['adult'] = 0
combinedata['adult'][combinedata['Age']>=18] = 1 
combinedata['AgeGroup']= 0
combinedata['AgeGroup'][combinedata['Age']<1] = 'Infant'
combinedata['AgeGroup'][(combinedata['Age']>=18) & (combinedata['Age']<=59.9)] = 'Adult'
combinedata['AgeGroup'][(combinedata['Age']>=13.0) & (combinedata['Age']<=17.9)] = 'Teen'
combinedata['AgeGroup'][combinedata['Age']>=60] = 'Senior'
combinedata['AgeGroup'][(combinedata['Age']>=1.0) & (combinedata['Age']<=12.0)] = 'Children'
combinedata['Age'].count()
numfeature_mean= combinedata.iloc[:,[0,3,7]].mean()
numfeature_count= combinedata.iloc[:,[0,3,7]].count()
numfeature_std = combinedata.iloc[:,[0,3,7]].std()
numfeature_min = combinedata.iloc[:,[0,3,7]].min()
numfeature_max = combinedata.iloc[:,[0,3,7]].max()
numfeatures = combinedata[['Age','Pclass','Fare']]
numfeature_25percentile = numfeatures.quantile(0.25)
numfeature_50percentile = numfeatures.quantile(0.5)
numfeature_75percentile = numfeatures.quantile(0.75)
catfeature_count= combinedata.iloc[:,[0,3,7]].count()
Sex_category = combinedata['Sex'].value_counts(dropna=False)
Sibling_category = combinedata['SibSp'].value_counts(dropna=False)
parent_child_category = combinedata['Parch'].value_counts(dropna=False)
passengerid_category = combinedata['PassengerId'].value_counts(dropna=False)
AgeGroup_category = combinedata['AgeGroup'].value_counts(dropna=False)
adult_category = combinedata['adult'].value_counts(dropna=False)
sex_unique_val = combinedata['Sex'].unique()
print(sex_unique_val)
sibling_unique_val = combinedata['SibSp'].unique()
print(sibling_unique_val)
Parch_unique_val = combinedata['Parch'].unique()
print(Parch_unique_val)
PassngerID_unique_val = combinedata['PassengerId'].unique()
print(PassngerID_unique_val)
AgeGroup_unique_val = combinedata['AgeGroup'].unique()
print(AgeGroup_unique_val)
Adult_unique_val = combinedata['adult'].unique()
print(Adult_unique_val)
Sex_category_Frequent = combinedata['Sex'].unique().max()
print(Sex_category_Frequent)
sibling_freq_val = combinedata['SibSp'].unique().max()
print(sibling_freq_val)
Parch_val_Freq = combinedata['Parch'].unique().max()
print(Parch_val_Freq)
PassngerID_val_freq = combinedata['PassengerId'].unique().max()
print(PassngerID_val_freq)
AgeGroup_val_freq = combinedata['AgeGroup'].unique().max()
print(AgeGroup_val_freq)
Adult_val_freq = combinedata['adult'].unique().max()
print(Adult_val_freq)

Sex_category_cat_high = combinedata['Sex'].value_counts.argmax()
print(Sex_category_cat_high)