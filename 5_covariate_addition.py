# -*- coding: utf-8 -*-
"""
Created on Tue Nov 12 09:32:48 2019

@author: jurem

Add demographic data to the prescriptions.

This exports 2 data frames.

1. 'age_sex_formatted.csv' is the updated 'age_sex.csv' and includes columns:
    - 'id'
    - 'data_provider': 1 = England (Vision), 2 = Scotland, 3 = England (TPP), 4 = Wales
    - 'sex'
    - 'birth_year'
    - 'birth_month'
    - 'birth_date': the month and year of birth formatted in a way that is recognisable for R- and Python- datetime libraries

2. '4_demographics.csv': The exported data frame RETAINS the following columns:
    - 'id': participant id
    - 'date': when the prescription was issued
    - 'prescription': full title of the prescription
    - 'aa': anticholinergic score of the drug based on the scale
    - 'drug_scale': the name of the drug as it appears in the scale
The following new columns are ADDED:
    - 'sex'
    - 'birth_date' (see above)
    - 'date_1': date of the 1st assessment
    - 'date_2': date of the 2nd assessment
    - 'date_3': date of the 3rd assessment
    - 'date_4': date of the 4th assessment
    - 'date_follow_up': date of the follow-up assessment  
    - 'smoking': current-, ex- or non-smoker
    - 'activity': level of physical activity
    - 'education': graduate degree or not
    - 'deprivation': Townsend index of deprivation
    - 'bmi': BMI
"""


import pandas as pd
import numpy as np
import os

os.chdir('D:\\PhD\\MAIN')


meds = pd.read_csv('UK Biobank/Processed files/tables/5_aa_scales_dosage_v2.csv', header=0, sep="|", dtype = str, encoding = 'cp1252')




## add age and sex

age_sex = pd.read_csv('UK Biobank/Raw files/age_sex.csv')
# transform id, birth year, and month to strings
age_sex['id'] = age_sex['id'].astype(str)
age_sex['birth_year'] = age_sex['birth_year'].astype(str)
age_sex['birth_month'] = age_sex['birth_month'].astype(str)
# add a column indicating the length of the month-string
age_sex['month_len'] = age_sex.loc[~age_sex['birth_month'].isna(), 'birth_month'].apply(len)
# add a '0' to the front of all 1-digit months so as to harmonize the formatting
age_sex.loc[age_sex['month_len']==1, 'birth_month'] = '0' + age_sex.loc[age_sex['month_len']==1, 'birth_month']
# create a new column with the properly formated month and year
age_sex['birth_date'] = "01" + '/' + age_sex['birth_month'] + '/' + age_sex['birth_year']
age_sex['birth_date'] = pd.to_datetime(age_sex['birth_date'], format = '%d/%m/%Y')
# merge the datasets
age_sex.drop(['birth_year','birth_month','month_len'], axis=1, inplace=True)
meds = pd.merge(meds, age_sex, on='id', how='left')
# add a column for age at prescription
meds['date'] = pd.to_datetime(meds.date)
meds['med_age'] = meds['date'] -  meds['birth_date']
meds['med_age'] = meds['med_age'].dt.total_seconds()/(24*3600)/365.242



## add dates of assessment
test_dates = pd.read_csv('UK Biobank/Raw files/ass_date.csv') # read in the date of the assessment
test_dates.columns = ['id', 'date_0', 'date_1', 'date_2', 'date_3'] # rename the columns
test_dates['id'] = test_dates['id'].astype(str)
# transorm to datetime format
test_dates['date_0'] = pd.to_datetime(test_dates['date_0'], format = '%Y-%m-%d')
test_dates['date_1'] = pd.to_datetime(test_dates['date_1'], format = '%Y-%m-%d')
test_dates['date_2'] = pd.to_datetime(test_dates['date_2'], format = '%Y-%m-%d')
test_dates['date_3'] = pd.to_datetime(test_dates['date_3'], format = '%Y-%m-%d')
# merge the datasets
meds = pd.merge(meds, test_dates, on='id', how='left')
del test_dates
follow_up_dates = pd.read_csv('UK Biobank/Raw files/follow_up_date.csv') # read in the date of the assessment
follow_up_dates = follow_up_dates[['eid', 'X20140.0.0']]
follow_up_dates.columns = ['id', 'date_follow_up'] # rename the columns
follow_up_dates['date_follow_up'] = pd.to_datetime(follow_up_dates['date_follow_up'], format = '%Y-%m-%d')
follow_up_dates['id'] = follow_up_dates['id'].astype(str)
meds = pd.merge(meds, follow_up_dates, on='id', how='left')
del follow_up_dates



## education
# read in education
education = pd.read_csv('UK Biobank/Raw files/education.csv', header=0, sep=",", dtype = str)
# change column names
education.columns = ['id', 'age_completed_0', 'age_completed_1', 'age_completed_2', 'qualifications_0_0',
                     'qualifications_0_1', 'qualifications_0_2', 'qualifications_0_3', 'qualifications_0_4',
                     'qualifications_0_5', 'qualifications_1_0', 'qualifications_1_1', 'qualifications_1_2', 
                     'qualifications_1_3', 'qualifications_1_4', 'qualifications_1_5', 'qualifications_2_0', 
                     'qualifications_2_1', 'qualifications_2_2', 'qualifications_2_3', 'qualifications_2_4', 
                     'qualifications_2_5', 'qualifications_3_0', 'qualifications_3_1', 'qualifications_3_2', 
                     'qualifications_3_3', 'qualifications_3_4', 'qualifications_3_5', 'year_ended']
# choose only education code columns
education.drop(['age_completed_0', 'age_completed_1', 'age_completed_2', 'year_ended'], axis=1, inplace=True)
# change all non-graduate-degree codings into 0
education[(education=='2') | (education=='3') | (education=='4') | (education=='5') | (education=='6') | (education=='-7')] = '0'
# change all non-answers to NaN's
education[(education=='-3')] = np.nan
# remove rows with only NaN's
education = education.dropna(subset=['qualifications_0_0', 'qualifications_0_1', 'qualifications_0_2', 
                                     'qualifications_0_3', 'qualifications_0_4', 'qualifications_0_5', 
                                     'qualifications_1_0', 'qualifications_1_1', 'qualifications_1_2', 
                                     'qualifications_1_3', 'qualifications_1_4', 'qualifications_1_5', 
                                     'qualifications_2_0', 'qualifications_2_1', 'qualifications_2_2', 
                                     'qualifications_2_3', 'qualifications_2_4', 'qualifications_2_5', 
                                     'qualifications_3_0', 'qualifications_3_1', 'qualifications_3_2', 
                                     'qualifications_3_3', 'qualifications_3_4', 'qualifications_3_5'], 
                             axis='rows', how='all')
# initialize columns
education['education_0'] = np.nan; education['education_1'] = np.nan; education['education_2'] = np.nan; education['education_3'] = np.nan
# put college-degree codes found in either one of the columns into a single new column
education.loc[(education['qualifications_0_0']=='0') | (education['qualifications_0_1']=='0') | (education['qualifications_0_2']=='0') | (education['qualifications_0_3']=='0') | (education['qualifications_0_4']=='0') | (education['qualifications_0_5']=='0'), 'education_0'] = '0'
education.loc[(education['qualifications_0_0']=='1') | (education['qualifications_0_1']=='1') | (education['qualifications_0_2']=='1') | (education['qualifications_0_3']=='1') | (education['qualifications_0_4']=='1') | (education['qualifications_0_5']=='1'), 'education_0'] = '1'
education.loc[(education['qualifications_1_0']=='0') | (education['qualifications_1_1']=='0') | (education['qualifications_1_2']=='0') | (education['qualifications_1_3']=='0') | (education['qualifications_1_4']=='0') | (education['qualifications_1_5']=='0'), 'education_1'] = '0'
education.loc[(education['qualifications_1_0']=='1') | (education['qualifications_1_1']=='1') | (education['qualifications_1_2']=='1') | (education['qualifications_1_3']=='1') | (education['qualifications_1_4']=='1') | (education['qualifications_1_5']=='1'), 'education_1'] = '1'
education.loc[(education['qualifications_2_0']=='0') | (education['qualifications_2_1']=='0') | (education['qualifications_2_2']=='0') | (education['qualifications_2_3']=='0') | (education['qualifications_2_4']=='0') | (education['qualifications_2_5']=='0'), 'education_2'] = '0'
education.loc[(education['qualifications_2_0']=='1') | (education['qualifications_2_1']=='1') | (education['qualifications_2_2']=='1') | (education['qualifications_2_3']=='1') | (education['qualifications_2_4']=='1') | (education['qualifications_2_5']=='1'), 'education_2'] = '1'
education.loc[(education['qualifications_3_0']=='0') | (education['qualifications_3_1']=='0') | (education['qualifications_3_2']=='0') | (education['qualifications_3_3']=='0') | (education['qualifications_3_4']=='0') | (education['qualifications_3_5']=='0'), 'education_3'] = '0'
education.loc[(education['qualifications_3_0']=='1') | (education['qualifications_3_1']=='1') | (education['qualifications_3_2']=='1') | (education['qualifications_3_3']=='1') | (education['qualifications_3_4']=='1') | (education['qualifications_3_5']=='1'), 'education_3'] = '1'
# keep only the relevant columns
education = education[['id','education_0','education_1','education_2','education_3']]
# add education to main data frame
meds = pd.merge(meds, education, on='id', how='left')
# for each prescription, keep only the date of education before the prescription was issued
meds['med_education'] = meds['education_0'].copy() # initiate new column; default education is education at visit 0
meds.loc[meds['date'] >= meds['date_1'], 'med_education'] = meds['education_1'].copy() # for all the prescriptions issued after date_1, education will correspond to the education at the visit 1
meds.loc[meds['date'] >= meds['date_2'], 'med_education'] = meds['education_2'].copy() 
meds.loc[meds['date'] >= meds['date_3'], 'med_education'] = meds['education_3'].copy()
meds.drop(['education_1','education_3'], axis=1, inplace=True)
del education



## deprivation
# read in the Townsend
deprivation = pd.read_csv('UK Biobank/Raw files/deprivation.csv', header=0, dtype = str)
# merge with main data frame
meds = pd.merge(meds, deprivation, on='id', how='left')
del deprivation



## smoking
smoking = pd.read_csv('UK Biobank/Raw files/tobacco.csv', header=0, dtype = str)
smoking.columns = ['id', 'smoking_0', 'smoking_1', 'smoking_2', 'smoking_3', 'age_stop_0', 'age_stop_1', 'age_stop_2', 'age_stop_3']
smoking.drop(['age_stop_0', 'age_stop_1', 'age_stop_2', 'age_stop_3'], axis=1, inplace=True)
smoking['smoking_0'] = smoking.smoking_0.astype(str)
smoking['smoking_1'] = smoking.smoking_1.astype(str)
smoking['smoking_2'] = smoking.smoking_2.astype(str)
smoking['smoking_3'] = smoking.smoking_3.astype(str)
smoking.loc[(smoking['smoking_0'] == '-3') | (smoking['smoking_0'] == 'nan'), 'smoking_0'] = np.nan
smoking.loc[(smoking['smoking_1'] == '-3') | (smoking['smoking_1'] == 'nan'), 'smoking_1'] = np.nan
smoking.loc[(smoking['smoking_2'] == '-3') | (smoking['smoking_2'] == 'nan'), 'smoking_2'] = np.nan
smoking.loc[(smoking['smoking_3'] == '-3') | (smoking['smoking_3'] == 'nan'), 'smoking_3'] = np.nan
# add to the main data frame
meds = pd.merge(meds, smoking, on='id', how='left')
# update using 2nd and 3rd visits
meds['med_smoking'] = meds['smoking_0'].copy()
meds.loc[meds['date'] >= meds['date_1'], 'med_smoking'] = meds['smoking_1'].copy() 
meds.loc[meds['date'] >= meds['date_2'], 'med_smoking'] = meds['smoking_2'].copy()
meds.loc[meds['date'] >= meds['date_3'], 'med_smoking'] = meds['smoking_3'].copy()
# remove columns
meds.drop(['smoking_1','smoking_3'], axis=1, inplace=True)
del smoking



## alcohol consumption
# read in the data frame
alcohol = pd.read_csv('UK Biobank/Raw files/alcohol.csv', header=0, dtype = str)
alcohol.columns = ['id', 'alc_freq_0', 'alc_freq_1', 'alc_freq_2', 'alc_freq_3']
# add to the main data frame
meds = pd.merge(meds, alcohol, on='id', how='left')
# for each data point on alcohol consumption frequency, update by using the 2nd and 3rd visits
meds['med_alc_freq'] = meds['alc_freq_0'].copy()
meds.loc[meds['date'] >= meds['date_1'], 'med_alc_freq'] = meds['alc_freq_1'].copy()
meds.loc[meds['date'] >= meds['date_2'], 'med_alc_freq'] = meds['alc_freq_2'].copy()
meds.loc[meds['date'] >= meds['date_3'], 'med_alc_freq'] = meds['alc_freq_3'].copy()
# set unknown data points to NaN
meds.loc[meds['med_alc_freq']=='-3', 'med_alc_freq'] = np.nan
# drop unneccesary columns
meds.drop(['alc_freq_1','alc_freq_3'], axis=1, inplace=True)
del alcohol



## physical activity
activity_type = pd.read_csv('UK Biobank/Raw files/activity_type.csv', header=0, dtype = str)
activity_type.columns = ['id','activity_0_0','activity_0_1','activity_0_2','activity_0_3','activity_0_4',
                         'activity_1_0','activity_1_1','activity_1_2','activity_1_3','activity_1_4','activity_2_0',
                         'activity_2_1','activity_2_2','activity_2_3','activity_2_4','activity_3_0','activity_3_1',
                         'activity_3_2','activity_3_3','activity_3_4']
# change 'none' or 'prefer not to answer' to NaN
activity_type[(activity_type=='-7') | (activity_type=='-3')] = np.nan
# re-code
activity_type[(activity_type=='1') | (activity_type=='4')] = '1'
activity_type[(activity_type=='2') | (activity_type=='5')] = '2'
# initiate columns for all visits
activity_type['activity_0'] = np.nan; activity_type['activity_1'] = np.nan; activity_type['activity_2'] = np.nan; activity_type['activity_3'] = np.nan
# if the higher level of physical activity occurs as a response during the visit, override the lower activity levels
activity_type.loc[(activity_type['activity_0_0']=='1') | (activity_type['activity_0_1']=='1') | (activity_type['activity_0_2']=='1') | (activity_type['activity_0_3']=='1') | (activity_type['activity_0_4']=='1'), 'activity_0'] = '1'
activity_type.loc[(activity_type['activity_0_0']=='2') | (activity_type['activity_0_1']=='2') | (activity_type['activity_0_2']=='2') | (activity_type['activity_0_3']=='2') | (activity_type['activity_0_4']=='2'), 'activity_0'] = '2'
activity_type.loc[(activity_type['activity_0_0']=='3') | (activity_type['activity_0_1']=='3') | (activity_type['activity_0_2']=='3') | (activity_type['activity_0_3']=='3') | (activity_type['activity_0_4']=='3'), 'activity_0'] = '3'
activity_type.loc[(activity_type['activity_1_0']=='1') | (activity_type['activity_1_1']=='1') | (activity_type['activity_1_2']=='1') | (activity_type['activity_1_3']=='1') | (activity_type['activity_1_4']=='1'), 'activity_1'] = '1'
activity_type.loc[(activity_type['activity_1_0']=='2') | (activity_type['activity_1_1']=='2') | (activity_type['activity_1_2']=='2') | (activity_type['activity_1_3']=='2') | (activity_type['activity_1_4']=='2'), 'activity_1'] = '2'
activity_type.loc[(activity_type['activity_1_0']=='3') | (activity_type['activity_1_1']=='3') | (activity_type['activity_1_2']=='3') | (activity_type['activity_1_3']=='3') | (activity_type['activity_1_4']=='3'), 'activity_1'] = '3'
activity_type.loc[(activity_type['activity_2_0']=='1') | (activity_type['activity_2_1']=='1') | (activity_type['activity_2_2']=='1') | (activity_type['activity_2_3']=='1') | (activity_type['activity_2_4']=='1'), 'activity_2'] = '1'
activity_type.loc[(activity_type['activity_2_0']=='2') | (activity_type['activity_2_1']=='2') | (activity_type['activity_2_2']=='2') | (activity_type['activity_2_3']=='2') | (activity_type['activity_2_4']=='2'), 'activity_2'] = '2'
activity_type.loc[(activity_type['activity_2_0']=='3') | (activity_type['activity_2_1']=='3') | (activity_type['activity_2_2']=='3') | (activity_type['activity_2_3']=='3') | (activity_type['activity_2_4']=='3'), 'activity_2'] = '3'
activity_type.loc[(activity_type['activity_3_0']=='1') | (activity_type['activity_3_1']=='1') | (activity_type['activity_3_2']=='1') | (activity_type['activity_3_3']=='1') | (activity_type['activity_3_4']=='1'), 'activity_3'] = '1'
activity_type.loc[(activity_type['activity_3_0']=='2') | (activity_type['activity_3_1']=='2') | (activity_type['activity_3_2']=='2') | (activity_type['activity_3_3']=='2') | (activity_type['activity_3_4']=='2'), 'activity_3'] = '2'
activity_type.loc[(activity_type['activity_3_0']=='3') | (activity_type['activity_3_1']=='3') | (activity_type['activity_3_2']=='3') | (activity_type['activity_3_3']=='3') | (activity_type['activity_3_4']=='3'), 'activity_3'] = '3'
# remove unneccesary columns
activity_type = activity_type[['id', 'activity_0', 'activity_1', 'activity_2', 'activity_3']]
# add to the main data frame
meds = pd.merge(meds, activity_type, on='id', how='left')
# for each data point on physical activity, update by using the 2nd and 3rd visits
meds['med_activity'] = meds['activity_0'].copy()
meds.loc[meds['date'] >= meds['date_1'], 'med_activity'] = meds['activity_1'].copy()
meds.loc[meds['date'] >= meds['date_2'], 'med_activity'] = meds['activity_2'].copy()
meds.loc[meds['date'] >= meds['date_3'], 'med_activity'] = meds['activity_3'].copy()
# drop unneccesary columns
meds.drop(['activity_1','activity_3'], axis=1, inplace=True)
del activity_type



## BMI
# read in the data frame
bmi = pd.read_csv('UK Biobank/Raw files/bmi.csv', header=0, dtype = str)
bmi.columns = ['id', 'bmi_0', 'bmi_1', 'bmi_2', 'bmi_3']
# add to the main data frame
meds = pd.merge(meds, bmi, on='id', how='left')
# for each data point on BMI, update by using the 2nd and 3rd visits
meds['med_bmi'] = meds['bmi_0'].copy()
meds.loc[meds['date'] >= meds['date_1'], 'med_bmi'] = meds['bmi_1'].copy()
meds.loc[meds['date'] >= meds['date_2'], 'med_bmi'] = meds['bmi_2'].copy()
meds.loc[meds['date'] >= meds['date_3'], 'med_bmi'] = meds['bmi_3'].copy()
# drop unneccesary columns
meds.drop(['bmi_1','bmi_3'], axis=1, inplace=True)
del bmi


# remove unnecessary columns
meds.drop(['prescription_old', 'quantity', 'prescription', 'aa_count', 'combo_drug', 'date_0',
                               'date_1', 'date_2', 'date_3', 'date_follow_up', 'scale_name'],
                               axis=1, inplace=True)

# assign to all topical, ophthalmic, nasal, or otic drugs an anticholinergic value of 0
meds[['admin_oral', 'aa_ancelin', 'aa_bishara', 'aa_boustani', 'aa_briet', 'aa_cancelli', 'aa_carnahan', 'aa_chew',
     'aa_duran', 'aa_ehrt', 'aa_han', 'aa_kiesel', 'aa_rudolph', 'aa_sittironnarit', 'aa_nery', 'aa_jun']] = \
    meds[['admin_oral', 'aa_ancelin', 'aa_bishara', 'aa_boustani', 'aa_briet', 'aa_cancelli', 'aa_carnahan', 'aa_chew',
         'aa_duran', 'aa_ehrt', 'aa_han', 'aa_kiesel', 'aa_rudolph', 'aa_sittironnarit', 'aa_nery', 'aa_jun']].\
        apply(pd.to_numeric)

scales = ['aa_ancelin', 'aa_bishara', 'aa_boustani', 'aa_briet', 'aa_cancelli', 'aa_carnahan', 'aa_chew',
     'aa_duran', 'aa_ehrt', 'aa_han', 'aa_kiesel', 'aa_rudolph', 'aa_sittironnarit', 'aa_nery', 'aa_jun']
for scale in scales:
    meds.loc[meds['admin_oral'] == 0, scale] = 0




### Identify prescriptions for dementia drugs and export (to help identify cases later on)
dementia = meds.loc[(meds['prescription'].str.contains('donepezil', na=False)) |
                    (meds['prescription'].str.contains('galantamine', na=False)) |
                    (meds['prescription'].str.contains('memantine', na=False)) |
                    (meds['prescription'].str.contains('rivastigmine', na=False))].copy()



#export .csv
age_sex.drop(['month_len'], axis=1, inplace=True)
#prescriptions = age_sex.to_csv('age_sex_formatted.csv',index=False, header=True, sep='|')
prescriptions = dementia.to_csv('dementia_meds.csv',index=False, header=True, sep='|')
prescriptions = meds.to_csv('6_demographics_v2.csv',index=False, header=True, sep='|')

