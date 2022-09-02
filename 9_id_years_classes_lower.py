# -*- coding: utf-8 -*-
"""
Created on Fri Mar 12 13:00:48 2021

@author: jurem
"""

import os
import numpy as np
import datetime
import pandas as pd

os.chdir('D:\\PhD\\MAIN')

### Create yearly drug count

# import main dataset
meds = pd.read_csv('UK Biobank/Processed files/tables/6_demographics_v2.csv', header=0, sep='|', dtype = str, encoding = 'cp1252')

# remove unnecessary columns to save memory
meds = meds[['id', 'date', 'data_provider', 'aa_duran', 'aa_name', 'dose_standardised', 'number']]

# change the type
meds['date'] = pd.to_datetime(meds['date'], format = '%Y-%m-%d')
meds['year'] = meds.date.dt.to_period('Y')
meds['data_provider'] = meds['data_provider'].astype(float)
meds['aa_duran'] = meds['aa_duran'].astype(float)

# change aa-burden so it reflects the drug count (as opposed to value-based burden)
meds.loc[meds['aa_duran'] > 0, 'aa_duran_n'] = 1


## Remove prescriptions that appear after the recorded date of death of the participant or afte the final day of prescription sampling.
id_present = pd.read_csv('UK Biobank/Processed files/Suppl. files/id_present.csv') #read in file
id_present['id'] = id_present['id'].astype(str)
# transform to date
id_present['date_first'] = pd.to_datetime(id_present['date_first'], format = '%Y-%m-%d')
id_present['date_death'] = pd.to_datetime(id_present['date_death'], format = '%Y-%m-%d')
id_present = id_present.sort_values(by='date_first')
# change column names
id_present.columns = ['id', 'date_first', 'date_last']
id_present['date_last_meds'] = (id_present['date_last']).copy()
# remove the prescriptions that received prescriptions after having reportedly died
id_present.loc[(id_present['date_last']).isnull(), 'date_last_meds'] = max(meds['date'])
meds = pd.merge(meds, id_present, on='id', how='left')
meds = meds.loc[meds['date'] <= meds['date_last_meds']] # removed 24,924 prescriptions


## create data frame with data provider and time in sample to be added later

dat_provs = meds.groupby(['id'], as_index=False).agg(data_provider=('data_provider', 'median'))
dat_provs['data_provider'] = round(dat_provs['data_provider'])


## transform to id-month format

id_years_poly = meds.groupby(['id','year'], as_index=True).agg(meds_count=('id','count'),
                                                               aa_duran=('aa_duran', 'sum'),
                                                               aa_duran_n=('aa_duran_n', 'sum'))\
                  .unstack(fill_value=0).stack()
id_years_poly['id'] = [item[0] for item in list(id_years_poly.index)]
id_years_poly['year'] = [item[1] for item in list(id_years_poly.index)]
id_years_poly = id_years_poly.reset_index(drop=True)
# add data provider
id_years_poly = pd.merge(id_years_poly, dat_provs, on='id', how='left')

del meds





### Yearly aa-burden for each drug class

# import aa-burden per class
meds_class = pd.read_csv('UK Biobank/Processed files/tables/meds_v2_class_lower.csv', header=0, dtype = str, encoding = 'cp1252')
meds_class['date'] = pd.to_datetime(meds_class['date'], format = '%Y-%m-%d').copy()
meds_class['year'] = meds_class.date.dt.to_period('Y').copy()

# remove the prescriptions that received prescriptions after having reportedly died
meds_class = pd.merge(meds_class, id_present, on='id', how='left')
meds_class = meds_class.loc[meds_class['date'] <= meds_class['date_last_meds']]

# change column classes
meds_class.loc[:,['ace inhibitors, plain', 'aminoglycoside antibacterials', 'antiarrhythmics, class i and iii', 
                  'anticholinergic agents', 'antidepressants', 'antiepileptics', 'antigout preparations', 
                  'antihistamines for systemic use', 'antiinflammatory and antirheumatic products, non-steroids',
                  'antimigraine preparations', 'antipropulsives', 'antipsychotics', 'antithrombotic agents', 'anxiolytics',
                  'arteriolar smooth muscle, agents acting on', 'belladonna and derivatives, plain', 
                  'beta-lactam antibacterials, penicillins', 'blood glucose lowering drugs, excl. insulins', 
                  'cardiac glycosides', 'corticosteroids for systemic use, plain',
                  'cough suppressants, excl. combinations with expectorants', 'dopaminergic agents', 'drugs for constipation',
                  'drugs for functional gastrointestinal disorders', 'drugs for peptic ulcer and gastro-oesophageal reflux disease (gord)',
                  'drugs for treatment of tuberculosis', 'expectorants, excl. combinations with cough suppressants',
                  'high-ceiling diuretics', 'hypnotics and sedatives', 'immunosuppressants', 'low-ceiling diuretics, excl. thiazides',
                  'macrolides, lincosamides and streptogramins', 'muscle relaxants, centrally acting agents',
                  'nasal decongestants for systemic use', 'opioids', 'other antibacterials', 
                  'other systemic drugs for obstructive airway diseases', 'potassium-sparing agents', 'propulsives', 
                  'quinolone antibacterials', 'selective calcium channel blockers with direct cardiac effects', 'urologicals', 
                  'vasodilators used in cardiac diseases']] = \
                  meds_class.loc[:,['ace inhibitors, plain', 'aminoglycoside antibacterials', 'antiarrhythmics, class i and iii', 
                  'anticholinergic agents', 'antidepressants', 'antiepileptics', 'antigout preparations', 
                  'antihistamines for systemic use', 'antiinflammatory and antirheumatic products, non-steroids',
                  'antimigraine preparations', 'antipropulsives', 'antipsychotics', 'antithrombotic agents', 'anxiolytics',
                  'arteriolar smooth muscle, agents acting on', 'belladonna and derivatives, plain', 
                  'beta-lactam antibacterials, penicillins', 'blood glucose lowering drugs, excl. insulins', 
                  'cardiac glycosides', 'corticosteroids for systemic use, plain',
                  'cough suppressants, excl. combinations with expectorants', 'dopaminergic agents', 'drugs for constipation',
                  'drugs for functional gastrointestinal disorders', 'drugs for peptic ulcer and gastro-oesophageal reflux disease (gord)',
                  'drugs for treatment of tuberculosis', 'expectorants, excl. combinations with cough suppressants',
                  'high-ceiling diuretics', 'hypnotics and sedatives', 'immunosuppressants', 'low-ceiling diuretics, excl. thiazides',
                  'macrolides, lincosamides and streptogramins', 'muscle relaxants, centrally acting agents',
                  'nasal decongestants for systemic use', 'opioids', 'other antibacterials', 
                  'other systemic drugs for obstructive airway diseases', 'potassium-sparing agents', 'propulsives', 
                  'quinolone antibacterials', 'selective calcium channel blockers with direct cardiac effects', 'urologicals', 
                  'vasodilators used in cardiac diseases']].apply(pd.to_numeric)

id_years = meds_class.groupby(['id', 'year'])[['ace inhibitors, plain', 'aminoglycoside antibacterials', 'antiarrhythmics, class i and iii', 
                  'anticholinergic agents', 'antidepressants', 'antiepileptics', 'antigout preparations', 
                  'antihistamines for systemic use', 'antiinflammatory and antirheumatic products, non-steroids',
                  'antimigraine preparations', 'antipropulsives', 'antipsychotics', 'antithrombotic agents', 'anxiolytics',
                  'arteriolar smooth muscle, agents acting on', 'belladonna and derivatives, plain', 
                  'beta-lactam antibacterials, penicillins', 'blood glucose lowering drugs, excl. insulins', 
                  'cardiac glycosides', 'corticosteroids for systemic use, plain',
                  'cough suppressants, excl. combinations with expectorants', 'dopaminergic agents', 'drugs for constipation',
                  'drugs for functional gastrointestinal disorders', 'drugs for peptic ulcer and gastro-oesophageal reflux disease (gord)',
                  'drugs for treatment of tuberculosis', 'expectorants, excl. combinations with cough suppressants',
                  'high-ceiling diuretics', 'hypnotics and sedatives', 'immunosuppressants', 'low-ceiling diuretics, excl. thiazides',
                  'macrolides, lincosamides and streptogramins', 'muscle relaxants, centrally acting agents',
                  'nasal decongestants for systemic use', 'opioids', 'other antibacterials', 
                  'other systemic drugs for obstructive airway diseases', 'potassium-sparing agents', 'propulsives', 
                  'quinolone antibacterials', 'selective calcium channel blockers with direct cardiac effects', 'urologicals', 
                  'vasodilators used in cardiac diseases']].agg('sum').unstack(fill_value=0).stack()                                                                        
id_years['id'] = [item[0] for item in list(id_years.index)]
id_years['year'] = [item[1] for item in list(id_years.index)]
id_years = id_years.reset_index(drop=True)
id_years.columns = ['ace_inh', 'aminoglycoside', 'antiarrhythmic', 'anticholinergic', 'antidepressant', 'antiepileptic', 
                    'antigout', 'antihistamines', 'antiinflammatory', 'antimigraine', 'antipropulsive', 'antipsychotic', 
                    'antithrombotic', 'anxiolytic', 'arteriolar', 'belladonna', 'penicillin', 'glucose_lowering',
                    'cardiac_glycoside', 'corticosteroid', 'anti_cough', 'dopaminergic', 'constipation',
                    'gastrointestinal', 'acid_reflux', 'tuberculosis', 'expectorant', 'diuretic_high', 'sedative', 'immunosuppressant', 
                    'diuretic_low', 'macro_linco_strepto', 'muscle_relaxant', 'decongestant', 'opioid', 'antibacterial_other',
                    'OAD_systemic', 'K_sparing', 'propulsive', 'quinolone', 'Ca_blocker_cardiac', 'urological', 'vasodilator', 'id', 'year']
id_years = pd.merge(id_years_poly, id_years, on=['id', 'year'], how='outer')


## Remove rows for before a participant was registered and after the sampling period ends
# add to the main data frame
id_years = pd.merge(id_years, id_present, on='id', how='inner') # removed 
# for each id, remove entries before the first date and entries after the date of death
id_years['first_year'] = id_years.date_first.dt.to_period('Y').astype(str).astype(int)
id_years['last_year_meds'] = id_years.date_last_meds.dt.to_period('Y').astype(str).astype(int)
id_years['date_last_meds'] = id_years.date_last_meds.dt.to_period('Y').astype(str).astype(int)
id_years['year'] = id_years['year'].astype(str).astype(int)
id_years = id_years.loc[id_years['year'] >= id_years['first_year']]
id_years = id_years.loc[id_years['year'] <= id_years['last_year_meds']]
id_present.drop(['date_last_meds'], axis=1, inplace=True) # column from before; we don't need it anymore
id_years.drop(['first_year', 'date_first', 'date_last', 'date_last_meds', 'last_year_meds'], axis=1, inplace=True) # column from before; we don't need it anymore

del [dat_provs, id_years_poly, meds_class]






# information on  when participant was registered in the sample and when they died
id_present = id_present[['id', 'date_first', 'date_last']]
id_present.loc[(id_present['date_last']).isnull(), 'date_last'] = pd.to_datetime(datetime.date(2021, 3, 23), format = "%Y-%m-%d")
# add to the main data frame
id_years = pd.merge(id_years, id_present, on='id', how='inner') # 'inner' to remove all individuals without prescription data
# remove rows of participants without prescription data
id_years = id_years.loc[id_years['id'].isin(id_years.id.unique())]
# for each id, remove entries before the first date
id_years['first_year'] = id_years.date_first.dt.to_period('Y').astype(str).astype(int)
id_years['last_year'] = id_years['date_last'].dt.to_period('Y').astype(str).astype(int)
id_years = id_years.loc[id_years['year'] >= id_years['first_year']]
# for each id, remove entries after death (since missing years were automatically filled, even if they were after death)
id_years = id_years.loc[id_years['year'] <= id_years['last_year']]







## add demographic- and lifestyle variables to the new data frames
# add age and sex
age_sex = pd.read_csv('UK Biobank/Raw files/age_sex.csv') #read in the age-and-sex data
# transform id, birth year, and month to string
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
id_years = pd.merge(id_years, age_sex, on='id', how='left')
# add a column for age at prescription
id_years['birth_year'] = id_years['birth_year'].astype(int)
id_years['med_age'] = id_years['year'] - id_years['birth_year']
id_years.drop(['birth_year','birth_month'], axis=1, inplace=True)
del(age_sex)




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
id_years = pd.merge(id_years, test_dates, on='id', how='left')
del test_dates
follow_up_dates = pd.read_csv('UK Biobank/Raw files/follow_up_date.csv') # read in the date of the assessment
follow_up_dates = follow_up_dates[['eid', 'X20140.0.0']]
follow_up_dates.columns = ['id', 'date_follow_up'] # rename the columns
follow_up_dates['date_follow_up'] = pd.to_datetime(follow_up_dates['date_follow_up'], format = '%Y-%m-%d')
follow_up_dates['id'] = follow_up_dates['id'].astype(str)
id_years = pd.merge(id_years, follow_up_dates, on='id', how='left')
del follow_up_dates



# assessment centre
# read in the data frame
centre = pd.read_csv('UK Biobank/Raw files/assessment_centre.csv', header=0, dtype = str)
centre.columns = ['id', 'centre_0', 'centre_1', 'centre_2']
# add to the main data frame
id_years = pd.merge(id_years, centre, on='id', how='left')
# for each data point on alcohol consumption frequency, update by using the 2nd and 3rd visits
id_years['med_centre'] = id_years['centre_0'].copy()
id_years['date'] = pd.to_datetime(id_years['year'], format = '%Y')
id_years.loc[id_years['date'] >= id_years['date_1'], 'med_centre'] = id_years['centre_1']
id_years.loc[id_years['date'] >= id_years['date_2'], 'med_centre'] = id_years['centre_2']
# drop unneccesary columns
id_years.drop(['centre_1'], axis=1, inplace=True)
del(centre)



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
id_years = pd.merge(id_years, education, on='id', how='left')
# for each prescription, keep only the date of education before the prescription was issued
id_years['med_education'] = id_years['education_0'].copy() # initiate new column; default education is education at visit 0
id_years.loc[id_years['date'] >= id_years['date_1'], 'med_education'] = id_years['education_1'].copy() # for all the prescriptions issued after date_1, education will correspond to the education at the visit 1
id_years.loc[id_years['date'] >= id_years['date_2'], 'med_education'] = id_years['education_2'].copy() 
id_years.loc[id_years['date'] >= id_years['date_3'], 'med_education'] = id_years['education_3'].copy()
id_years.drop(['education_1','education_3'], axis=1, inplace=True)
del education



## deprivation
# read in the Townsend
deprivation = pd.read_csv('UK Biobank/Raw files/deprivation.csv', header=0, dtype = str)
# merge with main data frame
id_years = pd.merge(id_years, deprivation, on='id', how='left')
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
id_years = pd.merge(id_years, smoking, on='id', how='left')
# update using 2nd and 3rd visits
id_years['med_smoking'] = id_years['smoking_0'].copy()
id_years.loc[id_years['date'] >= id_years['date_1'], 'med_smoking'] = id_years['smoking_1'].copy() 
id_years.loc[id_years['date'] >= id_years['date_2'], 'med_smoking'] = id_years['smoking_2'].copy()
id_years.loc[id_years['date'] >= id_years['date_3'], 'med_smoking'] = id_years['smoking_3'].copy()
# remove columns
id_years.drop(['smoking_1','smoking_3'], axis=1, inplace=True)
del smoking



## alcohol consumption
# read in the data frame
alcohol = pd.read_csv('UK Biobank/Raw files/alcohol.csv', header=0, dtype = str)
alcohol.columns = ['id', 'alc_freq_0', 'alc_freq_1', 'alc_freq_2', 'alc_freq_3']
# add to the main data frame
id_years = pd.merge(id_years, alcohol, on='id', how='left')
# for each data point on alcohol consumption frequency, update by using the 2nd and 3rd visits
id_years['med_alc_freq'] = id_years['alc_freq_0'].copy()
id_years.loc[id_years['date'] >= id_years['date_1'], 'med_alc_freq'] = id_years['alc_freq_1'].copy()
id_years.loc[id_years['date'] >= id_years['date_2'], 'med_alc_freq'] = id_years['alc_freq_2'].copy()
id_years.loc[id_years['date'] >= id_years['date_3'], 'med_alc_freq'] = id_years['alc_freq_3'].copy()
# set unknown data points to NaN
id_years.loc[id_years['med_alc_freq']=='-3', 'med_alc_freq'] = np.nan
# drop unneccesary columns
id_years.drop(['alc_freq_1','alc_freq_3'], axis=1, inplace=True)
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
id_years = pd.merge(id_years, activity_type, on='id', how='left')
# for each data point on physical activity, update by using the 2nd and 3rd visits
id_years['med_activity'] = id_years['activity_0'].copy()
id_years.loc[id_years['date'] >= id_years['date_1'], 'med_activity'] = id_years['activity_1'].copy()
id_years.loc[id_years['date'] >= id_years['date_2'], 'med_activity'] = id_years['activity_2'].copy()
id_years.loc[id_years['date'] >= id_years['date_3'], 'med_activity'] = id_years['activity_3'].copy()
# drop unneccesary columns
id_years.drop(['activity_1','activity_3'], axis=1, inplace=True)
del activity_type


## BMI
# read in the data frame
bmi = pd.read_csv('UK Biobank/Raw files/bmi.csv', header=0, dtype = str)
bmi.columns = ['id', 'bmi_0', 'bmi_1', 'bmi_2', 'bmi_3']
# add to the main data frame
id_years = pd.merge(id_years, bmi, on='id', how='left')
# for each data point on BMI, update by using the 2nd and 3rd visits
id_years['med_bmi'] = id_years['bmi_0'].copy()
id_years.loc[id_years['date'] >= id_years['date_1'], 'med_bmi'] = id_years['bmi_1'].copy()
id_years.loc[id_years['date'] >= id_years['date_2'], 'med_bmi'] = id_years['bmi_2'].copy()
id_years.loc[id_years['date'] >= id_years['date_3'], 'med_bmi'] = id_years['bmi_3'].copy()
# drop unneccesary columns
id_years.drop(['bmi_1','bmi_3'], axis=1, inplace=True)
del bmi




id_years = id_years[['id', 'year', 'meds_count', 'aa_duran', 'aa_duran_n', 'data_provider', 
                             'ace_inh', 'aminoglycoside', 'antiarrhythmic', 'anticholinergic', 'antidepressant', 
                             'antiepileptic', 'antigout', 'antihistamines', 'antiinflammatory', 'antimigraine', 
                             'antipropulsive', 'antipsychotic', 'antithrombotic', 'anxiolytic', 'arteriolar', 
                             'belladonna', 'penicillin', 'glucose_lowering', 'cardiac_glycoside', 
                             'corticosteroid', 'anti_cough', 'dopaminergic', 'constipation', 
                             'gastrointestinal', 'acid_reflux', 'tuberculosis', 'expectorant', 'diuretic_high', 
                             'sedative', 'immunosuppressant', 'diuretic_low', 'macro_linco_strepto', 'muscle_relaxant', 
                             'decongestant', 'opioid', 'antibacterial_other', 'OAD_systemic', 'K_sparing', 'propulsive', 
                             'quinolone', 'Ca_blocker_cardiac', 'urological', 'vasodilator', 'date_last', 'first_year', 'last_year', 'sex', 'month_len', 
                             'birth_date', 'med_age', 'date_0', 'date_1', 'date_2', 'date_3', 'date_follow_up',
                             'med_centre', 'date', 'med_education', 'deprivation', 'med_smoking', 'med_alc_freq', 'med_activity', 'med_bmi',
                             'centre_0', 'centre_2', 'education_0', 'education_2', 'smoking_0', 'smoking_2',
                             'alc_freq_0', 'alc_freq_2', 'activity_0', 'activity_2', 'bmi_0', 'bmi_2',]]

# export .csv
prescriptions = id_years.to_csv('id_years_class_lower_v2.csv')
