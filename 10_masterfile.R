library(dplyr)
library(purrr)
library(zoo)

id_years <- read.csv('id_years_oral_v2.csv', sep = '|', row.names = 1)

# rename some columns
colnames(id_years)[which(colnames(id_years) == 'date_0')] <- 'ass_date_0'
colnames(id_years)[which(colnames(id_years) == 'date_1')] <- 'ass_date_1'
colnames(id_years)[which(colnames(id_years) == 'date_2')] <- 'ass_date_2'

# remove unnecessary columns
id_years <- subset(id_years, select=c(id, year, data_provider, sex, birth_date, med_age, first_year, last_year, date_first, date_last,
                                      ass_date_0, ass_date_1, ass_date_2, med_centre, centre_0, centre_2, med_education, education_0, education_2, 
                                      deprivation, med_smoking, smoking_0, smoking_2, med_alc_freq, alc_freq_0, alc_freq_2, med_activity, 
                                      activity_0, activity_2, med_bmi, bmi_0, bmi_2, meds_count,
                                      aa_ancelin, aa_boustani, aa_carnahan, aa_cancelli, aa_chew, aa_rudolph,
                                      aa_ehrt, aa_han, aa_sittironnarit, aa_briet, aa_bishara, aa_nery, aa_jun, aa_duran, aa_kiesel,
                                      aa_ancelin_n, aa_boustani_n, aa_carnahan_n, aa_cancelli_n, aa_chew_n, aa_rudolph_n,
                                      aa_ehrt_n, aa_han_n, aa_sittironnarit_n, aa_briet_n, aa_bishara_n, 
                                      aa_nery_n, aa_jun_n, aa_duran_n, aa_kiesel_n))

id_years[, c("birth_date", "date_first", "date_last", "ass_date_0", "ass_date_1", "ass_date_2")] <- 
  lapply(id_years[, c("birth_date", "date_first", "date_last", "ass_date_0", "ass_date_1", "ass_date_2")], as.Date, "%Y-%m-%d")
id_years$ass_year_0 <- format(as.Date(id_years$ass_date_0), "%Y")
id_years$ass_year_1 <- format(as.Date(id_years$ass_date_1), "%Y")
id_years$ass_year_2 <- format(as.Date(id_years$ass_date_2), "%Y")

save.image("id_years_v2.RData")

## Set year 0 and identify participants that have died
dates <- read.csv('id_present.csv')
dates$date_first <- as.Date(dates$date_first,"%Y-%m-%d")
dates$date_death <- as.Date(dates$date_death,"%Y-%m-%d")

# remove the first year in the dataset for each participant, since it is unlikely to be complete (i.e. from Jan-Dec)
id_years$year <- as.integer(id_years$year); id_years$first_year <- as.integer(id_years$first_year)
id_years <- filter(id_years, year != first_year) # removed ? rows (other occurrences were already removed in prior data cleaning)
# identify the remaining earliest year (year 0) for each participant
early_years <- id_years %>% group_by(id) %>% summarise(year_0=min(year))
id_years <- merge(id_years, early_years, by='id')
# create a variable indicating whether the participant has died
id_years <- merge(id_years, subset(dates, select=c(id, date_death)), by='id', all.x = TRUE)
id_years$dead <- 0
id_years$dead[!is.na(id_years$date_death)] <- 1

# remove all years before 2000 (low ascertainment) and after 2015 (inaccurate/missing) prescriptions
id_years <- filter(id_years, year>=2000, year<=2015)




## Create variables for multimorbidity and for diagnoses of disorders to be used as covariates

# import all diagnoses
icd <- read.csv('diagnoses_ALL.csv')

icd$stroke <- icd$stroke_h + icd$stroke_i + icd$stroke_un
icd$stroke[icd$stroke > 0] <- 1
colnames(icd)[colnames(icd) == 'hypercholesterolemia'] <- 'hyperchol'
icd$date <- icd$date <- as.Date(icd$date, '%Y-%m-%d')
# create 'year' variable
icd$year <- as.numeric(format(icd$date, "%Y"))

# create yearly disorder counts and cumulative yearly disorder counts for each participant
# determne number of comorbidities
comorbidity <- subset(icd, select = c(id, year, diagnosis))
# remove repeat diagnoses within any year
comorbidity <- distinct(comorbidity, id, year, diagnosis, .keep_all = TRUE)
# compute cumulative count of diagnoses and disregard codes already diagnoses in the past
comorbidity <- comorbidity %>%
  group_by(id) %>%
  mutate(cum_sum = cumsum(!duplicated(diagnosis))) %>%
  group_by(id, year) %>%
  summarise(comorbidity_csum = max(cum_sum))
comorbidity <- data.frame(comorbidity)


## remove duplicate (same id, same positive diagnosis) rows (keeping oldest) of any participant for any disorder
# bind the non-diagnoses of the disorder with the duplicate-cleaned diagnoses
anxiety <- distinct(filter(icd, anxiety == 1), id, anxiety, .keep_all = TRUE)
diabetes <- distinct(filter(icd, diabetes == 1), id, diabetes, .keep_all = TRUE) 
dis_other <- distinct(filter(icd, dis_other == 1), id, dis_other, .keep_all = TRUE)
heart_attack <- distinct(filter(icd, heart_attack == 1), id, heart_attack, .keep_all = TRUE)
hyperchol <- distinct(filter(icd, hyperchol == 1), id, hyperchol, .keep_all = TRUE)
hypertension <- distinct(filter(icd, hypertension == 1), id, hypertension, .keep_all = TRUE)
mood_dis <- distinct(filter(icd, mood_dis == 1), id, mood_dis, .keep_all = TRUE)
psychosis <- distinct(filter(icd, psychosis == 1), id, psychosis, .keep_all = TRUE)
# retain only columns relevant for each disorder
anxiety <- subset(anxiety, select = c(id, date, anxiety)); colnames(anxiety)[colnames(anxiety) == 'date'] <- 'date_anxiety'
diabetes <- subset(diabetes, select = c(id, date, diabetes)); colnames(diabetes)[colnames(diabetes) == 'date'] <- 'date_diabetes'
dis_other <- subset(dis_other, select = c(id, date, dis_other)); colnames(dis_other)[colnames(dis_other) == 'date'] <- 'date_dis_other'
heart_attack <- subset(heart_attack, select = c(id, date, heart_attack)); colnames(heart_attack)[colnames(heart_attack) == 'date'] <- 'date_heart_attack'
hyperchol <- subset(hyperchol, select = c(id, date, hyperchol)); colnames(hyperchol)[colnames(hyperchol) == 'date'] <- 'date_hyperchol'
hypertension <- subset(hypertension, select = c(id, date, hypertension)); colnames(hypertension)[colnames(hypertension) == 'date'] <- 'date_hypertension'
mood_dis <- subset(mood_dis, select = c(id, date, mood_dis)); colnames(mood_dis)[colnames(mood_dis) == 'date'] <- 'date_mood_dis'
psychosis <- subset(psychosis, select = c(id, date, psychosis)); colnames(psychosis)[colnames(psychosis) == 'date'] <- 'date_psychosis'
# merge by id
morbidity <- merge(anxiety, diabetes, by = 'id', all = TRUE)
morbidity <- merge(morbidity, dis_other, by = 'id', all = TRUE)
morbidity <- merge(morbidity, heart_attack, by = 'id', all = TRUE)
morbidity <- merge(morbidity, hyperchol, by = 'id', all = TRUE)
morbidity <- merge(morbidity, hypertension, by = 'id', all = TRUE)
morbidity <- merge(morbidity, mood_dis, by = 'id', all = TRUE)
morbidity <- merge(morbidity, psychosis, by = 'id', all = TRUE)


## Import APOE data
apoe <- readRDS('APOE_SNPs_20112020.rds') # file APOE genotype
apoe$row_name_len <- sapply(rownames(apoe), nchar) # column with number of characters in row name
apoe <- filter(apoe, row_name_len == 15) # remove rows with invalid names
apoe$id <- substring(rownames(apoe), 1,7) # create proper id variable
# code APOE genotypes
apoe$apoe <- NA
apoe$apoe[apoe$rs429358_C == 2 & apoe$rs7412_T == 2] <- 'e1/e1'
apoe$apoe[apoe$rs429358_C == 1 & apoe$rs7412_T == 2] <- 'e1/e2'
apoe$apoe[apoe$rs429358_C == 1 & apoe$rs7412_T == 1] <- 'e1/e3 / e2/e4'
apoe$apoe[apoe$rs429358_C == 2 & apoe$rs7412_T == 1] <- 'e1/e4'
apoe$apoe[apoe$rs429358_C == 0 & apoe$rs7412_T == 2] <- 'e2/e2'
apoe$apoe[apoe$rs429358_C == 0 & apoe$rs7412_T == 1] <- 'e2/e3'
apoe$apoe[apoe$rs429358_C == 0 & apoe$rs7412_T == 0] <- 'e3/e3'
apoe$apoe[apoe$rs429358_C == 1 & apoe$rs7412_T == 0] <- 'e3/e4'
apoe$apoe[apoe$rs429358_C == 2 & apoe$rs7412_T == 0] <- 'e4/e4'
apoe <- subset(apoe, select=c(id, apoe))
# simplified genotypes
apoe$apoe_carrier <- NA
apoe$apoe_carrier[apoe$apoe=="e2/e2" | apoe$apoe=="e2/e3" | apoe$apoe=="e1/e2"] <- "e2"
apoe$apoe_carrier[apoe$apoe=="e3/e3" | apoe$apoe=='e1/e3 / e2/e4'] <- "e3"
apoe$apoe_carrier[apoe$apoe=="e3/e4" | apoe$apoe=="e4/e4" | apoe$apoe=="e1/e4"] <- "e4"



# Import cognition- and MRI data
cognition <- readRDS('cognition_assessment_centre.rds')
cognition <- subset(cognition, select=-c(ass_date_0, ass_date_1, ass_date_2, sex, birth_date, 
                                         ass_year_0, ass_year_1, ass_year_2, age_0, age_1, age_2,
                                         age_3, ass_year_3, ass_date_3)) # remove overlapping and redundant rows
mri <- readRDS("mri_imaging_clean.rds")




# add drug classes
class_body_sys <- read.csv('id_years_body_system_v2.csv', sep='|', row.names = 1)
class_lower <- read.csv('id_years_class_lower_v2.csv', row.names = 1)

# remove columns that overlap with the main dataframe
class_body_sys <- subset(class_body_sys, select=c(id, year, metabolic, antiinfective, immuno_modulating, blood, cardiovascular,
                                                  urinary, musculo_skeletal, neuro, respiratory, hormonal))
class_lower <- subset(class_lower, select=c(id, year, ace_inh,aminoglycoside,antiarrhythmic,anticholinergic,antidepressant,
                                            antiepileptic,antigout,antihistamines,antiinflammatory,antimigraine,antipropulsive,
                                            antipsychotic,antithrombotic,anxiolytic,arteriolar,belladonna,penicillin,
                                            glucose_lowering,cardiac_glycoside,corticosteroid,anti_cough,dopaminergic,constipation,
                                            gastrointestinal,acid_reflux,tuberculosis,expectorant,diuretic_high,sedative,
                                            immunosuppressant,diuretic_low,macro_linco_strepto,muscle_relaxant,decongestant,opioid,
                                            antibacterial_other,OAD_systemic,K_sparing,propulsive,quinolone,Ca_blocker_cardiac,
                                            urological,vasodilator))
# rename columns
colnames(class_body_sys)[3:ncol(class_body_sys)] <- paste('SYS', colnames(class_body_sys)[3:ncol(class_body_sys)], sep = '_')
colnames(class_lower)[3:ncol(class_lower)] <- paste('PHARM', colnames(class_lower)[3:ncol(class_lower)], sep = '_')








# Merge into one data frame
ethnicity <- read.csv('ethnicity.csv')
colnames(ethnicity) <- c('id', 'ethnicity', 'ethnicity_2', 'ethnicity_3'); ethnicity <- subset(ethnicity, select=c(id,ethnicity))
ethnicity$ethnicity[ethnicity$ethnicity=='1' | ethnicity$ethnicity=='2' | ethnicity$ethnicity=='3' | ethnicity$ethnicity=='4' | ethnicity$ethnicity=='5' | ethnicity$ethnicity=='6'] <- NA
ethnicity$ethnicity[ethnicity$ethnicity=='-1' | ethnicity$ethnicity=='-3'] <- NA
assCtr <- read.csv('assessment_centre.csv')
colnames(assCtr) <- c('id', 'ass_ctr_0', 'ass_ctr_1', 'ass_ctr_2'); assCtr$ass_ctr_1 <- NULL
id_years <- merge(id_years, ethnicity, by='id', all.x = TRUE)
id_years <- merge(id_years, assCtr, by='id', all.x = TRUE)
comorbidity$year <- as.integer(comorbidity$year)
id_years <- merge(id_years, comorbidity, by=c('id', 'year'), all = TRUE)
# merge with other data frames
id_years <- merge(id_years, apoe, by='id', all.x = TRUE)
# use cognition data frame (that has already had id's with problematic diagnoses removed) to clean the main data frame
id_years <- filter(id_years, id %in% cognition$id)
id_years <- merge(id_years, cognition, by='id', all.x = TRUE)
id_years <- merge(id_years, class_body_sys, by=c('id', 'year'), all.x = TRUE)
id_years <- merge(id_years, class_lower, by=c('id', 'year'), all.x = TRUE)
id_years <- merge(id_years, mri, by='id', all.x = TRUE)
morbidity <- subset(morbidity, select=-c(dis_other, date_dis_other))
id_years <- merge(id_years, morbidity, by='id', all.x = TRUE)

# for years with NAs for any disorder, set as 0
id_years[, c("anxiety", "diabetes", "heart_attack", "hyperchol", "hypertension", "mood_dis", "psychosis")] <- 
  sapply(id_years[, c("anxiety", "diabetes", "heart_attack", "hyperchol", "hypertension", "mood_dis", "psychosis")], na.fill, 0)

# for cumulative sum of comorbidities, fill the years with no new diagnoses with numbers from previous year
id_years <- id_years %>% arrange(id, year)
id_years$comorbidity_csum <- na.locf(id_years$comorbidity_csum)
id_years <- filter(id_years, !is.na(sex)) # remove rows without prescription data




## Clean up variables

# set missing values to NA
id_years[id_years==''] <- NA
# change variable classes
id_years$sex <- as.factor(id_years$sex)
id_years$dead <- as.factor(id_years$dead)
id_years$meds_count <- as.numeric(id_years$meds_count)
id_years$med_age <- as.numeric(id_years$med_age)
id_years$data_provider <- as.factor(id_years$data_provider)
id_years$med_education <- as.factor(id_years$med_education)
id_years$education_0 <- as.factor(id_years$education_0)
id_years$education_2 <- as.factor(id_years$education_2)
id_years$deprivation <- as.numeric(id_years$deprivation)
id_years$med_bmi <- as.numeric(id_years$med_bmi)
id_years$bmi_0 <- as.numeric(id_years$bmi_0)
id_years$bmi_2 <- as.numeric(id_years$bmi_2)
id_years$med_smoking[id_years$med_smoking==-3] <- NA
id_years$med_smoking <- as.factor(id_years$med_smoking)
id_years$smoking_0[id_years$smoking_0==-3] <- NA
id_years$smoking_0 <- as.factor(id_years$smoking_0)
id_years$smoking_2[id_years$smoking_2==-3] <- NA
id_years$smoking_2 <- as.factor(id_years$smoking_2)
id_years$med_alc_freq[id_years$med_alc_freq==-3] <- NA
id_years$med_alc_freq <- as.factor(id_years$med_alc_freq)
id_years$alc_freq_0[id_years$alc_freq_0==-3] <- NA
id_years$alc_freq_0 <- as.factor(id_years$alc_freq_0)
id_years$alc_freq_2[id_years$alc_freq_2==-3] <- NA
id_years$alc_freq_2 <- as.factor(id_years$alc_freq_2)
id_years$med_activity <- as.factor(id_years$med_activity)
id_years$activity_0 <- as.factor(id_years$activity_0)
id_years$activity_2 <- as.factor(id_years$activity_2)
id_years$comorbidity <- as.numeric(id_years$comorbidity)
id_years$apoe_carrier <- as.factor(id_years$apoe_carrier)
id_years$ass_year_0 <- as.numeric(id_years$ass_year_0)
id_years$ass_year_1 <- as.numeric(id_years$ass_year_1)
id_years$ass_year_2 <- as.numeric(id_years$ass_year_2)
id_years$ethnicity <- as.factor(id_years$ethnicity)
id_years$ass_ctr_0 <- as.factor(id_years$ass_ctr_0)
id_years$ass_ctr_2 <- as.factor(id_years$ass_ctr_2)



# set those drug classes with NAs to 0 (NAs were introduced by removing non-anticholinergic drugs before generating the 
# drug-based data frames in 6_prepare_v2.R)
cols <- c('SYS_metabolic','SYS_antiinfective','SYS_immuno_modulating','SYS_blood',
          'SYS_cardiovascular', 'SYS_urinary', 'SYS_musculo_skeletal','SYS_neuro',
          'SYS_respiratory','SYS_hormonal','PHARM_ace_inh','PHARM_aminoglycoside',
          'PHARM_antiarrhythmic','PHARM_anticholinergic','PHARM_antidepressant',
          'PHARM_antiepileptic','PHARM_antigout','PHARM_antihistamines',
          'PHARM_antiinflammatory','PHARM_antimigraine','PHARM_antipropulsive',
          'PHARM_antipsychotic','PHARM_antithrombotic','PHARM_anxiolytic',
          'PHARM_arteriolar','PHARM_belladonna','PHARM_penicillin',
          'PHARM_glucose_lowering','PHARM_cardiac_glycoside','PHARM_corticosteroid',
          'PHARM_anti_cough','PHARM_dopaminergic','PHARM_constipation',
          'PHARM_gastrointestinal','PHARM_acid_reflux','PHARM_tuberculosis',
          'PHARM_expectorant','PHARM_diuretic_high','PHARM_sedative',
          'PHARM_immunosuppressant','PHARM_diuretic_low','PHARM_macro_linco_strepto',
          'PHARM_muscle_relaxant','PHARM_decongestant','PHARM_opioid',
          'PHARM_antibacterial_other','PHARM_OAD_systemic','PHARM_K_sparing',
          'PHARM_propulsive','PHARM_quinolone','PHARM_Ca_blocker_cardiac',
          'PHARM_urological','PHARM_vasodilator')
for (col in cols){
  id_years[[col]][id_years$aa_duran==0] <- 0
}



# export
saveRDS(id_years, file = "masterfile.rds")