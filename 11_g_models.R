library(tidyverse)
library(car) # for vif()


outliers <- function(x, SD, variance) {
  if (variance == 'SD'){
    maximum <- (mean(x, na.rm=T)) + (SD * sd(x, na.rm=T))
    minimum <- (mean(x, na.rm=T)) - (SD * sd(x, na.rm=T))
    x[(x > maximum) | (x < minimum)] <- NA
  }
  if (variance == 'IQR'){
    maximum <- (median(x, na.rm=T)) + (SD * IQR(x, na.rm=T))
    minimum <- (median(x, na.rm=T)) - (SD * IQR(x, na.rm=T))
    x[(x > maximum) | (x < minimum)] <- NA
  }
  return(x)
}




### cumulative ACB vs. cognition at assessment centre

# read in data frame
id_years <- readRDS('masterfile.rds')
# calculate the difference between any prescription year and the assessment year
id_years$year_dif_0 <- id_years$ass_year_0 - id_years$year
id_years$year_dif_2 <- id_years$ass_year_2 - id_years$year

# remove cases with dopaminergic and antiepileptic drugs that got through
id_years <- filter(id_years, PHARM_dopaminergic == 0 & PHARM_antiepileptic == 0)

# for those without imaging: keep the prescription years before the first assessment date
# for those with imaging: keep the prescription years before the imaging assessment
id_years <- filter(id_years, (is.na(ass_year_2) & year_dif_0>0) | (!is.na(ass_year_2) & year_dif_2>0))

# change default APOE-carrier status
id_years$apoe_carrier <- as.character(id_years$apoe_carrier)
id_years$apoe_carrier[id_years$apoe_carrier=='e3'] <- 'e0'
id_years$apoe_carrier <- as.factor(id_years$apoe_carrier)

# calculate the number of years for which there is no prescription data
# these occur in participants that were imaged in 2016, 2017, etc. and will have 1, 2, etc. missing years
# it will be 0 for all participants that were not imaged
id_years$missing_years <- id_years$ass_year_2 - 2015
# if negative, it means that the person was imaged before 2015; but it still means that there are no "missing" years
id_years$missing_years[id_years$missing_years < 0 | is.na(id_years$missing_years)] <- 0 





## calculate the cumulative ACB for each participant and the number of years in the sample

# calculate number of years in sample
sample_time <- id_years %>% group_by(id) %>% summarise(sample_time=length(unique(year)))
# calculate cumulative burden
aa_cum <- id_years %>% group_by(id) %>% summarise(aa_ancelin=sum(aa_ancelin),aa_boustani=sum(aa_boustani),aa_carnahan=sum(aa_carnahan),
                                                  aa_cancelli=sum(aa_cancelli),aa_chew=sum(aa_chew),aa_rudolph=sum(aa_rudolph),
                                                  aa_ehrt=sum(aa_ehrt),aa_han=sum(aa_han),aa_sittironnarit=sum(aa_sittironnarit),
                                                  aa_briet=sum(aa_briet),aa_bishara=sum(aa_bishara),aa_nery=sum(aa_nery),
                                                  aa_jun=sum(aa_jun),aa_duran=sum(aa_duran),aa_kiesel=sum(aa_kiesel),meds_count=sum(meds_count),
                                                  aa_ancelin_n=sum(aa_ancelin_n),aa_boustani_n=sum(aa_boustani_n),
                                                  aa_carnahan_n=sum(aa_carnahan_n),aa_cancelli_n=sum(aa_cancelli_n),aa_chew_n=sum(aa_chew_n),
                                                  aa_rudolph_n=sum(aa_rudolph_n),aa_ehrt_n=sum(aa_ehrt_n),aa_han_n=sum(aa_han_n),
                                                  aa_sittironnarit_n=sum(aa_sittironnarit_n),aa_briet_n=sum(aa_briet_n),aa_bishara_n=sum(aa_bishara_n),
                                                  aa_nery_n=sum(aa_nery_n),aa_jun_n=sum(aa_jun_n),aa_duran_n=sum(aa_duran_n),
                                                  aa_kiesel_n=sum(aa_kiesel_n), comorbidity_csum=max(comorbidity_csum),
                                                  SYS_metabolic=sum(SYS_metabolic),
                                                  SYS_antiinfective=sum(SYS_antiinfective),SYS_immuno_modulating=sum(SYS_immuno_modulating),
                                                  SYS_blood=sum(SYS_blood),SYS_cardiovascular=sum(SYS_cardiovascular),
                                                  SYS_urinary=sum(SYS_urinary),SYS_musculo_skeletal=sum(SYS_musculo_skeletal),
                                                  SYS_neuro=sum(SYS_neuro),SYS_respiratory=sum(SYS_respiratory),
                                                  SYS_hormonal=sum(SYS_hormonal),PHARM_ace_inh=sum(PHARM_ace_inh),
                                                  PHARM_aminoglycoside=sum(PHARM_aminoglycoside),PHARM_antiarrhythmic=sum(PHARM_antiarrhythmic),
                                                  PHARM_anticholinergic=sum(PHARM_anticholinergic),PHARM_antidepressant=sum(PHARM_antidepressant),
                                                  PHARM_antiepileptic=sum(PHARM_antiepileptic),PHARM_antigout=sum(PHARM_antigout),
                                                  PHARM_antihistamines=sum(PHARM_antihistamines),PHARM_antiinflammatory=sum(PHARM_antiinflammatory),
                                                  PHARM_antimigraine=sum(PHARM_antimigraine),PHARM_antipropulsive=sum(PHARM_antipropulsive),
                                                  PHARM_antipsychotic=sum(PHARM_antipsychotic),PHARM_antithrombotic=sum(PHARM_antithrombotic),
                                                  PHARM_anxiolytic=sum(PHARM_anxiolytic),PHARM_arteriolar=sum(PHARM_arteriolar),
                                                  PHARM_belladonna=sum(PHARM_belladonna),PHARM_penicillin=sum(PHARM_penicillin),
                                                  PHARM_glucose_lowering=sum(PHARM_glucose_lowering),PHARM_cardiac_glycoside=sum(PHARM_cardiac_glycoside),
                                                  PHARM_corticosteroid=sum(PHARM_corticosteroid),PHARM_anti_cough=sum(PHARM_anti_cough),
                                                  PHARM_dopaminergic=sum(PHARM_dopaminergic),PHARM_constipation=sum(PHARM_constipation),
                                                  PHARM_gastrointestinal=sum(PHARM_gastrointestinal),PHARM_acid_reflux=sum(PHARM_acid_reflux),
                                                  PHARM_tuberculosis=sum(PHARM_tuberculosis),PHARM_expectorant=sum(PHARM_expectorant),
                                                  PHARM_diuretic_high=sum(PHARM_diuretic_high),PHARM_sedative=sum(PHARM_sedative),
                                                  PHARM_immunosuppressant=sum(PHARM_immunosuppressant),PHARM_diuretic_low=sum(PHARM_diuretic_low),
                                                  PHARM_macro_linco_strepto=sum(PHARM_macro_linco_strepto),PHARM_muscle_relaxant=sum(PHARM_muscle_relaxant),
                                                  PHARM_decongestant=sum(PHARM_decongestant),PHARM_opioid=sum(PHARM_opioid),
                                                  PHARM_antibacterial_other=sum(PHARM_antibacterial_other),PHARM_OAD_systemic=sum(PHARM_OAD_systemic),
                                                  PHARM_K_sparing=sum(PHARM_K_sparing),PHARM_propulsive=sum(PHARM_propulsive),
                                                  PHARM_quinolone=sum(PHARM_quinolone),PHARM_Ca_blocker_cardiac=sum(PHARM_Ca_blocker_cardiac),
                                                  PHARM_urological=sum(PHARM_urological),PHARM_vasodilator=sum(PHARM_vasodilator))
# merge with main frame
id_years <- subset(id_years, select=-c(meds_count, aa_ancelin, aa_boustani, aa_carnahan, aa_cancelli, aa_chew, aa_rudolph,
                                       aa_ehrt, aa_han, aa_sittironnarit, aa_briet, aa_bishara,aa_nery,aa_jun, aa_duran, aa_kiesel,
                                       aa_ancelin_n, aa_boustani_n, aa_carnahan_n, aa_cancelli_n, aa_chew_n, aa_rudolph_n,
                                       aa_ehrt_n, aa_han_n, aa_sittironnarit_n, aa_briet_n, aa_bishara_n,aa_nery_n,aa_jun_n, aa_duran_n, 
                                       aa_kiesel_n, comorbidity_csum, SYS_metabolic, SYS_antiinfective,SYS_immuno_modulating,SYS_blood,
                                       SYS_cardiovascular, SYS_urinary, SYS_musculo_skeletal,SYS_neuro,
                                       SYS_respiratory,SYS_hormonal,PHARM_ace_inh,PHARM_aminoglycoside,
                                       PHARM_antiarrhythmic,PHARM_anticholinergic,PHARM_antidepressant,
                                       PHARM_antiepileptic,PHARM_antigout,PHARM_antihistamines,
                                       PHARM_antiinflammatory,PHARM_antimigraine,PHARM_antipropulsive,
                                       PHARM_antipsychotic,PHARM_antithrombotic,PHARM_anxiolytic,
                                       PHARM_arteriolar,PHARM_belladonna,PHARM_penicillin,
                                       PHARM_glucose_lowering,PHARM_cardiac_glycoside,PHARM_corticosteroid,
                                       PHARM_anti_cough,PHARM_dopaminergic,PHARM_constipation,
                                       PHARM_gastrointestinal,PHARM_acid_reflux,PHARM_tuberculosis,
                                       PHARM_expectorant,PHARM_diuretic_high,PHARM_sedative,
                                       PHARM_immunosuppressant,PHARM_diuretic_low,PHARM_macro_linco_strepto,
                                       PHARM_muscle_relaxant,PHARM_decongestant,PHARM_opioid,
                                       PHARM_antibacterial_other,PHARM_OAD_systemic,PHARM_K_sparing,
                                       PHARM_propulsive,PHARM_quinolone,PHARM_Ca_blocker_cardiac,
                                       PHARM_urological,PHARM_vasodilator))
id_years <- distinct(id_years, id, .keep_all = TRUE)
id_years <- merge(id_years, aa_cum, by = 'id')
id_years <- merge(id_years, sample_time, by='id')
# variables for anticholinergic drug according to ANY scale
id_years$aa_any<-apply(X=subset(id_years, select = c(aa_ancelin_n,aa_boustani_n,aa_carnahan_n,aa_cancelli_n,aa_chew_n,aa_rudolph_n,aa_ehrt_n,aa_han_n,
                                                     aa_sittironnarit_n,aa_briet_n,aa_bishara_n,aa_nery_n,aa_jun_n,aa_duran_n,aa_kiesel_n)), 
                       MARGIN=1, FUN=max)
id_years$poly_cov <- id_years$meds_count-id_years$aa_any
# create polypharmacy covariate
id_years$aa_ancelin_poly <- id_years$meds_count - id_years$aa_ancelin_n 
id_years$aa_boustani_poly <- id_years$meds_count - id_years$aa_boustani_n 
id_years$aa_carnahan_poly <- id_years$meds_count - id_years$aa_carnahan_n 
id_years$aa_cancelli_poly <- id_years$meds_count - id_years$aa_cancelli_n 
id_years$aa_chew_poly <- id_years$meds_count - id_years$aa_chew_n 
id_years$aa_rudolph_poly <- id_years$meds_count - id_years$aa_rudolph_n 
id_years$aa_ehrt_poly <- id_years$meds_count - id_years$aa_ehrt_n 
id_years$aa_han_poly <- id_years$meds_count - id_years$aa_han_n 
id_years$aa_sittironnarit_poly <- id_years$meds_count - id_years$aa_sittironnarit_n 
id_years$aa_briet_poly <- id_years$meds_count - id_years$aa_briet_n 
id_years$aa_bishara_poly <- id_years$meds_count - id_years$aa_bishara_n 
id_years$aa_nery_poly <- id_years$meds_count - id_years$aa_nery_n 
id_years$aa_jun_poly <- id_years$meds_count - id_years$aa_jun_n
id_years$aa_duran_poly <- id_years$meds_count - id_years$aa_duran_n 
id_years$aa_kiesel_poly <- id_years$meds_count - id_years$aa_kiesel_n

# change to factors
id_years$mood_dis <- as.factor(id_years$mood_dis)
id_years$diabetes <- as.factor(id_years$diabetes)
id_years$hyperchol <- as.factor(id_years$hyperchol)
id_years$hypertension <- as.factor(id_years$hypertension)
id_years$heart_attack <- as.factor(id_years$heart_attack)
id_years$psychosis <- as.factor(id_years$psychosis)
id_years$anxiety <- as.factor(id_years$anxiety)

# calculate age at assessment
id_years$ass_age_0 <- as.numeric(difftime(id_years$ass_date_0, id_years$birth_date))/365.25
id_years$ass_age_2 <- as.numeric(difftime(id_years$ass_date_2, id_years$birth_date))/365.25

# set "year" to be year of first assessment or year of imaging assessment, depending on which data is used
id_years$year[is.na(id_years$ass_date_2)] <- id_years$ass_year_0[is.na(id_years$ass_date_2)]
id_years$year[!is.na(id_years$ass_date_2)] <- id_years$ass_year_2[!is.na(id_years$ass_date_2)]

# for disorders used as covariates: set to 1 only if it occurrs before assessment year
id_years$year_mood_dis <- as.numeric(format(id_years$date_mood_dis, "%Y"))
id_years$mood_dis <- 0
id_years$mood_dis[!is.na(id_years$year_mood_dis) & id_years$year_mood_dis < id_years$year] <- 1

id_years$year_diabetes <- as.numeric(format(id_years$date_diabetes, "%Y"))
id_years$diabetes <- 0
id_years$diabetes[!is.na(id_years$year_diabetes) & id_years$year_diabetes < id_years$year] <- 1

id_years$year_hyperchol <- as.numeric(format(id_years$date_hyperchol, "%Y"))
id_years$hyperchol <- 0
id_years$hyperchol[!is.na(id_years$year_hyperchol) & id_years$year_hyperchol < id_years$year] <- 1

id_years$year_hypertension <- as.numeric(format(id_years$date_hypertension, "%Y"))
id_years$hypertension <- 0
id_years$hypertension[!is.na(id_years$year_hypertension) & id_years$year_hypertension < id_years$year] <- 1

id_years$year_heart_attack <- as.numeric(format(id_years$date_heart_attack, "%Y"))
id_years$heart_attack <- 0
id_years$heart_attack[!is.na(id_years$year_heart_attack) & id_years$year_heart_attack < id_years$year] <- 1

id_years$year_psychosis <- as.numeric(format(id_years$date_psychosis, "%Y"))
id_years$psychosis <- 0
id_years$psychosis[!is.na(id_years$year_psychosis) & id_years$year_psychosis < id_years$year] <- 1

id_years$year_anxiety <- as.numeric(format(id_years$date_anxiety, "%Y"))
id_years$anxiety <- 0
id_years$anxiety[!is.na(id_years$year_anxiety) & id_years$year_anxiety < id_years$year] <- 1



# remove outliers for numerical variables (don't disregard 0s)
id_years[, c('deprivation','med_bmi','bmi_0','bmi_2','comorbidity_csum')] <- 
  lapply(id_years[, c('deprivation','med_bmi','bmi_0','bmi_2','comorbidity_csum')], outliers, SD=4, variance='IQR')


# remove outliers for numerical variables (disregard 0s)
cols <- c('aa_ancelin', 'aa_boustani', 'aa_carnahan', 'aa_cancelli', 'aa_chew', 'aa_any', 'poly_cov',
          'aa_rudolph', 'aa_ehrt', 'aa_han', 'aa_sittironnarit', 'aa_briet', 'aa_bishara', 'aa_nery', 'aa_jun',
          'aa_duran', 'aa_kiesel','aa_ancelin_n', 
          'aa_boustani_n', 'aa_carnahan_n', 'aa_cancelli_n', 'aa_chew_n', 'aa_rudolph_n', 'aa_ehrt_n', 'aa_han_n', 'aa_sittironnarit_n', 
          'aa_briet_n', 'aa_bishara_n','aa_nery_n', 'aa_jun_n', 'aa_duran_n', 'aa_kiesel_n','aa_ancelin_poly', 
          'aa_boustani_poly', 'aa_carnahan_poly', 'aa_cancelli_poly', 'aa_chew_poly', 'aa_rudolph_poly', 'aa_ehrt_poly', 'aa_han_poly', 'aa_sittironnarit_poly', 
          'aa_briet_poly', 'aa_bishara_poly','aa_nery_poly', 'aa_jun_poly', 'aa_duran_poly', 'aa_kiesel_poly', 
          'SYS_metabolic','SYS_antiinfective','SYS_immuno_modulating','SYS_blood',
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
  id_years[[col]][id_years[[col]]>0] <- outliers(id_years[[col]][id_years[[col]]>0], SD=4, variance='IQR')
}


# scale all numerical variables
id_years[, c('aa_ancelin', 'aa_boustani', 'aa_carnahan', 'aa_cancelli', 'aa_chew', 'aa_any', 'poly_cov',
             'aa_rudolph', 'aa_ehrt', 'aa_han', 'aa_sittironnarit', 'aa_briet', 'aa_bishara', 'aa_nery', 'aa_jun',
             'aa_duran', 'aa_kiesel','aa_ancelin_n', 
             'aa_boustani_n', 'aa_carnahan_n', 'aa_cancelli_n', 'aa_chew_n', 'aa_rudolph_n', 'aa_ehrt_n', 'aa_han_n', 'aa_sittironnarit_n', 
             'aa_briet_n', 'aa_bishara_n','aa_nery_n', 'aa_jun_n', 'aa_duran_n', 'aa_kiesel_n','aa_ancelin_poly', 
             'aa_boustani_poly', 'aa_carnahan_poly', 'aa_cancelli_poly', 'aa_chew_poly', 'aa_rudolph_poly', 'aa_ehrt_poly', 'aa_han_poly', 'aa_sittironnarit_poly', 
             'aa_briet_poly', 'aa_bishara_poly','aa_nery_poly', 'aa_jun_poly', 'aa_duran_poly', 'aa_kiesel_poly','deprivation','med_bmi','bmi_0','bmi_2','comorbidity_csum', 
             'SYS_metabolic','SYS_antiinfective','SYS_immuno_modulating','SYS_blood',
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
             'PHARM_urological','PHARM_vasodilator','VNR_0', 'RT_0', 'VisMem_0', 'ProsMem_0', 'NM_0', 'DSS_2', 'MR_2', 'TMTb_2', 'TR_2', 
             'g_0', 'g_2', 'sample_time','missing_years')] <- 
  lapply(id_years[, c('aa_ancelin', 'aa_boustani', 'aa_carnahan', 'aa_cancelli', 'aa_chew', 'aa_any', 'poly_cov',
                      'aa_rudolph', 'aa_ehrt', 'aa_han', 'aa_sittironnarit', 'aa_briet', 'aa_bishara', 'aa_nery', 'aa_jun',
                      'aa_duran', 'aa_kiesel','aa_ancelin_n', 
                      'aa_boustani_n', 'aa_carnahan_n', 'aa_cancelli_n', 'aa_chew_n', 'aa_rudolph_n', 'aa_ehrt_n', 'aa_han_n', 'aa_sittironnarit_n', 
                      'aa_briet_n', 'aa_bishara_n','aa_nery_n', 'aa_jun_n', 'aa_duran_n', 'aa_kiesel_n','aa_ancelin_poly', 
                      'aa_boustani_poly', 'aa_carnahan_poly', 'aa_cancelli_poly', 'aa_chew_poly', 'aa_rudolph_poly', 'aa_ehrt_poly', 'aa_han_poly', 'aa_sittironnarit_poly', 
                      'aa_briet_poly', 'aa_bishara_poly','aa_nery_poly', 'aa_jun_poly', 'aa_duran_poly', 'aa_kiesel_poly','deprivation','med_bmi','bmi_0','bmi_2','comorbidity_csum', 
                      'SYS_metabolic','SYS_antiinfective','SYS_immuno_modulating','SYS_blood',
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
                      'PHARM_urological','PHARM_vasodilator','VNR_0', 'RT_0', 'VisMem_0', 'ProsMem_0', 'NM_0', 'DSS_2', 'MR_2', 'TMTb_2', 'TR_2', 
                      'g_0', 'g_2','sample_time','missing_years')], scale)


## Define a new "g" that we will use for modelling; it will use all cognitive tests at the second assessment; also define new covariates
id_years$g <- id_years$g_0
id_years$g[!is.na(id_years$ass_year_2)] <- id_years$g_2[!is.na(id_years$ass_year_2)]

id_years$ass_age <- id_years$ass_age_0
id_years$ass_age[!is.na(id_years$ass_year_2)] <- id_years$ass_age_2[!is.na(id_years$ass_year_2)]

id_years$smoking <- id_years$smoking_0
id_years$smoking[!is.na(id_years$ass_year_2)] <- id_years$smoking_2[!is.na(id_years$ass_year_2)]

id_years$alc_freq <- id_years$alc_freq_0
id_years$alc_freq[!is.na(id_years$ass_year_2)] <- id_years$alc_freq_2[!is.na(id_years$ass_year_2)]

id_years$activity <- id_years$activity_0
id_years$activity[!is.na(id_years$ass_year_2)] <- id_years$activity_2[!is.na(id_years$ass_year_2)]

id_years$bmi <- id_years$bmi_0
id_years$bmi[!is.na(id_years$ass_year_2)] <- id_years$bmi_2[!is.na(id_years$ass_year_2)]

## cognition ~ ACB

model_duran <- lm(data=id_years, g ~ aa_duran + aa_duran_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                    activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                    apoe_carrier)
model_ancelin <- lm(data=id_years, g ~ aa_ancelin + aa_ancelin_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                      activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                      apoe_carrier)
model_boustani <- lm(data=id_years, g ~ aa_boustani + aa_boustani_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                       activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                       apoe_carrier)
model_carnahan <- lm(data=id_years, g ~ aa_carnahan + aa_carnahan_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                       activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                       apoe_carrier)
model_cancelli <- lm(data=id_years, g ~ aa_cancelli + aa_cancelli_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                       activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                       apoe_carrier)
model_chew <- lm(data=id_years, g ~ aa_chew + aa_chew_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                   activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                   apoe_carrier)
model_rudolph <- lm(data=id_years, g ~ aa_rudolph + aa_rudolph_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                      activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                      apoe_carrier)
model_ehrt <- lm(data=id_years, g ~ aa_ehrt + aa_ehrt_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                   activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                   apoe_carrier)
model_han <- lm(data=id_years, g ~ aa_han + aa_han_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                  activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                  apoe_carrier)
model_sittironnarit <- lm(data=id_years, g ~ aa_sittironnarit + aa_sittironnarit_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                            activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                            apoe_carrier)
model_briet <- lm(data=id_years, g ~ aa_briet + aa_briet_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                    activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                    apoe_carrier)
model_bishara <- lm(data=id_years, g ~ aa_bishara + aa_bishara_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                      activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                      apoe_carrier)
model_kiesel <- lm(data=id_years, g ~ aa_kiesel + aa_kiesel_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                     activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                     apoe_carrier)
model_nery <- lm(data=id_years, g ~ aa_nery + aa_nery_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                   activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                   apoe_carrier)
model_jun <- lm(data=id_years, g ~ aa_jun + aa_jun_poly + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                  activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                  apoe_carrier)
model_poly_simple <- lm(data=id_years, g ~ poly_cov + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                          activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                          apoe_carrier)
model_poly <- lm(data=id_years, g ~ poly_cov + aa_any + sample_time + sex + data_provider + ass_age + deprivation + smoking + alc_freq +
                   activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                   apoe_carrier)


models <- c()
models[[1]] <- model_ancelin; models[[2]] <- model_boustani; models[[3]] <- model_carnahan; models[[4]] <- model_cancelli; models[[5]] <- model_chew;
models[[6]] <- model_rudolph; models[[7]] <- model_ehrt; models[[8]] <- model_han; models[[9]] <- model_sittironnarit; 
models[[10]] <- model_briet; models[[11]] <- model_bishara; models[[12]] <- model_duran; models[[13]] <- model_kiesel;
models[[14]] <- model_nery; models[[15]] <- model_jun;
models[[16]] <- model_poly_simple; models[[17]] <- model_poly

scales_cognition <- data.frame(scale=c('ancelin', 'boustani', 'carnahan', 'cancelli', 'chew', 'rudolph', 
                                       'ehrt', 'han', 'sittironnarit', 'briet', 'bishara', 'duran', 'kiesel','nery','jun',
                                       'poly_simple', 'poly'))

## set p-threshold for each model (FDR)

n_models <- 17
p_base <- 0.05
scales_cognition$initial_order <- seq(1:n_models)# save initial order


# 1. get p-values
count = 0
scales_cognition$p <- NA
for(model in models){
  count <- count + 1
  coefs <- data.frame(summary(model)$coefficients)
  scales_cognition$p[count] <- (coefs[2,4])
}
# 2. determine significance
scales_cognition <- arrange(scales_cognition, p) # sort in ascending order
scales_cognition$p_crit <- p_base*(as.numeric(rownames(scales_cognition)))/n_models # calculate p-threshold (0.05*(i/n))
scales_cognition$sig <- FALSE; scales_cognition$sig[scales_cognition$p < scales_cognition$p_crit] <- TRUE # check if p < p threshold
# set all sig. values larger than the smallest index for which p<p_crit. to non-sig (i.e., FALSE)
if (length(as.numeric(rownames(scales_cognition[scales_cognition$sig==FALSE,]))) != 0){ # if there are any non-sig values
  first_false <- min(as.numeric(rownames(scales_cognition[scales_cognition$sig==FALSE,])))
  scales_cognition$sig[first_false:nrow(scales_cognition)] <- FALSE
}
# 3. get FDR-adjusted p-value
scales_cognition$p_adjusted <- p.adjust(scales_cognition$p, method='BH')
# 4. for CIs: get Z-value of the quantile for the standard normal distribution for the p
scales_cognition$z_val <- qnorm(1-scales_cognition$p_crit/2)

count = 0
scales_cognition$beta <- NA
scales_cognition$beta_low <- NA
scales_cognition$beta_high <- NA
scales_cognition$n_missing <- NA
scales_cognition$poly_beta <- NA
scales_cognition$poly_low <- NA
scales_cognition$poly_high <- NA
# reorder so that the correct stats from "model" map to the appropriate row
scales_cognition <- scales_cognition %>% arrange(initial_order)
scales_cognition$initial_order <- NULL
for (model in models){
  count <- count + 1
  coefs <- data.frame(summary(model)$coefficients)
  scales_cognition$beta[count] <- (coefs[2,1])
  scales_cognition$beta_high[count] <- (coefs[2,1] + coefs[2,2]*scales_cognition[count,6])
  scales_cognition$beta_low[count] <- (coefs[2,1] - coefs[2,2]*scales_cognition[count,6])
  scales_cognition$n_missing[count] <- length(unique(model$na.action))
  scales_cognition$poly_beta[count] <- (coefs[3,1])
  scales_cognition$poly_high[count] <- (coefs[3,1] + coefs[3,2]*scales_cognition[count,6])
  scales_cognition$poly_low[count] <- (coefs[3,1] - coefs[3,2]*scales_cognition[count,6])
}






# associations between effect size and number of drugs included in the scale
scales <- read.csv('aas_combined.csv')
scales[,2:ncol(scales)][scales[,2:ncol(scales)] > 0] <- 1 # set to binary
scales <- subset(scales, select=-c(aa_kiesel_add_on, aa_duran_add_on, aa_0, aa_05, aa_1, aa_2,
                                   aa_3, aa_4, aa_kiesel_duran, drug))
scales <- data.frame(t(scales)) # transpose
scales[] <- lapply(scales, as.numeric)
scales$n <- rowSums(scales, na.rm = TRUE)
scales$scale <- rownames(scales)
scales <- subset(scales, select=c(scale, n))
scales$scale <- gsub('aa_', '', scales$scale)
scales$scale <- c('ancelin', 'chew', 'cancelli', 'han', 'rudolph', 'ehrt', 'sittironnarit', 'boustani',
                  'carnahan', 'briet', 'bishara', 'nery', 'jun', 'kiesel', 'duran')
scales <- merge(scales, scales_cognition, by = 'scale', all.x = TRUE)
cor.test(scales$n, abs(scales$beta))


## Repeat the Duran scale model, but include interaction with age as a term

model_duran <- lm(data=id_years, g ~ aa_duran*ass_age + aa_duran_poly + sample_time + sex + data_provider + deprivation + smoking + alc_freq +
                    activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                    apoe_carrier)





## cognition ~ ACB of distinct classes of drugs
model_SYS <- lm(data=id_years, g ~ SYS_blood + SYS_metabolic + SYS_antiinfective + SYS_immuno_modulating +
                  SYS_cardiovascular + SYS_urinary + SYS_musculo_skeletal + SYS_neuro + SYS_respiratory + SYS_hormonal +
                  aa_duran_poly + sex + ass_age + deprivation + smoking + alc_freq + data_provider +
                  activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                  apoe_carrier + sample_time)
summary(model_SYS)



model_PHARM <- lm(data=id_years, g ~ PHARM_anticholinergic + PHARM_antidepressant + PHARM_antihistamines + 
                    PHARM_antimigraine + PHARM_antipropulsive + PHARM_antipsychotic + PHARM_diuretic_low +
                    PHARM_anxiolytic + PHARM_penicillin + PHARM_glucose_lowering + 
                    PHARM_cardiac_glycoside + PHARM_corticosteroid + PHARM_gastrointestinal + PHARM_acid_reflux + 
                    PHARM_diuretic_high + PHARM_sedative + PHARM_immunosuppressant + PHARM_muscle_relaxant +
                    PHARM_ace_inh + PHARM_antigout + PHARM_belladonna + PHARM_decongestant + PHARM_antiinflammatory + 
                    PHARM_antithrombotic + PHARM_OAD_systemic + PHARM_Ca_blocker_cardiac + PHARM_opioid + PHARM_propulsive + 
                    PHARM_urological + PHARM_vasodilator + PHARM_constipation + PHARM_macro_linco_strepto + PHARM_quinolone + 
                    aa_duran_poly + sex + ass_age + deprivation + smoking + alc_freq + data_provider +
                    activity + bmi + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                    apoe_carrier + sample_time)
summary(model_PHARM)




## Plots

# the code below (depending on what you comment out) plots either model
SYS_predictors <- data.frame(scale=c('blood', 'gastrointestinal', 'antiinfective', 'immuno-modulating', 
                                     'cardiovascular', 'urinary', 
                                     'musculo-skeletal' ,'nervous', 'respiratory', 'hormonal'))

PHARM_predictors <- data.frame(scale=c("anticholinergic", "antidepressant", "antihistamine", 
                                       "antimigraine", "antipropulsive",
                                       "antipsychotic", "low-ceiling diuretic", "anxiolytic", "β-lactam antibiotic", "glucose-lowering", "cardiac glycoside",
                                       "corticosteroid", "gastrointestinal", "acid reflux", "high ceiling diuretic",
                                       "sedative", "immunosuppressant", "muscle relaxant", "ACE inhibitor", "anti-gout",
                                       "belladonna", "decongestant", "anti-inflammatory","anti-thrombotic", "OAD", 
                                       "Ca-blocker cardiac", "opioid", "propulsive", "urological", "vasodilator",
                                       "constipation", "other antibacterial", "quinolone"))

#model <- model_SYS
model <- model_PHARM
#cog_predictors <- data.frame(SYS_predictors)
cog_predictors <- data.frame(PHARM_predictors)

cog_predictors$initial_order <- seq(1:nrow(cog_predictors))# save initial order

## FDR on drug-class models
# 1. get p-values
cog_predictors$p <- summary(model)$coefficients[(1:nrow(cog_predictors))+1, 4]
# 2. determine significance
cog_predictors <- arrange(cog_predictors, p) # sort in ascending order
cog_predictors$p_crit <- 0.05*(as.numeric(rownames(cog_predictors)))/nrow(cog_predictors) # calculate p-threshold (0.05*(i/n))
cog_predictors$sig <- FALSE; cog_predictors$sig[cog_predictors$p < cog_predictors$p_crit] <- TRUE # check if p < p threshold

# set all sig. values larger than the smallest index for which p<p_crit. to non-sig (i.e., FALSE)
if (length(as.numeric(rownames(cog_predictors[cog_predictors$sig==FALSE,]))) != 0){ # if there are any non-sig values
  first_false <- min(as.numeric(rownames(cog_predictors[cog_predictors$sig==FALSE,])))
  cog_predictors$sig[first_false:nrow(cog_predictors)] <- FALSE
}
# 3. get FDR-adjusted p-value
cog_predictors$p_adjusted <- p.adjust(cog_predictors$p, method='BH')
# 4. for CIs: get Z-value of the quantile for the standard normal distribution for the p
cog_predictors$z_val <- qnorm(1-cog_predictors$p_crit/2)

cog_predictors$beta <- NA
cog_predictors$beta_low <- NA
cog_predictors$beta_high <- NA
cog_predictors$n_missing <- NA
# reorder so that the correct stats from "model" map to the appropriate row
cog_predictors <- cog_predictors %>% arrange(initial_order)
cog_predictors$initial_order <- NULL
coefs <- data.frame(summary(model)$coefficients)
cog_predictors$beta <- (coefs[(1:nrow(cog_predictors))+1,1])
cog_predictors$beta_high <- (cog_predictors$beta + (coefs[(1:nrow(cog_predictors))+1,2])*cog_predictors[['z_val']])
cog_predictors$beta_low <- (cog_predictors$beta - (coefs[(1:nrow(cog_predictors))+1,2])*cog_predictors[['z_val']])
cog_predictors$n_missing <- length(unique(model$na.action))







## Individual cognitive tests with Duran et al. (2013)
model_VNR <- lm(data=id_years, VNR_0 ~ aa_duran + aa_duran_poly + sex + data_provider + ass_age_0 + deprivation + smoking_0 + alc_freq_0 +
                  activity_0 + bmi_0 + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                  apoe_carrier + sample_time)
model_RT <- lm(data=id_years, -RT_0 ~ aa_duran + aa_duran_poly + sex + data_provider + ass_age_0 + deprivation + smoking_0 + alc_freq_0 +
                 activity_0 + bmi_0 + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                 apoe_carrier + sample_time)
model_VisMem <- lm(data=id_years, -VisMem_0 ~ aa_duran + aa_duran_poly + sex + data_provider + ass_age_0 + deprivation + smoking_0 + alc_freq_0 +
                     activity_0 + bmi_0 + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                     apoe_carrier + sample_time)
model_ProsMem <- lm(data=id_years, ProsMem_0 ~ aa_duran + aa_duran_poly + sex + data_provider + ass_age_0 + deprivation + smoking_0 + alc_freq_0 +
                      activity_0 + bmi_0 + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                      apoe_carrier + sample_time)
model_NM <- lm(data=id_years, NM_0 ~ aa_duran + aa_duran_poly + sex + data_provider + ass_age_0 + deprivation + smoking_0 + alc_freq_0 +
                 activity_0 + bmi_0 + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                 apoe_carrier + sample_time)
model_DSS <- lm(data=id_years, DSS_2 ~ aa_duran + aa_duran_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                  activity_2 + bmi_2 + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                  apoe_carrier + sample_time)
model_MR <- lm(data=id_years, MR_2 ~ aa_duran + aa_duran_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                 activity_2 + bmi_2 + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                 apoe_carrier + sample_time)
model_TMTb <- lm(data=id_years, -TMTb_2 ~ aa_duran + aa_duran_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                   activity_2 + bmi_2 + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                   apoe_carrier + sample_time)
model_TR <- lm(data=id_years, TR_2 ~ aa_duran + aa_duran_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                 activity_2 + bmi_2 + comorbidity_csum  + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety +
                 apoe_carrier + sample_time)
models <- c()
models[[1]] <- model_VNR; models[[2]] <- model_RT; models[[3]] <- model_VisMem; models[[4]] <- model_ProsMem; models[[5]] <- model_NM;
models[[6]] <- model_DSS; models[[7]] <- model_MR; models[[8]] <- model_TMTb; models[[9]] <- model_TR

cog_tests <- data.frame(scale=c('VNR', 'RT', 'VisMem', 'ProsMem', 'NM', 'DSS', 
                                'MR', 'TMTb', 'TR'))

# Set p threshold
n_models <- 9
p_base <- 0.05
cog_tests$initial_order <- seq(1:n_models)# save initial order

# 1. get p-values
count = 0
cog_tests$p <- NA
for(model in models){
  count <- count + 1
  coefs <- data.frame(summary(model)$coefficients)
  cog_tests$p[count] <- (coefs[2,4])
}
# 2. determine significance
cog_tests <- arrange(cog_tests, p) # sort in ascending order
cog_tests$p_crit <- p_base*(as.numeric(rownames(cog_tests)))/n_models # calculate p-threshold (0.05*(i/n))
cog_tests$sig <- FALSE; cog_tests$sig[cog_tests$p < cog_tests$p_crit] <- TRUE # check if p < p threshold
# set all sig. values larger than the smallest index for which p<p_crit. to non-sig (i.e., FALSE)
if (length(as.numeric(rownames(cog_tests[cog_tests$sig==FALSE,]))) != 0){ # if there are any non-sig values
  first_false <- min(as.numeric(rownames(cog_tests[cog_tests$sig==FALSE,])))
  cog_tests$sig[first_false:nrow(cog_tests)] <- FALSE
}
# 3. get FDR-adjusted p-value
cog_tests$p_adjusted <- p.adjust(cog_tests$p, method='BH')
# 4. for CIs: get Z-value of the quantile for the standard normal distribution for the p
cog_tests$z_val <- qnorm(1-cog_tests$p_crit/2)



count = 0
cog_tests$beta <- NA
cog_tests$beta_low <- NA
cog_tests$beta_high <- NA
cog_tests$n_missing <- NA
cog_tests$poly_beta <- NA
cog_tests$poly_low <- NA
cog_tests$poly_high <- NA
# reorder so that the correct stats from "model" map to the appropriate row
cog_tests <- cog_tests %>% arrange(initial_order)
cog_tests$initial_order <- NULL
for (model in models){
  count <- count + 1
  coefs <- data.frame(summary(model)$coefficients)
  cog_tests$beta[count] <- (coefs[2,1])
  cog_tests$beta_high[count] <- (coefs[2,1] + coefs[2,2]*cog_tests[count,6])
  cog_tests$beta_low[count] <- (coefs[2,1] - coefs[2,2]*cog_tests[count,6])
  cog_tests$n_missing[count] <- length(unique(model$na.action))
  cog_tests$poly_beta[count] <- (coefs[3,1])
  cog_tests$poly_high[count] <- (coefs[3,1] + coefs[3,2]*cog_tests[count,6])
  cog_tests$poly_low[count] <- (coefs[3,1] - coefs[3,2]*cog_tests[count,6])
}