library(tidyverse)
library(car)


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



## AChB vs. cognition at assessment centre

# read in data frame
id_years <- readRDS('masterfile.rds')
# remove cases with dopaminergic and antiepileptic drugs that got through
id_years <- filter(id_years, PHARM_dopaminergic == 0 & PHARM_antiepileptic == 0)
# remove cases with no T2-Flair imaging
id_years <- filter(id_years, !is.na(T2_flair) & T2_flair=='1')
# calculate the difference between any prescription year and the assessment year
id_years$year_dif_2 <- id_years$ass_year_2 - id_years$year
# keep the prescription years before the imaging assessment
id_years <- filter(id_years, (!is.na(ass_year_2) & year_dif_2>0))
# calculate the difference between the year 2015 and the assessment date to get the number of years for which AChB is not calculated
id_years$missing_years <- id_years$ass_year_2 - 2015
id_years$missing_years[id_years$missing_years < 0] <- 0 

# change default APOE-carrier status
id_years$apoe_carrier <- as.character(id_years$apoe_carrier)
id_years$apoe_carrier[id_years$apoe_carrier=='e3'] <- 'e0'
id_years$apoe_carrier <- as.factor(id_years$apoe_carrier)

## calculate the cumulative ACB for each participant and the number of years in the sample

# calculate number of years in sample
sample_time <- id_years %>% group_by(id) %>% summarise(sample_time=max(year_dif_2))
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


# for disorders used as covariates: set to 1 only if it occurrs before assessment year
id_years$year_mood_dis <- as.numeric(format(id_years$date_mood_dis, "%Y"))
id_years$mood_dis <- 0
id_years$mood_dis[!is.na(id_years$year_mood_dis) & id_years$year_mood_dis < id_years$ass_year_2] <- 1

id_years$year_diabetes <- as.numeric(format(id_years$date_diabetes, "%Y"))
id_years$diabetes <- 0
id_years$diabetes[!is.na(id_years$year_diabetes) & id_years$year_diabetes < id_years$ass_year_2] <- 1

id_years$year_hyperchol <- as.numeric(format(id_years$date_hyperchol, "%Y"))
id_years$hyperchol <- 0
id_years$hyperchol[!is.na(id_years$year_hyperchol) & id_years$year_hyperchol < id_years$ass_year_2] <- 1

id_years$year_hypertension <- as.numeric(format(id_years$date_hypertension, "%Y"))
id_years$hypertension <- 0
id_years$hypertension[!is.na(id_years$year_hypertension) & id_years$year_hypertension < id_years$ass_year_2] <- 1

id_years$year_heart_attack <- as.numeric(format(id_years$date_heart_attack, "%Y"))
id_years$heart_attack <- 0
id_years$heart_attack[!is.na(id_years$year_heart_attack) & id_years$year_heart_attack < id_years$ass_year_2] <- 1

id_years$year_psychosis <- as.numeric(format(id_years$date_psychosis, "%Y"))
id_years$psychosis <- 0
id_years$psychosis[!is.na(id_years$year_psychosis) & id_years$year_psychosis < id_years$ass_year_2] <- 1

id_years$year_anxiety <- as.numeric(format(id_years$date_anxiety, "%Y"))
id_years$anxiety <- 0
id_years$anxiety[!is.na(id_years$year_anxiety) & id_years$year_anxiety < id_years$ass_year_2] <- 1

# remove outliers for numerical variables (don't disregard 0s)
id_years[, c('deprivation','bmi_2','comorbidity_csum')] <- 
  lapply(id_years[, c('deprivation','bmi_2','comorbidity_csum')], outliers, SD=4, variance='IQR')


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
             'aa_briet_poly', 'aa_bishara_poly','aa_nery_poly', 'aa_jun_poly', 'aa_duran_poly', 'aa_kiesel_poly','deprivation','bmi_2','comorbidity_csum', 
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
             'PHARM_urological','PHARM_vasodilator',"headposY","headposZ","tablepos",'TBV',
             "TBVicv",'GM','GMicv','WM','WMicv','WMH','ICV','WMHicv',"headposX","BStem_4Vent","VentCSF","VentCSFicv",
             "lh_bankssts_volume","lh_caudalanteriorcingulate_volume", 
             "lh_caudalmiddlefrontal_volume","lh_cuneus_volume","lh_entorhinal_volume", 
             "lh_fusiform_volume","lh_inferiorparietal_volume","lh_inferiortemporal_volume", 
             "lh_isthmuscingulate_volume","lh_lateraloccipital_volume","lh_lateralorbitofrontal_volume", 
             "lh_lingual_volume","lh_medialorbitofrontal_volume","lh_middletemporal_volume", 
             "lh_parahippocampal_volume","lh_paracentral_volume","lh_parsopercularis_volume", 
             "lh_parsorbitalis_volume","lh_parstriangularis_volume","lh_pericalcarine_volume", 
             "lh_postcentral_volume","lh_posteriorcingulate_volume","lh_precentral_volume", 
             "lh_precuneus_volume","lh_rostralanteriorcingulate_volume","lh_rostralmiddlefrontal_volume", 
             "lh_superiorfrontal_volume","lh_superiorparietal_volume","lh_superiortemporal_volume", 
             "lh_supramarginal_volume","lh_frontalpole_volume","lh_temporalpole_volume", 
             "lh_transversetemporal_volume","lh_insula_volume","rh_bankssts_volume", 
             "rh_caudalanteriorcingulate_volume","rh_caudalmiddlefrontal_volume","rh_cuneus_volume", 
             "rh_entorhinal_volume","rh_fusiform_volume","rh_inferiorparietal_volume", 
             "rh_inferiortemporal_volume","rh_isthmuscingulate_volume","rh_lateraloccipital_volume", 
             "rh_lateralorbitofrontal_volume","rh_lingual_volume","rh_medialorbitofrontal_volume", 
             "rh_middletemporal_volume","rh_parahippocampal_volume","rh_paracentral_volume", 
             "rh_parsopercularis_volume","rh_parsorbitalis_volume","rh_parstriangularis_volume", 
             "rh_pericalcarine_volume","rh_postcentral_volume","rh_posteriorcingulate_volume", 
             "rh_precentral_volume","rh_precuneus_volume","rh_rostralanteriorcingulate_volume",
             "rh_rostralmiddlefrontal_volume","rh_superiorfrontal_volume","rh_superiorparietal_volume", 
             "rh_superiortemporal_volume","rh_supramarginal_volume","rh_frontalpole_volume", 
             "rh_temporalpole_volume","rh_transversetemporal_volume","rh_insula_volume",
             'fx_st_FA_l', 'fx_st_FA_r', 'fx_st_MD_l', 'fx_st_MD_r', 'ex_cap_FA_l', 'ex_cap_FA_r',
             'ex_cap_MD_l', 'ex_cap_MD_r', 'unc_fasc_FA_l', 'unc_fasc_FA_r', 'unc_fasc_MD_l', 'unc_fasc_MD_r',
             'g_rect_area_l', 'g_rect_area_r', 'g_rect_thick_l', 'g_rect_thick_r', 
             'g_rect_vol_l', 'g_rect_vol_r','thalamus_l', 'thalamus_r', 'caudate_l', 'caudate_r',
             'putamen_l', 'putamen_r', 'pallidum_l', 'pallidum_r',
             'hippocampus_l', 'hippocampus_r', 'amygdala_l', 'amygdala_r',
             'accumbens_l', 'accumbens_r','FA_acoustic_l', 'FA_acoustic_r', 'FA_anterior_thalamic_l', 'FA_anterior_thalamic_r',
             'FA_cingulate_gyrus_l', 'FA_cingulate_gyrus_r', 'FA_corticospinal_l', 'FA_corticospinal_r',
             'FA_forceps_major', 'FA_forceps_minor', 'FA_fronto_occipital_l', 'FA_fronto_occipital_r',
             'FA_longitudinal_l', 'FA_longitudinal_r', 'FA_medial_lemn_l', 'FA_medial_lemn_r',
             'FA_middle_cereb_ped', 'FA_parahippo_l', 'FA_parahippo_r', 'FA_posterior_thalamic_l',
             'FA_posterior_thalamic_r', 'FA_superior_long_l', 'FA_superior_long_r', 'FA_superior_thalamic_l',
             'FA_superior_thalamic_r', 'FA_uncinate_l', 'FA_uncinate_r',
             'MD_acoustic_l', 'MD_acoustic_r', 'MD_anterior_thalamic_l', 'MD_anterior_thalamic_r',
             'MD_cingulate_gyrus_l', 'MD_cingulate_gyrus_r', 'MD_corticospinal_l', 'MD_corticospinal_r',
             'MD_forceps_major', 'MD_forceps_minor', 'MD_fronto_occipital_l', 'MD_fronto_occipital_r',
             'MD_longitudinal_l', 'MD_longitudinal_r', 'MD_medial_lemn_l', 'MD_medial_lemn_r',
             'MD_middle_cereb_ped', 'MD_parahippo_l', 'MD_parahippo_r', 'MD_posterior_thalamic_l',
             'MD_posterior_thalamic_r', 'MD_superior_long_l', 'MD_superior_long_r', 'MD_superior_thalamic_l',
             'MD_superior_thalamic_r', 'MD_uncinate_l', 'MD_uncinate_r','sample_time', 'missing_years','scal_fac_2')] <- 
  lapply(id_years[, c('aa_ancelin', 'aa_boustani', 'aa_carnahan', 'aa_cancelli', 'aa_chew', 'aa_any', 'poly_cov',
                      'aa_rudolph', 'aa_ehrt', 'aa_han', 'aa_sittironnarit', 'aa_briet', 'aa_bishara', 'aa_nery', 'aa_jun',
                      'aa_duran', 'aa_kiesel','aa_ancelin_n', 
                      'aa_boustani_n', 'aa_carnahan_n', 'aa_cancelli_n', 'aa_chew_n', 'aa_rudolph_n', 'aa_ehrt_n', 'aa_han_n', 'aa_sittironnarit_n', 
                      'aa_briet_n', 'aa_bishara_n','aa_nery_n', 'aa_jun_n', 'aa_duran_n', 'aa_kiesel_n','aa_ancelin_poly', 
                      'aa_boustani_poly', 'aa_carnahan_poly', 'aa_cancelli_poly', 'aa_chew_poly', 'aa_rudolph_poly', 'aa_ehrt_poly', 'aa_han_poly', 'aa_sittironnarit_poly', 
                      'aa_briet_poly', 'aa_bishara_poly','aa_nery_poly', 'aa_jun_poly', 'aa_duran_poly', 'aa_kiesel_poly','deprivation','bmi_2','comorbidity_csum', 
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
                      'PHARM_urological','PHARM_vasodilator',"headposY","headposZ","tablepos",'TBV',
                      "TBVicv",'GM','GMicv','WM','WMicv','WMH','ICV','WMHicv',"headposX","BStem_4Vent","VentCSF","VentCSFicv",
                      "lh_bankssts_volume","lh_caudalanteriorcingulate_volume", 
                      "lh_caudalmiddlefrontal_volume","lh_cuneus_volume","lh_entorhinal_volume", 
                      "lh_fusiform_volume","lh_inferiorparietal_volume","lh_inferiortemporal_volume", 
                      "lh_isthmuscingulate_volume","lh_lateraloccipital_volume","lh_lateralorbitofrontal_volume", 
                      "lh_lingual_volume","lh_medialorbitofrontal_volume","lh_middletemporal_volume", 
                      "lh_parahippocampal_volume","lh_paracentral_volume","lh_parsopercularis_volume", 
                      "lh_parsorbitalis_volume","lh_parstriangularis_volume","lh_pericalcarine_volume", 
                      "lh_postcentral_volume","lh_posteriorcingulate_volume","lh_precentral_volume", 
                      "lh_precuneus_volume","lh_rostralanteriorcingulate_volume","lh_rostralmiddlefrontal_volume", 
                      "lh_superiorfrontal_volume","lh_superiorparietal_volume","lh_superiortemporal_volume", 
                      "lh_supramarginal_volume","lh_frontalpole_volume","lh_temporalpole_volume", 
                      "lh_transversetemporal_volume","lh_insula_volume","rh_bankssts_volume", 
                      "rh_caudalanteriorcingulate_volume","rh_caudalmiddlefrontal_volume","rh_cuneus_volume", 
                      "rh_entorhinal_volume","rh_fusiform_volume","rh_inferiorparietal_volume", 
                      "rh_inferiortemporal_volume","rh_isthmuscingulate_volume","rh_lateraloccipital_volume", 
                      "rh_lateralorbitofrontal_volume","rh_lingual_volume","rh_medialorbitofrontal_volume", 
                      "rh_middletemporal_volume","rh_parahippocampal_volume","rh_paracentral_volume", 
                      "rh_parsopercularis_volume","rh_parsorbitalis_volume","rh_parstriangularis_volume", 
                      "rh_pericalcarine_volume","rh_postcentral_volume","rh_posteriorcingulate_volume", 
                      "rh_precentral_volume","rh_precuneus_volume","rh_rostralanteriorcingulate_volume",
                      "rh_rostralmiddlefrontal_volume","rh_superiorfrontal_volume","rh_superiorparietal_volume", 
                      "rh_superiortemporal_volume","rh_supramarginal_volume","rh_frontalpole_volume", 
                      "rh_temporalpole_volume","rh_transversetemporal_volume","rh_insula_volume",
                      'fx_st_FA_l', 'fx_st_FA_r', 'fx_st_MD_l', 'fx_st_MD_r', 'ex_cap_FA_l', 'ex_cap_FA_r',
                      'ex_cap_MD_l', 'ex_cap_MD_r', 'unc_fasc_FA_l', 'unc_fasc_FA_r', 'unc_fasc_MD_l', 'unc_fasc_MD_r',
                      'g_rect_area_l', 'g_rect_area_r', 'g_rect_thick_l', 'g_rect_thick_r', 
                      'g_rect_vol_l', 'g_rect_vol_r','thalamus_l', 'thalamus_r', 'caudate_l', 'caudate_r',
                      'putamen_l', 'putamen_r', 'pallidum_l', 'pallidum_r',
                      'hippocampus_l', 'hippocampus_r', 'amygdala_l', 'amygdala_r',
                      'accumbens_l', 'accumbens_r','FA_acoustic_l', 'FA_acoustic_r', 'FA_anterior_thalamic_l', 'FA_anterior_thalamic_r',
                      'FA_cingulate_gyrus_l', 'FA_cingulate_gyrus_r', 'FA_corticospinal_l', 'FA_corticospinal_r',
                      'FA_forceps_major', 'FA_forceps_minor', 'FA_fronto_occipital_l', 'FA_fronto_occipital_r',
                      'FA_longitudinal_l', 'FA_longitudinal_r', 'FA_medial_lemn_l', 'FA_medial_lemn_r',
                      'FA_middle_cereb_ped', 'FA_parahippo_l', 'FA_parahippo_r', 'FA_posterior_thalamic_l',
                      'FA_posterior_thalamic_r', 'FA_superior_long_l', 'FA_superior_long_r', 'FA_superior_thalamic_l',
                      'FA_superior_thalamic_r', 'FA_uncinate_l', 'FA_uncinate_r',
                      'MD_acoustic_l', 'MD_acoustic_r', 'MD_anterior_thalamic_l', 'MD_anterior_thalamic_r',
                      'MD_cingulate_gyrus_l', 'MD_cingulate_gyrus_r', 'MD_corticospinal_l', 'MD_corticospinal_r',
                      'MD_forceps_major', 'MD_forceps_minor', 'MD_fronto_occipital_l', 'MD_fronto_occipital_r',
                      'MD_longitudinal_l', 'MD_longitudinal_r', 'MD_medial_lemn_l', 'MD_medial_lemn_r',
                      'MD_middle_cereb_ped', 'MD_parahippo_l', 'MD_parahippo_r', 'MD_posterior_thalamic_l',
                      'MD_posterior_thalamic_r', 'MD_superior_long_l', 'MD_superior_long_r', 'MD_superior_thalamic_l',
                      'MD_superior_thalamic_r', 'MD_uncinate_l', 'MD_uncinate_r','sample_time', 'missing_years','scal_fac_2')], scale)

model_duran <- lm(data=id_years, TBVicv ~ aa_duran + aa_duran_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                    activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                    apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_ancelin <- lm(data=id_years, TBVicv ~ aa_ancelin + aa_ancelin_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                      activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                      apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_boustani <- lm(data=id_years, TBVicv ~ aa_boustani + aa_boustani_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                       activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                       apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_carnahan <- lm(data=id_years, TBVicv ~ aa_carnahan + aa_carnahan_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                       activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                       apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_cancelli <- lm(data=id_years, TBVicv ~ aa_cancelli + aa_cancelli_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                       activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                       apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_chew <- lm(data=id_years, TBVicv ~ aa_chew + aa_chew_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                   activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                   apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_rudolph <- lm(data=id_years, TBVicv ~ aa_rudolph + aa_rudolph_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                      activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                      apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_ehrt <- lm(data=id_years, TBVicv ~ aa_ehrt + aa_ehrt_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                   activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                   apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_han <- lm(data=id_years, TBVicv ~ aa_han + aa_han_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                  activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                  apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_sittironnarit <- lm(data=id_years, TBVicv ~ aa_sittironnarit + aa_sittironnarit_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                            activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                            apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_briet <- lm(data=id_years, TBVicv ~ aa_briet + aa_briet_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                    activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                    apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_bishara <- lm(data=id_years, TBVicv ~ aa_bishara + aa_bishara_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                      activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                      apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_kiesel <- lm(data=id_years, TBVicv ~ aa_kiesel + aa_kiesel_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                     activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                     apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_nery <- lm(data=id_years, TBVicv ~ aa_nery + aa_nery_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                   activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                   apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_jun <- lm(data=id_years, TBVicv ~ aa_jun + aa_jun_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                  activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                  apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_poly_simple <- lm(data=id_years, TBVicv ~ poly_cov + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                          activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                          apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
model_poly <- lm(data=id_years, TBVicv ~ poly_cov + aa_any + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                   activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                   apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)

models <- c()
models[[1]] <- model_ancelin; models[[2]] <- model_boustani; models[[3]] <- model_carnahan; models[[4]] <- model_cancelli; models[[5]] <- model_chew;
models[[6]] <- model_rudolph; models[[7]] <- model_ehrt; models[[8]] <- model_han; models[[9]] <- model_sittironnarit; 
models[[10]] <- model_briet; models[[11]] <- model_bishara; models[[12]] <- model_duran; models[[13]] <- model_kiesel;
models[[14]] <- model_nery; models[[15]] <- model_jun;
models[[16]] <- model_poly_simple; models[[17]] <- model_poly

scales_mri <- data.frame(scale=c('ancelin', 'boustani', 'carnahan', 'cancelli', 'chew', 'rudolph', 
                                 'ehrt', 'han', 'sittironnarit', 'briet', 'bishara', 'duran', 'kiesel',
                                 'nery', 'jun',
                                 'poly_simple', 'poly'))

n_models <- 17
p_base <- 0.05
scales_mri$initial_order <- seq(1:n_models)# save initial order


# 1. get p-values
count = 0
scales_mri$p <- NA
for(model in models){
  count <- count + 1
  coefs <- data.frame(summary(model)$coefficients)
  scales_mri$p[count] <- (coefs[2,4])
}
# 2. determine significance
scales_mri <- arrange(scales_mri, p) # sort in ascending order
scales_mri$p_crit <- p_base*(as.numeric(rownames(scales_mri)))/n_models # calculate p-threshold (0.05*(i/n))
scales_mri$sig <- FALSE; scales_mri$sig[scales_mri$p < scales_mri$p_crit] <- TRUE # check if p < p threshold
# set all sig. values larger than the smallest index for which p<p_crit. to non-sig (i.e., FALSE)
if (length(as.numeric(rownames(scales_mri[scales_mri$sig==FALSE,]))) != 0){ # if there are any non-sig values
  first_false <- min(as.numeric(rownames(scales_mri[scales_mri$sig==FALSE,])))
  scales_mri$sig[first_false:nrow(scales_mri)] <- FALSE
}
# 3. get FDR-adjusted p-value
scales_mri$p_adjusted <- p.adjust(scales_mri$p, method='BH')
# 4. for CIs: get Z-value of the quantile for the standard normal distribution for the p
scales_mri$z_val <- qnorm(1-scales_mri$p_crit/2)

count = 0
scales_mri$beta <- NA
scales_mri$beta_low <- NA
scales_mri$beta_high <- NA
scales_mri$n_missing <- NA
scales_mri$poly_beta <- NA
scales_mri$poly_low <- NA
scales_mri$poly_high <- NA
# reorder so that the correct stats from "model" map to the appropriate row
scales_mri <- scales_mri %>% arrange(initial_order)
scales_mri$initial_order <- NULL
for (model in models){
  count <- count + 1
  coefs <- data.frame(summary(model)$coefficients)
  scales_mri$beta[count] <- (coefs[2,1])
  scales_mri$beta_high[count] <- (coefs[2,1] + coefs[2,2]*scales_mri[count,6])
  scales_mri$beta_low[count] <- (coefs[2,1] - coefs[2,2]*scales_mri[count,6])
  scales_mri$n_missing[count] <- length(unique(model$na.action))
  scales_mri$poly_beta[count] <- (coefs[3,1])
  scales_mri$poly_high[count] <- (coefs[3,1] + coefs[3,2]*scales_mri[count,6])
  scales_mri$poly_low[count] <- (coefs[3,1] - coefs[3,2]*scales_mri[count,6])
}



## Cortex

area <- c("lh_bankssts_volume","lh_caudalanteriorcingulate_volume", 
          "lh_caudalmiddlefrontal_volume","lh_cuneus_volume","lh_entorhinal_volume", 
          "lh_fusiform_volume","lh_inferiorparietal_volume","lh_inferiortemporal_volume", 
          "lh_isthmuscingulate_volume","lh_lateraloccipital_volume","lh_lateralorbitofrontal_volume", 
          "lh_lingual_volume","lh_medialorbitofrontal_volume","lh_middletemporal_volume", 
          "lh_parahippocampal_volume","lh_paracentral_volume","lh_parsopercularis_volume", 
          "lh_parsorbitalis_volume","lh_parstriangularis_volume","lh_pericalcarine_volume", 
          "lh_postcentral_volume","lh_posteriorcingulate_volume","lh_precentral_volume", 
          "lh_precuneus_volume","lh_rostralanteriorcingulate_volume","lh_rostralmiddlefrontal_volume", 
          "lh_superiorfrontal_volume","lh_superiorparietal_volume","lh_superiortemporal_volume", 
          "lh_supramarginal_volume","lh_frontalpole_volume","lh_temporalpole_volume", 
          "lh_transversetemporal_volume","lh_insula_volume","rh_bankssts_volume", 
          "rh_caudalanteriorcingulate_volume","rh_caudalmiddlefrontal_volume","rh_cuneus_volume", 
          "rh_entorhinal_volume","rh_fusiform_volume","rh_inferiorparietal_volume", 
          "rh_inferiortemporal_volume","rh_isthmuscingulate_volume","rh_lateraloccipital_volume", 
          "rh_lateralorbitofrontal_volume","rh_lingual_volume","rh_medialorbitofrontal_volume", 
          "rh_middletemporal_volume","rh_parahippocampal_volume","rh_paracentral_volume", 
          "rh_parsopercularis_volume","rh_parsorbitalis_volume","rh_parstriangularis_volume", 
          "rh_pericalcarine_volume","rh_postcentral_volume","rh_posteriorcingulate_volume", 
          "rh_precentral_volume","rh_precuneus_volume","rh_rostralanteriorcingulate_volume",
          "rh_rostralmiddlefrontal_volume","rh_superiorfrontal_volume","rh_superiorparietal_volume", 
          "rh_superiortemporal_volume","rh_supramarginal_volume","rh_frontalpole_volume", 
          "rh_temporalpole_volume","rh_transversetemporal_volume","rh_insula_volume")

models <- c() # array with all models
for (a in area){
  model <- lm(data=id_years, id_years[[a]] ~  aa_duran + aa_duran_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
  models[[length(models)+1]] <- model # add model to models array  
}
cortex <- data.frame(area)

n_models <- nrow(cortex)
p_base <- 0.05
cortex$initial_order <- seq(1:n_models)# save initial order


# 1. get p-values
count = 0
cortex$p <- NA
for(model in models){
  count <- count + 1
  coefs <- data.frame(summary(model)$coefficients)
  cortex$p[count] <- (coefs[2,4])
}
# 2. determine significance
cortex <- arrange(cortex, p) # sort in ascending order
cortex$p_crit <- p_base*(as.numeric(rownames(cortex)))/n_models # calculate p-threshold (0.05*(i/n))
cortex$sig <- FALSE; cortex$sig[cortex$p < cortex$p_crit] <- TRUE # check if p < p threshold
# set all sig. values larger than the smallest index for which p<p_crit. to non-sig (i.e., FALSE)
if (length(as.numeric(rownames(cortex[cortex$sig==FALSE,]))) != 0){ # if there are any non-sig values
  first_false <- min(as.numeric(rownames(cortex[cortex$sig==FALSE,])))
  cortex$sig[first_false:nrow(cortex)] <- FALSE
}
# 3. get FDR-adjusted p-value
cortex$p_adjusted <- p.adjust(cortex$p, method='BH')
# 4. for CIs: get Z-value of the quantile for the standard normal distribution for the p
cortex$z_val <- qnorm(1-cortex$p_crit/2)

count = 0
cortex$beta <- NA
cortex$beta_low <- NA
cortex$beta_high <- NA
cortex$n_missing <- NA
cortex$poly_beta <- NA
cortex$poly_low <- NA
cortex$poly_high <- NA
# reorder so that the correct stats from "model" map to the appropriate row
cortex <- cortex %>% arrange(initial_order)
cortex$initial_order <- NULL
for (model in models){
  count <- count + 1
  coefs <- data.frame(summary(model)$coefficients)
  cortex$beta[count] <- (coefs[2,1])
  cortex$beta_high[count] <- (coefs[2,1] + coefs[2,2]*cortex[count,6])
  cortex$beta_low[count] <- (coefs[2,1] - coefs[2,2]*cortex[count,6])
  cortex$n_missing[count] <- length(unique(model$na.action))
  cortex$poly_beta[count] <- (coefs[3,1])
  cortex$poly_high[count] <- (coefs[3,1] + coefs[3,2]*cortex[count,6])
  cortex$poly_low[count] <- (coefs[3,1] - coefs[3,2]*cortex[count,6])
}



# Subcortical areas

area <- c('thalamus_l', 'thalamus_r', 'caudate_l', 'caudate_r',
          'putamen_l', 'putamen_r', 'pallidum_l', 'pallidum_r',
          'hippocampus_l', 'hippocampus_r', 'amygdala_l', 'amygdala_r',
          'accumbens_l', 'accumbens_r')

models <- c() # array with all models
for (a in area){
  model <- lm(data=id_years, id_years[[a]] ~  aa_duran + aa_duran_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
  models[[length(models)+1]] <- model # add model to models array  
}
subcortex <- data.frame(area)

n_models <- nrow(subcortex)
p_base <- 0.05
subcortex$initial_order <- seq(1:n_models)# save initial order


# 1. get p-values
count = 0
subcortex$p <- NA
for(model in models){
  count <- count + 1
  coefs <- data.frame(summary(model)$coefficients)
  subcortex$p[count] <- (coefs[2,4])
}
# 2. determine significance
subcortex <- arrange(subcortex, p) # sort in ascending order
subcortex$p_crit <- p_base*(as.numeric(rownames(subcortex)))/n_models # calculate p-threshold (0.05*(i/n))
subcortex$sig <- FALSE; subcortex$sig[subcortex$p < subcortex$p_crit] <- TRUE # check if p < p threshold
# set all sig. values larger than the smallest index for which p<p_crit. to non-sig (i.e., FALSE)
if (length(as.numeric(rownames(subcortex[subcortex$sig==FALSE,]))) != 0){ # if there are any non-sig values
  first_false <- min(as.numeric(rownames(subcortex[subcortex$sig==FALSE,])))
  subcortex$sig[first_false:nrow(subcortex)] <- FALSE
}
# 3. get FDR-adjusted p-value
subcortex$p_adjusted <- p.adjust(subcortex$p, method='BH')
# 4. for CIs: get Z-value of the quantile for the standard normal distribution for the p
subcortex$z_val <- qnorm(1-subcortex$p_crit/2)

count = 0
subcortex$beta <- NA
subcortex$beta_low <- NA
subcortex$beta_high <- NA
subcortex$n_missing <- NA
subcortex$poly_beta <- NA
subcortex$poly_low <- NA
subcortex$poly_high <- NA
# reorder so that the correct stats from "model" map to the appropriate row
subcortex <- subcortex %>% arrange(initial_order)
subcortex$initial_order <- NULL
for (model in models){
  count <- count + 1
  coefs <- data.frame(summary(model)$coefficients)
  subcortex$beta[count] <- (coefs[2,1])
  subcortex$beta_high[count] <- (coefs[2,1] + coefs[2,2]*subcortex[count,6])
  subcortex$beta_low[count] <- (coefs[2,1] - coefs[2,2]*subcortex[count,6])
  subcortex$n_missing[count] <- length(unique(model$na.action))
  subcortex$poly_beta[count] <- (coefs[3,1])
  subcortex$poly_high[count] <- (coefs[3,1] + coefs[3,2]*subcortex[count,6])
  subcortex$poly_low[count] <- (coefs[3,1] - coefs[3,2]*subcortex[count,6])
}


## White matter tracts


area <- c('FA_cingulate_gyrus_l', 'FA_cingulate_gyrus_r', 'FA_corticospinal_l', 'FA_corticospinal_r',
          'FA_forceps_major', 'FA_forceps_minor', 'FA_fronto_occipital_l', 'FA_fronto_occipital_r',
          'FA_longitudinal_l', 'FA_longitudinal_r', 'FA_medial_lemn_l', 'FA_medial_lemn_r',
          'FA_middle_cereb_ped', 'FA_parahippo_l', 'FA_parahippo_r', 'FA_posterior_thalamic_l',
          'FA_posterior_thalamic_r', 'FA_superior_long_l', 'FA_superior_long_r', 'FA_superior_thalamic_l',
          'FA_superior_thalamic_r', 'FA_uncinate_l', 'FA_uncinate_r', 'FA',
          'MD_acoustic_l', 'MD_acoustic_r', 'MD_anterior_thalamic_l', 'MD_anterior_thalamic_r',
          'MD_cingulate_gyrus_l', 'MD_cingulate_gyrus_r', 'MD_corticospinal_l', 'MD_corticospinal_r',
          'MD_forceps_major', 'MD_forceps_minor', 'MD_fronto_occipital_l', 'MD_fronto_occipital_r',
          'MD_longitudinal_l', 'MD_longitudinal_r', 'MD_medial_lemn_l', 'MD_medial_lemn_r',
          'MD_middle_cereb_ped', 'MD_parahippo_l', 'MD_parahippo_r', 'MD_posterior_thalamic_l',
          'MD_posterior_thalamic_r', 'MD_superior_long_l', 'MD_superior_long_r', 'MD_superior_thalamic_l',
          'MD_superior_thalamic_r', 'MD_uncinate_l', 'MD_uncinate_r', 'MD')

models <- c() # array with all models
for (a in area){
  model <- lm(data=id_years, id_years[[a]] ~  aa_duran + aa_duran_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
  models[[length(models)+1]] <- model # add model to models array  
}
white_matter <- data.frame(area)

n_models <- nrow(white_matter)
p_base <- 0.05
white_matter$initial_order <- seq(1:n_models)# save initial order


# 1. get p-values
count = 0
white_matter$p <- NA
for(model in models){
  count <- count + 1
  coefs <- data.frame(summary(model)$coefficients)
  white_matter$p[count] <- (coefs[2,4])
}
# 2. determine significance
white_matter <- arrange(white_matter, p) # sort in ascending order
white_matter$p_crit <- p_base*(as.numeric(rownames(white_matter)))/n_models # calculate p-threshold (0.05*(i/n))
white_matter$sig <- FALSE; white_matter$sig[white_matter$p < white_matter$p_crit] <- TRUE # check if p < p threshold
# set all sig. values larger than the smallest index for which p<p_crit. to non-sig (i.e., FALSE)
if (length(as.numeric(rownames(white_matter[white_matter$sig==FALSE,]))) != 0){ # if there are any non-sig values
  first_false <- min(as.numeric(rownames(white_matter[white_matter$sig==FALSE,])))
  white_matter$sig[first_false:nrow(white_matter)] <- FALSE
}
# 3. get FDR-adjusted p-value
white_matter$p_adjusted <- p.adjust(white_matter$p, method='BH')
# 4. for CIs: get Z-value of the quantile for the standard normal distribution for the p
white_matter$z_val <- qnorm(1-white_matter$p_crit/2)

count = 0
white_matter$beta <- NA
white_matter$beta_low <- NA
white_matter$beta_high <- NA
white_matter$n_missing <- NA
white_matter$poly_beta <- NA
white_matter$poly_low <- NA
white_matter$poly_high <- NA
# reorder so that the correct stats from "model" map to the appropriate row
white_matter <- white_matter %>% arrange(initial_order)
white_matter$initial_order <- NULL
for (model in models){
  count <- count + 1
  coefs <- data.frame(summary(model)$coefficients)
  white_matter$beta[count] <- (coefs[2,1])
  white_matter$beta_high[count] <- (coefs[2,1] + coefs[2,2]*white_matter[count,6])
  white_matter$beta_low[count] <- (coefs[2,1] - coefs[2,2]*white_matter[count,6])
  white_matter$n_missing[count] <- length(unique(model$na.action))
  white_matter$poly_beta[count] <- (coefs[3,1])
  white_matter$poly_high[count] <- (coefs[3,1] + coefs[3,2]*white_matter[count,6])
  white_matter$poly_low[count] <- (coefs[3,1] - coefs[3,2]*white_matter[count,6])
}


## Drug classes

model_SYS <- lm(data=id_years,  TBVicv ~ SYS_metabolic + SYS_antiinfective + SYS_immuno_modulating +
                  SYS_cardiovascular + SYS_urinary + SYS_musculo_skeletal + SYS_neuro + SYS_respiratory + SYS_hormonal +
                  aa_duran_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                  activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                  apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
summary(model_SYS)



model_PHARM <- lm(data=id_years, TBVicv ~ PHARM_antidepressant + PHARM_antihistamines + 
                    PHARM_antimigraine + PHARM_antipropulsive + PHARM_antipsychotic +
                    PHARM_anxiolytic + PHARM_penicillin + PHARM_glucose_lowering + 
                    PHARM_corticosteroid + PHARM_acid_reflux +
                    PHARM_diuretic_high + PHARM_sedative + PHARM_immunosuppressant + PHARM_muscle_relaxant +
                    PHARM_decongestant + PHARM_opioid + PHARM_propulsive + 
                    PHARM_antigout + PHARM_antiinflammatory + PHARM_gastrointestinal +
                    PHARM_urological + PHARM_vasodilator + aa_duran_poly + sex + data_provider + ass_age_2 + deprivation + smoking_2 + alc_freq_2 +
                    activity_2 + bmi_2 + comorbidity_csum + diabetes + hyperchol + hypertension + heart_attack + psychosis + anxiety + mood_dis +
                    apoe_carrier + I(ass_age_2^2) + ass_age_2*sex + I(ass_age_2^2)*sex + headposX + headposY + headposZ + ethnicity + ass_ctr_2 + sample_time)
summary(model_PHARM)




## Plots

# the code below (depending on what you comment out) plots either model
SYS_predictors <- data.frame(scale=c('gastrointestinal', 'antiinfective', 'immuno-modulating', 
                                     'cardiovascular', 'urinary', 
                                     'musculo-skeletal' ,'nervous', 'respiratory', 'hormonal'))

PHARM_predictors <- data.frame(scale=c("antidepressant", "antihistamine", 
                                       "antimigraine", "antipropulsive",
                                       "antipsychotic", "anxiolytic", "β-lactam antibiotic", "glucose-lowering",
                                       "corticosteroid", "acid reflux", "high ceiling diuretic",
                                       "sedative", "immunosuppressant", "muscle relaxant", "decongestant", "opioid",
                                       "propulsive", "anti-gout", "anti-inflammatory", "gastrointestinal",
                                       "urological", "vasodilator"))

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