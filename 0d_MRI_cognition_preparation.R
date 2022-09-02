library(tidyverse)
library(lavaan)
library(semPlot)
library(MPsychoR)
library(corrplot)
library(fdrci)

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



# import all diagnoses
icd <- read.csv('diagnoses_ALL.csv')

# import imaging data
mri_data <- read.csv('mri_imaging_raw.csv', row.names = 1)

icd$stroke <- icd$stroke_h + icd$stroke_i + icd$stroke_un
icd$stroke[icd$stroke > 0] <- 1
icd$date <- icd$date <- as.Date(icd$date, '%Y-%m-%d')

## remove duplicate rows (keeping oldest) of any participant for any disorder
# bind the non-diagnoses of the disorder with the duplicate-cleaned diagnoses
dis_other <- distinct(filter(icd, dis_other == 1), id, dis_other, .keep_all = TRUE)
stroke <- distinct(filter(icd, stroke == 1), id, stroke, .keep_all = TRUE)
encephalitis <- distinct(filter(icd, encephalitis == 1), id, encephalitis, .keep_all = TRUE)
meningitis <- distinct(filter(icd, meningitis == 1), id, meningitis, .keep_all = TRUE)
ns_infection <- distinct(filter(icd, ns_infection == 1), id, ns_infection, .keep_all = TRUE)
# retain only columns relevant
dis_other <- subset(dis_other, select = c(id, date, dis_other)); colnames(dis_other)[colnames(dis_other) == 'date'] <- 'date_dis_other'
stroke <- subset(stroke, select = c(id, date, stroke)); colnames(stroke)[colnames(stroke) == 'date'] <- 'date_stroke'
encephalitis <- subset(encephalitis, select = c(id, date, encephalitis)); colnames(encephalitis)[colnames(encephalitis) == 'date'] <- 'date_encephalitis'
meningitis <- subset(meningitis, select = c(id, date, meningitis)); colnames(meningitis)[colnames(meningitis) == 'date'] <- 'date_meningitis'
ns_infection <- subset(ns_infection, select = c(id, date, ns_infection)); colnames(ns_infection)[colnames(ns_infection) == 'date'] <- 'date_ns_infection'

# keep only those cases of stroke, encephalitis, meningitis, and ns infection diagnosed after the assessment date
dates <- read.csv('ass_date.csv')
colnames(dates) <- c('id', 'date_0', 'date_1', 'date_2', 'date_3'); dates <- subset(dates, select=c(id, date_0, date_2))
dates$date_0 <- as.Date(dates$date_0, '%Y-%m-%d')
dates$date_2 <- as.Date(dates$date_2, '%Y-%m-%d')
diags <- merge(dates, stroke, by='id', all.x = TRUE)
diags <- merge(diags, encephalitis, by='id', all.x = TRUE)
diags <- merge(diags, meningitis, by='id', all.x = TRUE)
diags <- merge(diags, ns_infection, by='id', all.x = TRUE)
diags <- merge(diags, dis_other, by='id', all.x = TRUE)
# set diagnoses after date of assessment to 0
diags$stroke[(!is.na(diags$date_2) & diags$date_stroke > diags$date_2) | (is.na(diags$date_2) & diags$date_stroke > diags$date_0)] <- 0
diags$encephalitis[(!is.na(diags$date_2) & diags$date_encephalitis > diags$date_2) | (is.na(diags$date_2) & diags$date_encephalitis > diags$date_0)] <- 0
diags$meningitis[(!is.na(diags$date_2) & diags$date_meningitis > diags$date_2) | (is.na(diags$date_2) & diags$date_meningitis > diags$date_0)] <- 0
diags$ns_infection[(!is.na(diags$date_2) & diags$date_ns_infection > diags$date_2) | (is.na(diags$date_2) & diags$date_ns_infection > diags$date_0)] <- 0
# create data frames with dementia diagnoses based on medication
dem_meds <- read.csv('dementia_meds.csv', sep='|') # import data on dementia medication (donepezil, galantamine, memantine, rivastigmine) to make sure to exclude all with dementia diagnosis
dem_meds$date <- as.Date(dem_meds$date, '%Y-%m-%d')
dem_meds <- dem_meds %>% arrange(date) %>% distinct(id, .keep_all=TRUE) # identify earliest date of dementia medication
dem_meds <- subset(dem_meds, select=c(id, date))
dem_meds$date <- 1
colnames(dem_meds) <- c('id', 'dem_any')

# remove data points with diagnoses before assessment date (stroke, etc.) or at any point (for "dis_other" and dementia meds)
colnames(mri_data)[colnames(mri_data)=='eid'] <- 'id'
mri_data <- filter(mri_data, ! id %in% filter(diags, dis_other == 1 | stroke == 1 | encephalitis == 1 | meningitis == 1 | ns_infection == 1)$id) # removed 31,143 participants
mri_data <- filter(mri_data, ! id %in% dem_meds$id)

# remove based on self-reports
diagnoses_self <- read.csv('diagnoses_self.csv')
diagnoses_self[] <- lapply(diagnoses_self, as.character)
diagnoses_self[is.na(diagnoses_self)] <- '9999999' # remove NAs because they cause trouble
colnames(diagnoses_self)[which(colnames(diagnoses_self) == 'eid')] <- 'id'

# for some disorders, remove participant for any report at any time
a <- diagnoses_self[apply(diagnoses_self %>% select(starts_with("X20002")), 1, function(x) any(x == "1263")), 'id'] # dementia
b <- diagnoses_self[apply(diagnoses_self %>% select(starts_with("X20002")), 1, function(x) any(x == "1262")), 'id'] # parkinsons
c <- diagnoses_self[apply(diagnoses_self %>% select(starts_with("X20002")), 1, function(x) any(x == "1258")), 'id'] # other chronic/ neurodegenerative
d <- diagnoses_self[apply(diagnoses_self %>% select(starts_with("X20002")), 1, function(x) any(x == "1256")), 'id'] # Guillain-Barr
e <- diagnoses_self[apply(diagnoses_self %>% select(starts_with("X20002")), 1, function(x) any(x == "1261")), 'id'] # mutliple sclerosis
f <- diagnoses_self[apply(diagnoses_self %>% select(starts_with("X20002")), 1, function(x) any(x == "1397")), 'id'] # other demyelinating
g <- diagnoses_self[apply(diagnoses_self %>% select(starts_with("X20002")), 1, function(x) any(x == "1245")), 'id'] # brain/intracranial abcess
h <- diagnoses_self[apply(diagnoses_self %>% select(starts_with("X20002")), 1, function(x) any(x == "1433")), 'id'] # cerebral palsy
i <- diagnoses_self[apply(diagnoses_self %>% select(starts_with("X20002")), 1, function(x) any(x == "1264")), 'id'] # epilepsy
j <- diagnoses_self[apply(diagnoses_self %>% select(starts_with("X20002")), 1, function(x) any(x == "1659")), 'id'] # meningioma (benign)
k <- diagnoses_self[apply(diagnoses_self %>% select(starts_with("X20002")), 1, function(x) any(x == "1259")), 'id'] # motor neurone disease 
l <- diagnoses_self[apply(diagnoses_self %>% select(starts_with("X20002")), 1, function(x) any(x == "1524")), 'id'] # spina bifida
m <- diagnoses_self[apply(diagnoses_self %>% select(starts_with("X20001")), 1, function(x) any(x == "1032")), 'id'] # brain cancer
n <- diagnoses_self[apply(diagnoses_self %>% select(starts_with("X20001")), 1, function(x) any(x == "1031")), 'id'] # meneingeal cancer
allExclusions <- c(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
uniqueExclusions = unique(allExclusions)
mri_data <- filter(mri_data, ! id %in% uniqueExclusions)

# get id's of people with self-reported conditions during first assessment and exclude them from the sample
o1 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.0.")), 1, function(x) any(x == "1081"))), 'id'] # stroke
p1 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.0.")), 1, function(x) any(x == "1491"))), 'id'] # brain haemorrhage
q1 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.0.")), 1, function(x) any(x == "1425"))), 'id'] # cerebral aneurysm
r1 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.0.")), 1, function(x) any(x == "1246"))), 'id'] # encephalitis
s1 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.0.")), 1, function(x) any(x == "1266"))), 'id'] # head injury
t1 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.0.")), 1, function(x) any(x == "1244"))), 'id'] # infection of nervous system
u1 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.0.")), 1, function(x) any(x == "1583"))), 'id'] # ischaemic stroke
v1 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.0.")), 1, function(x) any(x == "1247"))), 'id'] # meningitis 
w1 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.0.")), 1, function(x) any(x == "1240"))), 'id'] # neurological injury/trauma
x1 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.0.")), 1, function(x) any(x == "1083"))), 'id'] # subdural haematoma
y1 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.0.")), 1, function(x) any(x == "1086"))), 'id'] # subarachnoid haemorrhage
z1 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.0.")), 1, function(x) any(x == "1082"))), 'id'] # transient ischaemic attack
allExclusions1 <- c(o1,p1,q1,r1,s1,t1,u1,v1,w1,x1,y1,z1)
uniqueExclusions1 = unique(allExclusions1)
mri_data <- filter(mri_data, ! id %in% uniqueExclusions1)

# get id's of people with self-reported conditions during imaging assessment and exclude them (you can exclude the entire participant, since we would be using only their imaging-assessment data anyway)
o2 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.2.")), 1, function(x) any(x == "1081"))), 'id'] # stroke
p2 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.2.")), 1, function(x) any(x == "1491"))), 'id'] # brain haemorrhage
q2 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.2.")), 1, function(x) any(x == "1425"))), 'id'] # cerebral aneurysm
r2 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.2.")), 1, function(x) any(x == "1246"))), 'id'] # encephalitis
s2 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.2.")), 1, function(x) any(x == "1266"))), 'id'] # head injury
t2 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.2.")), 1, function(x) any(x == "1244"))), 'id'] # infection of nervous system
u2 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.2.")), 1, function(x) any(x == "1583"))), 'id'] # ischaemic stroke
v2 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.2.")), 1, function(x) any(x == "1247"))), 'id'] # meningitis 
w2 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.2.")), 1, function(x) any(x == "1240"))), 'id'] # neurological injury/trauma
x2 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.2.")), 1, function(x) any(x == "1083"))), 'id'] # subdural haematoma
y2 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.2.")), 1, function(x) any(x == "1086"))), 'id'] # subarachnoid haemorrhage
z2 <- diagnoses_self[which(apply(diagnoses_self %>% select(starts_with("X20002.2.")), 1, function(x) any(x == "1082"))), 'id'] # transient ischaemic attack
allExclusions2 <- c(o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2)
uniqueExclusions2 = unique(allExclusions2)
mri_data <- filter(mri_data, ! id %in% uniqueExclusions2)

# export
saveRDS(mri_data, file = "mri_imaging.rds")


# remove rows with all NAs
mri_data$REMOVE <- apply(mri_data[,2:ncol(mri_data)], 1, function(x){all(is.na(x))})
mri_data <- filter(mri_data, REMOVE==FALSE); mri_data$REMOVE <- NULL

cortical <- read.csv('cortical.csv')
colnames(cortical)[1] <- 'id'
cortical$REMOVE <- apply(cortical[,2:ncol(cortical)], 1, function(x){all(is.na(x))})
cortical <- filter(cortical, REMOVE==FALSE); cortical$REMOVE <- NULL

subcortical <- read.csv('subcortical.csv')
subcortical$empty <- rowSums(is.na(subcortical))
subcortical <- filter(subcortical, empty!=ncol(subcortical)-2); subcortical$empty <- NULL
subcortical <- data.frame(subcortical[,c(1, seq(2,ncol(subcortical),2))]) # retain imaging visit
colnames(subcortical) <- c('id', 'thalamus_l', 'thalamus_r', 'caudate_l', 'caudate_r',
                           'putamen_l', 'putamen_r', 'pallidum_l', 'pallidum_r',
                           'hippocampus_l', 'hippocampus_r', 'amygdala_l', 'amygdala_r',
                           'accumbens_l', 'accumbens_r')
subcortical$REMOVE <- apply(subcortical[,2:ncol(subcortical)], 1, function(x){all(is.na(x))})
subcortical <- filter(subcortical, REMOVE==FALSE); subcortical$REMOVE <- NULL

FA <- read.csv('FA.csv'); FA$empty <- rowSums(is.na(FA))
FA <- filter(FA, empty!=ncol(FA)-2); FA$empty <- NULL
FA <- data.frame(FA[,c(1, seq(2,ncol(FA),2))]) # retain imaging visit
colnames(FA) <- c('id', 'FA_acoustic_l', 'FA_acoustic_r', 'FA_anterior_thalamic_l', 'FA_anterior_thalamic_r',
                  'FA_cingulate_gyrus_l', 'FA_cingulate_gyrus_r', 'FA_corticospinal_l', 'FA_corticospinal_r',
                  'FA_forceps_major', 'FA_forceps_minor', 'FA_fronto_occipital_l', 'FA_fronto_occipital_r',
                  'FA_longitudinal_l', 'FA_longitudinal_r', 'FA_medial_lemn_l', 'FA_medial_lemn_r',
                  'FA_middle_cereb_ped', 'FA_parahippo_l', 'FA_parahippo_r', 'FA_posterior_thalamic_l',
                  'FA_posterior_thalamic_r', 'FA_superior_long_l', 'FA_superior_long_r', 'FA_superior_thalamic_l',
                  'FA_superior_thalamic_r', 'FA_uncinate_l', 'FA_uncinate_r')
FA$REMOVE <- apply(FA[,2:ncol(FA)], 1, function(x){all(is.na(x))})
FA <- filter(FA, REMOVE==FALSE); FA$REMOVE <- NULL

MD <- read.csv('MD.csv'); MD$empty <- rowSums(is.na(MD))
MD <- filter(MD, empty!=ncol(MD)-2); MD$empty <- NULL
MD <- data.frame(MD[,c(1, seq(2,ncol(MD),2))]) # retain imaging visit
colnames(MD) <- c('id', 'MD_acoustic_l', 'MD_acoustic_r', 'MD_anterior_thalamic_l', 'MD_anterior_thalamic_r',
                  'MD_cingulate_gyrus_l', 'MD_cingulate_gyrus_r', 'MD_corticospinal_l', 'MD_corticospinal_r',
                  'MD_forceps_major', 'MD_forceps_minor', 'MD_fronto_occipital_l', 'MD_fronto_occipital_r',
                  'MD_longitudinal_l', 'MD_longitudinal_r', 'MD_medial_lemn_l', 'MD_medial_lemn_r',
                  'MD_middle_cereb_ped', 'MD_parahippo_l', 'MD_parahippo_r', 'MD_posterior_thalamic_l',
                  'MD_posterior_thalamic_r', 'MD_superior_long_l', 'MD_superior_long_r', 'MD_superior_thalamic_l',
                  'MD_superior_thalamic_r', 'MD_uncinate_l', 'MD_uncinate_r')
MD$REMOVE <- apply(MD[,2:ncol(MD)], 1, function(x){all(is.na(x))})
MD <- filter(MD, REMOVE==FALSE); MD$REMOVE <- NULL


# merge
mri_data <- merge(mri_data, chol_white, by='id', all.x=TRUE)
mri_data <- merge(mri_data, cortical, by = 'id', all.x = TRUE)
mri_data <- merge(mri_data, subcortical, by = 'id', all.x = TRUE)
mri_data <- merge(mri_data, FA, by = 'id', all.x = TRUE)
mri_data <- merge(mri_data, MD, by = 'id', all.x = TRUE)

# Remove outliers
# all to numeric
mri_data[, -c(1, which(colnames(mri_data)=='T2_flair'))] <- lapply(mri_data[, -c(1, which(colnames(mri_data)=='T2_flair'))], as.numeric)
mri_data[, -c(1, which(colnames(mri_data)=='T2_flair'))] <- lapply(mri_data[, -c(1, which(colnames(mri_data)=='T2_flair'))], outliers, SD=4, variance='SD')



## FA & MD

# temporary dataset with variables included in PCA for FA
tmp_fa <- data.frame(subset(mri_data, select=c(FA_acoustic_l, FA_acoustic_r, FA_anterior_thalamic_l, FA_anterior_thalamic_r,
                                               FA_cingulate_gyrus_l, FA_cingulate_gyrus_r, FA_corticospinal_l, FA_corticospinal_r,
                                               FA_forceps_major, FA_forceps_minor, FA_fronto_occipital_l, FA_fronto_occipital_r,
                                               FA_longitudinal_l, FA_longitudinal_r, FA_medial_lemn_l, FA_medial_lemn_r,
                                               FA_middle_cereb_ped, FA_parahippo_l, FA_parahippo_r, FA_posterior_thalamic_l,
                                               FA_posterior_thalamic_r, FA_superior_long_l, FA_superior_long_r, FA_superior_thalamic_l,
                                               FA_superior_thalamic_r, FA_uncinate_l, FA_uncinate_r)))
# PCA for fractional anisotropy
PC_fa <- psych::principal(tmp_fa, nfactors=3, rotate="none",scores=T, covar=FALSE, missing=F)
PC_fa$loadings
fa <- data.frame(matrix(as.numeric(PC_fa$loadings), attributes(PC_fa$loadings)$dim, dimnames=attributes(PC_fa$loadings)$dimnames))
mri_data$FA <- PC_fa$scores[, 1]

# temporary dataset with variables included in PCA for MD
tmp_md <- data.frame(subset(mri_data, select=c(MD_acoustic_l, MD_acoustic_r, MD_anterior_thalamic_l, MD_anterior_thalamic_r,
                                               MD_cingulate_gyrus_l, MD_cingulate_gyrus_r, MD_corticospinal_l, MD_corticospinal_r,
                                               MD_forceps_major, MD_forceps_minor, MD_fronto_occipital_l, MD_fronto_occipital_r,
                                               MD_longitudinal_l, MD_longitudinal_r, MD_medial_lemn_l, MD_medial_lemn_r,
                                               MD_middle_cereb_ped, MD_parahippo_l, MD_parahippo_r, MD_posterior_thalamic_l,
                                               MD_posterior_thalamic_r, MD_superior_long_l, MD_superior_long_r, MD_superior_thalamic_l,
                                               MD_superior_thalamic_r, MD_uncinate_l, MD_uncinate_r)))
# PCA for mean diffusivity
PC_md <- psych::principal(tmp_md, nfactors=3, rotate="none",scores=T, covar=FALSE, missing=F)
PC_md$loadings
md <- data.frame(matrix(as.numeric(PC_md$loadings), attributes(PC_md$loadings)$dim, dimnames=attributes(PC_md$loadings)$dimnames))
mri_data$MD <- PC_md$scores[, 1]


# the cortical and subcortical measures have to be corrected for head size
scal_fac <- read.csv('scal_fac.csv')
colnames(scal_fac) <- c('id', 'scal_fac_2', 'scal_fac_3')
mri_data <- merge(mri_data, scal_fac, by='id', all.x = TRUE)

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
          "rh_temporalpole_volume","rh_transversetemporal_volume","rh_insula_volume", 'thalamus_l', 'thalamus_r', 'caudate_l', 'caudate_r',
          'putamen_l', 'putamen_r', 'pallidum_l', 'pallidum_r',
          'hippocampus_l', 'hippocampus_r', 'amygdala_l', 'amygdala_r',
          'accumbens_l', 'accumbens_r')
for (a in area){
  mri_data[[a]] <- mri_data[[a]]*mri_data$scal_fac_2
}

saveRDS(mri_data, file = "mri_imaging_clean.rds")



## Import and organise the cognitive data
VNR <- read.csv('VNR_centre.csv', header = TRUE, sep = ',')
colnames(VNR) <- c('id', 'VNR_0', 'VNR_1', 'VNR_2', 'VNR_3')
RT <- read.csv('RT_centre.csv', header = TRUE, sep = ',')
colnames(RT) <- c('id', 'RT_0', 'RT_1', 'RT_2', 'RT_3')
VisMem <- read.csv('pair_match_centre.csv', header = TRUE, sep = ',' )
VisMem <- subset(VisMem, select=-c(X399.0.3, X399.1.3, X399.2.3, X399.3.3)) # remove pilot rounds
colnames(VisMem) <- c('id', 'VisMem_0_1', 'VisMem_0_2', 'VisMem_1_1', 'VisMem_1_2', 'VisMem_2_1', 'VisMem_2_2', 'VisMem_3_1', 'VisMem_3_2')
VisMem <- subset(VisMem, select=c(id, VisMem_0_2, VisMem_1_2, VisMem_2_2, VisMem_3_2)) # use only second round
colnames(VisMem) <- c('id', 'VisMem_0', 'VisMem_1', 'VisMem_2', 'VisMem_3')
ProsMem <- read.csv('pros_mem_centre.csv', header = TRUE, sep = ',' )
colnames(ProsMem) <- c('id', 'ProsMem_0', 'ProsMem_1', 'ProsMem_2', 'ProsMem_3')
DSS <- read.csv('sym_dig_centre.csv', header = TRUE, sep = ',')
colnames(DSS) <- c('id', 'DSS_2', 'DSS_3')
MR <- read.csv('mat_pat_centre.csv', header = TRUE, sep = ',')
colnames(MR) <- c('id', 'MR_2', 'MR_3')
TMTb <- read.csv('trail_making_centre.csv', header = TRUE, sep = ',')
colnames(TMTb) <- c('id', 'TMTb_num_2', 'TMTb_num_3', 'TMTb_alphanum_2', 'TMTb_alphanum_3')
TMTb <- subset(TMTb, select=c(id, TMTb_alphanum_2, TMTb_alphanum_3)) # use only the alphanumeric trail
colnames(TMTb) <- c('id', 'TMTb_2', 'TMTb_3')
NM <- read.csv('num_mem_centre.csv')
colnames(NM) <- c('id', 'NM_0', 'NM_1', 'NM_2', 'NM_3')
NM <- subset(NM, select=-c(NM_1)) # remove "NM_1", as numerical memory was not tested then, and the columns has all NAs
TR <- read.csv('tower_arrange.csv')
colnames(TR) <- c('id', 'TR_2', 'TR_3')

# merge cognitive tests
cog_centre <- merge(VNR, RT, by = 'id', all = TRUE)
cog_centre <- merge(cog_centre, VisMem, by = 'id', all = TRUE)
cog_centre <- merge(cog_centre, ProsMem, by = 'id', all = TRUE)
cog_centre <- merge(cog_centre, DSS, by = 'id', all = TRUE)
cog_centre <- merge(cog_centre, MR, by = 'id', all = TRUE)
cog_centre <- merge(cog_centre, TMTb, by = 'id', all = TRUE)
cog_centre <- merge(cog_centre, NM, by = 'id', all = TRUE)
cog_centre <- merge(cog_centre, TR, by = 'id', all = TRUE)

# also clean cognition data frame
cog_centre <- filter(cog_centre, ! id %in% filter(diags, dis_other == 1 | stroke == 1 | encephalitis == 1 | meningitis == 1 | ns_infection == 1)$id) # removed 31,143 participants
cog_centre <- filter(cog_centre, ! id %in% dem_meds$id)
cog_centre <- filter(cog_centre, ! id %in% uniqueExclusions)
cog_centre <- filter(cog_centre, ! id %in% uniqueExclusions1)
cog_centre <- filter(cog_centre, ! id %in% uniqueExclusions2)

## Calculate age when taking test
# import dates of taking tests
dates <- read.csv('ass_date.csv')
colnames(dates) <- c('id', 'ass_date_0', 'ass_date_1', 'ass_date_2', 'ass_date_3')
dates$ass_date_0 <- as.Date(dates$ass_date_0, format='%Y-%m-%d')
dates$ass_date_1 <- as.Date(dates$ass_date_1, format='%Y-%m-%d')
dates$ass_date_2 <- as.Date(dates$ass_date_2, format='%Y-%m-%d')
dates$ass_date_3 <- as.Date(dates$ass_date_3, format='%Y-%m-%d')
cog_centre <- merge(cog_centre, dates)
# import dates of birth
age_sex <- read.csv('age_sex_formatted.csv', sep='|')
age_sex <- subset(age_sex, select=c(id, sex, birth_date))
age_sex$birth_date <- as.Date(age_sex$birth_date, format="%Y-%m-%d")
cog_centre <- merge(cog_centre, age_sex, all.x = TRUE)
# calculate age
cog_centre$age_0 <- as.numeric(difftime(cog_centre$ass_date_0, cog_centre$birth_date, units='days')/365.25)
cog_centre$age_1 <- as.numeric(difftime(cog_centre$ass_date_1, cog_centre$birth_date, units='days')/365.25)
cog_centre$age_2 <- as.numeric(difftime(cog_centre$ass_date_2, cog_centre$birth_date, units='days')/365.25)
cog_centre$age_3 <- as.numeric(difftime(cog_centre$ass_date_3, cog_centre$birth_date, units='days')/365.25)
# blank spaces to NAs
cog_centre[cog_centre == ''] <- NA
# create year columns
cog_centre$ass_year_0 <- as.numeric(format(as.Date(cog_centre$ass_date_0), "%Y"))
cog_centre$ass_year_1 <- as.numeric(format(as.Date(cog_centre$ass_date_1), "%Y"))
cog_centre$ass_year_2 <- as.numeric(format(as.Date(cog_centre$ass_date_2), "%Y"))
cog_centre$ass_year_3 <- as.numeric(format(as.Date(cog_centre$ass_date_3), "%Y"))


# remove outliers 
cog_centre[, c("VNR_0", "VNR_1", "VNR_2", "VNR_3", "RT_0", "RT_1", "RT_2", "RT_3", "VisMem_0", "VisMem_1", "VisMem_2", "VisMem_3", 
               "ProsMem_0", "ProsMem_1", "ProsMem_2", "ProsMem_3", "DSS_2", "DSS_3", "MR_2", "MR_3", 
               "TMTb_2", "TMTb_3", "NM_0", "NM_2", "NM_3", "TR_2", "TR_3")] <- 
  lapply(cog_centre[, c("VNR_0", "VNR_1", "VNR_2", "VNR_3", "RT_0", "RT_1", "RT_2", "RT_3", "VisMem_0", "VisMem_1", "VisMem_2", "VisMem_3", 
                        "ProsMem_0", "ProsMem_1", "ProsMem_2", "ProsMem_3", "DSS_2", "DSS_3", "MR_2", "MR_3", 
                        "TMTb_2", "TMTb_3", "NM_0", "NM_2", "NM_3", "TR_2", "TR_3")], outliers, SD=4, variance='SD')




## Calculate the latent G

# g for first visit for those who were not imaged during 2nd visit
cog_centre$RT_0 = log(cog_centre$RT_0)
cog_centre$VisMem_0 = log(cog_centre$VisMem_0+1)
cog_centre$ProsMem_0[which(cog_centre$ProsMem_0 != 1)] = 0
cog_centre$ProsMem_0[which(cog_centre$ProsMem_0 == 1)] = 1

model_0 <- '
            # Structural relation
            g =~ VNR_0 + RT_0 + VisMem_0 + ProsMem_0 + NM_0
            
'

fit_0 <- sem(model_0, data=cog_centre, missing="fiml.x")
cog_centre$g_0 <- as.vector(predict(fit_0, cog_centre)) # extract the g-values


# g for second visit for those who were there for the 2nd visit

# perform the same transformations as for the 1st visit on the imaging visit data
cog_centre$RT_2 = log(cog_centre$RT_2)
cog_centre$VisMem_2 = log(cog_centre$VisMem_2+1)
cog_centre$ProsMem_2[which(cog_centre$ProsMem_2 != 1)] = 0
cog_centre$ProsMem_2[which(cog_centre$ProsMem_2 == 1)] = 1

# remove those that didn't complete trails B
cog_centre$TMTb_2[cog_centre$TMTb_2==0] <- NA   
cog_centre$TMTb_2 = log(cog_centre$TMTb_2+10)

# SEM
model_2<- '
      G =~ MR_2 + DSS_2 + VNR_2 + TMTb_2 + RT_2 + VisMem_2 + ProsMem_2 + NM_2 + TR_2
      MR_2 ~~ VNR_2               # include residual correlations here - these two tests are more similar than they are to other tests in the battery, also below.
      RT_2 ~~ DSS_2
      '
fit_2 <- sem(model_2, data=cog_centre, missing="fiml.x")
cog_centre$g_2 <- as.vector(predict(fit_2, cog_centre)) # extract the g-values

# export
saveRDS(cog_centre, file = "cognition_assessment_centre.rds")