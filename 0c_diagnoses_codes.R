library(tidyverse)

## Import the codes for each disorder
codes <- read.csv('codes.csv', header=TRUE)

# remove potential white space and convert to lower case
codes <- data.frame(sapply(codes, trimws))
codes$code <- as.character(codes$code)
codes$n <- NA

# put several disorders into the "other" category
codes <- filter(codes, disorder != 'af')
codes$disorder[codes$disorder == 'mood disorder'] <- 'mood_dis'
codes$disorder[codes$disorder == 'depression'] <- 'mood_dis'
codes$disorder[codes$disorder == 'myocardial infarction'] <- 'heart_attack'
codes$disorder[codes$disorder == 'stroke unspecified'] <- 'stroke_un'
codes$disorder[codes$disorder == 'tia'] <- 'stroke_i'
codes$disorder[codes$disorder == 'schizophrenia'] <- 'psychosis'
codes$disorder[codes$disorder == 'ns infection'] <- 'ns_infection'
codes$disorder[codes$disorder %in% c('cerebral palsy', 'encepalopathy', 'epilepsy', 'hd', 'intracranial abscess',
                                     'mental retardation', 'mnd', 'ms', 'parkinsonism', 'vad', 'adem', 'other dementia')] <- 'dis_other'

# separate codes into inpatient and primary care
codes_gp <- filter(codes, code_system %in% c('read2', 'read3'))
codes_inpatient <- filter(codes, code_system %in% c('icd9', 'icd10'))



# get hospital records
icd <- read.csv('inpatient_clinical.csv')



## Get the death records
death <- read.csv('death_cause.csv')
colnames(death)[1] <- 'id'

# change empty columns to NAs and remove them
death[death==""]  <- NA 
death <- death[rowSums(is.na(death))!=ncol(death)-1,]

# change class of all columns to character
death <- as.data.frame(sapply(death, as.character))

# change to long format
death <- death %>%  pivot_longer(-id, names_to = "diagnosis", values_drop_na=TRUE); colnames(death) <- c('id', 'column', 'diagnosis');
death$column <- NULL
death <- data.frame(death)


# Hospital records: add a column for each relevant diagnosis in hospital record

icd$anxiety <- 0
icd$diabetes <- 0
icd$dis_other <- 0
icd$heart_attack <- 0
icd$hypercholesterolemia <- 0
icd$hypertension <- 0
icd$mood_dis <- 0
icd$psychosis <- 0
icd$stroke_h <- 0
icd$stroke_i <- 0
icd$stroke_un <- 0
icd$encephalitis <- 0
icd$meningitis <- 0
icd$ns_infection <- 0



diseases <- c('anxiety', 'diabetes', 'dis_other', 'heart_attack', 'hypercholesterolemia', 'hypertension', 'mood_dis', 'psychosis', 'stroke_h', 'stroke_i', 'stroke_un',
              'encephalitis', 'meningitis', 'ns_infection')

col_count <- seq(1,ncol(icd))[colnames(icd)=='anxiety'] # identify the first column to start with search
for (d in diseases){
  print(paste('Searching for ', d, '.', sep = ''))
  # icd9
  for (c in codes_inpatient$code[codes_inpatient$disorder==d & codes_inpatient$code_system=='icd9']){
    icd[icd$diagnosis==c & icd$version==9, col_count] <- 1 # label diagnosis as present
    codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c & icd$version==9])) # count the number of occurrences
  }
  # icd10
  for (c in codes_inpatient$code[codes_inpatient$disorder==d & codes_inpatient$code_system=='icd10']){
    icd[icd$diagnosis==c & icd$version==10, col_count] <- 1 # label diagnosis as present
    codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c & icd$version==10])) # count the number of occurrences
  }
  col_count <- col_count + 1
}

## Add a column for each relevant diagnosis in death record (include only select diagnoses)
codes_inpatient$n_death <- NA

death$anxiety <- 0
death$diabetes <- 0
death$dis_other <- 0
death$heart_attack <- 0
death$hypercholesterolemia <- 0
death$hypertension <- 0
death$mood_dis <- 0
death$psychosis <- 0
death$stroke_h <- 0
death$stroke_i <- 0
death$stroke_un <- 0
death$encephalitis <- 0
death$meningitis <- 0
death$ns_infection <- 0

col_count <- seq(1,ncol(death))[colnames(death)=='anxiety'] # identify the first column to start with search
for (d in diseases){
  print(paste('Searching for ', d, '.', sep = ''))
  for (c in codes_inpatient$code[codes_inpatient$disorder==d & codes_inpatient$code_system=='icd10']){
    death[death$diagnosis==c, col_count] <- 1 # label diagnosis as present
    codes_inpatient$n_death[codes_inpatient$code==c] <- length(unique(death$id[death$diagnosis==c])) # count the number of occurrences
  }
  col_count <- col_count + 1
}

# export
write.csv(icd, 'diagnoses_inpatient.csv', row.names = FALSE)
write.csv(death, 'diagnoses_death.csv', row.names = FALSE)







### Add primary care diagnoses ###

## Import and prepare
meds_diagnoses <- read.csv('gp_clinical.txt', sep="\t", header=TRUE, quote="")

# rename columns
colnames(meds_diagnoses)[which(names(meds_diagnoses) == "eid")] <- "id"
colnames(meds_diagnoses)[which(names(meds_diagnoses) == "event_dt")] <- "date"
colnames(meds_diagnoses)[which(names(meds_diagnoses) == "read_3")] <- "diagnosis"
meds_diagnoses$read_2 <- NULL
meds_diagnoses$value1 <- NULL
meds_diagnoses$value2 <- NULL
meds_diagnoses$value3 <- NULL

# remove rows without dates or with invalid dates
meds_diagnoses <- filter(meds_diagnoses, date != '')
meds_diagnoses$date[meds_diagnoses$date=="01/01/1901" | meds_diagnoses$date=="02/02/1902" | meds_diagnoses$date=="03/03/1903" | meds_diagnoses$date=="07/07/2037"] <- NA
meds_diagnoses <- filter(meds_diagnoses, !is.na(id) & !is.na(date))

# change class of all columns to character
meds_diagnoses <- as.data.frame(sapply(meds_diagnoses, as.character))




# Primary cares: add a column for each relevant diagnosis in hospital record

meds_diagnoses$anxiety <- 0
meds_diagnoses$diabetes <- 0
meds_diagnoses$dis_other <- 0
meds_diagnoses$heart_attack <- 0
meds_diagnoses$hypercholesterolemia <- 0
meds_diagnoses$hypertension <- 0
meds_diagnoses$mood_dis <- 0
meds_diagnoses$psychosis <- 0
meds_diagnoses$stroke_h <- 0
meds_diagnoses$stroke_i <- 0
meds_diagnoses$stroke_un <- 0
meds_diagnoses$encephalitis <- 0
meds_diagnoses$meningitis <- 0
meds_diagnoses$ns_infection <- 0

col_count <- seq(1,ncol(meds_diagnoses))[colnames(meds_diagnoses)=='anxiety'] # identify the first column to start with search
for (d in diseases){
  print(paste('Searching for ', d, '.', sep = ''))
  # meds_diagnoses
  for (c in codes_gp$code[codes_gp$disorder==d]){
    meds_diagnoses[meds_diagnoses$diagnosis==c, col_count] <- 1 # label diagnosis as present
    codes_gp$n[codes_gp$code==c] <- length(unique(meds_diagnoses$id[meds_diagnoses$diagnosis==c])) # count the number of occurrences
  }
  col_count <- col_count + 1
}

write.csv(meds_diagnoses, 'diagnoses_GP.csv', row.names = FALSE)



# combine counts of inpatient- and GP- codes and export
codes_gp$n_death <- NA
codes_new <- rbind(codes_gp, codes_inpatient)
codes_new <- filter(codes_new, n!=0 | n_death!=0)
write.csv(codes_new, 'diagnoses_counts.csv', row.names = FALSE)












### Combine inpatient and GP files ###
gp <- read.csv('diagnoses_GP.csv')
inpatient <- read.csv('diagnoses_inpatient.csv') #load ICD-diagnoses
death <- read.csv('diagnoses_death.csv')
death_date <- read.csv('death.csv')

# harmonise columns and bind
gp$version <- 'read'
gp <- subset(gp, select = c(id, data_provider, version, date, diagnosis, anxiety, diabetes, dis_other, heart_attack, hypercholesterolemia,
                            hypertension, mood_dis, psychosis, stroke_h, stroke_i, stroke_un, encephalitis, meningitis, ns_infection))
inpatient$data_provider <- NA
inpatient <- subset(inpatient, select = c(id, data_provider, version, date, diagnosis, anxiety, diabetes, dis_other, heart_attack,
                                          hypercholesterolemia, hypertension, mood_dis, psychosis, stroke_h, stroke_i, stroke_un,
                                          encephalitis, meningitis, ns_infection))
colnames(death_date) <- c('id', 'date', 'del')
death_date$del <- NULL
death <- merge(death, death_date, by='id', all.x = TRUE)
death$version <- 10
death$data_provider <- NA
death <- subset(death, select = c(id, data_provider, version, date, diagnosis, anxiety, diabetes, dis_other, heart_attack,
                                  hypercholesterolemia, hypertension, mood_dis, psychosis, stroke_h, stroke_i, stroke_un,
                                  encephalitis, meningitis, ns_infection))

# merge and remove duplicates
gp$date <- as.Date(gp$date, "%d/%m/%Y")
inpatient$date <- as.Date(inpatient$date, "%Y-%m-%d")
death$date <- as.Date(death$date, "%Y-%m-%d")
diagnoses <- rbind(gp, inpatient, death)
diagnoses <- arrange(diagnoses, date)
diagnoses <- distinct(diagnoses) # remove "perfect" duplicates (i.e., where every column is identical)
diagnoses[diagnoses == ''] <- NA # blank spaces to NAs
diagnoses <- filter(diagnoses, !is.na(diagnosis)) # remove rows without diagnostic code

# export (diagnosis-based df that has only had "perfect" duplicates (every column identical) removed)
write.csv(diagnoses, 'diagnoses_ALL.csv', row.names = FALSE)