###
#
#
# Cleans the data to prepare them for transformations.
#
#
###

library(zoo)
library(tidyverse)
library(car)
library(reshape2)

setwd('D:/PhD/MAIN/')



## read in the files


## add drug classes

setwd('D:/PhD/MAIN')

meds <- read.csv('UK Biobank/Processed files/tables/6_demographics_v2.csv', sep="|", header=TRUE)
class <- read.csv('anticholinergic burden scales/drug_groups.csv')

# remove rows with no anticholinergic drugs (so everything below runs faster)
meds <- filter(meds, aa_duran > 0)

## miscelaneous formatting etc.
meds$id <- as.character(meds$id)
meds$data_provider <- as.factor(meds$data_provider)
meds$date <- as.Date(meds$date,"%Y-%m-%d")

# convert everything to lower-case character and trim leading/trailing white spaces
class <- as.data.frame(lapply(class, as.character))
class <- as.data.frame(lapply(class, tolower))
class <- as.data.frame(lapply(class, trimws))
colnames(class)[which(names(class) == "drug")] <- "aa_name"
class <- subset(class, select=-c(several_categories, in_sample))
class[class==''] <- 'unknown'
# remove drugs with unknown category
#class <- filter(class, category != 'unknown')

# merge and subset
meds <- merge(meds, class, by='aa_name', all.x = TRUE)
meds <- subset(meds, select=c(id, aa_name, aa_duran, data_provider, date, body_system, lower))

# spaces to NA's
meds[meds==''] <- NA

# check frequencies of different levels of drug categories
prop.table(table(meds$body_system))

# keep only the common drugs
#drug_counts <- meds %>% group_by(chemical) %>% summarise(n=length(id))
#drug_counts <- filter(drug_counts, n >= 1000)
#meds <- merge(meds, drug_counts, by='chemical', all.y = TRUE)


# transform to wide format by category
meds_wide <- dcast(meds, id + aa_name + aa_duran + data_provider + date ~ body_system, length)
meds_wide_lower <- dcast(meds, id + aa_name + aa_duran + data_provider + date ~ lower, length)

# calculate aa-burden of category
meds_wide[,6:length(meds_wide)] <- meds_wide[,6:length(meds_wide)]*meds_wide$aa_duran
meds_wide_lower[,6:length(meds_wide_lower)] <- meds_wide_lower[,6:length(meds_wide_lower)]*meds_wide_lower$aa_duran

write.csv(meds_wide, 'meds_v2_body_system.csv', row.names = FALSE)
write.csv(meds_wide_lower, 'meds_v2_class_lower.csv', row.names = FALSE)

