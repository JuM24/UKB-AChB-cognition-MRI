# UKB-AChB-cognition-MRI
Associations between anticholinergic burden (AChB), general cognitive ability, and brain structural MRI in UK Biobank.

Rough description for each code file (number indicates prefix):

0.	Code for supplementary files used in other steps:
a.	Creation of a file indicating when each participant was first registered in the sample and when they died.
b.	Harmonisation of drug names across all scales and inclusion in a single data frame.
c.	Import of a previously created table of diagnosis codes to find the relevant diagnoses in the sample.
d.	Preparation of the MRI data by removing individuals with the history of certain brain-affecting disorders and scaling the brain measures; calculation of the latent g from the cognitive tests.
1.	Change of names of drugs from brand names to generic and removal of rows of participants that have opted out of the study, rows without content, and rows without dates.
2.	Identification of anticholinergic drugs as such and addition of anticholinergic values to each prescription (i.e., to each row).
3.	Identification of the dosage for each drug and division of combinations of anticholinergic drugs into individual compounds. It includes plenty of manual cleaning based on exploration of the data.
4.	Re-calculation the anticholinergic value for each compound.
5.	Addition of participant-level covariates to each row.
6.	Misc. cleaning and creation of class-based data frames.
7.	Transformation of the data frame from a prescription-based to a period-based data frame, where each row is a participant-year combination. Contains separate columns for each anticholinergic scale.
8.	Transformation of the data frame from a prescription-based to a period-based data frame, where each row is a participant-year combination. Contains separate columns for each anatomical class.
9.	Transformation of the data frame from a prescription-based to a period-based data frame, where each row is a participant-year combination. Contains separate columns for each pharmacological subclass.
10.	Combination of all data frames relevant to the analyses into a single Masterfile.
11.	Removal of outliers and modelling for when g is the outcome.
12.	Removal of outliers and modelling for when MRI measures are the outcomes.
