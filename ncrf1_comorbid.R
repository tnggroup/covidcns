## ----Setup, include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  comment = '',
  prompt = FALSE,
  cache = FALSE
  )


## ----Clear global environment-----------------------------------------------------------------------------------------
remove(list = ls())


## ----Read in functions------------------------------------------------------------------------------------------------
source(file = "scripts/functions/add_numeric_1.R")
source(file = "scripts/functions/remove_duplicates.R")
source(file = "scripts/functions/sumscores.R")
source(file = "scripts/functions/package_check.R")
source(file = "scripts/functions/imp_check_1.R")


## ----Install load dependencies----------------------------------------------------------------------------------------
packages <- c("summarytools", "sjlabelled", "Amelia", "gtsummary", "tidyverse")
package_check(packages)


## ----Read in file with path to ilovecovidcns channel on teams---------------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----COVIDCNS load data-----------------------------------------------------------------------------------------------
covidcns_dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/neuro_case_report/ncrf1_comorbid_covid_cns.rds")
  )
  
# Check variable names in dataframe
covidcns_dat %>%
  colnames()

# Inspect dimensions of dataframe 
covidcns_dat %>%
  dim()


## ----COVIDCNS specify excluded columns--------------------------------------------------------------------------------
exclude_cols_numeric <- c(
  "ID",
  "sample",
  "startDate",
  "endDate",
  "ncrf1_comorbid.type_of_movement_disorder.txt",
  "ncrf1_comorbid.other_neurological_disease.txt",
  "ncrf1_comorbid.psychiatricpsychological_disorder.txt",
  "ncrf1_comorbid.any_other_issue_of_significance_to_note.txt"
  )


## ----COVIDCNS select--------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  remove_duplicates("externalDataReference") %>% # Remove duplicate IDs
  add_column(sample = "COVIDCNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         ncrf1_comorbid.hypertension,
         ncrf1_comorbid.current_tobacco_smoking,
         ncrf1_comorbid.current_ecigarette_smoking,
         ncrf1_comorbid.hypercholesterolaemia,
         ncrf1_comorbid.muscular_disease,
         ncrf1_comorbid.multiple_sclerosis,
         ncrf1_comorbid.movement_disorder,
         ncrf1_comorbid.dementia,
         ncrf1_comorbid.epilepsy,
         ncrf1_comorbid.ischemic_stroke,
         ncrf1_comorbid.hemorrhagic_stroke,
         ncrf1_comorbid.diabetes,
         ncrf1_comorbid.motor_neuron_disease,
         ncrf1_comorbid.other_neurological_disease,
         ncrf1_comorbid.atrial_fibrillation,
         ncrf1_comorbid.hiv,
         ncrf1_comorbid.psychiatricpsychological_disorder,
         ncrf1_comorbid.brain_cancer,
         ncrf1_comorbid.chronic_cardiac_disease,
         ncrf1_comorbid.chronic_kidney_disease,
         ncrf1_comorbid.tb, # tb column is repeated
         ncrf1_comorbid.malignant_neoplasm,
         ncrf1_comorbid.myasthenia_gravis,
         ncrf1_comorbid.developmental_delay,
         ncrf1_comorbid.chronic_pulmonary_disease,
         ncrf1_comorbid.chronic_liver_disease,
         ncrf1_comorbid.autoimmune_disease,
         ncrf1_comorbid.type_of_movement_disorder.txt,
         ncrf1_comorbid.type_of_diabetes,
         ncrf1_comorbid.other_neurological_disease.txt,
         ncrf1_comorbid.psychiatricpsychological_disorder.txt,
         ncrf1_comorbid.clinical_frailty_scale,
         ncrf1_comorbid.any_other_issue_of_significance_to_note.txt
         ) %>%
  add_numeric_1(exclude = exclude_cols_numeric)

# Inspect colnames
covidcns_dat_id %>%
  colnames()


## ----COVIDCNS number excluded-----------------------------------------------------------------------------------------
# Inspect dimensions of new data set
covidcns_dat_id %>%
  dim()

# Inspect number of rows dropped
covidcns_excluded <- dim(covidcns_dat_id)[1] - dim(covidcns_dat)[1]
covidcns_excluded


## ----COVIDCNS inspect missingness-------------------------------------------------------------------------------------
covidcns_miss_map <- covidcns_dat_id %>% 
  missmap()

covidcns_miss_map


## ----Create dat-------------------------------------------------------------------------------------------------------
dat <- covidcns_dat_id 

# Check
dat %>% glimpse()


## ----Recode NA values-------------------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(ends_with("numeric"),
                ~case_when(
                  . == -55 ~ -555,
                  . == -77 ~ -777,
                  . == -88 ~ -888,
                  . == -99 ~ -999,
                  TRUE ~ .)))


## ----List unique values-----------------------------------------------------------------------------------------------
ulst <- sapply(dat, unique)
llst <- sapply(dat, levels)


## ----Vector categorical values----------------------------------------------------------------------------------------
vals_cat_1 <- c(
  "Yes",
  "No",
  "Unknown",
  "Seen but not answered",
  NA
)

vals_cat_2 <- c(
  "Type 1",
  "Type 2 on oral medication",
  "Type 2 on insulin",
  "Type 2 diabetes diet controlled",
  "Seen but not answered",
  NA
)

vals_cat_3 <- c(
  "Very fit",
  "Fit",
  "Managing well",
  "Living with very mild frailty",
  "Living with mild frailty",
  "Living with moderate frailty", #missspelled in qualtrics
  "Living with severe frailty",
  "Living with very severe Frailty",
  "Terminally ill",
  "Seen but not answered",
  NA
)


## ----List categorical values vectors----------------------------------------------------------------------------------
values_cat_list <- list(
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_2,
  vals_cat_3
)


## ----Vector categorical variables-------------------------------------------------------------------------------------
variables_cat <- c(
  "ncrf1_comorbid.hypertension",
  "ncrf1_comorbid.current_tobacco_smoking",
  "ncrf1_comorbid.current_ecigarette_smoking",
  "ncrf1_comorbid.hypercholesterolaemia",
  "ncrf1_comorbid.muscular_disease",
  "ncrf1_comorbid.multiple_sclerosis",
  "ncrf1_comorbid.movement_disorder",
  "ncrf1_comorbid.dementia",
  "ncrf1_comorbid.epilepsy",
  "ncrf1_comorbid.ischemic_stroke",
  "ncrf1_comorbid.hemorrhagic_stroke",
  "ncrf1_comorbid.diabetes",
  "ncrf1_comorbid.motor_neuron_disease",
  "ncrf1_comorbid.other_neurological_disease",
  "ncrf1_comorbid.atrial_fibrillation",
  "ncrf1_comorbid.hiv",
  "ncrf1_comorbid.psychiatricpsychological_disorder",
  "ncrf1_comorbid.brain_cancer",
  "ncrf1_comorbid.chronic_cardiac_disease",
  "ncrf1_comorbid.chronic_kidney_disease",
  "ncrf1_comorbid.tb",
  "ncrf1_comorbid.malignant_neoplasm",
  "ncrf1_comorbid.myasthenia_gravis",
  "ncrf1_comorbid.developmental_delay",
  "ncrf1_comorbid.chronic_pulmonary_disease",
  "ncrf1_comorbid.chronic_liver_disease",
  "ncrf1_comorbid.autoimmune_disease",
  "ncrf1_comorbid.type_of_diabetes",
  "ncrf1_comorbid.clinical_frailty_scale"
)


## ----Set list names cat-----------------------------------------------------------------------------------------------
names(values_cat_list) <- variables_cat


## ----Imp_check categorical variables----------------------------------------------------------------------------------
# Create empty list
imp_list_cat <- list()

# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(variables_cat)) {
  imp_list_cat[i] <- imp_check_1(data = dat,
                                 variables = names(values_cat_list)[i],
                                 values = values_cat_list[[i]]) 

}

# Name list with var names to correspond to imp_messages
names(imp_list_cat) <- variables_cat

# View list of imp_messages with corresponding var names
print(imp_list_cat)


## ----Summary table categorical variables------------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(variables_cat),
    missing_text = "Missing")


## ----Vector numeric values--------------------------------------------------------------------------------------------
vals_num_1 <- c(
  0,
  1,
  -777,
  -888,
  NA
)

vals_num_2 <- c(
  1,
  2,
  3,
  4,
  -777,
  NA
)

vals_num_3 <- c(
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8,
  9,
  -777,
  NA
)


## ----List numeric values vectors--------------------------------------------------------------------------------------
values_num_list <- list(
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_2,
  vals_num_3
)


## ----Vector numeric variables-----------------------------------------------------------------------------------------
variables_num <- c(
  "ncrf1_comorbid.hypertension_numeric",
  "ncrf1_comorbid.current_tobacco_smoking_numeric",
  "ncrf1_comorbid.current_ecigarette_smoking_numeric",
  "ncrf1_comorbid.hypercholesterolaemia_numeric",
  "ncrf1_comorbid.muscular_disease_numeric",
  "ncrf1_comorbid.multiple_sclerosis_numeric",
  "ncrf1_comorbid.movement_disorder_numeric",
  "ncrf1_comorbid.dementia_numeric",
  "ncrf1_comorbid.epilepsy_numeric",
  "ncrf1_comorbid.ischemic_stroke_numeric",
  "ncrf1_comorbid.hemorrhagic_stroke_numeric",
  "ncrf1_comorbid.diabetes_numeric",
  "ncrf1_comorbid.motor_neuron_disease_numeric",
  "ncrf1_comorbid.other_neurological_disease_numeric",
  "ncrf1_comorbid.atrial_fibrillation_numeric",
  "ncrf1_comorbid.hiv_numeric",
  "ncrf1_comorbid.psychiatricpsychological_disorder_numeric",
  "ncrf1_comorbid.brain_cancer_numeric",
  "ncrf1_comorbid.chronic_cardiac_disease_numeric",
  "ncrf1_comorbid.chronic_kidney_disease_numeric",
  "ncrf1_comorbid.tb_numeric",
  "ncrf1_comorbid.malignant_neoplasm_numeric",
  "ncrf1_comorbid.myasthenia_gravis_numeric",
  "ncrf1_comorbid.developmental_delay_numeric",
  "ncrf1_comorbid.chronic_pulmonary_disease_numeric",
  "ncrf1_comorbid.chronic_liver_disease_numeric",
  "ncrf1_comorbid.autoimmune_disease_numeric",
  "ncrf1_comorbid.type_of_diabetes_numeric",
  "ncrf1_comorbid.clinical_frailty_scale_numeric"
)


## ----Set list names---------------------------------------------------------------------------------------------------
names(values_num_list) <- variables_num


## ----Imp_check numeric variables--------------------------------------------------------------------------------------
# Create empty list
imp_list_num <- list()

# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(variables_num)) {
  imp_list_num[i] <- imp_check_1(data = dat,
                                 variables = names(values_num_list)[i],
                                 values = values_num_list[[i]]) 

}

# Name list with var names to correspond to imp_messages
names(imp_list_num) <- variables_num

# View list of imp_messages with corresponding var names
print(imp_list_num)


## ----Summary table numeric variables----------------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(variables_num),
    missing_text = "Missing")


## ----Inspect incorrect variable---------------------------------------------------------------------------------------
dat %>%
  select(
    ncrf1_comorbid.clinical_frailty_scale,
    ncrf1_comorbid.clinical_frailty_scale_numeric) %>%
  tbl_summary(missing_text = "Missing")


## ----Recode incorrect variable----------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(ncrf1_comorbid.clinical_frailty_scale,
                ~recode_factor(.,
                  "Seen but not answered" = "Seen but not answered",
                  "Very fit" = "Very fit",
                  "Fit" = "Fit",
                  "Managing well" = "Managing well",
                  "Living with very mild frailty" = "Living with very mild frailty",
                  "Living with mild frailty" = "Living with mild frailty",
                  "Living wiht moderate frailty" = "Living with moderate frailty",
                  "Living with severe frailty" = "Living with severe frailty",
                  "Living with very severe Frailty" = "Living with very severe frailty",
                  "Terminally ill" = "Terminally ill",
                  missing = NA_character_
                              )
                )
        )


## ----Recheck variable coding------------------------------------------------------------------------------------------
dat %>%
  select(
    ncrf1_comorbid.clinical_frailty_scale,
    ncrf1_comorbid.clinical_frailty_scale_numeric) %>%
  tbl_summary(missing_text = "Missing")


## ----Inspect text vars------------------------------------------------------------------------------------------------
variables_text <- c(
  "ncrf1_comorbid.type_of_movement_disorder.txt",
  "ncrf1_comorbid.other_neurological_disease.txt",
  "ncrf1_comorbid.psychiatricpsychological_disorder.txt",
  "ncrf1_comorbid.any_other_issue_of_significance_to_note.txt"
)

dat %>%
  select(all_of(variables_text)) %>%
  tbl_summary(
    missing_text = "Missing"
  )


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/neuro_case_report/ncrf1_comorbid_covidcns_clean.rds")
    )

