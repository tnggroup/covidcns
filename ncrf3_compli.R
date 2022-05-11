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
source(file = "scripts/functions/imp_check.R")


## ----Install load dependencies----------------------------------------------------------------------------------------
packages <- c("summarytools", "sjlabelled", "Amelia", "gtsummary", "tidyverse")
package_check(packages)


## ----Read in file with path to ilovecovidcns channel on teams---------------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----COVIDCNS load data-----------------------------------------------------------------------------------------------
covidcns_dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/neuro_case_report/ncrf3_compli_covid_cns.rds")
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
  "ncrf3_compli.encephalitis.1",
  "ncrf3_compli.encephalopathy_or_delirium.1",
  "ncrf3_compli.acute_necrotizing_encephalopathy.1",
  "ncrf3_compli.posterior_reversible_encephalopathy_syndrome.1",
  "ncrf3_compli.coma_not_otherwise_diagnosed.1",
  "ncrf3_compli.meningitis.1",
  "ncrf3_compli.demyelinating_disease.1",
  "ncrf3_compli.acute_hemorrhagic_necrotizing_encephalopathy.1",
  "ncrf3_compli.acute_disseminated_encephalomyelitis.1",
  "ncrf3_compli.myelitis.1",
  "ncrf3_compli.seizures.1",
  "ncrf3_compli.stroke_ischemic.1",
  "ncrf3_compli.stroke_vasulitic.1",
  "ncrf3_compli.intracerebral_haemorrhage.1",
  "ncrf3_compli.subarachnoid_haemorrhage.1",
  "ncrf3_compli.venous_sinus_thrombosis.1",
  "ncrf3_compli.cerebral_microangiopathy.1",
  "ncrf3_compli.cranial_nerve_palsy.1",
  "ncrf3_compli.parkinsonism.1",
  "ncrf3_compli.guillain_barre_syndrome_miller_fisher_syndrome.1",
  "ncrf3_compli.neuromuscular_junction_disorder.1",
  "ncrf3_compli.autonomic_dysfucntion.1",
  "ncrf3_compli.anosmiaageusia.1",
  "ncrf3_compli.psychosis.1",
  "ncrf3_compli.depression.1",
  "ncrf3_compli.catatonia.1",
  "ncrf3_compli.anxiety.1",
  "ncrf3_compli.dysexecutive_syndromecognitive_changes.1",
  "ncrf3_compli.psychiatric_diagnosis.1",
  "ncrf3_compli.compression_or_critical_illness_neuropathymyopathy.1",
  "ncrf3_compli.hypoxic_brain_injury.1",
  "ncrf3_compli.myalgia_myositis_myopathy.1",
  "ncrf3_compli.neuroleptic_malignant_syndrome.1",
  "ncrf3_compli.cranial_nerve_palsy.txt",
  "ncrf3_compli.dysexecutive_syndromecognitive_changes.txt",
  "ncrf3_compli.psychiatric_diagnosis.txt",
  "ncrf3_compli.any_other_issue_of_significance_to_note.txt" 
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
         # disease variables
         ncrf3_compli.encephalitis,
         ncrf3_compli.encephalopathy_or_delirium,
         ncrf3_compli.acute_necrotizing_encephalopathy,
         ncrf3_compli.posterior_reversible_encephalopathy_syndrome,
         ncrf3_compli.coma_not_otherwise_diagnosed,
         ncrf3_compli.meningitis,
         ncrf3_compli.demyelinating_disease,
         ncrf3_compli.acute_hemorrhagic_necrotizing_encephalopathy,
         ncrf3_compli.acute_disseminated_encephalomyelitis,
         ncrf3_compli.myelitis,
         ncrf3_compli.seizures,
         ncrf3_compli.stroke_ischemic,
         ncrf3_compli.stroke_vasulitic,
         ncrf3_compli.intracerebral_haemorrhage,
         ncrf3_compli.subarachnoid_haemorrhage,
         ncrf3_compli.venous_sinus_thrombosis,
         ncrf3_compli.cerebral_microangiopathy,
         ncrf3_compli.cranial_nerve_palsy,
         ncrf3_compli.parkinsonism,
         ncrf3_compli.guillain_barre_syndrome_miller_fisher_syndrome,
         ncrf3_compli.neuromuscular_junction_disorder,
         ncrf3_compli.autonomic_dysfucntion,
         ncrf3_compli.anosmiaageusia,
         ncrf3_compli.psychosis,
         ncrf3_compli.depression,
         ncrf3_compli.catatonia,
         ncrf3_compli.anxiety,
         ncrf3_compli.dysexecutive_syndromecognitive_changes,
         ncrf3_compli.psychiatric_diagnosis,
         ncrf3_compli.compression_or_critical_illness_neuropathymyopathy,
         ncrf3_compli.hypoxic_brain_injury,
         ncrf3_compli.myalgia_myositis_myopathy,
         ncrf3_compli.neuroleptic_malignant_syndrome,
         # date variables
         ncrf3_compli.encephalitis.1,
         ncrf3_compli.encephalopathy_or_delirium.1,
         ncrf3_compli.acute_necrotizing_encephalopathy.1,
         ncrf3_compli.posterior_reversible_encephalopathy_syndrome.1,
         ncrf3_compli.coma_not_otherwise_diagnosed.1,
         ncrf3_compli.meningitis.1,
         ncrf3_compli.demyelinating_disease.1,
         ncrf3_compli.acute_hemorrhagic_necrotizing_encephalopathy.1,
         ncrf3_compli.acute_disseminated_encephalomyelitis.1,
         ncrf3_compli.myelitis.1,
         ncrf3_compli.seizures.1,
         ncrf3_compli.stroke_ischemic.1,
         ncrf3_compli.stroke_vasulitic.1,
         ncrf3_compli.intracerebral_haemorrhage.1,
         ncrf3_compli.subarachnoid_haemorrhage.1,
         ncrf3_compli.venous_sinus_thrombosis.1,
         ncrf3_compli.cerebral_microangiopathy.1,
         ncrf3_compli.cranial_nerve_palsy.1,
         ncrf3_compli.parkinsonism.1,
         ncrf3_compli.guillain_barre_syndrome_miller_fisher_syndrome.1,
         ncrf3_compli.neuromuscular_junction_disorder.1,
         ncrf3_compli.autonomic_dysfucntion.1,
         ncrf3_compli.anosmiaageusia.1,
         ncrf3_compli.psychosis.1,
         ncrf3_compli.depression.1,
         ncrf3_compli.catatonia.1,
         ncrf3_compli.anxiety.1,
         ncrf3_compli.dysexecutive_syndromecognitive_changes.1,
         ncrf3_compli.psychiatric_diagnosis.1,
         ncrf3_compli.compression_or_critical_illness_neuropathymyopathy.1,
         ncrf3_compli.hypoxic_brain_injury.1,
         ncrf3_compli.myalgia_myositis_myopathy.1,
         ncrf3_compli.neuroleptic_malignant_syndrome.1,
         # text variables
         ncrf3_compli.cranial_nerve_palsy.txt,
         ncrf3_compli.dysexecutive_syndromecognitive_changes.txt,
         ncrf3_compli.psychiatric_diagnosis.txt,
         ncrf3_compli.any_other_issue_of_significance_to_note.txt
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
values_categorical <- c(
  "Yes",
  "No",
  "Seen but not answered",
  NA
  )
values_categorical


## ----Vector categorical variables-------------------------------------------------------------------------------------
variables_categorical <- c(
  "ncrf3_compli.encephalitis",
  "ncrf3_compli.encephalopathy_or_delirium",
  "ncrf3_compli.acute_necrotizing_encephalopathy",
  "ncrf3_compli.posterior_reversible_encephalopathy_syndrome",
  "ncrf3_compli.coma_not_otherwise_diagnosed",
  "ncrf3_compli.meningitis",
  "ncrf3_compli.demyelinating_disease",
  "ncrf3_compli.acute_hemorrhagic_necrotizing_encephalopathy",
  "ncrf3_compli.acute_disseminated_encephalomyelitis",
  "ncrf3_compli.myelitis",
  "ncrf3_compli.seizures",
  "ncrf3_compli.stroke_ischemic",
  "ncrf3_compli.stroke_vasulitic",
  "ncrf3_compli.intracerebral_haemorrhage",
  "ncrf3_compli.subarachnoid_haemorrhage",
  "ncrf3_compli.venous_sinus_thrombosis",
  "ncrf3_compli.cerebral_microangiopathy",
  "ncrf3_compli.cranial_nerve_palsy",
  "ncrf3_compli.parkinsonism",
  "ncrf3_compli.guillain_barre_syndrome_miller_fisher_syndrome",
  "ncrf3_compli.neuromuscular_junction_disorder",
  "ncrf3_compli.autonomic_dysfucntion",
  "ncrf3_compli.anosmiaageusia",
  "ncrf3_compli.psychosis",
  "ncrf3_compli.depression",
  "ncrf3_compli.catatonia",
  "ncrf3_compli.anxiety",
  "ncrf3_compli.dysexecutive_syndromecognitive_changes",
  "ncrf3_compli.psychiatric_diagnosis",
  "ncrf3_compli.compression_or_critical_illness_neuropathymyopathy",
  "ncrf3_compli.hypoxic_brain_injury",
  "ncrf3_compli.myalgia_myositis_myopathy",
  "ncrf3_compli.neuroleptic_malignant_syndrome"
  )
variables_categorical


## ----Imp_check categorical variables----------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical,
          values = values_categorical)


## ----Vector numeric values--------------------------------------------------------------------------------------------
values_numeric <- c(
  0,
  1,
  -777,
  NA
  )
values_numeric


## ----Vector numeric variables-----------------------------------------------------------------------------------------
variables_numeric <- c(
  "ncrf3_compli.encephalitis_numeric",
  "ncrf3_compli.encephalopathy_or_delirium_numeric",
  "ncrf3_compli.acute_necrotizing_encephalopathy_numeric",
  "ncrf3_compli.posterior_reversible_encephalopathy_syndrome_numeric",
  "ncrf3_compli.coma_not_otherwise_diagnosed_numeric",
  "ncrf3_compli.meningitis_numeric",
  "ncrf3_compli.demyelinating_disease_numeric",
  "ncrf3_compli.acute_hemorrhagic_necrotizing_encephalopathy_numeric",
  "ncrf3_compli.acute_disseminated_encephalomyelitis_numeric",
  "ncrf3_compli.myelitis_numeric",
  "ncrf3_compli.seizures_numeric",
  "ncrf3_compli.stroke_ischemic_numeric",
  "ncrf3_compli.stroke_vasulitic_numeric",
  "ncrf3_compli.intracerebral_haemorrhage_numeric",
  "ncrf3_compli.subarachnoid_haemorrhage_numeric",
  "ncrf3_compli.venous_sinus_thrombosis_numeric",
  "ncrf3_compli.cerebral_microangiopathy_numeric",
  "ncrf3_compli.cranial_nerve_palsy_numeric",
  "ncrf3_compli.parkinsonism_numeric",
  "ncrf3_compli.guillain_barre_syndrome_miller_fisher_syndrome_numeric",
  "ncrf3_compli.neuromuscular_junction_disorder_numeric",
  "ncrf3_compli.autonomic_dysfucntion_numeric",
  "ncrf3_compli.anosmiaageusia_numeric",
  "ncrf3_compli.psychosis_numeric",
  "ncrf3_compli.depression_numeric",
  "ncrf3_compli.catatonia_numeric",
  "ncrf3_compli.anxiety_numeric",
  "ncrf3_compli.dysexecutive_syndromecognitive_changes_numeric",
  "ncrf3_compli.psychiatric_diagnosis_numeric",
  "ncrf3_compli.compression_or_critical_illness_neuropathymyopathy_numeric",
  "ncrf3_compli.hypoxic_brain_injury_numeric",
  "ncrf3_compli.myalgia_myositis_myopathy_numeric",
  "ncrf3_compli.neuroleptic_malignant_syndrome_numeric"
  )
variables_numeric


## ----Imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_numeric,
          values = values_numeric)


## ----COVIDCNS vector date variables-----------------------------------------------------------------------------------
date_vars <- c(
  "ncrf3_compli.encephalitis.1",
  "ncrf3_compli.encephalopathy_or_delirium.1",
  "ncrf3_compli.acute_necrotizing_encephalopathy.1",
  "ncrf3_compli.posterior_reversible_encephalopathy_syndrome.1",
  "ncrf3_compli.coma_not_otherwise_diagnosed.1",
  "ncrf3_compli.meningitis.1",
  "ncrf3_compli.demyelinating_disease.1",
  "ncrf3_compli.acute_hemorrhagic_necrotizing_encephalopathy.1",
  "ncrf3_compli.acute_disseminated_encephalomyelitis.1",
  "ncrf3_compli.myelitis.1",
  "ncrf3_compli.seizures.1",
  "ncrf3_compli.stroke_ischemic.1",
  "ncrf3_compli.stroke_vasulitic.1",
  "ncrf3_compli.intracerebral_haemorrhage.1",
  "ncrf3_compli.subarachnoid_haemorrhage.1",
  "ncrf3_compli.venous_sinus_thrombosis.1",
  "ncrf3_compli.cerebral_microangiopathy.1",
  "ncrf3_compli.cranial_nerve_palsy.1",
  "ncrf3_compli.parkinsonism.1",
  "ncrf3_compli.guillain_barre_syndrome_miller_fisher_syndrome.1",
  "ncrf3_compli.neuromuscular_junction_disorder.1",
  "ncrf3_compli.autonomic_dysfucntion.1",
  "ncrf3_compli.anosmiaageusia.1",
  "ncrf3_compli.psychosis.1",
  "ncrf3_compli.depression.1",
  "ncrf3_compli.catatonia.1",
  "ncrf3_compli.anxiety.1",
  "ncrf3_compli.dysexecutive_syndromecognitive_changes.1",
  "ncrf3_compli.psychiatric_diagnosis.1",
  "ncrf3_compli.compression_or_critical_illness_neuropathymyopathy.1",
  "ncrf3_compli.hypoxic_brain_injury.1",
  "ncrf3_compli.myalgia_myositis_myopathy.1",
  "ncrf3_compli.neuroleptic_malignant_syndrome.1"
)


## ----COVIDCNS inspect dates-------------------------------------------------------------------------------------------
dat %>%
  select(all_of(date_vars)) %>%
  glimpse()


## ----COVIDCNS recode -77 to NA----------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(
    all_of(date_vars),
    ~na_if(., "-77")
                )
         )


## ----COVIDCNS parse dates---------------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(
    all_of(date_vars),
    ~lubridate::parse_date_time(
    x = .,
    orders = c("d m y", "d/m/y", "d.m.y"),
    tz = "Europe/London"
    )
                )
         )


## ----COVIDCNS reinspect dates-----------------------------------------------------------------------------------------
dat %>%
  select(all_of(date_vars)) %>%
  tbl_summary(missing_text = "Missing")


## ----Define limits----------------------------------------------------------------------------------------------------
upper_limit <- as.POSIXct("2022-02-22")
lower_limit <- as.POSIXct("2020-01-30")


## ----Recode outliers to NA--------------------------------------------------------------------------------------------
dat <- dat %>%
    mutate(across(
    all_of(date_vars),
    ~ifelse(
      . > upper_limit | # bigger than the upper limit
        . < lower_limit, # smaller than the lower limit
      yes = NA_real_,
      no = .
        )
    )
) %>%
  mutate(across(
    all_of(date_vars),
    ~as.POSIXct(., origin = lubridate::origin))
  )


## ----COVID CNS recheck dates------------------------------------------------------------------------------------------
dat %>%
  select(all_of(date_vars)) %>%
  tbl_summary(missing_text = "Missing")


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/neuro_case_report/ncrf3_compli_covidcns_clean.rds")
    )

