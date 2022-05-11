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
source(file = "scripts/functions/imp_check_1.R")
source(file = "scripts/functions/cont_clean.R")


## ----Install load dependencies----------------------------------------------------------------------------------------
packages <- c("summarytools", "sjlabelled", "Amelia", "gtsummary", "tidyverse")
package_check(packages)


## ----Read in file with path to ilovecovidcns channel on teams---------------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----COVIDCNS load data-----------------------------------------------------------------------------------------------
covidcns_dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/neuro_case_report/ncrf1_vital_covid_cns.rds")
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
  "ncrf1_vital.respiratory_symptom_onset.txt",
  "ncrf1_vital.fever_symptom_onset.txt",
  "ncrf1_vital.neurological_symptom_onset.txt",
  "ncrf1_vital.date_of_admission_to_this_facility.txt",
  "ncrf1_vital.which_disease.txt",
  "ncrf1_vital.vitals",
  "ncrf1_vital.vitals.1",
  "ncrf1_vital.vitals.2",
  "ncrf1_vital.vitals.3",
  "ncrf1_vital.vitals.4",
  "ncrf1_vital.oxygen_saturation.txt",
  "ncrf1_vital.oxygen_saturation.txt.1",
  "ncrf1_vital.glasgow_coma_score.txt",
  "ncrf1_vital.height.txt",
  "ncrf1_vital.weight.txt",
  "ncrf1_vital.any_other_issue_of_significance_to_note.txt"
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
         ncrf1_vital.respiratory_symptom_onset.txt,
         ncrf1_vital.fever_symptom_onset.txt,
         ncrf1_vital.neurological_symptom_onset.txt,
         ncrf1_vital.date_of_admission_to_this_facility.txt,
         ncrf1_vital.admitted_for_covid_,
         ncrf1_vital.admitted_for_a_neurological_complication_,
         ncrf1_vital.admitted_for_another_disease,
         ncrf1_vital.which_disease.txt,
         ncrf1_vital.vitals,
         ncrf1_vital.vitals.1,
         ncrf1_vital.vitals.2,
         ncrf1_vital.vitals.3,
         ncrf1_vital.vitals.4,
         ncrf1_vital.severe_dehydration,
         ncrf1_vital.oxygen_saturation,
         ncrf1_vital.oxygen_saturation.txt,
         ncrf1_vital.oxygen_saturation.txt.1,
         ncrf1_vital.avpu,
         ncrf1_vital.glasgow_coma_score.txt,
         ncrf1_vital.malnutrition,
         ncrf1_vital.height.txt,
         ncrf1_vital.weight.txt,
         ncrf1_vital.admission_hours_severity_covid19,
         ncrf1_vital.any_other_issue_of_significance_to_note.txt,
         ncrf1_vital.admission_severity_worst_covid19,
         ncrf1_vital.time_worst_prior_neurological,
         ncrf1_vital.severity_prior_neurological_onset,
         ncrf1_vital.admitted_for_complication_post_vaccination
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
  "Seen but not answered",
  NA
)

vals_cat_2 <- c(
  "Yes",
  "No",
  "Seen but not answered",
  "Unknown",
  NA
)

vals_cat_3 <- c(
  "Unknown",
  "Seen but not answered",
  "Room air",
  "Oxygen therapy",
  NA
)

vals_cat_4 <- c(
  "Seen but not answered",
  "Awake",
  "Verbal",
  "Pain",
  "Unresponsive",
  NA
)

vals_cat_5 <- c(
  "Intubation and mechanical ventilation (pO2/FiO2 >= 150 or SpO2/FiO2 >= 200)",
  "Seen but not answered",
  "Hospitalised (Oxygen by mask or nasal prongs)",
  "Symptomatic (Independent)",
  "Symptomatic (Assistance needed)",
  "Hospitalised (No oxygen therapy)",
  "Hospitalised (Oxygen by NIV or high flow)",
  "Uninfected (No viral RNA detected)",
  "Asymptomatic (Viral RNA detected)",
  "Mechanical ventilation (pO2/FiO2 <150 or SpO2/FiO2 <200 or vasopressors)",
  "Mechanical ventilation (pO2/FiO2 <150 and vasopressors, dialysis or ECMO)",
  "Dead",
  NA
)

vals_cat_6 <- c(
  "Yes",
  "No",
  "Seen but not answered",
  "Not Applicable",
  NA
)


## ----List categorical values vectors----------------------------------------------------------------------------------
values_cat_list <- list(
  vals_cat_1,
  vals_cat_1,
  vals_cat_1,
  vals_cat_2,
  vals_cat_3,
  vals_cat_4,
  vals_cat_2,
  vals_cat_5,
  vals_cat_5,
  vals_cat_6,
  vals_cat_5,
  vals_cat_1
)


## ----Remove value vectors cat-----------------------------------------------------------------------------------------
rm(
  vals_cat_1,
  vals_cat_2,
  vals_cat_3,
  vals_cat_4,
  vals_cat_5,
  vals_cat_6
)


## ----Vector categorical variables-------------------------------------------------------------------------------------
variables_cat <- c(
  "ncrf1_vital.admitted_for_covid_",
  "ncrf1_vital.admitted_for_a_neurological_complication_",
  "ncrf1_vital.admitted_for_another_disease",
  "ncrf1_vital.severe_dehydration",
  "ncrf1_vital.oxygen_saturation",
  "ncrf1_vital.avpu",
  "ncrf1_vital.malnutrition",
  "ncrf1_vital.admission_hours_severity_covid19",
  "ncrf1_vital.admission_severity_worst_covid19",
  "ncrf1_vital.time_worst_prior_neurological",
  "ncrf1_vital.severity_prior_neurological_onset",
  "ncrf1_vital.admitted_for_complication_post_vaccination"
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
  NA
)

vals_num_2 <- c(
  0,
  1,
  -777,
  -888,
  NA
)

vals_num_3 <- c(
  1,
  2,
  -777,
  -888,
  NA
)

vals_num_4 <- c(
  1,
  2,
  3,
  4,
  -777,
  NA
)

vals_num_5 <- c(
  0,
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

vals_num_6 <- c(
  0,
  1,
  -555,
  -777,
  NA
)


## ----List numeric values vectors--------------------------------------------------------------------------------------
values_num_list <- list(
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_2,
  vals_num_3,
  vals_num_4,
  vals_num_2,
  vals_num_5,
  vals_num_5,
  vals_num_6,
  vals_num_5,
  vals_num_1
)


## ----Remove value vectors---------------------------------------------------------------------------------------------
rm(
  vals_num_1,
  vals_num_2,
  vals_num_3,
  vals_num_4,
  vals_num_5,
  vals_num_6
)


## ----Vector numeric variables-----------------------------------------------------------------------------------------
variables_num <- c(
  "ncrf1_vital.admitted_for_covid__numeric",
  "ncrf1_vital.admitted_for_a_neurological_complication__numeric",
  "ncrf1_vital.admitted_for_another_disease_numeric",
  "ncrf1_vital.severe_dehydration_numeric",
  "ncrf1_vital.oxygen_saturation_numeric",
  "ncrf1_vital.avpu_numeric",
  "ncrf1_vital.malnutrition_numeric",
  "ncrf1_vital.admission_hours_severity_covid19_numeric",
  "ncrf1_vital.admission_severity_worst_covid19_numeric",
  "ncrf1_vital.time_worst_prior_neurological_numeric",
  "ncrf1_vital.severity_prior_neurological_onset_numeric",
  "ncrf1_vital.admitted_for_complication_post_vaccination_numeric"
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


## ----Vector date variables--------------------------------------------------------------------------------------------
variables_date <- c(
  "ncrf1_vital.respiratory_symptom_onset.txt",
  "ncrf1_vital.fever_symptom_onset.txt",
  "ncrf1_vital.neurological_symptom_onset.txt",
  "ncrf1_vital.date_of_admission_to_this_facility.txt"
)


## ----COVIDCNS inspect dates-------------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_date)) %>%
  glimpse()


## ----COVIDCNS recode -77 to NA----------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(
    all_of(variables_date),
    ~na_if(., "-77")
                )
         )


## ----COVIDCNS parse dates---------------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(
    all_of(variables_date),
    ~lubridate::parse_date_time(
    x = .,
    orders = c("d m y", "d/m/y", "d.m.y"),
    tz = "Europe/London"
    )
                )
         )


## ----COVIDCNS reinspect dates-----------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_date)) %>%
  tbl_summary(missing_text = "Missing")


## ----Define limits----------------------------------------------------------------------------------------------------
upper_limit <- as.POSIXct("2022-02-22")
lower_limit <- as.POSIXct("2020-01-30")


## ----Recode outliers to NA--------------------------------------------------------------------------------------------
dat <- dat %>%
    mutate(across(
    all_of(variables_date),
    ~ifelse(
      . > upper_limit | # bigger than the upper limit
      . < lower_limit, # smaller than the lower limit
      yes = NA_real_,
      no = .
        )
    )
) %>%
  mutate(across(all_of(variables_date),
                ~as.POSIXct(., origin = lubridate::origin)
                )
         )


## ----COVID CNS recheck dates------------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_date)) %>%
  tbl_summary(missing_text = "Missing")


## ----Create cont vars vector------------------------------------------------------------------------------------------
variables_cont <- c(
  "ncrf1_vital.vitals",
  "ncrf1_vital.vitals.1",
  "ncrf1_vital.vitals.2",
  "ncrf1_vital.vitals.3",
  "ncrf1_vital.vitals.4",
  "ncrf1_vital.oxygen_saturation.txt",
  "ncrf1_vital.oxygen_saturation.txt.1",
  "ncrf1_vital.glasgow_coma_score.txt",
  "ncrf1_vital.height.txt",
  "ncrf1_vital.weight.txt"
)


## ----Inspect cont vars------------------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_cont)) %>%
  tbl_summary(
    missing_text = "Missing",
    statistic = list(all_continuous() ~ "{median} ({p0}, {p5}, {p95}, {p100})"))


## ----Cont vars to numeric---------------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(all_of(variables_cont),
                ~as.numeric(.)
                )
         )


## ----Cont vars nonanswer----------------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(
    all_of(variables_cont),
    ~case_when(. == -55 ~ -555,
               . == -77 ~ -777,
               . == -88 ~ -888,
               . == -99 ~ -999,
               TRUE ~ .)
                )
         )


## ----Recheck cont vars------------------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_cont)) %>%
  tbl_summary(
    missing_text = "Missing",
    statistic = list(all_continuous() ~ "{median} ({p0}, {p5}, {p95}, {p100})"))


## ----Create matrix limits---------------------------------------------------------------------------------------------
limits_mat <- rbind(
  c(11, 47),
  c(0, 480),
  c(0, 200),
  c(0, 370),
  c(0, 360),
  c(0, 100),
  c(0, 100),
  c(3, 15),
  c(54, 251),
  c(0, 610)
  )


## ----Set lim_mat names------------------------------------------------------------------------------------------------
rownames(limits_mat) <- variables_cont
colnames(limits_mat) <- c("Lower", "Upper")


## ----Cont_clean cont vars---------------------------------------------------------------------------------------------
cont_list <- cont_clean(
  variables = variables_cont,
  limits_mat = limits_mat,
  dat = dat
)
for (i in 1:length(variables_cont)){
  print(paste0("Implausibles in ", variables_cont[i], ": ", cont_list[[variables_cont[i]]]$Count))
}


## ----Recode cont vars-------------------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(
    ncrf1_vital.vitals.1 = cont_list$ncrf1_vital.vitals.1$Replacement
  )

dat <- dat %>%
  mutate(
    ncrf1_vital.vitals.4 = cont_list$ncrf1_vital.vitals.4$Replacement
  )

dat <- dat %>%
  mutate(
    ncrf1_vital.glasgow_coma_score.txt = cont_list$ncrf1_vital.glasgow_coma_score.txt$Replacement
  )

dat <- dat %>%
  mutate(
    ncrf1_vital.height.txt = cont_list$ncrf1_vital.height.txt$Replacement
  )


## ----Inspect after cleaning-------------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_cont)) %>%
  tbl_summary(
    missing_text = "Missing",
    statistic = list(all_continuous() ~ "{median} ({p0}, {p100})")
  )


## ----Inspect text vars------------------------------------------------------------------------------------------------
variables_text <- c(
  "ncrf1_vital.which_disease.txt",
  "ncrf1_vital.any_other_issue_of_significance_to_note.txt"
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
    file = paste0(ilovecovidcns, "/data/latest_freeze/neuro_case_report/ncrf1_vital_covidcns_clean.rds")
    )

