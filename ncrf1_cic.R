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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/neuro_case_report/ncrf1_cic_covid_cns.rds")
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
  "ncrf1_cic.viral_rna_pcr_positive.2",
  "ncrf1_cic.viral_rna_pcr_positive.3",
  "ncrf1_cic.chest_xray_evidence_of_covid19.1",
  "ncrf1_cic.positive_antibody_test.1",
  "ncrf1_cic.lateral_flow_assay_positive.1",
  "ncrf1_cic.clinical_diagnosis_of_covid19.1"
  )


## ----COVIDCNS select--------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  distinct(externalDataReference, .keep_all = TRUE) %>% # Remove duplicate IDs, had to revert to this for now
  add_column(sample = "COVIDCNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         ncrf1_cic.viral_rna_pcr_positive,
         ncrf1_cic.viral_rna_pcr_positive.1,
         ncrf1_cic.chest_xray_evidence_of_covid19,
         ncrf1_cic.positive_antibody_test,
         ncrf1_cic.lateral_flow_assay_positive,
         ncrf1_cic.clinical_diagnosis_of_covid19,
         ncrf1_cic.viral_rna_pcr_positive.2,
         ncrf1_cic.viral_rna_pcr_positive.3,
         ncrf1_cic.chest_xray_evidence_of_covid19.1,
         ncrf1_cic.positive_antibody_test.1,
         ncrf1_cic.lateral_flow_assay_positive.1,
         ncrf1_cic.clinical_diagnosis_of_covid19.1
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
  "ncrf1_cic.viral_rna_pcr_positive",
  "ncrf1_cic.viral_rna_pcr_positive.1",
  "ncrf1_cic.chest_xray_evidence_of_covid19",
  "ncrf1_cic.positive_antibody_test",
  "ncrf1_cic.lateral_flow_assay_positive",
  "ncrf1_cic.clinical_diagnosis_of_covid19"
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
  "ncrf1_cic.viral_rna_pcr_positive_numeric",
  "ncrf1_cic.viral_rna_pcr_positive.1_numeric",
  "ncrf1_cic.chest_xray_evidence_of_covid19_numeric",
  "ncrf1_cic.positive_antibody_test_numeric",
  "ncrf1_cic.lateral_flow_assay_positive_numeric",
  "ncrf1_cic.clinical_diagnosis_of_covid19_numeric"
  )
variables_numeric


## ----Imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_numeric,
          values = values_numeric)


## ----Vector date variables--------------------------------------------------------------------------------------------
variables_date <- c(
  "ncrf1_cic.viral_rna_pcr_positive.2",
  "ncrf1_cic.viral_rna_pcr_positive.3",
  "ncrf1_cic.chest_xray_evidence_of_covid19.1",
  "ncrf1_cic.positive_antibody_test.1",
  "ncrf1_cic.lateral_flow_assay_positive.1",
  "ncrf1_cic.clinical_diagnosis_of_covid19.1"
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
  mutate(across(
    all_of(variables_date),
    ~as.POSIXct(., origin = lubridate::origin))
  )


## ----COVID CNS recheck dates------------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_date)) %>%
  tbl_summary(missing_text = "Missing")


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/neuro_case_report/ncrf1_cic_covidcns_clean.rds")
    )

