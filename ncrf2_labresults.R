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
source(file = "scripts/functions/add_numeric.R")
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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/neuro_case_report/ncrf2_lab_covid_cns.rds")
  )
  
# Check variable names in dataframe
covidcns_dat %>%
  colnames()

# Inspect dimensions of dataframe 
covidcns_dat %>%
  dim()


## ----COVIDCNS select--------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  distinct(externalDataReference, .keep_all = TRUE) %>% # Remove duplicate IDs
  add_column(sample = "COVIDCNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         ncrf2_lab.hemoglobin,
         ncrf2_lab.wbc_count,
         ncrf2_lab.hematocrit,
         ncrf2_lab.platelets,
         ncrf2_lab.apttaptr,
         ncrf2_lab.pt,
         ncrf2_lab.inr,
         ncrf2_lab.astsgot,
         ncrf2_lab.total_bilirubin,
         ncrf2_lab.urea,
         ncrf2_lab.lactate,
         ncrf2_lab.po2,
         ncrf2_lab.lowest_po2,
         ncrf2_lab.pco2,
         ncrf2_lab.csf_wcc,
         ncrf2_lab.csf_protein,
         ncrf2_lab.creatinine,
         ncrf2_lab.sodium,
         ncrf2_lab.potassium,
         ncrf2_lab.procalcitonin,
         ncrf2_lab.crp,
         ncrf2_lab.ldh,
         ncrf2_lab.creatine_kinase,
         ncrf2_lab.troponin,
         ncrf2_lab.esr,
         ncrf2_lab.ddimer,
         ncrf2_lab.ferritin,
         ncrf2_lab.ph,
         ncrf2_lab.bicarbonate,
         ncrf2_lab.csf_rcc,
         ncrf2_lab.csfserum_glucose,
         ncrf2_lab.csf_autoantibodies,
         ncrf2_lab.csf_pcr,
         ncrf2_lab.serum_autoantibodies,
         ncrf2_lab.csf_antiviral_antibodies,
         ncrf2_lab.csf_autoantibodies_.txt,
         ncrf2_lab.csf_pcr_.txt,
         ncrf2_lab.serum_autoantibodies_.txt,
         ncrf2_lab.csf_antiviral_antibodies_.txt,
         ncrf2_lab.any_other_issue_of_significance_to_note.txt
         )

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


## ----COVIDCNS convert text to numeric---------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(starts_with("ncrf2") & !ends_with("txt"),
                ~round(as.numeric(.), digits = 1)
                )
    
        )


## ----Recode NA values-------------------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(starts_with("ncrf2") & !ends_with("txt"),
                ~case_when(
                  . == -55 ~ -555,
                  . == -77 ~ -777,
                  . == -88 ~ -888,
                  . == -99 ~ -999,
                  TRUE ~ .)))


## ----List unique values-----------------------------------------------------------------------------------------------
ulst <- sapply(dat, unique)


## ----Recode nonanswer values------------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(starts_with("ncrf2") & !ends_with("txt"),
                ~case_when(
                  . == -777 ~ NA_real_,
                  TRUE ~ .)))


## ----Check range of values--------------------------------------------------------------------------------------------
dat %>%
  select(starts_with("ncrf2") & !ends_with("txt")) %>%
  tbl_summary()


## ----Outlier count----------------------------------------------------------------------------------------------------
dat %>%
  select(starts_with("ncrf2") & !ends_with("txt")) %>%
  filter(. < 0) %>%
  nrow()


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/neuro_case_report/ncrf2_lab_covidcns_clean.rds")
    )

