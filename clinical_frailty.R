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
packages <- c(
  "summarytools", 
  "sjlabelled", 
  "Amelia", 
  "gtsummary", 
  "tidyverse"
  )
package_check(packages)


## ----Recent date------------------------------------------------------------------------------------------------------
date <- Sys.Date()
date


## ----Read in file with path to ilovecovidcns channel on teams---------------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----CNS read in data-------------------------------------------------------------------------------------------------
cns_data <- readRDS(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/covid_cns/neuro_case_report/ncrf_comorbid_covid_cns.rds")
  )


  
# Check variable names in dataframe
cns_data %>%
  colnames()
# Inspect dimensions of dataframe 
cns_data %>%
  dim()


## ----CNS specify excluded columns-------------------------------------------------------------------------------------
exclude_cols_numeric <- c(
  "ID",
  "sample",
  "startDate",
  "endDate"
  )


## ----CNS select-------------------------------------------------------------------------------------------------------
cns_data_id <- cns_data %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  remove_duplicates("externalDataReference") %>% # Remove duplicate IDs
  add_column(sample = "CNS", 
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         ncrf_comorbid.clinical_frailty_scale
         ) %>%
  add_numeric(exclude = exclude_cols_numeric)
# Inspect colnames
cns_data_id %>%
  colnames()


## ----CNS number excluded----------------------------------------------------------------------------------------------
# Inspect dimensions of new data set
cns_data_id %>%
  dim()
# Inspect number of rows dropped
cns_excluded <- dim(cns_data_id)[1] - dim(cns_data)[1]
cns_excluded


## ----CNS inspect numeric variables------------------------------------------------------------------------------------
cns_data_id %>%
  select(all_of(ends_with("numeric"))) %>%
  tbl_summary(missing_text = "Missing")


## ----CNS inspect missingness------------------------------------------------------------------------------------------
 cns_miss_map <- cns_data_id %>% 
   missmap()
 cns_miss_map


## ----Rename data set--------------------------------------------------------------------------------------------------
dat <- cns_data_id
#Check
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


## ----vector of numeric values-----------------------------------------------------------------------------------------
values_numeric_1_to_9 <- c(
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
values_numeric_1_to_9


## ----vector numeric variables-----------------------------------------------------------------------------------------
variables_numeric_1_to_9 <- c(
    "ncrf_comorbid.clinical_frailty_scale_numeric"
  )
variables_numeric_1_to_9


## ----imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = dat,
           variables = variables_numeric_1_to_9,
          values = values_numeric_1_to_9)


## ----vector of categorical labels-------------------------------------------------------------------------------------
values_categorical_clinical_frailty <- c(
  "Very fit",
"Fit",
"Managing well",
"Living with very mild frailty",
"Living with mild frailty",
"Living wiht moderate frailty",
"Living with severe frailty",
"Living with very severe Frailty",
"Terminally ill",
"Seen but not answered",
  NA
  )
values_categorical_clinical_frailty


## ----vector categorical variables-------------------------------------------------------------------------------------
variables_categorical_clinical_frailty<- c(
    "ncrf_comorbid.clinical_frailty_scale"
  )
variables_categorical_clinical_frailty


## ----imp_check categorical variables----------------------------------------------------------------------------------
imp_check(data = dat,
           variables = variables_categorical_clinical_frailty,
          values = values_categorical_clinical_frailty)


## ----check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----CNS EDGI save excluded participants------------------------------------------------------------------------------
cns_excluded <- as.data.frame(cns_excluded)
colnames(cns_excluded) <- c("Number of Participants Excluded")
rownames(cns_excluded) <- c("CNS")
cns_excluded


## ----Write cleaned CNS variables to a .rds file-----------------------------------------------------------------------
dat %>% 
 saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/covidcns/ncrf_clinical_frailty_clean.rds")
    )

