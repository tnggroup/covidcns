## ----setup, include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      comment=NA,
                      prompt=FALSE,
                      cache=FALSE)


## ----Delete everything in your global environment---------------------------------------------------------------------
remove(list = ls())


## ----Read in functions------------------------------------------------------------------------------------------------
source(file = "scripts/functions/add_numeric.R")
source(file = "scripts/functions/remove_duplicates.R")
source(file = "scripts/functions/sumscores.R")
source(file = "scripts/functions/package_check.R")
source(file = "scripts/functions/imp_check.R")


## ----Install load dependencies----------------------------------------------------------------------------------------
packages = c(
  "summarytools",
  "sjlabelled",
  "Amelia",
  "gtsummary",
  "tidyverse"
  )
package_check(packages)


## ----Recent date------------------------------------------------------------------------------------------------------
date = Sys.Date()
date


## ----Read in file with path to ilovecovidcns channel on Teams---------------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----COVID CNS read in data-------------------------------------------------------------------------------------------
covidcns_dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/gad7_covid_cns.rds")
  )
  
# Check variable names in dataframe
covidcns_dat %>%
  colnames()

# Inspect dimensions of dataframe 
covidcns_dat %>%
  dim()


## ----COVID CNS exclude continuous variables---------------------------------------------------------------------------
exclude_cols_numeric <- c(
  "ID",
  "sample",
  "startDate",
  "endDate"
  )


## ----COVID CNS select-------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  remove_duplicates("externalDataReference") %>% # Remove duplicate IDs
  add_column(sample = "COVIDCNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference,# ID
         sample,
         startDate,
         endDate,
         gad7.feeling_nervous_anxious_or_on_edge,
         gad7.control_worrying_stop,
         gad7.worrying_too_much_about_different_things,
         gad7.trouble_relaxing,
         gad7.sit_restless_hard,
         gad7.becoming_easily_annoyed_or_irritable,
         gad7.feeling_afraid_awful_happen
         )%>%
  add_numeric(exclude = exclude_cols_numeric)

  
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


## ----COVIDCNS inspect numeric variables-------------------------------------------------------------------------------
covidcns_dat_id %>%
  select(all_of(ends_with("numeric"))) %>%
  tbl_summary(missing_text = "Missing")


## ----COVIDCNS inspect missingness-------------------------------------------------------------------------------------
covidcns_miss_map <- covidcns_dat_id %>% 
  missmap()
covidcns_miss_map


## ----Rename covidcns_dat_id to dat------------------------------------------------------------------------------------
dat <- covidcns_dat_id


## ----Recode NA values to 3 digits-------------------------------------------------------------------------------------
dat <- dat %>%
   mutate(across(ends_with("numeric"),
             ~case_when(
             . == -55 ~ -555,
             . == -77 ~ -777,
             . == -88 ~ -888,
             . == -99 ~ -999,
             TRUE ~ .)
   )
   )


## ----vector of numeric values 0-3-------------------------------------------------------------------------------------
values_numeric_0_3 <- c(
  0,
  1,
  2,
  3,
  -777, #only has Seen but not answered
  NA
  )

values_numeric_0_3 


## ----vector numeric variables-----------------------------------------------------------------------------------------
variables_numeric_0_3 <- c(
  "gad7.feeling_nervous_anxious_or_on_edge_numeric",
  "gad7.control_worrying_stop_numeric",
  "gad7.worrying_too_much_about_different_things_numeric",
  "gad7.trouble_relaxing_numeric",
  "gad7.sit_restless_hard_numeric",
  "gad7.becoming_easily_annoyed_or_irritable_numeric",
  "gad7.feeling_afraid_awful_happen_numeric"
  )
variables_numeric_0_3


## ----imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = dat,
           variables = variables_numeric_0_3,
          values = values_numeric_0_3)


## ----vector of categorical labels 0-3---------------------------------------------------------------------------------
values_categorical_0_3 <- c(
  "Seen but not answered",
  "Not at all",
  "Several days",
  "More than half the days",
  "Nearly every day", 
  NA)

values_categorical_0_3


## ----categorical variables vector 0-3---------------------------------------------------------------------------------
variables_categorical_0_3 <- c(
  "gad7.feeling_nervous_anxious_or_on_edge",
  "gad7.control_worrying_stop",
  "gad7.worrying_too_much_about_different_things",
  "gad7.trouble_relaxing",
  "gad7.sit_restless_hard",
  "gad7.becoming_easily_annoyed_or_irritable",
  "gad7.feeling_afraid_awful_happen"
  )

variables_categorical_0_3


## ----imp_check categorical variables----------------------------------------------------------------------------------
imp_check(data = dat,
           variables = variables_categorical_0_3,
          values = values_categorical_0_3)


## ----Sumscores inputs-------------------------------------------------------------------------------------------------
keys_gad7 <- c(
  1,
  1,
  1,
  1,
  1,
  1,
  1
  )

sum_vars_gad7 <- c(
  "gad7.feeling_nervous_anxious_or_on_edge_numeric",
  "gad7.control_worrying_stop_numeric",
  "gad7.worrying_too_much_about_different_things_numeric",
  "gad7.trouble_relaxing_numeric",
  "gad7.sit_restless_hard_numeric",
  "gad7.becoming_easily_annoyed_or_irritable_numeric",
  "gad7.feeling_afraid_awful_happen_numeric"
  )


## ----generate sumscores-----------------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(
    gad7.sum_score = 
         sumscores(input = dat,
                   sum_vars = sum_vars_gad7,
                   coding_keys = keys_gad7,
                   na_allowed = 0, 
                   min_item = 0,
                   max_item = 3,
                   min_score = 0,
                   max_score = 21
                   )[["scores"]]
         )

dat %>%
  select(gad7.sum_score) %>%
  freq()


## ----gad7 clinical cut off 10 or more---------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(
    gad7.binary_clinical_numeric =
      case_when(
        gad7.sum_score >= 10 ~ 1,
        gad7.sum_score < 10 ~ 0
        )
    )

dat %>%
  freq(gad7.binary_clinical_numeric)


## ----GAD-7 binary creation--------------------------------------------------------------------------------------------
dat <- dat %>%
    mutate(gad7.binary_clinical =
           recode_factor(gad7.binary_clinical_numeric,
                         "0" = "No GAD", 
                         "1" = "Clinial GAD"
                         )
           )

dat %>%
  freq(gad7.binary_clinical)


## ----gad7 severity thresholds 5, 10, 15-------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(
    gad7.severity_thresholds_numeric =
      case_when(
        gad7.sum_score < 5 ~ 0, #none
        gad7.sum_score >= 5 &
        gad7.sum_score < 10 ~ 1, #5-9 = mild GAD
        gad7.sum_score >= 10 &
        gad7.sum_score < 15~ 2, #10-14 = moderate GAD
        gad7.sum_score >= 15~ 3 #15 or more = severe GAD
        )
    )

dat %>%
  freq(gad7.severity_thresholds_numeric)


## ----GAD-7 severity threshold creation--------------------------------------------------------------------------------
dat <- dat %>%
    mutate(gad7.severity_thresholds =
           recode_factor(gad7.severity_thresholds_numeric,
                         "0" = "None", 
                         "1" = "Mild",
                         "2" = "Moderate",
                         "3" = "Severe"
           )
  )

dat %>%
  freq(gad7.severity_thresholds)


## ----check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----COVIDCNS save excluded participants------------------------------------------------------------------------------
cns_excluded <- as.data.frame(covidcns_excluded)
colnames(cns_excluded) <- c("Number of Participants Excluded")
rownames(cns_excluded) <- c("COVIDCNS")
cns_excluded


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>% 
  filter(sample == "COVIDCNS") %>%  # select only COVIDCNS participants
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/gad7_covidcns_clean.rds")
    )

