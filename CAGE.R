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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/cage_covid_cns.rds")
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
  "endDate"
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
         cage.drinking_cut_felt,
         cage.criticising_people_annoyed_drinking,
         cage.guilty_felt_bad_drinking,
         cage.hangover_morning_steady_rid
         ) %>%
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
  "cage.drinking_cut_felt",
  "cage.criticising_people_annoyed_drinking",
  "cage.guilty_felt_bad_drinking",
  "cage.hangover_morning_steady_rid"
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
variables_numeric <-
  c(
    "cage.drinking_cut_felt_numeric",
    "cage.criticising_people_annoyed_drinking_numeric",
    "cage.guilty_felt_bad_drinking_numeric",
    "cage.hangover_morning_steady_rid_numeric"
    )
variables_numeric


## ----Imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_numeric,
          values = values_numeric)


## ----Sumscores inputs-------------------------------------------------------------------------------------------------
keys <- c(
  1,
  1,
  1,
  1
  )

sum_vars <- c(
  "cage.drinking_cut_felt_numeric",
  "cage.criticising_people_annoyed_drinking_numeric",
  "cage.guilty_felt_bad_drinking_numeric",
  "cage.hangover_morning_steady_rid_numeric"
  )


## ----Generate sumscores-----------------------------------------------------------------------------------------------
sum_out <- sumscores(input = dat,
                     sum_vars = sum_vars,
                     coding_keys = keys,
                     na_allowed = 0,
                     min_item = 0,
                     max_item = 1,
                     min_score = 0,
                     max_score = 4
                     )


## ----Add sumscores----------------------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(cage.sum_score = sum_out$scores)


## ----Numeric binary investigation variable----------------------------------------------------------------------------
dat <- dat %>%
  mutate(
    cage.investigate_numeric =
      case_when(
        cage.sum_score >= 2 ~ 1,
        cage.sum_score < 2 ~ 0
      )
  )
dat %>%
  select(cage.investigate_numeric) %>%
  tbl_summary()


## ----Categorical binary investigation variable------------------------------------------------------------------------
dat <- dat %>%
  mutate(
    cage.investigate =
      recode_factor(
        cage.investigate_numeric,
        "0" = "Do not investigate for alcohol use disorder",
        "1" = "Investigate for alcohol use disorder"
      )
  )
dat %>%
  select(cage.investigate) %>%
  tbl_summary()


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/cage_covidcns_clean.rds")
    )

