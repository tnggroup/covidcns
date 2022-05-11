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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/dem_covid_cns.rds")
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
  "dem.long_ago_diagnosed_required",
  "dem.long_ago_diagnosed_required.1"
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
         dem.diagnosed_required_question_covid19,
         dem.long_ago_diagnosed_required,
         dem.long_ago_diagnosed_required.1,
         dem.diagnosed_covid19_experienced_similar
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


## ----COVIDCNS inspect variables initial-------------------------------------------------------------------------------
dat %>%
  tbl_summary(include = c(dem.diagnosed_required_question_covid19,
                          dem.long_ago_diagnosed_required,
                          dem.long_ago_diagnosed_required.1,
                          dem.diagnosed_covid19_experienced_similar,
                          dem.diagnosed_required_question_covid19_numeric,
                          dem.diagnosed_covid19_experienced_similar_numeric),
              missing_text = "Missing")


## ----vector categorical values----------------------------------------------------------------------------------------
values_categorical <- c(
  "Yes",
  "No",
  "Don't know",
  "Prefer not to answer",
  "Seen but not answered",
  NA
  )
values_categorical


## ----vector categorical variables-------------------------------------------------------------------------------------
variables_categorical <-
  c(
    "dem.diagnosed_required_question_covid19",
    "dem.diagnosed_covid19_experienced_similar"
    )
variables_categorical


## ----imp_check categorical variables----------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical,
          values = values_categorical)



## ----vector numeric values--------------------------------------------------------------------------------------------
values_numeric <- c(
  0,
  1,
  -777,
  -888,
  -999,
  NA
  )
values_numeric


## ----vector numeric variables-----------------------------------------------------------------------------------------
variables_numeric <-
  c(
    "dem.diagnosed_required_question_covid19_numeric",
    "dem.diagnosed_covid19_experienced_similar_numeric"
    )
variables_numeric


## ----imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_numeric,
          values = values_numeric)


## ----check range of values years--------------------------------------------------------------------------------------
dat %>%
  select("dem.long_ago_diagnosed_required") %>%
  tbl_summary()


## ----define limits years----------------------------------------------------------------------------------------------
dem.long_ago_diagnosed_required_upper_limit = 2
dem.long_ago_diagnosed_required_lower_limit = 0


## ----outlier count years----------------------------------------------------------------------------------------------
dat %>%
  filter(
    dem.long_ago_diagnosed_required > dem.long_ago_diagnosed_required_upper_limit | # bigger than the upper limit
      dem.long_ago_diagnosed_required < dem.long_ago_diagnosed_required_lower_limit & dem.long_ago_diagnosed_required > -555 # smaller than the lower limit
      # or special conditions
    ) %>%
  nrow()


## ----Recode outliers to -666 years------------------------------------------------------------------------------------
dat <- dat %>%
    mutate(
      dem.long_ago_diagnosed_required = 
        if_else(
          dem.long_ago_diagnosed_required > dem.long_ago_diagnosed_required_upper_limit | # bigger than the upper limit
           dem.long_ago_diagnosed_required < dem.long_ago_diagnosed_required_lower_limit & dem.long_ago_diagnosed_required > -555, # smaller than the lower limit
            # or special conditions
          true = -666,
          false = dem.long_ago_diagnosed_required,
          missing = NA_real_
        )
    )


## ----inspect after recoding to -666 years-----------------------------------------------------------------------------
dat %>%
  select("dem.long_ago_diagnosed_required") %>%
  tbl_summary()


## ----check range of values months-------------------------------------------------------------------------------------
dat %>%
  select("dem.long_ago_diagnosed_required.1") %>%
  tbl_summary()


## ----define limits months---------------------------------------------------------------------------------------------
dem.long_ago_diagnosed_required.1_upper_limit = 12
dem.long_ago_diagnosed_required.1_lower_limit = 0


## ----outlier count months---------------------------------------------------------------------------------------------
dat %>%
  filter(
    dem.long_ago_diagnosed_required.1 > dem.long_ago_diagnosed_required.1_upper_limit | # bigger than the upper limit
      dem.long_ago_diagnosed_required.1 < dem.long_ago_diagnosed_required.1_lower_limit & dem.long_ago_diagnosed_required.1 > -555 # smaller than the lower limit
      # or special conditions
    ) %>%
  nrow()


## ----Recode outliers to -666 months-----------------------------------------------------------------------------------
dat <- dat %>%
    mutate(
      dem.long_ago_diagnosed_required.1 = 
        if_else(
          dem.long_ago_diagnosed_required.1 > dem.long_ago_diagnosed_required.1_upper_limit | # bigger than the upper limit
           dem.long_ago_diagnosed_required.1 < dem.long_ago_diagnosed_required.1_lower_limit & dem.long_ago_diagnosed_required.1 > -555, # smaller than the lower limit
            # or special conditions
          true = -666,
          false = dem.long_ago_diagnosed_required.1,
          missing = NA_real_
        )
    )


## ----inspect after recoding to -666 months----------------------------------------------------------------------------
dat %>%
  select("dem.long_ago_diagnosed_required.1") %>%
  tbl_summary()


## ----COVIDCNS inspect variables---------------------------------------------------------------------------------------
dat %>%
  tbl_summary(include = c(dem.diagnosed_required_question_covid19,
                          dem.long_ago_diagnosed_required,
                          dem.long_ago_diagnosed_required.1,
                          dem.diagnosed_covid19_experienced_similar,
                          dem.diagnosed_required_question_covid19_numeric,
                          dem.diagnosed_covid19_experienced_similar_numeric),
              missing_text = "Missing")


## ----check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/dem_covidpositive_covidcns_clean.rds")
    )

