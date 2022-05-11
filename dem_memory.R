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
  "endDate"
  )


## ----COVIDCNS select--------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  distinct(externalDataReference, .keep_all = TRUE) %>% # Changed to distinct due to NA coercion
  add_column(sample = "COVIDCNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         dem.affects_concerned_live_memory,
         dem.memory_problem_worse_year,
         dem.affects_concerned_live_memory.1,
         dem.affects_concerned_live_memory.2,
         dem.has_your_memory_got_progressively_worse
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


## ----vector categorical values----------------------------------------------------------------------------------------
values_categorical <- c(
  "Yes",
  "No",
  "Don't know",
  "Prefer not to say",
  "Seen but not answered",
  NA
  )
values_categorical


## ----vector categorical variables-------------------------------------------------------------------------------------
variables_categorical <-
  c(
    "dem.affects_concerned_live_memory",
    "dem.memory_problem_worse_year",
    "dem.affects_concerned_live_memory.1",
    "dem.affects_concerned_live_memory.2",
    "dem.has_your_memory_got_progressively_worse"
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
    "dem.affects_concerned_live_memory_numeric",
    "dem.memory_problem_worse_year_numeric",
    "dem.affects_concerned_live_memory.1_numeric",
    "dem.affects_concerned_live_memory.2_numeric",
    "dem.has_your_memory_got_progressively_worse_numeric"
    )
variables_numeric


## ----imp_check numeric variables memory got worse---------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_numeric,
          values = values_numeric)


## ----COVIDCNS check incorrect variable--------------------------------------------------------------------------------
dat %>%
  select(
    dem.affects_concerned_live_memory.1,
    dem.affects_concerned_live_memory.1_numeric
  )


## ----COVIDCNS recode incorrect variable-------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(dem.affects_concerned_live_memory.1_numeric,
                ~case_when(
                  . == 2 ~ 0,
                  TRUE ~ .)))


## ----imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = "dem.affects_concerned_live_memory.1_numeric",
          values = values_numeric)


## ----COVIDCNS inspect variables final---------------------------------------------------------------------------------
dat %>%
  tbl_summary(include = c(dem.affects_concerned_live_memory,
                          dem.memory_problem_worse_year,
                          dem.affects_concerned_live_memory.1,
                          dem.affects_concerned_live_memory.2,
                          dem.has_your_memory_got_progressively_worse,
                          dem.affects_concerned_live_memory_numeric,
                          dem.memory_problem_worse_year_numeric,
                          dem.affects_concerned_live_memory.1_numeric,
                          dem.affects_concerned_live_memory.2_numeric,
                          dem.has_your_memory_got_progressively_worse_numeric),
              missing_text = "Missing")


## ----check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/dem_memory_covidcns_clean.rds")
    )

