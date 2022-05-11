## ----setup, include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      comment=NA,
                      prompt=FALSE,
                      cache=FALSE)


## ----Delete everything in your global environment---------------------------------------------------------------------
remove(list = ls())


## ----Read in functions------------------------------------------------------------------------------------------------
source(file = "scripts/functions/add_numeric_1.R")
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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/dem_covid_cns.rds")
  )
  
# check variable names in dataframe
covidcns_dat %>%
  colnames()
# Inspect dimensions of dataframe (number of rows and columns)
covidcns_dat %>%
  dim()


## ----COVID CNS specify excluded columns-------------------------------------------------------------------------------
exclude_cols_numeric <- c(
  "ID",
  "sample",
  "startDate",
  "endDate",
  "dem.what_is_your_first_language.txt"
  )


## ----COVID CNS select & rename relevant columns-----------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat %>% # new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  distinct(externalDataReference, .keep_all = TRUE) %>% # Changed to distinct due to NA coercion
  add_column(sample = "COVIDCNS", 
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference,# ID
         sample,
         startDate,
         endDate,
         dem.what_is_your_first_language,
         dem.what_is_your_first_language.txt,
         dem.is_english_your_first_language
         )  %>% 
   
 add_numeric_1(exclude = exclude_cols_numeric)
 
# Inspect colnames
covidcns_dat_id %>%
  colnames()


## ----COVID CNS number excluded----------------------------------------------------------------------------------------
# Inspect dimensions of new data set
covidcns_dat_id %>%
  dim()
# Inspect number of rows dropped
covidcns_excluded <- dim(covidcns_dat_id)[1]-dim(covidcns_dat)[1]
covidcns_excluded


## ----COVID CNS inspect numeric variables------------------------------------------------------------------------------
covidcns_dat_id %>%
  select(all_of(ends_with("numeric"))) %>%
  tbl_summary(missing_text = "Missing")


## ----COVID CNS inspect missingness------------------------------------------------------------------------------------
 covidcns_miss_map <- covidcns_dat_id %>% 
   missmap()
 covidcns_miss_map


## ----Rename covidcns_dat_id to dat------------------------------------------------------------------------------------
dat <- covidcns_dat_id


## ----Recode NA values-------------------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(ends_with("numeric"),
                ~case_when(
                  . == -55 ~ -555,
                  . == -77 ~ -777,
                  . == -88 ~ -888,
                  . == -99 ~ -999,
                  TRUE ~ .)))


## ----add English first language to dem.what_is_your_first_language_numeric--------------------------------------------
dat <- dat %>%
  mutate(
    dem.what_is_your_first_language_numeric = 
      if_else(
        dem.is_english_your_first_language_numeric == 1,
        true = 5,
        false = dem.what_is_your_first_language_numeric,
        missing = NA_real_
      )
  )

#check 
dat %>% 
  select(dem.is_english_your_first_language_numeric,
         dem.what_is_your_first_language_numeric)


## ----vector of numeric values 1_24------------------------------------------------------------------------------------
values_numeric_24 <- c(
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8,
  9,
  10,
  11,
  12,
  13,
  14,
  15,
  16,
  17,
  18,
  19,
  20,
  21,
  22,
  23,
  24,
  -777,
  NA
  )
values_numeric_24


## ----vector numeric variables _24-------------------------------------------------------------------------------------
variables_numeric_24 <- c(
    "dem.what_is_your_first_language_numeric"
  )
variables_numeric_24


## ----imp_check numeric variables _24----------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_numeric_24,
          values = values_numeric_24)


## ----vector of numeric values 0---------------------------------------------------------------------------------------
values_numeric_0 <- c(
  0,
  1,
  -999,
  NA
  )
values_numeric_0


## ----vector numeric variables 0---------------------------------------------------------------------------------------
variables_numeric_0 <- c(
    "dem.is_english_your_first_language_numeric"
  )
variables_numeric_0


## ----imp_check numeric variables 0------------------------------------------------------------------------------------
imp_check(data = dat,
           variables = variables_numeric_0,
          values = values_numeric_0)


## ----add English first language to dem.what_is_your_first_language categorical label----------------------------------
dat <- dat %>%
  mutate(
    dem.what_is_your_first_language = 
      case_when(
        dem.what_is_your_first_language_numeric == 5 ~ "English",
        TRUE ~ as.character(dem.what_is_your_first_language)
      )
  )
#check 
dat %>% 
  select(dem.what_is_your_first_language,
         dem.what_is_your_first_language_numeric)


## ----vector categorical values 24-------------------------------------------------------------------------------------
values_categorical_24 <- c(
  "Arabic",
  "Bengali",
  "Chinese",
  "Danish",
  "English",
  "French",
  "Gaelic",
  "German",
  "Hindi",
  "Japanese",
  "Javanese",
  "Korean",
  "Lahnda",
  "Mandarin",
  "Polish",
  "Portuguese",
  "Punjabi",
  "Russian",
  "Spanish",
  "Tamil",
  "Turkish",
  "Vietnamese",
  "Welsh",
  "Other",
  "Seen but not answered",
  NA
  )
values_categorical_24


## ----vector categorical variables 24----------------------------------------------------------------------------------
variables_categorical_24 <-
  c(
    "dem.what_is_your_first_language"
    )
variables_categorical_24


## ----imp_check categorical variables 24-------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical_24,
          values = values_categorical_24)


## ----vector categorical values----------------------------------------------------------------------------------------
values_categorical_0 <- c(
  "No",
  "Yes",
  "Prefer not to say",
  NA)
values_categorical_0


## ----vector categorical variables-------------------------------------------------------------------------------------
variables_categorical_0 <-
  c(
    "dem.is_english_your_first_language"
    )
variables_categorical_0


## ----imp_check categorical variables----------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical_0,
          values = values_categorical_0)


## ----check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned variables to a .rds file---------------------------------------------------------------------------
dat %>% 
  filter(sample == "COVIDCNS") %>%  # select only COVID CNS participants
  saveRDS(
    file = paste0(ilovecovidcns,"/data/latest_freeze/baseline/dem_language_covidcns_clean.rds")
    )

