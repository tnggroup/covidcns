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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/dem_covid_cns.rds")
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
  distinct(externalDataReference, .keep_all = TRUE) %>% # Changed to distinct due to NA coercion
  add_column(sample = "CNS", 
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         dem.medical_history_birth_relevant,
         dem.what_gender_do_you_identify_with,
         dem.do_you_consider_yourself_to_be_transgender,
         dem.what_is_your_sexual_orientation,
         dem.have_you_ever_been_pregnant
         ) %>%
  add_numeric_1(exclude = exclude_cols_numeric)

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


## ----vector of numeric values 0_1-------------------------------------------------------------------------------------
values_numeric_0 <- c(
  0,
  1,
  -777,
  NA
  )
values_numeric_0


## ----vector numeric variables 0_1-------------------------------------------------------------------------------------
variables_numeric_0 <- c(
    "dem.medical_history_birth_relevant_numeric",
    "dem.do_you_consider_yourself_to_be_transgender_numeric",
    "dem.have_you_ever_been_pregnant_numeric"
  )
variables_numeric_0


## ----imp_check numeric variables 0_1----------------------------------------------------------------------------------
imp_check(data = dat,
           variables = variables_numeric_0,
          values = values_numeric_0)


## ----vector of numeric values 0_3-------------------------------------------------------------------------------------
values_numeric_0_3 <- c(
  0,
  1,
  2,
  3,
  -777,
  -888,
  -999,
  NA
  )
values_numeric_0_3


## ----numeric variables vector 0_3-------------------------------------------------------------------------------------
variables_numeric_0_3 <- c(
    "dem.what_gender_do_you_identify_with_numeric"
  )
variables_numeric_0_3


## ----imp_check numeric variables 0_3----------------------------------------------------------------------------------
imp_check(data = dat,
           variables = variables_numeric_0_3,
          values = values_numeric_0_3)


## ----vector of numeric values 1_5-------------------------------------------------------------------------------------
values_numeric_5 <- c(
  0,
  1,
  2,
  3,
  4,
  -777,
  -999,
  NA
  )
values_numeric_5


## ----numeric variables vector 1_5-------------------------------------------------------------------------------------
variables_numeric_5 <- c(
    "dem.what_is_your_sexual_orientation_numeric"
  )
variables_numeric_5


## ----imp_check numeric variables 1_5----------------------------------------------------------------------------------
imp_check(data = dat,
           variables = variables_numeric_5,
          values = values_numeric_5)


## ----vector of categorical labels sex---------------------------------------------------------------------------------
values_categorical_sex <- c(
  "Male",
  "Female",
  "Seen but not answered",
  NA
  )
values_categorical_sex


## ----vector categorical variables sex---------------------------------------------------------------------------------
variables_categorical_sex <- c(
    "dem.medical_history_birth_relevant"
  )
variables_categorical_sex


## ----imp_check categorical variables sex------------------------------------------------------------------------------
imp_check(data = dat,
           variables = variables_categorical_sex,
          values = values_categorical_sex)


## ----vector of categorical labels y/n---------------------------------------------------------------------------------
values_categorical_0 <- c(
  "No",
  "Yes",
  "Seen but not answered",
  NA
  )
values_categorical_0


## ----vector categorical variables  y/n--------------------------------------------------------------------------------
variables_categorical_0 <- c(
    "dem.do_you_consider_yourself_to_be_transgender",
    "dem.have_you_ever_been_pregnant"
  )
variables_categorical_0


## ----imp_check categorical variables  y/n-----------------------------------------------------------------------------
imp_check(data = dat,
           variables = variables_categorical_0,
          values = values_categorical_0)


## ----vector of categorical labels cat 0_3-----------------------------------------------------------------------------
values_categorical_0_3 <- c(
  "Male",
  "Female",
  "Non-binary",
  "Self-define",
  "Seen but not answered",
  "Don't know",
  "Prefer not to answer",
  NA
  )
values_categorical_0_3


## ----vector categorical variables  cat 0_3----------------------------------------------------------------------------
variables_categorical_0_3 <- c(
    "dem.what_gender_do_you_identify_with"
  )
variables_categorical_0_3


## ----imp_check categorical variables  cat 0_3-------------------------------------------------------------------------
imp_check(data = dat,
           variables = variables_categorical_0_3,
          values = values_categorical_0_3)


## ----Recode incorrect categorical variables cat 0_3-------------------------------------------------------------------
dat <- dat %>%
  mutate(
    across(all_of(variables_categorical_0_3), 
           .fns = ~dplyr::recode(.,
             "Prefer to self-define (please tell us more):" = "Self-define"
            )
          )
  )


## ----Check correction applied cat 0_3---------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_categorical_0_3)) %>%
  freq()


## ----vector of categorical labels cat 1_5-----------------------------------------------------------------------------
values_categorical_5 <- c(
  "Heterosexual",
  "Homosexual",
  "Bisexual",
  "Asexual",
  "Other",
  "Seen but not answered",
  "Don't know",
  "Prefer not to answer",
  NA
  )
values_categorical_5


## ----vector categorical variables cat 1_5-----------------------------------------------------------------------------
variables_categorical_5 <- c(
    "dem.what_is_your_sexual_orientation"
  )
variables_categorical_5


## ----imp_check categorical variables cat 1_5--------------------------------------------------------------------------
imp_check(data = dat,
           variables = variables_categorical_5,
          values = values_categorical_5)


## ----Recode incorrect categorical variables---------------------------------------------------------------------------
dat <- dat %>%
  mutate(
    across(all_of(variables_categorical_5), 
           .fns = ~dplyr::recode(.,
             "Prefer not to say" = "Prefer not to answer"
            )
          )
  )


## ----Check correction applied-----------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_categorical_5)) %>%
  freq()


## ----check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----CNS EDGI save excluded participants------------------------------------------------------------------------------
cns_excluded <- as.data.frame(cns_excluded)
colnames(cns_excluded) <- c("Number of Participants Excluded")
rownames(cns_excluded) <- c("CNS")
cns_excluded


## ----Write cleaned CNS variables to a .rds file-----------------------------------------------------------------------
dat %>% 
  filter(sample == "CNS") %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/sex_gender_sexuality_covidcns_clean.rds")
    )

