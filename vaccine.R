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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/vaccine_covid_cns.rds")
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
  "vaccine.date_day_ddmmyyyy_doseplease.txt",
  "vaccine.date_day_ddmmyyyy_doseplease.txt.1",
  "vaccine.approximately_day_ddmmyyyy_enter.txt"
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
         vaccine.have_you_had_a_covid19_vaccine,
         vaccine.date_day_ddmmyyyy_doseplease.txt,
         vaccine.have_you_had_your_second_dose,
         vaccine.date_day_ddmmyyyy_doseplease.txt.1,
         vaccine.which_vaccine,
         vaccine.invited_covid19_vaccine,
         vaccine.are_you_thinking_of_accepting_the_invitation,
         vaccine.approximately_day_ddmmyyyy_enter,
         vaccine.approximately_day_ddmmyyyy_enter.txt,
         vaccine.are_you_thinking_of_rejecting_the_invitation,
         vaccine.reason_options_provide_selecting
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
  "Don't know",
  "Seen but not answered",
  NA
  )
values_categorical


## ----Vector categorical variables-------------------------------------------------------------------------------------
variables_categorical <- c(
  "vaccine.have_you_had_a_covid19_vaccine",
  "vaccine.have_you_had_your_second_dose",
  "vaccine.invited_covid19_vaccine",
  "vaccine.are_you_thinking_of_accepting_the_invitation",
  "vaccine.are_you_thinking_of_rejecting_the_invitation"
  )
variables_categorical


## ----Imp_check categorical variables----------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical,
          values = values_categorical)


## ----Vector categorical values.1--------------------------------------------------------------------------------------
values_categorical.1 <- c(
  "Don't know",
  "Seen but not answered",
  "Oxford/AstraZeneca",
  "Pfizer/BioNTech",
  "Moderna", 
  NA
  )
values_categorical.1


## ----Imp_check categorical variables.1--------------------------------------------------------------------------------
imp_check(data = dat,
          variables = "vaccine.which_vaccine",
          values = values_categorical.1)


## ----Vector categorical values.2--------------------------------------------------------------------------------------
values_categorical.2 <- c(
  "Date:",
  "Don't know",
  "Seen but not answered",
  NA
  )
values_categorical


## ----Imp_check categorical variables.2--------------------------------------------------------------------------------
imp_check(data = dat,
          variables = "vaccine.approximately_day_ddmmyyyy_enter",
          values = values_categorical)


## ----Vector categorical values.3--------------------------------------------------------------------------------------
values_categorical.3 <- c(
  "I prefer not to say why",
  "Seen but not answered",
  "Medical reasons",
  "I prefer not to be vaccinated",
  "Other reason",
  NA
  )
values_categorical.3


## ----Imp_check categorical variables.3--------------------------------------------------------------------------------
imp_check(data = dat,
          variables = "vaccine.reason_options_provide_selecting",
          values = values_categorical.3)


## ----Vector numeric values--------------------------------------------------------------------------------------------
values_numeric <- c(
  0,
  1,
  -777,
  -888,
  NA
  )
values_numeric


## ----Vector numeric variables-----------------------------------------------------------------------------------------
variables_numeric <- c(
  "vaccine.have_you_had_a_covid19_vaccine_numeric",
  "vaccine.have_you_had_your_second_dose_numeric",
  "vaccine.invited_covid19_vaccine_numeric",
  "vaccine.are_you_thinking_of_accepting_the_invitation_numeric",
  "vaccine.approximately_day_ddmmyyyy_enter_numeric",
  "vaccine.are_you_thinking_of_rejecting_the_invitation_numeric"
  )
variables_numeric


## ----Imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_numeric,
          values = values_numeric)


## ----Vector numeric values.1------------------------------------------------------------------------------------------
values_numeric.1 <- c(
  1,
  2,
  3,
  -777,
  -888,
  NA
  )
values_numeric.1


## ----Imp_check numeric variables.1------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = "vaccine.which_vaccine_numeric",
          values = values_numeric.1)


## ----Vector numeric values.2------------------------------------------------------------------------------------------
values_numeric.2 <- c(
  1,
  2,
  3,
  -777,
  -999,
  NA
  )
values_numeric.2


## ----Imp_check numeric variables.2------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = "vaccine.reason_options_provide_selecting_numeric",
          values = values_numeric.2)


## ----Inspect incorrect variable---------------------------------------------------------------------------------------
dat %>%
  select(
    vaccine.which_vaccine,
    vaccine.which_vaccine_numeric) %>%
  tbl_summary(missing_text = "Missing")


## ----Recode incorrect variable----------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(vaccine.which_vaccine_numeric,
                ~case_when(
                  . == 5 ~ 3,
                  TRUE ~ .)))


## ----Recheck variable coding------------------------------------------------------------------------------------------
dat %>%
  select(
    vaccine.which_vaccine,
    vaccine.which_vaccine_numeric) %>%
  tbl_summary(missing_text = "Missing")


## ----Vector date variables--------------------------------------------------------------------------------------------
variables_date <- c(
  "vaccine.date_day_ddmmyyyy_doseplease.txt",
  "vaccine.date_day_ddmmyyyy_doseplease.txt.1",
  "vaccine.approximately_day_ddmmyyyy_enter.txt"
)


## ----COVIDCNS inspect dates initial-----------------------------------------------------------------------------------
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



## ----COVIDCNS inspect dates-------------------------------------------------------------------------------------------
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
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/vaccine_covidcns_clean.rds")
    )

