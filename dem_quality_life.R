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
source(file = "scripts/functions/imp_check_1.R")


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
  remove_duplicates("externalDataReference") %>% # Remove duplicate IDs
  add_column(sample = "COVIDCNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         dem.what_is_your_current_maritalrelationship_status,
         dem.quality_rate_life,
         dem.energy_everyday_life,
         dem.opportunity_leisure_activities,
         dem.happy_general_health
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
llst <- sapply(dat, levels)


## ----Vector categorical values----------------------------------------------------------------------------------------
vals_cat_1 <- c(
  "Prefer not to answer",
  "Seen but not answered",
  "Single",
  "Steady not living together",
  "Steady living together",
  "Married",
  "Separated",
  "Divorced",
  "Widowed",
  "Other",
  NA
)
vals_cat_2 <- c(
  "Prefer not to say",
  "Seen but not answered",
  "Very poor",
  "Poor",
  "Neither good nor bad",
  "Good",
  "Very good",
  NA
)
vals_cat_3 <- c(
  "Prefer not to say",
  "Seen but not answered",
  "Not at all",
  "A little",
  "Moderately",
  "Mostly",
  "Completely",
  NA
)
vals_cat_4 <- c(
  "Prefer not to say",
  "Seen but not answered",
  "Very unhappy",
  "Unhappy",
  "Neither happy nor unhappy",
  "Good",
  "Very good",
  NA
)


## ----List categorical values vectors----------------------------------------------------------------------------------
values_cat_list <- list(
  vals_cat_1,
  vals_cat_2,
  vals_cat_3,
  vals_cat_3,
  vals_cat_4
)


## ----Vector categorical variables-------------------------------------------------------------------------------------
variables_cat <- c(
  "dem.what_is_your_current_maritalrelationship_status",
  "dem.quality_rate_life",
  "dem.energy_everyday_life",
  "dem.opportunity_leisure_activities",
  "dem.happy_general_health"
)


## ----Set list names categorical---------------------------------------------------------------------------------------
names(values_cat_list) <- variables_cat


## ----Imp_check categorical variables----------------------------------------------------------------------------------
# Create empty list
imp_list_cat <- list()
# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(variables_cat)) {
  imp_list_cat[i] <- imp_check_1(data = dat,
                                 variables = names(values_cat_list)[i],
                                 values = values_cat_list[[i]]) 
}
# Name list with var names to correspond to imp_messages
names(imp_list_cat) <- variables_cat
# View list of imp_messages with corresponding var names
print(imp_list_cat)


## ----Summary table categorical variables------------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(variables_cat),
    missing_text = "Missing")


## ----Vector numeric values--------------------------------------------------------------------------------------------
vals_num_1 <- c(
  0,
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  -777,
  -999,
  NA
)
vals_num_2 <- c(
  0,
  1,
  2,
  3,
  4,
  -777,
  -999,
  NA
)


## ----List numeric values vectors--------------------------------------------------------------------------------------
values_num_list <- list(
  vals_num_1,
  vals_num_2,
  vals_num_2,
  vals_num_2,
  vals_num_2
)


## ----Vector numeric variables-----------------------------------------------------------------------------------------
variables_num <- c(
  "dem.what_is_your_current_maritalrelationship_status_numeric",
  "dem.quality_rate_life_numeric",
  "dem.energy_everyday_life_numeric",
  "dem.opportunity_leisure_activities_numeric",
  "dem.happy_general_health_numeric"
)


## ----Set list names numeric-------------------------------------------------------------------------------------------
names(values_num_list) <- variables_num


## ----Imp_check numeric variables--------------------------------------------------------------------------------------
# Create empty list
imp_list_num <- list()
# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(variables_num)) {
  imp_list_num[i] <- imp_check_1(data = dat,
                                 variables = names(values_num_list)[i],
                                 values = values_num_list[[i]]) 
}
# Name list with var names to correspond to imp_messages
names(imp_list_num) <- variables_num
# View list of imp_messages with corresponding var names
print(imp_list_num)


## ----Summary table numeric variables----------------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(variables_num),
    missing_text = "Missing")


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/dem_quality_life_covidcns_clean.rds")
    )

