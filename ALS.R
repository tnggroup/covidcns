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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/als_covid_cns.rds")
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
  "als.noticed_limb_weakness_list.txt"
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
         "als.limb_weakness_past_month",
         "als.noticed_limb_weakness_list.txt",
         "als.handwriting_change_noticed",
         "als.have_you_noticed_any_difficulty_walking",
         "als.do_you_have_any_difficulty_climbing_stairs",
         "als.required_start_question_covid19"
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


## ----Vector list categorical values-----------------------------------------------------------------------------------
values_cat_list <- list(
  vals_limb_weak <- c(
    "Seen but not answered",
    "No",
    "Yes", 
    NA
  ),
  
  vals_writing <- c(
    "Seen but not answered",
    "Normal",
    "Slow or sloppy, all words are legible",
    "Not all words are legible",
    "Able to grip pen",
    "Unable to grip pen",
    NA
  ),
  
  vals_walking <- c(
    "Seen but not answered",
    "Normal",
    "Early walking difficulty",
    "Need a person, crutch or frame to help you walk",
    "Able to stand for transfers",
    "No purposeful leg movements",
    NA
  ),
  
  vals_stairs <- c(
    "Seen but not answered",
    "Normal",
    "Slow",
    "Mild unsteadiness or fatigue",
    "Need assistance of a person or a rail",
    "Cannot climb stairs",
    NA
  ),
  
  vals_covid <- c(
    "Don't know",
    "Seen but not answered",
    "No - I had these symptoms or experiences before I had COVID-19",
    "Yes - it started after I had COVID-19 or during my infection with it",
    NA
  )
)


## ----Set list names categorical variables-----------------------------------------------------------------------------
names(values_cat_list) <- c(
  "als.limb_weakness_past_month",
  "als.handwriting_change_noticed",
  "als.have_you_noticed_any_difficulty_walking",
  "als.do_you_have_any_difficulty_climbing_stairs",
  "als.required_start_question_covid19"
)


## ----Imp_check categorical variables----------------------------------------------------------------------------------
# Create empty list
imp_list_cat <- list()
# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(names(values_cat_list))) {
  imp_list_cat[i] <- imp_check_1(data = dat,
                                 variables = names(values_cat_list)[i],
                                 values = values_cat_list[[i]]) 
}
# Name list with var names to correspond to imp_messages
names(imp_list_cat) <- names(values_cat_list)
# View list of imp_messages with corresponding var names
print(imp_list_cat)


## ----Summary table categorical variables------------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(names(values_cat_list)),
    missing_text = "Missing")


## ----Vector list numeric values---------------------------------------------------------------------------------------
values_num_list <- list(
  
  vals_limb_weak <- c(
    0,
    1,
    -777,
    NA
  ),
  
  vals_writing <- c(
    0,
    1,
    2,
    3,
    4,
    -777,
    NA
  ),
  
  vals_walking <- c(
    0,
    1,
    2,
    3,
    4,
    -777,
    NA
  ),
  
  vals_stairs <- c(
    0,
    1,
    2,
    3,
    4,
    -777,
    NA
  ),
  
  vals_covid <- c(
    0,
    1,
    -777,
    -888,
    NA
  )
)


## ----Set list names numeric variables---------------------------------------------------------------------------------
names(values_num_list) <- c(
  "als.limb_weakness_past_month_numeric",
  "als.handwriting_change_noticed_numeric",
  "als.have_you_noticed_any_difficulty_walking_numeric",
  "als.do_you_have_any_difficulty_climbing_stairs_numeric",
  "als.required_start_question_covid19_numeric"
)


## ----Imp_check numeric variables--------------------------------------------------------------------------------------
# Create empty list
imp_list_num <- list()
# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(names(values_num_list))) {
  imp_list_num[i] <- imp_check_1(data = dat,
                                 variables = names(values_num_list)[i],
                                 values = values_num_list[[i]]) 
}
# Name list with var names to correspond to imp_messages
names(imp_list_num) <- names(values_num_list)
# View list of imp_messages with corresponding var names
print(imp_list_num)


## ----Summary table numeric variables----------------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(names(values_num_list)),
    missing_text = "Missing")


## ----Inspect incorrect climbing stairs variable-----------------------------------------------------------------------
dat %>%
  select(
    als.do_you_have_any_difficulty_climbing_stairs,
    als.do_you_have_any_difficulty_climbing_stairs_numeric) %>%
  tbl_summary(missing_text = "Missing")


## ----Recode incorrect variable----------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(als.do_you_have_any_difficulty_climbing_stairs,
                ~recode_factor(.,
                  "0" = "Normal",
                  "1" = "Slow",
                  "2" = "Mild unsteadiness or fatigue",
                  "3" = "Need assistance of a person or a rail",
                  "-77" = "Seen but not answered")
                )
         )


## ----Recheck climbing stairs coding-----------------------------------------------------------------------------------
dat %>%
  select(
    als.do_you_have_any_difficulty_climbing_stairs,
    als.do_you_have_any_difficulty_climbing_stairs_numeric) %>%
  tbl_summary(missing_text = "Missing")


## ----Recode climbing stairs numeric variable--------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(als.do_you_have_any_difficulty_climbing_stairs_numeric,
                ~case_when(
                  . == 5 ~ 4,
                    TRUE ~ .)
                )
         )


## ----Recheck stairs variable coding-----------------------------------------------------------------------------------
dat %>%
  select(
    als.do_you_have_any_difficulty_climbing_stairs,
    als.do_you_have_any_difficulty_climbing_stairs_numeric) %>%
  tbl_summary(missing_text = "Missing")


## ----Recode appetite variable-----------------------------------------------------------------------------------------
levels(dat$als.do_you_have_any_difficulty_climbing_stairs)[dat$als.do_you_have_any_difficulty_climbing_stairs_numeric == 4] <- "Cannot climb stairs"


## ----Recheck new level coding-----------------------------------------------------------------------------------------
dat %>%
  select(
    als.do_you_have_any_difficulty_climbing_stairs,
    als.do_you_have_any_difficulty_climbing_stairs_numeric) %>%
  tbl_summary(missing_text = "Missing")


## ----Recode to cannot climb stairs------------------------------------------------------------------------------------
dat$als.do_you_have_any_difficulty_climbing_stairs[dat$als.do_you_have_any_difficulty_climbing_stairs_numeric == 4] <- "Cannot climb stairs"


## ----Recheck stairs new coding----------------------------------------------------------------------------------------
dat %>%
  select(
    als.do_you_have_any_difficulty_climbing_stairs,
    als.do_you_have_any_difficulty_climbing_stairs_numeric) %>%
  tbl_summary(missing_text = "Missing")


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/als_covidcns_clean.rds")
    )

