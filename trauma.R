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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/trauma_covid_cns.rds")
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
         trauma.support_information_section_asks,
         trauma.i_felt_loved,
         trauma.bruises_left_hard_people,
         trauma.felt_family_hated,
         trauma.someone_molested_me,
         trauma.doctor_needed,
         trauma.i_have_been_in_a_confiding_relationship,
         trauma.violence_partner_expartner_deliberately,
         trauma.extent_partner_expartner_repeatedly,
         trauma.forced_sex_partner_expartner,
         trauma.pay_money_rent_mortgage,
         trauma.i_have_been_in_a_confiding_relationship.1,
         trauma.violence_partner_expartner_deliberately.1,
         trauma.extent_partner_expartner_repeatedly.1,
         trauma.forced_sex_partner_expartner.1,
         trauma.pay_money_rentmortgage_payment,
         trauma.sexual_assault_stranger_victim,
         trauma.attacked_mugged_robbed_victim,
         trauma.accident_believed_time_lifethreatening,
         trauma.witnessed_a_sudden_violent_death,
         trauma.been_diagnosed_with_a_lifethreatening_illness,
         trauma.combat_involved_exposed_warzone
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
  "Seen but not answered",
  "Skip",
  "Continue",
  NA
)

vals_cat_2 <- c(
  "Prefer not to answer",
  "Seen but not answered",
  "Never true",
  "Rarely true",
  "Sometimes true",
  "Often true",
  "Very often true",
  NA
)

vals_cat_3 <- c(
  "Prefer not to answer",
  "Seen but not answered",
  "Never",
  "Yes, but not in the last 12 months",
  "Yes, within the last 12 months",
  NA
)


## ----List categorical values vectors----------------------------------------------------------------------------------
values_cat_list <- list(
  vals_cat_1,
  vals_cat_2,
  vals_cat_2,
  vals_cat_2,
  vals_cat_2,
  vals_cat_2,
  vals_cat_3,
  vals_cat_3,
  vals_cat_3,
  vals_cat_3,
  vals_cat_3,
  vals_cat_2,
  vals_cat_2,
  vals_cat_2,
  vals_cat_2,
  vals_cat_2,
  vals_cat_3,
  vals_cat_3,
  vals_cat_3,
  vals_cat_3,
  vals_cat_3,
  vals_cat_3
)


## ----Vector categorical variables-------------------------------------------------------------------------------------
variables_cat <- c(
  "trauma.support_information_section_asks",
  "trauma.i_felt_loved",
  "trauma.bruises_left_hard_people",
  "trauma.felt_family_hated",
  "trauma.someone_molested_me",
  "trauma.doctor_needed",
  "trauma.i_have_been_in_a_confiding_relationship",
  "trauma.violence_partner_expartner_deliberately",
  "trauma.extent_partner_expartner_repeatedly",
  "trauma.forced_sex_partner_expartner",
  "trauma.pay_money_rent_mortgage",
  "trauma.i_have_been_in_a_confiding_relationship.1",
  "trauma.violence_partner_expartner_deliberately.1",
  "trauma.extent_partner_expartner_repeatedly.1",
  "trauma.forced_sex_partner_expartner.1",
  "trauma.pay_money_rentmortgage_payment",
  "trauma.sexual_assault_stranger_victim",
  "trauma.attacked_mugged_robbed_victim",
  "trauma.accident_believed_time_lifethreatening",
  "trauma.witnessed_a_sudden_violent_death",
  "trauma.been_diagnosed_with_a_lifethreatening_illness",
  "trauma.combat_involved_exposed_warzone"
)


## ----Set list names cat-----------------------------------------------------------------------------------------------
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
  -777,
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

vals_num_3 <- c(
  0,
  1,
  2,
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
  vals_num_2,
  vals_num_2,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_2,
  vals_num_2,
  vals_num_2,
  vals_num_2,
  vals_num_2,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3
)


## ----Vector numeric variables-----------------------------------------------------------------------------------------
variables_num <- c(
  "trauma.support_information_section_asks_numeric",
  "trauma.i_felt_loved_numeric",
  "trauma.bruises_left_hard_people_numeric",
  "trauma.felt_family_hated_numeric",
  "trauma.someone_molested_me_numeric",
  "trauma.doctor_needed_numeric",
  "trauma.i_have_been_in_a_confiding_relationship_numeric",
  "trauma.violence_partner_expartner_deliberately_numeric",
  "trauma.extent_partner_expartner_repeatedly_numeric",
  "trauma.forced_sex_partner_expartner_numeric",
  "trauma.pay_money_rent_mortgage_numeric",
  "trauma.i_have_been_in_a_confiding_relationship.1_numeric",
  "trauma.violence_partner_expartner_deliberately.1_numeric",
  "trauma.extent_partner_expartner_repeatedly.1_numeric",
  "trauma.forced_sex_partner_expartner.1_numeric",
  "trauma.pay_money_rentmortgage_payment_numeric",
  "trauma.sexual_assault_stranger_victim_numeric",
  "trauma.attacked_mugged_robbed_victim_numeric",
  "trauma.accident_believed_time_lifethreatening_numeric",
  "trauma.witnessed_a_sudden_violent_death_numeric",
  "trauma.been_diagnosed_with_a_lifethreatening_illness_numeric",
  "trauma.combat_involved_exposed_warzone_numeric"
)


## ----Set list names---------------------------------------------------------------------------------------------------
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


## ----Inspect incorrect variable---------------------------------------------------------------------------------------
change_vars <- c(
  "trauma.i_have_been_in_a_confiding_relationship.1",
  "trauma.violence_partner_expartner_deliberately.1",
  "trauma.extent_partner_expartner_repeatedly.1",
  "trauma.forced_sex_partner_expartner.1",
  "trauma.pay_money_rentmortgage_payment"
)

dat %>%
  select(all_of(change_vars)) %>%
  tbl_summary(missing_text = "Missing")


## ----Recode incorrect variable----------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(all_of(change_vars),
                ~recode_factor(.,
                               "Prefer not to answer" = "Prefer not to answer",
                               "Seen but not answered" = "Seen but not answered",
                               "Never true" = "Never true",
                               "Rarely true" = "Rarely true",
                               "Sometimes true" = "Sometimes true",
                               "Often" = "Often true",
                               "Very often true" = "Very often true",
                               missing = NA_character_
                              )
                )
         )


## ----Recheck variable coding------------------------------------------------------------------------------------------
dat %>%
  select(all_of(change_vars)) %>%
  tbl_summary(missing_text = "Missing")


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/trauma_covidcns_clean.rds")
    )

