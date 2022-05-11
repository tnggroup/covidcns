## ----setup, include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  comment = '',
  prompt = FALSE,
  cache = FALSE
  )


## ----Clear global environment-----------------------------------------------------------------------------------------
rm(list=ls())


## ----Read in functions------------------------------------------------------------------------------------------------
source(file = "scripts/functions/add_numeric.R")
source(file = "scripts/functions/sumscores.R")
source(file = "scripts/functions/package_check.R")
source(file = "scripts/functions/remove_duplicates.R")
source(file = "scripts/functions/imp_check.R")


## ----Install load dependencies----------------------------------------------------------------------------------------
packages = c(
  "summarytools",
  "sjlabelled",
  "Amelia",
  "knitr",
  "gtsummary",
  "tidyverse"
  )
package_check(packages)


## ----Get system date--------------------------------------------------------------------------------------------------
date <- Sys.Date()
date


## ----Source the credentials file--------------------------------------------------------------------------------------
source("scripts/credentials/paths.R")


## ----COVID CNS employment data----------------------------------------------------------------------------------------
covidcns_dat <- read_rds(file = 
                   paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/impact_covid_cns.rds")
                 )

# check
covidcns_dat %>% 
  dim()

covidcns_dat %>% 
  colnames()


## ----COVID CNS specify excluded columns-------------------------------------------------------------------------------
exclude_cols_numeric <- c(
  "ID",
  "sample",
  "startDate",
  "endDate",
  "impact.othertext.txt"
  )


## ----COVID CNS select-------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  remove_duplicates("externalDataReference") %>% # Remove duplicate IDs
  add_column(sample = "COVID-CNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         startDate,
         endDate,
         sample,
         impact.fulltime_employed,
         impact.unemployed,
         impact.stayathome_parent_or_carer,
         impact.contract_or_freelance_work,
         impact.receiving_state_income,
         impact.student_,
         impact.prefer_not_to_say,
         impact.parttime_employed,
         impact.zerohours_contract,
         impact.selfemployed,
         impact.small_buisness_owner,
         impact.retired,
         impact.student_.1,
         #employment chnage: 
         impact.my_employment_status_has_not_changed,
         impact.become_unemployed,
         impact.become_employed,
         impact.reduction_in_hours,
         impact.increased_hours,
         impact.benefits_increased,
         impact.furloughed_or_paid_leave_,
         impact.paid_leave_furloughed,
         impact.other,
         impact.changes_in_duties_or_responsibilities,
         impact.reduction_in_salary,
         impact.increased_salary,
         impact.benefits_decreased,
         impact.furloughed_or_paid_leave_.1,
         impact.taking_unpaid_leave,
         impact.prefer_not_to_say.1,
         impact.losing_skip_risk_prefer,
         impact.symptoms_impacted_carry_employment,
         impact.employment_status_changed_contracting
         ) %>%
  add_numeric(exclude = exclude_cols_numeric)
# Inspect colnames
covidcns_dat_id %>%
  colnames()


## ----COVID CNS number excluded----------------------------------------------------------------------------------------
# Inspect dimensions of new data set
covidcns_dat_id %>%
  dim()
# Inspect number of rows dropped
covidcns_excluded <- dim(covidcns_dat_id)[1] - dim(covidcns_dat)[1]
covidcns_excluded


## ----COVID CNS inspect numeric variables------------------------------------------------------------------------------
covidcns_dat_id %>%
  select(all_of(ends_with("numeric"))) %>%
  tbl_summary(missing_text = "Missing")


## ----COVID CNS inspect missingness------------------------------------------------------------------------------------
covidcns_miss_map <- covidcns_dat_id %>% 
  missmap()
covidcns_miss_map


## ----Recode NA values-------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat_id %>%
  mutate(across(ends_with("numeric"),
                ~case_when(
                  . == -55 ~ -555,
                  . == -77 ~ -777,
                  . == -88 ~ -888,
                  . == -99 ~ -999,
                  TRUE ~ .)))


## ----vector of numeric values-----------------------------------------------------------------------------------------
values_numeric_0_1 <- c(
  0,
  1,
  -777,
  NA
  )
values_numeric_0_1


## ----vector numeric variables-----------------------------------------------------------------------------------------
variables_numeric_0_1 <- c(
  "impact.fulltime_employed_numeric",
  "impact.unemployed_numeric",
  "impact.stayathome_parent_or_carer_numeric",
  "impact.contract_or_freelance_work_numeric",
  "impact.receiving_state_income_numeric",
  "impact.student__numeric",
  "impact.prefer_not_to_say_numeric",
  "impact.parttime_employed_numeric",
  "impact.zerohours_contract_numeric",
  "impact.selfemployed_numeric",
  "impact.small_buisness_owner_numeric",
  "impact.retired_numeric",
  "impact.student_.1_numeric",
  "impact.my_employment_status_has_not_changed_numeric",
  "impact.become_unemployed_numeric",
  "impact.become_employed_numeric",
  "impact.reduction_in_hours_numeric",
  "impact.increased_hours_numeric",
  "impact.benefits_increased_numeric",
  "impact.furloughed_or_paid_leave__numeric",
  "impact.paid_leave_furloughed_numeric",
  "impact.other_numeric",
  "impact.changes_in_duties_or_responsibilities_numeric",
  "impact.reduction_in_salary_numeric",
  "impact.increased_salary_numeric",
  "impact.benefits_decreased_numeric",
  "impact.furloughed_or_paid_leave_.1_numeric",
  "impact.taking_unpaid_leave_numeric",
  "impact.prefer_not_to_say.1_numeric",
  "impact.my_employment_status_has_not_changed_numeric",
  "impact.become_unemployed_numeric",
  "impact.become_employed_numeric",
  "impact.reduction_in_hours_numeric",
  "impact.increased_hours_numeric",
  "impact.benefits_increased_numeric",
  "impact.furloughed_or_paid_leave__numeric",
  "impact.paid_leave_furloughed_numeric",
  "impact.other_numeric",
  "impact.changes_in_duties_or_responsibilities_numeric",
  "impact.reduction_in_salary_numeric",
  "impact.increased_salary_numeric",
  "impact.benefits_decreased_numeric",
  "impact.furloughed_or_paid_leave_.1_numeric",
  "impact.taking_unpaid_leave_numeric",
  "impact.prefer_not_to_say.1_numeric",
  "impact.symptoms_impacted_carry_employment_numeric"
  )
variables_numeric_0_1


## ----imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = covidcns_dat_id,
           variables = variables_numeric_0_1,
          values = values_numeric_0_1)


## ----vector categorical values----------------------------------------------------------------------------------------
values_categorical_0_1 <- c(
  "Yes",
  "No",
  "Prefer not to answer",
  "Seen but not answered",
  NA
  )
values_categorical_0_1


## ----vector categorical variables-------------------------------------------------------------------------------------
variables_categorical_0_1 <-
  c(
    "impact.symptoms_impacted_carry_employment",
    "impact.employment_status_changed_contracting"
    )
variables_categorical_0_1


## ----imp_check categorical variables----------------------------------------------------------------------------------
imp_check(data = covidcns_dat_id,
          variables = variables_categorical_0_1,
          values = values_categorical_0_1)


## ----change COVID CNS variables to match GLAD 1-----------------------------------------------------------------------

covidcns_dat_id <- covidcns_dat_id %>%
  mutate(
    impact.what_is_your_current_employment_status_numeric = 
      case_when(
         impact.fulltime_employed_numeric == 1 ~ 1,
         impact.parttime_employed_numeric == 1 ~ 1,
         impact.unemployed_numeric == 1 ~ 5,
         impact.zerohours_contract_numeric == 1 ~ 1,
         impact.stayathome_parent_or_carer_numeric == 1 ~ 3,
         impact.selfemployed_numeric == 1 ~ 1,
         impact.contract_or_freelance_work_numeric == 1 ~ 1,
         impact.small_buisness_owner_numeric == 1 ~ 1,
         impact.receiving_state_income_numeric == 1 ~ 4,
         impact.retired_numeric == 1 ~ 2,
         impact.student__numeric == 1 ~ 7,
         impact.student_.1_numeric == 1 ~ 7,
           (impact.fulltime_employed_numeric == -77 |
            impact.parttime_employed_numeric == -77 |
            impact.unemployed_numeric == -77 |
            impact.zerohours_contract_numeric == -77 |
            impact.stayathome_parent_or_carer_numeric == -77 |
            impact.selfemployed_numeric == -77 |
            impact.contract_or_freelance_work_numeric == -77 |
            impact.small_buisness_owner_numeric == -77 |
            impact.receiving_state_income_numeric == -77 |
            impact.retired_numeric == -77 |
            impact.student__numeric == -77 |
            impact.student_.1_numeric == -77) ~ -77
      )
  )

covidcns_dat_id <- covidcns_dat_id %>%
  mutate(
    impact.what_is_your_current_employment_status =
      recode_factor(impact.what_is_your_current_employment_status_numeric,
                    "1" = "In paid employment or self-employed",
                    "2" = "Retired",
                    "3" = "Looking after home and/or family",
                    "4" = "Unable to work because of sickness or disability",
                    "5" = "Unemployed",
                    "6" = "Doing unpaid or voluntary work",
                    "7" = "Full or part-time student",
                    "8" = "None of the above",
                    "-77" = "Seen not to answered",
                    missing = NA_character_
      )
  )


## ----change COVID CNS variables to match GLAD-------------------------------------------------------------------------

covidcns_dat_id <- covidcns_dat_id %>%
  mutate(
    impact.employment_changed_due_to_covid_numeric = 
      case_when(
         impact.my_employment_status_has_not_changed_numeric == 1 ~ 0,
         impact.become_unemployed_numeric == 1 ~ 1,
         impact.become_employed_numeric == 1 ~ 2,
         impact.reduction_in_hours_numeric == 1 ~ 3,
         impact.increased_hours_numeric == 1 ~ 4,
         impact.benefits_increased_numeric == 1 ~ 5,
         impact.furloughed_or_paid_leave__numeric == 1 ~ 6,
         impact.paid_leave_furloughed_numeric == 1 ~ 7,
         impact.other_numeric == 1 ~ 8,
         impact.changes_in_duties_or_responsibilities_numeric == 1 ~ 9,
         impact.reduction_in_salary_numeric == 1 ~ 10,
         impact.increased_salary_numeric == 1 ~ 11,
         impact.benefits_decreased_numeric == 1 ~ 12,
         impact.furloughed_or_paid_leave_.1_numeric == 1 ~ 13,
         impact.taking_unpaid_leave_numeric == 1 ~ 14,
         impact.prefer_not_to_say.1_numeric == 1 ~ -999,
           (impact.my_employment_status_has_not_changed_numeric == -777 |
            impact.become_unemployed_numeric == -777 |
            impact.become_employed_numeric == -777 |
            impact.reduction_in_hours_numeric == -777 |
            impact.increased_hours_numeric == -777 |
            impact.benefits_increased_numeric == -777 |
            impact.furloughed_or_paid_leave__numeric == -777 |
            impact.paid_leave_furloughed_numeric == -777 |
            impact.other_numeric == -777 |
            impact.changes_in_duties_or_responsibilities_numeric == -777 |
            impact.reduction_in_salary_numeric == -777 |
            impact.increased_salary_numeric == -777 |
            impact.benefits_decreased_numeric == -777 |
            impact.furloughed_or_paid_leave_.1_numeric == -777 |
            impact.taking_unpaid_leave_numeric == -777 |
            impact.prefer_not_to_say.1_numeric == -777) ~ -777
      )
)
  

covidcns_dat_id <- covidcns_dat_id %>%
  mutate(
    impact.employment_changed_due_to_covid =
      recode_factor(impact.employment_changed_due_to_covid_numeric,
                    "0" = "My employment status has not changed",
                    "1" = "Become unemployed",
                    "2" = "Become employed",
                    "3" = "Reduction in hours",
                    "4" = "Increased hours",
                    "5" = "Benefits increased",
                    "6" = "Furloughed or paid leave (Government funded)",
                    "7" = "Furloughed or paid leave (Government funded with company supplement)",
                    "8" = "Other",
                    "9" = "Changes in duties or responsibilities",
                    "10" = "Reduction in salary", 
                    "11" = "Increased Salary", 
                    "12" = "Benefits decreased",
                    "13" = "Furloughed or paid leave (Company funded)", 
                    "14" = "Taking unpaid leave", 
                    "-777" = "Seen not to answered",
                    "-999" = "Prefer not to say",
                    missing = NA_character_
      )
  )


## ----check colnames---------------------------------------------------------------------------------------------------
colnames(covidcns_dat_id)


## ----Write cleaned COVID-CNS variables to a .rds file-----------------------------------------------------------------
covidcns_dat_id %>% 
  filter(sample == "COVID-CNS") %>%  # select only COVID-CNS participants
  saveRDS(file = 
            paste0(ilovecovidcns, "/data/latest_freeze/baseline/impact_covidcns_clean.rds")
    )

