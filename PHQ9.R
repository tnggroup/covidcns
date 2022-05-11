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


## ----COVID CNS PHQ9 data----------------------------------------------------------------------------------------------
phq9 <- read_rds(file = 
                   paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/phq9_covid_cns.rds")
                 )

# check
phq9 %>% 
  dim()

phq9 %>% 
  colnames()


## ----Specify columns to be excluded from add_numeric function---------------------------------------------------------
exclude_cols_numeric <- c(
  "ID",
  "sample",
  "startDate",
  "endDate"
  )


## ----Select and rename relevant columns-------------------------------------------------------------------------------
phq9_id <- phq9 %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop NAs
  remove_duplicates("externalDataReference") %>% # Remove duplicate IDs
  add_column(sample = "COVIDCNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         phq9.little_interest_or_pleasure_in_doing_things,
         phq9.feeling_down_depressed_or_hopeless,         
         phq9.staying_asleep_sleeping_trouble,            
         phq9.feeling_tired_or_having_little_energy,       
         phq9.poor_appetite_or_overeating,                
         phq9.feeling_bad_failure_family = phq9.feeling_bad_felt_family,                                 # renamed to match GLAD                   
         phq9.trouble_concentrating_reading_newspaper = phq9.trouble_concentrating_newspaper_reading,    # renamed to match GLAD            
         phq9.moving_fidgety_noticed_opposite = phq9.fidgety_opposite_slowly_restless,                   # renamed to match GLAD               
         phq9.dead_hurting_thoughts,                      
         phq9.problems_made_care_home,                     
         phq9.similar_experiences_thoughts_required
        ) %>%
   add_numeric(exclude = exclude_cols_numeric)

# Inspect colnames
phq9_id %>% 
  colnames()


## ----Number excluded--------------------------------------------------------------------------------------------------
# Inspect dimensions
dim(phq9_id)
# Differences
phq9_excluded <- dim(phq9_id)[1]-dim(phq9)[1]
phq9_excluded


## ----Inspect numeric variables----------------------------------------------------------------------------------------
phq9_id %>%
  select(all_of(ends_with("numeric"))) %>%
  tbl_summary(missing_text = "Missing")

phq9_id %>% 
  select(ends_with("numeric")) %>% 
  freq()


## ----Recode NA values-------------------------------------------------------------------------------------------------
phq9_id <- phq9_id %>%
   mutate(
     across(
       ends_with("numeric"),
             ~case_when(
             . == -55 ~ -555,   
             . == -77 ~ -777,
             . == -88 ~ -888,
             . == -99 ~ -999,
             TRUE ~ .)
   )
   )


## ----Select numeric PHQ9 variables for cleaning-----------------------------------------------------------------------
phq9_variables_numeric <- c(
  "phq9.little_interest_or_pleasure_in_doing_things_numeric",
  "phq9.feeling_down_depressed_or_hopeless_numeric",         
  "phq9.staying_asleep_sleeping_trouble_numeric",            
  "phq9.feeling_tired_or_having_little_energy_numeric",       
  "phq9.poor_appetite_or_overeating_numeric",                
  "phq9.feeling_bad_failure_family_numeric",                     
  "phq9.trouble_concentrating_reading_newspaper_numeric",    
  "phq9.moving_fidgety_noticed_opposite_numeric",            
  "phq9.dead_hurting_thoughts_numeric"          
)

# check
phq9_variables_numeric


## ----vector of plausible numeric values for PHQ9 variables------------------------------------------------------------
phq9_vector_numeric <- c(
  0,
  1,
  2,
  3,
  -777,
  NA
  )


## ----imp_check numeric PHQ9 variables---------------------------------------------------------------------------------
imp_check(data = phq9_id,
          variables = phq9_variables_numeric,
          values = phq9_vector_numeric)


## ----Select numeric variable about problems---------------------------------------------------------------------------
phq9_problems_numeric <- c(
  "phq9.problems_made_care_home_numeric"      
)


# check
phq9_problems_numeric


## ----Vector of plausible numeric values for variable about problems---------------------------------------------------
phq9_problems_vector_numeric <- c(
  1,
  2,
  3,
  4,
  -777,
  NA
  )


## ----imp_check numeric problems variable------------------------------------------------------------------------------
imp_check(data = phq9_id,
          variables = phq9_problems_numeric,
          values = phq9_problems_vector_numeric)


## ----Select numeric variable about problems since COVID-19------------------------------------------------------------
phq9_covid19_numeric <- c(
  "phq9.similar_experiences_thoughts_required_numeric"      
)


# check
phq9_problems_numeric


## ----vector of plausible numeric values for variable about problems since COVID-19------------------------------------
phq9_covid19_vector_numeric <- c(
  1,
  2,
  3,
  -777,
  NA
  )


## ----imp_check numeric variable about problems since COVID-19---------------------------------------------------------
imp_check(data = phq9_id,
          variables = phq9_covid19_numeric,
          values = phq9_covid19_vector_numeric)


## ----Select PHQ9 variables for cleaning-------------------------------------------------------------------------------
phq9_variables <- c(
  "phq9.little_interest_or_pleasure_in_doing_things",
  "phq9.feeling_down_depressed_or_hopeless",         
  "phq9.staying_asleep_sleeping_trouble",            
  "phq9.feeling_tired_or_having_little_energy",       
  "phq9.poor_appetite_or_overeating",                
  "phq9.feeling_bad_failure_family",                     
  "phq9.trouble_concentrating_reading_newspaper",    
  "phq9.moving_fidgety_noticed_opposite",            
  "phq9.dead_hurting_thoughts"          
)

  
# check
phq9_variables


## ----vector of plausible values for PHQ9 variables--------------------------------------------------------------------
phq9_vector <- c(
  "Not at all",
  "Several days",
  "More than half the days",
  "Nearly every day",
  "Seen but not answered",
  NA
  )


## ----imp_check PHQ9 variables-----------------------------------------------------------------------------------------
imp_check(data = phq9_id,
          variables = phq9_variables,
          values = phq9_vector)


## ----Select variable about problems-----------------------------------------------------------------------------------
phq9_problems <- c(
  "phq9.problems_made_care_home"      
)


# check
phq9_problems


## ----vector of plausible values for variable about problems-----------------------------------------------------------
phq9_problems_vector <- c(
  "Not difficult at all",
  "Somewhat difficult",
  "Very difficult",
  "Extremely difficult",
  "Seen but not answered",
  NA
  )


## ----imp_check problems variable--------------------------------------------------------------------------------------
imp_check(data = phq9_id,
          variables = phq9_problems,
          values = phq9_problems_vector)


## ----Select variable about problems since COVID-19--------------------------------------------------------------------
phq9_covid19 <- c(
  "phq9.similar_experiences_thoughts_required"      
)


# check
phq9_problems


## ----Vector of plausible values for variable about problems since COVID-19--------------------------------------------
phq9_covid19_vector <- c(
  "Yes - they started after I had COVID-19 or during my infection with it",
  "No - I had these symptoms or experiences before I had COVID-19",
  "Don't know",
  "Seen but not answered",
  NA
  )


## ----imp_check COVID19 variables--------------------------------------------------------------------------------------
imp_check(data = phq9_id,
          variables = phq9_covid19,
          values = phq9_covid19_vector)


## ----Sumscore inputs--------------------------------------------------------------------------------------------------
keys <- c(
  1, #1
  1, #2
  1, #3
  1, #4
  1, #5
  1, #6
  1, #7
  1, #8
  1 #9
  ) # should be 9 1s (none of the PHQ9 items are reverse coded)

sum_vars <- c(
  "phq9.dead_hurting_thoughts_numeric",
  "phq9.feeling_bad_failure_family_numeric",
  "phq9.feeling_down_depressed_or_hopeless_numeric",
  "phq9.feeling_tired_or_having_little_energy_numeric",
  "phq9.little_interest_or_pleasure_in_doing_things_numeric",
  "phq9.moving_fidgety_noticed_opposite_numeric",
  "phq9.poor_appetite_or_overeating_numeric",
  "phq9.staying_asleep_sleeping_trouble_numeric",
  "phq9.trouble_concentrating_reading_newspaper_numeric"
  )


## ----generate sumscore------------------------------------------------------------------------------------------------
phq9_id <- phq9_id %>% 
  mutate(
    phq9.sum_score = 
         sumscores(input = phq9_id,
                   sum_vars = sum_vars,
                   coding_keys = keys,
                   na_allowed = 0,
                   min_item = 0,
                   max_item = 3,
                   min_score = 0,
                   max_score = 27
                   )[["scores"]]
         )

# check
phq9_id %>% 
  descr(phq9.sum_score)


## ----current major depression-----------------------------------------------------------------------------------------
# numeric
phq9_id <- phq9_id %>% 
  mutate(
    phq9.binary_depression_numeric =
      case_when(
        phq9.sum_score >= 10 ~ 1, # current depression
        phq9.sum_score < 10 ~ 0 # no current depression
        )
    )

# recode as categorical 
phq9_id <- phq9_id %>% 
  mutate(
    phq9.binary_depression =
      recode_factor(phq9.binary_depression_numeric,
        "1" = "Current depression", # current depression
        "0" = "No current depression" # no current depression
        )
    )

# check
phq9_id %>% 
  select(
    phq9.binary_depression_numeric,
    phq9.binary_depression
    ) %>% 
   tbl_summary(missing_text = "Missing")

phq9_id %>% 
  select(
    phq9.binary_depression,
    phq9.binary_depression_numeric) %>% # variable is fine - just tbl_summary() giving wrong result 
  freq()


## ----severity of depression-------------------------------------------------------------------------------------------
# numeric
phq9_id <- phq9_id %>% 
  mutate(
    phq9.severity_depression_numeric =
      case_when(
        phq9.sum_score <= 4 ~ 0,
        phq9.sum_score > 4 & phq9.sum_score <= 9 ~ 1,
        phq9.sum_score > 9 & phq9.sum_score <= 14 ~ 2,
        phq9.sum_score > 15 & phq9.sum_score <= 19 ~ 3,
        phq9.sum_score > 19 & phq9.sum_score <= 27 ~ 4
        )
    )

# recode as categorical
phq9_id <- phq9_id %>% 
  mutate(
    phq9.severity_depression =
      recode_factor(phq9.severity_depression_numeric,
        "0" = "None",
        "1" = "Mild",
        "2" = "Moderate",
        "3" = "Moderately severe",
        "4" = "Severe"
        )
    )

# check
phq9_id %>% 
  select(
    phq9.severity_depression,
    phq9.severity_depression_numeric
  ) %>% 
   tbl_summary(missing_text = "Missing")


## ----Save variables for export----------------------------------------------------------------------------------------
export_variables <- c("ID",
                      "startDate",
                      "endDate",
                      "sample",
                      "phq9.dead_hurting_thoughts",                               
                      "phq9.feeling_bad_failure_family",                         
                      "phq9.feeling_down_depressed_or_hopeless",                  
                      "phq9.feeling_tired_or_having_little_energy",              
                      "phq9.little_interest_or_pleasure_in_doing_things",         
                      "phq9.moving_fidgety_noticed_opposite",                    
                      "phq9.poor_appetite_or_overeating",                        
                      "phq9.staying_asleep_sleeping_trouble",                    
                      "phq9.trouble_concentrating_reading_newspaper",             
                      "phq9.dead_hurting_thoughts_numeric",                      
                      "phq9.feeling_bad_failure_family_numeric",                  
                      "phq9.feeling_down_depressed_or_hopeless_numeric",         
                      "phq9.feeling_tired_or_having_little_energy_numeric",       
                      "phq9.little_interest_or_pleasure_in_doing_things_numeric",
                      "phq9.moving_fidgety_noticed_opposite_numeric",             
                      "phq9.poor_appetite_or_overeating_numeric",                
                      "phq9.staying_asleep_sleeping_trouble_numeric",             
                      "phq9.trouble_concentrating_reading_newspaper_numeric",    
                      "phq9.sum_score",                                           
                      "phq9.binary_depression",                                  
                      "phq9.binary_depression_numeric",                           
                      "phq9.severity_depression",                                
                      "phq9.severity_depression_numeric",
                      "phq9.problems_made_care_home",
                      "phq9.similar_experiences_thoughts_required",
                      "phq9.problems_made_care_home_numeric",
                      "phq9.similar_experiences_thoughts_required_numeric"
                      )




## ----Save cleaned data as .rds file-----------------------------------------------------------------------------------
phq9_id %>% 
  select(all_of(export_variables)) %>% 
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/phq9_covidcns_clean.rds")
    )

