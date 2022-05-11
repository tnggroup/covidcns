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


## ----load data--------------------------------------------------------------------------------------------------------
covidcns_dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/covid19_covid_cns.rds")
  )
  
# Check variable names in dataframe
covidcns_dat %>%
  colnames()
# Inspect dimensions of dataframe 
covidcns_dat %>%
  dim()


## ----specify excluded columns-----------------------------------------------------------------------------------------
exclude_cols_numeric <- c(
  "ID",
  "sample",
  "startDate",
  "endDate",
  "covid19.exact_date_symptoms_start.txt",
  "covid19.latest_test_date_yyyy.txt",
  "covid19.please_describe_your_other_symptoms.txt"
 # other columns as required
  )


## ----select-----------------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  remove_duplicates("externalDataReference") %>% # Remove duplicate IDs
  add_column(sample = "COVID-CNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         covid19.have_you_had_suspected_covid19_symptoms,
         covid19.exact_date_symptoms_start.txt,
         covid19.tested_covid,
         covid19.what_test_did_you_have,
         covid19.test_positive_covid,
         covid19.coivd_occasion_test_positive,
         covid19.latest_test_date_yyyy.txt,
         covid19.recover_home_stay_time,
         covid19.get_treatment_at_hospital_,
         covid19.get_treatment_in_intensive_care_unit,
         covid19.persistent_cough,
         covid19.persistent_cough.1,
         covid19.fever,
         covid19.fever.1,
         covid19.loss_of_tasteloss_of_smell,
         covid19.loss_of_tasteloss_of_smell.1,
         covid19.intermittent_cough,
         covid19.intermittent_cough.1,
         covid19.hoarseness,
         covid19.hoarseness.1,
         covid19.sore_throat,
         covid19.sore_throat.1,
         covid19.nasal_dischargecongestion,
         covid19.nasal_dischargecongestion.1,
         covid19.wheeze,
         covid19.wheeze.1,
         covid19.shortness_of_breath,
         covid19.shortness_of_breath.1,
         covid19.headache,
         covid19.headache.1,
         covid19.muscle_aches,
         covid19.muscle_aches.1,
         covid19.nausea,
         covid19.nausea.1,
         covid19.diarrhoea,
         covid19.diarrhoea.1,
         covid19.other,
         covid19.other.1,
         covid19.please_describe_your_other_symptoms.txt,
         covid19.individual_contact_positive_test 
         # other columns as necessary
         ) %>%
  add_numeric(exclude = exclude_cols_numeric)
# Inspect colnames
covidcns_dat_id %>%
  colnames()


## ----number excluded--------------------------------------------------------------------------------------------------
# Inspect dimensions of new data set
covidcns_dat_id %>%
  dim()
# Inspect number of rows dropped
covidcns_excluded <- dim(covidcns_dat_id)[1] - dim(covidcns_dat)[1]
covidcns_excluded


## ----inspect numeric variables----------------------------------------------------------------------------------------
covidcns_dat_id %>%
  select(all_of(ends_with("numeric"))) %>%
  tbl_summary(missing_text = "Missing")


## ----GLAD inspect missingness-----------------------------------------------------------------------------------------
covidcns_miss_map <- covidcns_dat_id %>% 
  missmap()
covidcns_miss_map


## ----recode incorrect variable----------------------------------------------------------------------------------------
# Have you had suspected COVID-19 symptoms?

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.have_you_had_suspected_covid19_symptoms_numeric =
        case_when(
        covid19.have_you_had_suspected_covid19_symptoms_numeric == "2" ~ 0,
        covid19.have_you_had_suspected_covid19_symptoms_numeric == "1" ~ 1,
        covid19.have_you_had_suspected_covid19_symptoms_numeric == "-77" ~ -77
           ) 
        )
    
covidcns_dat_id %>%
   freq(covid19.have_you_had_suspected_covid19_symptoms_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.have_you_had_suspected_covid19_symptoms =
        recode_factor(covid19.have_you_had_suspected_covid19_symptoms_numeric,
                      "0" = "No I haven't had tCOVID19 symptoms",
                      "1" = "Yes I've had COVID19 symptoms",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.have_you_had_suspected_covid19_symptoms)
   

#### When did your symptoms start? 
   
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.exact_date_symptoms_start =
        as.Date(covid19.exact_date_symptoms_start.txt,
  format = "%d/%m/%Y")
    )
       

covidcns_dat_id %>%
   freq(covid19.exact_date_symptoms_start)


## ---------------------------------------------------------------------------------------------------------------------

# Have you been tested for COVID19
  
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.tested_covid_numeric =
          case_when(
            covid19.tested_covid_numeric == "2" ~ 0,
            covid19.tested_covid_numeric == "1" ~ 1,
            covid19.tested_covid_numeric == "-77" ~ -77
           ) 
        )
    

covidcns_dat_id %>%
   freq(covid19.tested_covid_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.tested_covid =
        recode_factor(covid19.tested_covid_numeric,
                      "0" = "No I haven't been tested for COVID19",
                      "1" = "Yes I've been tested for COVID19",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.tested_covid)
   
### What test did you have? 
   
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.what_test_did_you_have_numeric =
        case_when(
        covid19.what_test_did_you_have_numeric == "1" ~ 1,
        covid19.what_test_did_you_have_numeric == "2" ~ 2,
        covid19.what_test_did_you_have_numeric == "3" ~ 3,
        covid19.what_test_did_you_have_numeric == "-77" ~ -77
           ) 
        )
    

covidcns_dat_id %>%
   freq(covid19.what_test_did_you_have_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.what_test_did_you_have =
        recode_factor(covid19.what_test_did_you_have_numeric,
                      "1" = "Swab test",
                      "2" = "Lateral flow test",
                      "3" = "Antibody test",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.what_test_did_you_have)
   
### Did you [ever] test positive for COVID-19?
   
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.test_positive_covid_numeric =
        case_when(
        covid19.test_positive_covid_numeric == "2" ~ 0,
        covid19.test_positive_covid_numeric == "1" ~ 1,
        covid19.test_positive_covid_numeric == "-77" ~ -77
           ) 
        )
    

covidcns_dat_id %>%
   freq(covid19.test_positive_covid_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.test_positive_covid =
        recode_factor(covid19.test_positive_covid_numeric,
                      "0" = "No I haven't tested positive for COVID19",
                      "1" = "Yes I've  tested positive for COVID19",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.test_positive_covid)

### Did you test positive for COIVD-19 on more than one occasion?
   
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.coivd_occasion_test_positive_numeric =
        case_when(
          covid19.coivd_occasion_test_positive_numeric == "2" ~ 0,
          covid19.coivd_occasion_test_positive_numeric == "1" ~ 1,
          covid19.coivd_occasion_test_positive_numeric == "-77" ~ -77
           ) 
        )
    

covidcns_dat_id %>%
   freq(covid19.coivd_occasion_test_positive_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.coivd_occasion_test_positive =
        recode_factor(covid19.coivd_occasion_test_positive_numeric,
                      "0" = "No I haven't tested positive more than once for COVID19",
                      "1" = "Yes I've  tested positive more than once for COVID19",
                       "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.coivd_occasion_test_positive)
   

#### Date of positive or latest test Please provide at least month and year. Please enter as dd/mm/yyyy and enter 01/mm/yyyy for the day if you do not know.
   
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.latest_test_date =
        as.Date(covid19.latest_test_date_yyyy.txt,
  format = "%d/%m/%Y")
    )
       
    
covidcns_dat_id %>%
   freq(covid19.latest_test_date)


## ---------------------------------------------------------------------------------------------------------------------

# Severity of treatment - where did you seek treatment:

### Stay at home the whole time to recover?
   
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.recover_home_stay_time_numeric =
          case_when(
            covid19.recover_home_stay_time_numeric == "1" ~ 1,
            covid19.recover_home_stay_time_numeric == "2" ~ 0,
            covid19.recover_home_stay_time_numeric == "-77" ~ -77,
          )
    )


covidcns_dat_id %>%
   freq(covid19.recover_home_stay_time_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.recover_home_stay_time =
        recode_factor(covid19.recover_home_stay_time_numeric,
                       "1" = "Yes",
                       "0" = "No",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.recover_home_stay_time)

### Get treatment at hospital ?

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.get_treatment_at_hospital__numeric =
          case_when(
            covid19.get_treatment_at_hospital__numeric == "1" ~ 1,
            covid19.get_treatment_at_hospital__numeric == "2" ~ 0,
            covid19.get_treatment_at_hospital__numeric == "-77" ~ -77,
          )
    )


covidcns_dat_id %>%
   freq(covid19.get_treatment_at_hospital__numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.get_treatment_at_hospital_ =
        recode_factor(covid19.get_treatment_at_hospital__numeric,
                       "1" = "Yes",
                       "0" = "No",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.get_treatment_at_hospital_)

### Get treatment in Intensive Care Unit?

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.get_treatment_in_intensive_care_unit_numeric =
          case_when(
            covid19.get_treatment_in_intensive_care_unit_numeric == "1" ~ 1,
            covid19.get_treatment_in_intensive_care_unit_numeric == "2" ~ 0,
            covid19.get_treatment_in_intensive_care_unit_numeric == "-77" ~ -77,
          )
    )


covidcns_dat_id %>%
   freq(covid19.get_treatment_in_intensive_care_unit_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.get_treatment_in_intensive_care_unit =
        recode_factor(covid19.get_treatment_in_intensive_care_unit_numeric,
                       "1" = "Yes",
                       "0" = "No",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.get_treatment_in_intensive_care_unit)

# If you had symptoms or a confirmed diagnosis of COVID-19, did you:

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.where_did_you_seek_treatment_numeric =
        case_when(
        covid19.get_treatment_in_intensive_care_unit_numeric == "1" ~ 3,
        covid19.get_treatment_at_hospital__numeric == "1" ~ 2,
        covid19.recover_home_stay_time_numeric == "1" ~ 1,
        covid19.get_treatment_in_intensive_care_unit_numeric == "-77" ~ -77,
        covid19.get_treatment_at_hospital__numeric == "-77" ~ -77,
        covid19.recover_home_stay_time_numeric == "-77" ~ -77
           ) 
        )
    

covidcns_dat_id %>%
   freq(covid19.where_did_you_seek_treatment_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.where_did_you_seek_treatment =
        recode_factor(covid19.where_did_you_seek_treatment_numeric,
                      "1" = "Stay at home the whole time to recover",
                      "2" = "Get treatment at hospital",
                      "3" = "Get treatment in Intensive Care Unit",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.where_did_you_seek_treatment)
   
   ### What symptoms did you have? 
   
   ### Persistent cough
   
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.persistent_cough_numeric =
          case_when(
            covid19.persistent_cough.1_numeric == "1" ~ 0,
            covid19.persistent_cough_numeric == "1" ~ 1,
            covid19.persistent_cough.1_numeric == "-77" ~ -77,
            covid19.persistent_cough_numeric == "-77" ~ -77

          )
    )


 covidcns_dat_id %>%
   freq(covid19.persistent_cough_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.persistent_cough =
        recode_factor(covid19.persistent_cough_numeric,
                       "0" = "No persistent cough",
                       "1" = "Yes persistant cough",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.persistent_cough)
   
### Fever
   
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.fever_numeric =
          case_when(
            covid19.fever.1_numeric == "1" ~ 0,
            covid19.fever_numeric == "1" ~ 1,
            covid19.fever.1_numeric == "-77" ~ -77,
            covid19.fever.1_numeric == "-77" ~ -77
          )
    )


covidcns_dat_id %>%
   freq(covid19.fever_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.fever =
        recode_factor(covid19.fever_numeric,
                      "0" = "No fever",
                      "1" = "Yes fever",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.fever)
   
   
### Loss of taste/smell
   
      
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.loss_of_tasteloss_of_smell_numeric =
          case_when(
        covid19.loss_of_tasteloss_of_smell_numeric == "1" ~ 1,
        covid19.loss_of_tasteloss_of_smell.1_numeric == "1" ~ 0,
        covid19.loss_of_tasteloss_of_smell_numeric == "-77" ~ -77,
        covid19.loss_of_tasteloss_of_smell.1_numeric == "-77" ~ -77
          )
    )


covidcns_dat_id %>%
   freq(covid19.loss_of_tasteloss_of_smell_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.loss_of_tasteloss_of_smell =
        recode_factor(covid19.loss_of_tasteloss_of_smell_numeric,
                      "1" = "Yes loss of taste/smell",
                      "0" = "No loss of taste/smell",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.loss_of_tasteloss_of_smell)
   
### Intermittent cough
   
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.intermittent_cough_numeric =
          case_when(
        covid19.intermittent_cough_numeric == "1" ~ 1,
        covid19.intermittent_cough.1_numeric == "1" ~ 0,
        covid19.intermittent_cough_numeric == "-77" ~ -77,
        covid19.intermittent_cough.1_numeric == "-77" ~ -77
          )
    )


covidcns_dat_id %>%
   freq(covid19.intermittent_cough_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.intermittent_cough =
        recode_factor(covid19.intermittent_cough_numeric,
                      "1" = "Yes intermittent cough",
                      "0" = "No intermittent cough",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.intermittent_cough)
   
   
### Hoarseness
   
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.hoarseness_numeric =
          case_when(
        covid19.hoarseness_numeric == "1" ~ 1,
        covid19.hoarseness.1_numeric == "1" ~ 0,
        covid19.hoarseness_numeric == "-77" ~ -77,
        covid19.hoarseness.1_numeric == "-77" ~ -77
          )
    )


covidcns_dat_id %>%
   freq(covid19.hoarseness_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.hoarseness =
        recode_factor(covid19.hoarseness_numeric,
                      "1" = "Yes hoarseness",
                      "0" = "No hoarseness",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.hoarseness)
   
### Sore throat
   
      
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.sore_throat_numeric =
          case_when(
        covid19.sore_throat_numeric == "1" ~ 1,
        covid19.sore_throat.1_numeric == "1" ~ 0,
        covid19.sore_throat_numeric == "-77" ~ -77,
        covid19.sore_throat.1_numeric == "-77" ~ -77
          )
    )


covidcns_dat_id %>%
   freq(covid19.sore_throat_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.sore_throat =
        recode_factor(covid19.sore_throat_numeric,
                      "1" = "Yes sore throat",
                      "0" = "No sore throat",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.sore_throat)
   
   
### Nasal discharge / congestion
   
      
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.nasal_dischargecongestion_numeric =
          case_when(
        covid19.nasal_dischargecongestion_numeric == "1" ~ 1,
        covid19.nasal_dischargecongestion.1_numeric == "1" ~ 0,
        covid19.nasal_dischargecongestion_numeric == "-77" ~ -77,
        covid19.nasal_dischargecongestion_numeric == "-77" ~ -77
          )
    )


covidcns_dat_id %>%
   freq(covid19.nasal_dischargecongestion_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.nasal_dischargecongestion =
        recode_factor(covid19.nasal_dischargecongestion_numeric,
                      "1" = "Yes nasal discharge/congestion",
                      "0" = "No nasal discharge/congestion",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.nasal_dischargecongestion)
   
   
### Wheeze
   
      
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.wheeze_numeric =
          case_when(
        covid19.wheeze_numeric == "1" ~ 1,
        covid19.wheeze.1_numeric == "1" ~ 0,
        covid19.wheeze_numeric == "-77" ~ -77,
        covid19.wheeze.1_numeric == "-77" ~ -77
          )
    )


covidcns_dat_id %>%
   freq(covid19.wheeze_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.wheeze =
        recode_factor(covid19.wheeze_numeric,
                      "1" = "Yes wheeze",
                      "0" = "No wheeze",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.wheeze)
   
   
### Shortness of breath
   
      
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.shortness_of_breath_numeric =
          case_when(
        covid19.shortness_of_breath_numeric == "1" ~ 1,
        covid19.shortness_of_breath.1_numeric == "1" ~ 0,
        covid19.shortness_of_breath_numeric == "-77" ~ -77,
        covid19.shortness_of_breath.1_numeric == "-77" ~ -77
          )
    )


covidcns_dat_id %>%
   freq(covid19.shortness_of_breath_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.shortness_of_breath =
        recode_factor(covid19.shortness_of_breath_numeric,
                      "1" = "Yes shortness of breath",
                      "0" = "No shortness of breath",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.shortness_of_breath)
   
   
### Headache
   
      
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.headache_numeric =
          case_when(
        covid19.headache_numeric == "1" ~ 1,
        covid19.headache.1_numeric == "1" ~ 0,
        covid19.headache_numeric == "-77" ~ -77,
        covid19.headache.1_numeric == "-77" ~ -77
          )
    )


covidcns_dat_id %>%
   freq(covid19.headache_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.headache =
        recode_factor(covid19.headache_numeric,
                      "1" = "Yes headache",
                      "0" = "No headache",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.headache)
   
### Muscle ache
   
      
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.muscle_aches_numeric =
          case_when(
        covid19.muscle_aches_numeric == "1" ~ 1,
        covid19.muscle_aches.1_numeric == "1" ~ 0,
        covid19.muscle_aches_numeric == "-77" ~ -77,
        covid19.muscle_aches.1_numeric == "-77" ~ -77
          )
    )


covidcns_dat_id %>%
   freq(covid19.muscle_aches_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.muscle_aches =
        recode_factor(covid19.muscle_aches_numeric,
                      "1" = "Yes muscle ache",
                      "0" = "No mucle ache",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.muscle_aches)
   
   
### Nausea
   
      
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.nausea_numeric =
          case_when(
        covid19.nausea_numeric == "1" ~ 1,
        covid19.nausea.1_numeric == "1" ~ 0,
        covid19.nausea_numeric == "-77" ~ -77,
        covid19.nausea.1_numeric == "-77" ~ -77
          )
    )


covidcns_dat_id %>%
   freq(covid19.nausea_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.nausea =
        recode_factor(covid19.nausea_numeric,
                      "1" = "Yes nausea",
                      "0" = "No nausea",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )

covidcns_dat_id %>%
   freq(covid19.nausea)
   
   
### Diarhoea
   
      
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.diarrhoea_numeric =
          case_when(
        covid19.diarrhoea_numeric == "1" ~ 1,
        covid19.diarrhoea.1_numeric == "1" ~ 0,
        covid19.diarrhoea_numeric == "-77" ~ -77,
        covid19.diarrhoea.1_numeric == "-77" ~ -77
          )
    )


covidcns_dat_id %>%
   freq(covid19.diarrhoea_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.diarrhoea =
        recode_factor(covid19.diarrhoea_numeric,
                      "1" = "Yes diarrhoea",
                      "0" = "No diarrhoea",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.diarrhoea)
   
   
### Other
   
      
ovidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.please_describe_your_other_symptoms =
                  covid19.please_describe_your_other_symptoms.txt
          )
    
print(covidcns_dat_id$covid19.other)


      
### At the time of your symptoms, were you a contact of an individual who had COVID-19 symptoms or a positive test?
   
      
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.individual_contact_positive_test_numeric =
          case_when(
        covid19.individual_contact_positive_test_numeric == "1" ~ 1,
        covid19.individual_contact_positive_test_numeric == "3" ~ 0,
        covid19.individual_contact_positive_test_numeric == "-77" ~ -77
          )
    )


covidcns_dat_id %>%
   freq(covid19.individual_contact_positive_test_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.individual_contact_positive_test =
        recode_factor(covid19.individual_contact_positive_test_numeric,
                      "1" = "Yes contact with an individual with COVID19",
                      "0" = "No contact with an individual with COVID19",
                      "-77" = "Seen but not answered",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.individual_contact_positive_test)
   


## ----recheck variable coding------------------------------------------------------------------------------------------
covidcns_dat_id %>%
  select(all_of(starts_with("covid19"))) %>%
  tbl_summary(missing_text = "Missing")


## ----Recode NA values-------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat_id %>%
  mutate(across(ends_with("numeric"),
                ~case_when(
                  . == -55 ~ -555,
                  . == -77 ~ -777,
                  . == -88 ~ -888,
                  . == -99 ~ -999,
                  TRUE ~ .)))


## ----Sumscores inputs-------------------------------------------------------------------------------------------------
keys <- c(
  1,1,1,1,1,1,1,1,1,1,1,1,1,1
  )
sum_vars <- c(
    "covid19.persistent_cough_numeric",
    "covid19.fever_numeric",
    "covid19.loss_of_tasteloss_of_smell_numeric",
    "covid19.intermittent_cough_numeric",
    "covid19.hoarseness_numeric",
    "covid19.sore_throat_numeric",
    "covid19.nasal_dischargecongestion_numeric",
    "covid19.wheeze_numeric",
    "covid19.shortness_of_breath_numeric",
    "covid19.headache_numeric",
    "covid19.muscle_aches_numeric",
    "covid19.nausea_numeric",
    "covid19.diarrhoea_numeric",
    "covid19.other_numeric"
  )


## ----generate sumscores-----------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat_id %>% 
  mutate(
    covid19.sum_score = 
         sumscores(input = covidcns_dat_id,
                   sum_vars = sum_vars,
                   reverse = FALSE,
                   coding_keys = keys,
                   # method = zero_replace,
                   na_limit = 0.01,
                   min_item = 0,
                   max_item = 1,
                   min_score = 0,
                   max_score = 14
                   )
         )

freq(covidcns_dat_id$covid19.sum_score)



## ----severity---------------------------------------------------------------------------------------------------------

### Measure of COVID19 severity
   
covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
        covid19.severity_numeric =
          case_when(
            covid19.sum_score == 0 ~ '0',
            covid19.sum_score >= 1 & covid19.sum_score <= 4 ~ '1',
            covid19.sum_score >= 4 & covid19.sum_score <= 7 ~ '2',
            covid19.sum_score >= 7 & covid19.sum_score <= 10 ~ '3',
            covid19.sum_score >= 10 & covid19.sum_score <= 14 ~ '4'
          )
    )


covidcns_dat_id %>%
   freq(covid19.severity_numeric)

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(
      covid19.severity =
        recode_factor(covid19.severity_numeric,
                      "0" = "No Symptoms",
                      "1" = "Mild Symptoms",
                      "2" = "Moderate Symptoms",
                      "3" = "Severe Symptoms",
                      "4" = "Very Severe Symptoms",
                      missing = NA_character_
        )
    )
  
covidcns_dat_id %>%
   freq(covid19.severity)
   


## ----check colnames---------------------------------------------------------------------------------------------------
colnames(covidcns_dat_id)


## ----Write cleaned GLAD variables to a .rds file----------------------------------------------------------------------
covidcns_dat_id %>% 
  filter(sample == "COVID-CNS") %>%  # select only GLAD participants
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/covid19_covid_cns_clean.rds")
    )

