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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/neuro_case_report/ncrf1_admission_covid_cns.rds")
  )
  
# Check variable names in dataframe
covidcns_dat %>%
  colnames()

# Inspect dimensions of dataframe 
covidcns_dat %>%
  dim()


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
         ncrf1_admission.name_of_facility.txt,
         ncrf1_admission.country.txt,
         ncrf1_admission.date_of_inpatient_admission.txt,
         ncrf1_admission.date_of_positive_covid19_test.txt
         )

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
  mutate(across(ends_with("txt"),
                ~case_when(
                  . == "-55" ~ "-555",
                  . == "-77" ~ "-777",
                  . == "-88" ~ "-888",
                  . == "-99" ~ "-999",
                  TRUE ~ .)))


## ----List unique values-----------------------------------------------------------------------------------------------
ulst <- sapply(dat, unique)


## ----COVIDCNS inspect variables---------------------------------------------------------------------------------------
dat %>%
  tbl_summary(include = c(ncrf1_admission.name_of_facility.txt,
                          ncrf1_admission.country.txt,
                          ncrf1_admission.date_of_inpatient_admission.txt,
                          ncrf1_admission.date_of_positive_covid19_test.txt),
              missing_text = "Missing")


## ----COVIDCNS recode name of facility variable------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(ncrf1_admission.name_of_facility.txt,
                ~case_when(
                  str_detect(ncrf1_admission.name_of_facility.txt, "ddenbrooke") |
                    str_detect(ncrf1_admission.name_of_facility.txt, "ambridge") ~ "Addenbrooke's Hospital, Cambridge",
                  str_detect(ncrf1_admission.name_of_facility.txt, "intree") &
                    !str_detect(ncrf1_admission.name_of_facility.txt, "alton") ~ "Aintree University Hospital",
                  str_detect(ncrf1_admission.name_of_facility.txt, "ohn Radcliffe") ~ "John Radcliffe Hospital",
                  str_detect(ncrf1_admission.name_of_facility.txt, "llege Hosp") |
                    str_detect(ncrf1_admission.name_of_facility.txt, "KCH") ~ "King's College Hospital",
                  . == "Princess Royal University Hospital" |
                    . == "PRUH" |
                    . == "pruh" ~ "Princess Royal University Hospital",
                  str_detect(ncrf1_admission.name_of_facility.txt, "allamshire") &
                    !str_detect(ncrf1_admission.name_of_facility.txt, "and") ~ "Royal Hallamshire Hospital",
                  str_detect(ncrf1_admission.name_of_facility.txt, "Royal Liverpool") ~ "The Royal Liverpool and Broadgreen University Hospital NHS Trust",
                  str_detect(ncrf1_admission.name_of_facility.txt, "ldham") ~ "The Royal Oldham Hospital",
                  str_detect(ncrf1_admission.name_of_facility.txt, "alton") &
                    !str_detect(ncrf1_admission.name_of_facility.txt, "and") &
                    !str_detect(ncrf1_admission.name_of_facility.txt, ",") ~ "The Walton Centre",
                  str_detect(ncrf1_admission.name_of_facility.txt, "ales") ~ "University Hospital of Wales",
                  str_detect(ncrf1_admission.name_of_facility.txt, "Horton") ~ "Horton General Hospital",
                  str_detect(ncrf1_admission.name_of_facility.txt, "Maccles") ~ "Macclesfield District General Hospital",
                  str_detect(ncrf1_admission.name_of_facility.txt, "NHNN") ~ "National Hospital for Neurology and Neurosurgery",
                  str_detect(ncrf1_admission.name_of_facility.txt, "Salford") ~ "Salford Royal Hospital",
                  str_detect(ncrf1_admission.name_of_facility.txt, "Wigan") |
                    str_detect(ncrf1_admission.name_of_facility.txt, "ALbert Edward") ~ "Royal Albert Edward Infirmary",
                  str_detect(ncrf1_admission.name_of_facility.txt, "UCLH") ~ "University College Hospital",
                  str_detect(ncrf1_admission.name_of_facility.txt, "homas H") ~ "St. Thomas' Hospital",
                  str_detect(ncrf1_admission.name_of_facility.txt, "eorge's H") ~ "St. George's Hospital",
                  TRUE ~ .)))


## ----COVIDCNS recheck name of facility coding-------------------------------------------------------------------------
dat %>%
  select(all_of(starts_with("ncrf1_admission.name_of_facility.txt"))) %>%
  tbl_summary(missing_text = "Missing")


## ----COVIDCNS recode country variable---------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(ncrf1_admission.country.txt,
                ~case_when(
                  str_detect(ncrf1_admission.country.txt, "ngland")  ~ "England",
                  str_detect(ncrf1_admission.country.txt, "nited ") |
                    . == "UK" |
                    . == "uk" ~ "United Kingdom",
                  str_detect(ncrf1_admission.country.txt, "ollege")  ~ "England",
                  TRUE ~ .)))
                  


## ----COVIDCNS recheck country coding----------------------------------------------------------------------------------
dat %>%
  select(all_of(starts_with("ncrf1_admission.country.txt"))) %>%
  tbl_summary(missing_text = "Missing")


## ----Vector date variables--------------------------------------------------------------------------------------------
variables_date <- c(
  "ncrf1_admission.date_of_inpatient_admission.txt",
  "ncrf1_admission.date_of_positive_covid19_test.txt"
)


## ----COVIDCNS inspect dates-------------------------------------------------------------------------------------------
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



## ----COVIDCNS reinspect dates-----------------------------------------------------------------------------------------
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


## ----check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/neuro_case_report/ncrf1_admission_covidcns_clean.rds")
    )

