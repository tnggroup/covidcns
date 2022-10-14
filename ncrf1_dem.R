## ----Setup, include=FALSE------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  comment = '',
  prompt = FALSE,
  cache = FALSE
  )


## ----Clear global environment--------------------------------------------------------------------------------
remove(list = ls())


## ----Read in functions---------------------------------------------------------------------------------------
source(file = "scripts/functions/add_numeric_1.R")
source(file = "scripts/functions/remove_duplicates.R")
source(file = "scripts/functions/sumscores.R")
source(file = "scripts/functions/package_check.R")
source(file = "scripts/functions/recode_check.R")
source(file = "scripts/functions/imp_clean.R")
source(file = "scripts/functions/cont_clean.R")


## ----Install load dependencies-------------------------------------------------------------------------------
packages <- c("summarytools", "sjlabelled", "Amelia", "gtsummary", "lubridate", "tidyverse")
package_check(packages)


## ----Read in file with path to ilovecovidcns channel on teams------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----COVIDCNS load data--------------------------------------------------------------------------------------
covidcns_dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/2022-05-09/neuro_case_report/ncrf1_dem_covid_cns.rds")
  )
  
# Check variable names in dataframe
covidcns_dat %>%
  colnames()

# Inspect dimensions of dataframe 
covidcns_dat %>%
  dim()


## ----COVIDCNS specify excluded columns-----------------------------------------------------------------------
exclude_cols_numeric <- c(
  "ID",
  "sample",
  "startDate",
  "endDate",
  "ncrf1_dem.date_of_birth.txt",
  "ncrf1_dem.country_of_birth.txt",
  "ncrf1_dem.ethnicity.txt",
  "ncrf1_dem.please_describe_these_difficulties.txt",
  "ncrf1_dem.carers_occupation.txt"
  )


## ----COVIDCNS select-----------------------------------------------------------------------------------------
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
         ncrf1_dem.sex_at_birth,
         ncrf1_dem.date_of_birth.txt,
         ncrf1_dem.patient_currently_hospitalised,
         ncrf1_dem.patient_currently_pregnant,
         ncrf1_dem.patient_currently_incarcerated,
         ncrf1_dem.patient_in_long_term_care_facility,
         ncrf1_dem.country_of_birth.txt,
         ncrf1_dem.ethnicity.txt,
         ncrf1_dem.level_of_education,
         ncrf1_dem.child_attend_school_type,
         ncrf1_dem.does_the_child_receive_educational_support,
         ncrf1_dem.childs_education_performance_illness,
         ncrf1_dem.difficulties_illness_child_impact,
         ncrf1_dem.please_describe_these_difficulties.txt,
         ncrf1_dem.will_the_carers_seek_additional_educational_support,
         ncrf1_dem.carers_report_return_school,
         ncrf1_dem.carers_occupation.txt,
         ncrf1_dem.carers_highest_level_of_education_
         ) %>%
  add_numeric_1(exclude = exclude_cols_numeric)

# Inspect colnames
covidcns_dat_id %>%
  colnames()


## ----COVIDCNS number excluded--------------------------------------------------------------------------------
# Inspect dimensions of new data set
covidcns_dat_id %>%
  dim()

# Inspect number of rows dropped
covidcns_excluded <- dim(covidcns_dat_id)[1] - dim(covidcns_dat)[1]
covidcns_excluded


## ----COVIDCNS inspect missingness----------------------------------------------------------------------------
covidcns_miss_map <- covidcns_dat_id %>% 
  missmap()

covidcns_miss_map


## ----Create dat----------------------------------------------------------------------------------------------
dat <- covidcns_dat_id 

# Check
dat %>% glimpse()


## ----Discard empty vars--------------------------------------------------------------------------------------
dat <- dat %>%
  discard(~all(is.na(.)))


## ----Recode NA values----------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(ends_with("numeric"),
                ~case_when(. == -55 ~ -555,
                           . == -77 ~ -777,
                           . == -88 ~ -888,
                           . == -99 ~ -999,
                           TRUE ~ .)
                )
         )


## ----List unique values levels-------------------------------------------------------------------------------
ulst <- sapply(dat, unique)
llst <- sapply(dat, levels)


## ----Vector categorical values-------------------------------------------------------------------------------
vals_cat_1 <- c(
  "Male",
  "Female",
  "Seen but not answered",
  "Not specified",
  NA
)
vals_cat_2 <- c(
  "Yes",
  "No",
  "Seen but not answered",
  NA
)
vals_cat_3 <- c(
  "Seen but not answered",
  "Primary school",
  "Secondary school",
  "Graduate",
  "Postgraduate",
  NA
)



## ----List categorical values vectors-------------------------------------------------------------------------
values_cat_list <- list(
  vals_cat_1,
  vals_cat_2,
  vals_cat_2,
  vals_cat_2,
  vals_cat_2,
  vals_cat_3
  )


## ----Remove categorical values vectors-----------------------------------------------------------------------
rm(
  vals_cat_1,
  vals_cat_2,
  vals_cat_3
)


## ----Set list names categorical------------------------------------------------------------------------------
names(values_cat_list) <- c(
  "ncrf1_dem.sex_at_birth",
  "ncrf1_dem.patient_currently_hospitalised",
  "ncrf1_dem.patient_currently_pregnant",
  "ncrf1_dem.patient_currently_incarcerated",
  "ncrf1_dem.patient_in_long_term_care_facility",
  "ncrf1_dem.level_of_education"
)


## ----Imp_clean categorical variables-------------------------------------------------------------------------
imp_clean(values_list = values_cat_list,
          dat = dat)


## ----Summary table categorical variables---------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(names(values_cat_list)),
    missing_text = "Missing")


## ----Vector numeric values-----------------------------------------------------------------------------------
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
  -777,
  NA
)


## ----List numeric values vectors-----------------------------------------------------------------------------
values_num_list <- list(
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_2
)


## ----Remove numeric values vectors---------------------------------------------------------------------------
rm(
  vals_num_1,
  vals_num_2
)


## ----Set list names numeric----------------------------------------------------------------------------------
names(values_num_list) <- c(
  "ncrf1_dem.sex_at_birth_numeric",
  "ncrf1_dem.patient_currently_hospitalised_numeric",
  "ncrf1_dem.patient_currently_pregnant_numeric",
  "ncrf1_dem.patient_currently_incarcerated_numeric",
  "ncrf1_dem.patient_in_long_term_care_facility_numeric",
  "ncrf1_dem.level_of_education_numeric"
)


## ----Imp_clean numeric variables-----------------------------------------------------------------------------
imp_clean(values_list = values_num_list,
          dat = dat)


## ----Summary table numeric variables-------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(names(values_num_list)),
    missing_text = "Missing")


## ----Vector date vars----------------------------------------------------------------------------------------
variables_date <- c(
  "ncrf1_dem.date_of_birth.txt"
)


## ----Inspect date vars---------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_date)) %>%
  glimpse()


## ----Date vars recode -77 to NA------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(all_of(variables_date),
                ~na_if(., "-77")
                )
         )


## ----Parse date vars-----------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(all_of(variables_date),
                ~lubridate::parse_date_time(x = .,
                                            orders = c("d m y", "d/m/y", "d.m.y"),
                                            tz = "Europe/London")
                )
         ) %>%
  mutate(across(all_of(variables_date),
                ~as.POSIXct(.,
                            origin = lubridate::origin)
                )
         )


## ----Recheck parsed date vars--------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_date)) %>%
  tbl_summary(missing_text = "Missing")


## ------------------------------------------------------------------------------------------------------------
str(dat$ncrf1_dem.date_of_birth.txt)
str(dat$startDate)
dat$ncrf1_dem.date_of_birth.txt


## ----age from DOB--------------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(ncrf1_dem.dob_age = floor((startDate - ncrf1_dem.date_of_birth.txt)/dyears(1)))

dat$ncrf1_dem.dob_age <- set_label(dat$ncrf1_dem.dob_age, "Age")

str(dat$ncrf1_dem.dob_age)


## ----Create cont vars vector---------------------------------------------------------------------------------
variables_cont <- c(
  "ncrf1_dem.dob_age"
)


## ----Inspect cont vars---------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_cont)) %>%
  tbl_summary(
    missing_text = "Missing",
    statistic = list(all_continuous() ~ "{median} ({p0}, {p5}, {p95}, {p100})"))


## ----Create matrix limits------------------------------------------------------------------------------------
limits_mat <- rbind(
  c(0, 117)
  )


## ----Set lim_mat names---------------------------------------------------------------------------------------
rownames(limits_mat) <- variables_cont
colnames(limits_mat) <- c("Lower", "Upper")


## ----Cont_clean cont vars------------------------------------------------------------------------------------
cont_list <- cont_clean(
  variables = variables_cont,
  limits_mat = limits_mat,
  dat = dat
)
for (i in 1:length(variables_cont)){
  print(paste0("Implausibles in ", variables_cont[i], ": ", cont_list[[variables_cont[i]]]$Count))
}


## ----Mutate cont vars----------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(
    ncrf1_dem.dob_age = cont_list$ncrf1_dem.dob_age$Replacement
  )


## ----Inspect after cleaning----------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_cont)) %>%
  tbl_summary(
    missing_text = "Missing",
    statistic = list(all_continuous() ~ "{median} ({p0}, {p5}, {p95}, {p100})")
  )


## ----Vector text vars----------------------------------------------------------------------------------------
variables_text <- c(
  "ncrf1_dem.country_of_birth.txt",
  "ncrf1_dem.ethnicity.txt"
)


## ----View text vars------------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_text)) %>%
  tbl_summary()


## ----COVIDCNS recode country of birth variable---------------------------------------------------------------
dat <- dat %>% 
  mutate(across(ncrf1_dem.country_of_birth.txt,
                ~case_when(
                  str_detect(ncrf1_dem.country_of_birth.txt, "rit") |
                    str_detect(ncrf1_dem.country_of_birth.txt, "nite") |
                    str_detect(ncrf1_dem.country_of_birth.txt, "ngla") |
                    str_detect(ncrf1_dem.country_of_birth.txt, "ale") |
                    str_detect(ncrf1_dem.country_of_birth.txt, "relan") |
                    str_detect(ncrf1_dem.country_of_birth.txt, "uk") |
                    str_detect(ncrf1_dem.country_of_birth.txt, "UK")
                    ~ "United Kingdom",
                  
                  str_detect(ncrf1_dem.country_of_birth.txt, "Not") |
                    str_detect(ncrf1_dem.country_of_birth.txt, "not") |
                    str_detect(ncrf1_dem.country_of_birth.txt, "nk") |
                    str_detect(ncrf1_dem.country_of_birth.txt, "NK") |
                    str_detect(ncrf1_dem.country_of_birth.txt, "arribea") |
                    str_detect(ncrf1_dem.country_of_birth.txt, "urdista") |
                    str_detect(ncrf1_dem.country_of_birth.txt, "ndicat")
                    ~ "Not Known",
                  
                  str_detect(ncrf1_dem.country_of_birth.txt, "iberi")
                  ~ "Liberia",
                  
                  str_detect(ncrf1_dem.country_of_birth.txt, "igeri")
                  ~ "Nigeria",
                  
                  str_detect(ncrf1_dem.country_of_birth.txt, "akista")
                  ~ "Pakistan",
                  
                  str_detect(ncrf1_dem.country_of_birth.txt, "-77")
                  ~ "Seen but not answered",
                  
                  TRUE ~ .)))

dat$ncrf1_dem.country_of_birth.txt <- set_label(dat$ncrf1_dem.country_of_birth.txt, "Country of birth:")


## ----Text vars unique----------------------------------------------------------------------------------------
dat %>%
  select(ncrf1_dem.ethnicity.txt) %>%
  sapply(., unique) %>%
  sort()


## ----COVIDCNS recode ethnicity variable----------------------------------------------------------------------
dat <- dat %>% 
  mutate(ncrf1_dem.simplified_ethnicity =
                case_when(
                  str_detect(ncrf1_dem.ethnicity.txt, "ixed") ~ "Mixed",
                  
                  str_detect(ncrf1_dem.ethnicity.txt, "hite") ~ "White",
                  
                  
                  str_detect(ncrf1_dem.ethnicity.txt, "lack") ~ "Black",
                  str_detect(ncrf1_dem.ethnicity.txt, "fri") ~ "Black",
                  str_detect(ncrf1_dem.ethnicity.txt, "Car") ~ "Black",
                  
                  str_detect(ncrf1_dem.ethnicity.txt, "sia") ~ "Asian",
                  str_detect(ncrf1_dem.ethnicity.txt, "SIA") ~ "Asian",
                  str_detect(ncrf1_dem.ethnicity.txt, "Ind") ~ "Asian",
                  str_detect(ncrf1_dem.ethnicity.txt, "Beng") ~ "Asian",
                  str_detect(ncrf1_dem.ethnicity.txt, "Punj") ~ "Asian",
                  str_detect(ncrf1_dem.ethnicity.txt, "Pak") ~ "Asian",
                  
                  str_detect(ncrf1_dem.ethnicity.txt, "the") ~ "Other",
                  str_detect(ncrf1_dem.ethnicity.txt, "Kurd") ~ "Other",
                  str_detect(ncrf1_dem.ethnicity.txt, "Cypr") ~ "Other",
                  str_detect(ncrf1_dem.ethnicity.txt, "Fil") ~ "Other",
                  str_detect(ncrf1_dem.ethnicity.txt, "ARA") ~ "Other",
                  
                  str_detect(ncrf1_dem.ethnicity.txt, "Bri") ~ "White",
                  str_detect(ncrf1_dem.ethnicity.txt, "BRI") ~ "White",
                  str_detect(ncrf1_dem.ethnicity.txt, "Pol") ~ "White",
                  str_detect(ncrf1_dem.ethnicity.txt, "Wel") ~ "White",
                  str_detect(ncrf1_dem.ethnicity.txt, "Iris") ~ "White",
                  str_detect(ncrf1_dem.ethnicity.txt, "Engl") ~ "White",
                  
                  str_detect(ncrf1_dem.ethnicity.txt, "nk") ~ NA_character_,
                  str_detect(ncrf1_dem.ethnicity.txt, "NK") ~ NA_character_,
                  str_detect(ncrf1_dem.ethnicity.txt, "ot") ~ NA_character_)
         )

dat$ncrf1_dem.simplified_ethnicity <- set_label(dat$ncrf1_dem.simplified_ethnicity, "Ethnicity:")


## ----Recheck text vars---------------------------------------------------------------------------------------
dat %>%
  select(ncrf1_dem.ethnicity.txt, ncrf1_dem.simplified_ethnicity) %>%
  View()


## ----Check colnames------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file---------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/neuro_case_report/ncrf1_dem_covidcns_clean.rds")
    )

