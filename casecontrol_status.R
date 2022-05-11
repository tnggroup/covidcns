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
source(file = "scripts/functions/package_check.R")
source(file = "scripts/functions/imp_clean.R")


## ----Install load dependencies----------------------------------------------------------------------------------------
packages <- c("summarytools", "sjlabelled", "Amelia", "gtsummary", "tidyverse")
package_check(packages)


## ----Read in file with path to ilovecovidcns channel on teams---------------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----COVIDCNS load data-----------------------------------------------------------------------------------------------
covidcns_dat <- read_csv(
  file = paste0(ilovecovidcns, "/data_raw/assessment_status/COVID_CNS_case_control_status_04.03.2022.csv"),
  col_types = list(
    identifier = col_character(),
    type = col_character(),
    updated_at = col_character()
    )
  )
  
# Check variable names in dataframe
covidcns_dat %>%
  colnames()

# Inspect dimensions of dataframe 
covidcns_dat %>%
  dim()


## ----COVIDCNS select--------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat %>% #new dataset with ID
  drop_na(identifier) %>% # Drop participants with no ID
  distinct(identifier, .keep_all = TRUE) %>% # Remove duplicate IDs
  select(
    ID = identifier, # ID
    case_control = type,
    updated_at)

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


## ----List unique values-----------------------------------------------------------------------------------------------
ulst <- sapply(dat, unique)


## ----Vector categorical values----------------------------------------------------------------------------------------
vals_cat_1 <- c(
  "Case: COVID-19 positive (i.e. neurological or psychiatric complication)",
  "Vaccine: COVID-19 vaccine complication (i.e. neurological or psychiatric complication)",
  "Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)",
  "Control: COVID-19 negative (e.g. pneumonia  sepsis)",
  "Control",
  NA
)



## ----List categorical values vectors----------------------------------------------------------------------------------
values_cat_list <- list(
  vals_cat_1
)


## ----Set list names---------------------------------------------------------------------------------------------------
names(values_cat_list) <- c(
  "case_control"
)


## ----Imp_clean categorical variables----------------------------------------------------------------------------------
imp_clean(values_list = values_cat_list,
          dat = dat)


## ----Summary table categorical variables------------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(names(values_cat_list)),
    missing_text = "Missing")


## ----Vector date variables--------------------------------------------------------------------------------------------
variables_date <- c(
  "updated_at"
)


## ----COVIDCNS inspect dates-------------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_date)) %>%
  glimpse()


## ----Date varss recode -77 to NA--------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(all_of(variables_date),
                ~na_if(., "-77")
                )
         )


## ----Parse date vars--------------------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(all_of(variables_date),
                ~lubridate::parse_date_time(x = .,
                                            orders = c("d/m/y %H:%M"),
                                            tz = "Europe/London")
                )
         )


## ----COVIDCNS check dates---------------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_date)) %>%
  tbl_summary(missing_text = "Missing")


## ----Define limits----------------------------------------------------------------------------------------------------
upper_limit <- as.POSIXct("2022-03-04")
lower_limit <- as.POSIXct("2020-01-30")


## ----Date vars recode outliers to NA----------------------------------------------------------------------------------
dat <- dat %>%
    mutate(across(all_of(variables_date),
                  ~ifelse(. > upper_limit | # bigger than the upper limit
                          . < lower_limit, # smaller than the lower limit
                          yes = NA_real_,
                          no = .)
                  )
           ) %>%
  mutate(across(all_of(variables_date),
                ~as.POSIXct(.,
                            origin = lubridate::origin)
                )
         )


## ----Recheck dates----------------------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_date)) %>%
  tbl_summary(missing_text = "Missing")


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/assessment_status/COVID_CNS_casecontrol_clean.rds")
    )

