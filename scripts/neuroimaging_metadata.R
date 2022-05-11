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
source(file = "scripts/functions/recode_check.R")


## ----Install load dependencies----------------------------------------------------------------------------------------
packages <- c("summarytools", "sjlabelled", "Amelia", "gtsummary", "tidyverse")
package_check(packages)


## ----Read in file with path to ilovecovidcns channel on teams-------------------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----COVIDCNS load data-----------------------------------------------------------------------------------------------
covidcns_dat <- read_csv(
  file = paste0(ilovecovidcns, "/data_raw/assessment_status/Covid_CNS_neuro_data_03.03.2022.csv"),
  col_types = list(
    kit_id = col_character(),
    primary_site = col_character(),
    site_name = col_character(),
    recurited_at_site = col_character(),
    recurited_at_site_name = col_character(),
    processing_problems = col_character(),
    notes = col_character()
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
  drop_na(kit_id) %>% # Drop participants with no ID
  distinct(kit_id, .keep_all = TRUE) %>% # Remove duplicate IDs
  add_column(sample = "COVIDCNS",
             .after = "kit_id") %>% # Create new sample column
  select(
         ID = kit_id, # ID
         primary_site_code = primary_site,
         primary_site_name = site_name,
         recruitment_site_code = recurited_at_site,
         recruitment_site_name = recurited_at_site_name,
         processing_problems,
         notes
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


## ----List unique values-----------------------------------------------------------------------------------------------
ulst <- sapply(dat, unique)


## ----Vector categorical values----------------------------------------------------------------------------------------
vals_cat_1 <- c(
  "RJZ",
  "RET",
  "RGT",
  "RM3",
  "RV5",
  "RTH",
  "RJ1",
  "7A4",
  "RHQ",
  "RRV",
  NA
)

vals_cat_2 <- c(
  "KCH",
  "Liverpool",
  "Cambridge",
  "Salford Royal",
  "SLaM",
  "Oxford",
  "GSTT",
  "Cardiff",
  "Sheffield",
  "UCLH",
  NA
)

vals_cat_3 <- c(
  "RJZ01",
  "RET20",
  "RGT01",
  "REM21",
  "REMRQ",
  "RM301",
  "RV504",
  "RTH08",
  "RJ122",
  "7A4BV",
  "RHQHH",
  "RRVNQ",
  NA
)

vals_cat_4 <- c(
  "King's College Hospital (Denmark Hill)",
  "The Walton Centre",
  "Addenbrooke's Hospital",
  "Aintree University Hospital",
  "The Royal Liverpool University Hospital",
  "Salford Royal Hospital",
  "Maudsley Hospital",
  "John Radcliffe Hospital",
  "St Thomas' Hospital",
  "University Hospital of Wales",
  "Royal Hallamshire Hospital",
  "The National Hospital for Neurology and Neurosurgery",
  NA
)


## ----List categorical values vectors----------------------------------------------------------------------------------
values_cat_list <- list(
  vals_cat_1,
  vals_cat_2,
  vals_cat_3,
  vals_cat_4
)


## ----Set list names---------------------------------------------------------------------------------------------------
names(values_cat_list) <- c(
  "primary_site_code",
  "primary_site_name",
  "recruitment_site_code",
  "recruitment_site_name"
)


## ----Imp_clean categorical variables----------------------------------------------------------------------------------
imp_clean(values_list = values_cat_list,
          dat = dat)


## ----Summary table categorical variables------------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(names(values_cat_list)),
    missing_text = "Missing")


## ----Inspect incorrect variable---------------------------------------------------------------------------------------
dat %>%
  select(
    recruitment_site_name) %>%
  tbl_summary(missing_text = "Missing")


## ----Recode incorrect variable----------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(recruitment_site_name,
                ~case_when(
                  . == "THE ROYAL LIVERPOOL UNIVERSITY HOSPITAL (RLUH NHS TRUST)" ~ "The Royal Liverpool University Hospital",
                  TRUE ~ .)))


## ----Recheck variable coding------------------------------------------------------------------------------------------
recode_check(variables = "recruitment_site_name",
          values = vals_cat_4,
          data = dat)


## ----Add attempted scan column----------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(attempted_scan = 1)

dat %>%
  tbl_summary(include = attempted_scan)


## ----Inspect text vars------------------------------------------------------------------------------------------------
variables_text <- c(
  "processing_problems",
  "notes"
)

dat %>%
  select(all_of(variables_text)) %>%
  tbl_summary(
    missing_text = "Missing"
  )


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/assessment_status/COVID_CNS_neuroimaging_metadata_clean.rds")
    )

