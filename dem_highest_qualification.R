## ----setup, include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  comment = '',
  prompt = FALSE,
  cache = FALSE
)


## ----Delete everything in your global environment---------------------------------------------------------------------
remove(list = ls())


## ----Read in functions------------------------------------------------------------------------------------------------
source(file = "scripts/functions/remove_duplicates.R")
source(file = "scripts/functions/sumscores.R")
source(file = "scripts/functions/package_check.R")
source(file = "scripts/functions/imp_check.R")
source(file = "scripts/functions/add_numeric_1.R")


## ----Install load dependencies----------------------------------------------------------------------------------------
packages <- c("summarytools",
              "sjlabelled",
              "Amelia",
              "gtsummary",
              "tidyverse")
package_check(packages)


## ----Recent date------------------------------------------------------------------------------------------------------
date <- Sys.Date()
date


## ----Read in file with path to ilovecovidcns channel on teams---------------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----Covid cns load data----------------------------------------------------------------------------------------------
cns_dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/dem_covid_cns.rds")
  )
  
# Check variable names in dataframe
cns_dat %>%
  colnames()

# Inspect dimensions of dataframe 
cns_dat %>%
  dim()


## ----covid cns specify excluded columns-------------------------------------------------------------------------------
exclude_cols_numeric <- c(
  "ID",
  "sample",
  "startDate",
  "endDate",
  "dem.other_professional_qualifications_text.txt"
  )


## ----CNS select-------------------------------------------------------------------------------------------------------
cns_dat_id <- cns_dat %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  distinct(externalDataReference, .keep_all = TRUE) %>% # Changed to distinct due to NA coercion
  add_column(sample = "COVIDCNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         dem.college_or_university_degree,
         dem.a_levelsas_levels_or_equivalent,
         dem.o_levelsgcses_or_equivalent,
         dem.cses_or_equivalent,
         dem.nvq_or_hnd_or_hnc_or_equivalent,
         dem.other_professional_qualifications_,
         dem.other_professional_qualifications_text.txt,
         dem.none_of_the_above,
         dem.prefer_not_to_say
         ) %>%
  add_numeric_1(exclude = exclude_cols_numeric)

# Inspect colnames
cns_dat_id %>%
  colnames()


## ----cns number excluded----------------------------------------------------------------------------------------------
# Inspect dimensions of new data set
cns_dat_id %>%
  dim()

# Inspect number of rows dropped
cns_excluded <- dim(cns_dat_id)[1] - dim(cns_dat)[1]
cns_excluded


## ----inspect numeric variables----------------------------------------------------------------------------------------
cns_dat_id %>%
  select(all_of(ends_with("numeric"))) %>%
  tbl_summary(missing_text = "Missing")


## ----inspect missingness----------------------------------------------------------------------------------------------
miss_map <- cns_dat_id %>% 
  missmap()

miss_map


## ----Recode NA values-------------------------------------------------------------------------------------------------
 dat <- cns_dat_id %>%
   mutate(across(ends_with("numeric"),
                 ~case_when(
                   . == -55 ~ -555,
                   . == -77 ~ -777,
                   . == -88 ~ -888,
                   . == -99 ~ -999,
                   TRUE ~ .)))


## ----vector categorical variables university--------------------------------------------------------------------------
variables_categorical_university <-
  c(
    "dem.college_or_university_degree"
    )
variables_categorical_university


## ----vector categorical values university degree----------------------------------------------------------------------
values_categorical_university <- 
  c(
    "Not College or university degree",
    "College or university degree",
    NA
  )
values_categorical_university


## ----imp_check categorical variables university-----------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical_university,
          values = values_categorical_university)
#There are no implausible values in the dataset. Can leave these variables as they are


## ----vector categorical variables a levels----------------------------------------------------------------------------
variables_categorical_a_levels <-
  c(
    "dem.a_levelsas_levels_or_equivalent"
    )
variables_categorical_a_levels


## ----vector categorical values a levels-------------------------------------------------------------------------------
values_categorical_a_levels <- 
  c(
    "Not A levels/AS levels or equivalent",
    "A levels/AS levels or equivalent",
    NA
  )
values_categorical_a_levels


## ----imp_check categorical variables a levels-------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical_a_levels,
          values = values_categorical_a_levels)
#There are no implausible values in the dataset. Can leave these variables as they are


## ----vector categorical variables O levels----------------------------------------------------------------------------
variables_categorical_o_levels <-
  c(
    "dem.o_levelsgcses_or_equivalent"
    )
variables_categorical_o_levels


## ----vector categorical values O levels-------------------------------------------------------------------------------
values_categorical_o_levels <- 
  c(
    "Not O levels/GCSEs or equivalent",
    "O levels/GCSEs or equivalent",
    NA
  )
values_categorical_o_levels


## ----imp_check categorical variables O levels-------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical_o_levels,
          values = values_categorical_o_levels)
#There are no implausible values in the dataset. Can leave these variables as they are


## ----vector categorical variables cses--------------------------------------------------------------------------------
variables_categorical_cses <-
  c(
    "dem.cses_or_equivalent"
    )
variables_categorical_cses


## ----vector categorical values cses-----------------------------------------------------------------------------------
values_categorical_cses <- 
  c(
    "Not CSEs or equivalent",
    "CSEs or equivalent",
    NA
  )
values_categorical_cses


## ----imp_check categorical variables cses-----------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical_cses,
          values = values_categorical_cses)
#There are no implausible values in the dataset. Can leave these variables as they are


## ----vector categorical variables nvq---------------------------------------------------------------------------------
variables_categorical_nvq <-
  c(
    "dem.nvq_or_hnd_or_hnc_or_equivalent"
    )
variables_categorical_nvq


## ----vector categorical values nvq------------------------------------------------------------------------------------
values_categorical_nvq <- 
  c(
    "Not NVQ or HND or HNC or equivalent",
    "NVQ or HND or HNC or equivalent",
    NA
  )
values_categorical_nvq


## ----imp_check categorical variables nvq------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical_nvq,
          values = values_categorical_nvq)
#There are no implausible values in the dataset. Can leave these variables as they are


## ----vector categorical variables other-------------------------------------------------------------------------------
variables_categorical_other <-
  c(
    "dem.other_professional_qualifications_"
    )
variables_categorical_other


## ----vector categorical values other----------------------------------------------------------------------------------
values_categorical_other <- 
  c(
    "Not Other professional qualifications",
    "Other professional qualifications",
    NA
  )
values_categorical_other


## ----imp_check categorical variables other----------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical_other,
          values = values_categorical_other)
#The number of implausible values in the dataset is 227. Please investigate.


## ----Unify all labels for other professional qualifications-----------------------------------------------------------
dat <- dat %>%  
  mutate(dem.other_professional_qualifications_ = 
           fct_recode(dem.other_professional_qualifications_,
    "Not Other professional qualifications" = "Not Other professional qualifications (e.g. nursing, teaching)",
    "Other professional qualifications" = "Other professional qualifications (e.g. nursing, teaching)")
  )


## ----redo imp_check categorical variables other-----------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical_other,
          values = values_categorical_other)
#There are no implausible values in the dataset. Can leave these variables as they are


## ----vector categorical variables none of the above-------------------------------------------------------------------
variables_categorical_none <-
  c(
    "dem.none_of_the_above"
    )
variables_categorical_none


## ----vector categorical values none of the above----------------------------------------------------------------------
values_categorical_none <- 
  c(
    "Not None of the above",
    "None of the above",
    NA
  )
values_categorical_none


## ----imp_check categorical variables none of the above----------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical_none,
          values = values_categorical_none)
#There are no implausible values in the dataset. Can leave these variables as they are


## ----vector categorical variables Prefer not to say-------------------------------------------------------------------
variables_categorical_prefer_not_to_say <-
  c(
    "dem.prefer_not_to_say"
    )
variables_categorical_prefer_not_to_say


## ----vector categorical values Prefer not to say----------------------------------------------------------------------
values_categorical_prefer_not_to_say <- 
  c(
    "Not Prefer not to say",
    "Prefer not to say",
    NA
  )
values_categorical_prefer_not_to_say


## ----imp_check categorical variables Prefer not to say----------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical_prefer_not_to_say,
          values = values_categorical_prefer_not_to_say)
#There are no implausible values in the dataset. Can leave these variables as they are


## ----vector numeric variables 0_1-------------------------------------------------------------------------------------
variables_numeric_0_1 <-
  c(
    "dem.college_or_university_degree_numeric",
    "dem.a_levelsas_levels_or_equivalent_numeric",
    "dem.o_levelsgcses_or_equivalent_numeric",
    "dem.cses_or_equivalent_numeric",
    "dem.nvq_or_hnd_or_hnc_or_equivalent_numeric",
    "dem.other_professional_qualifications__numeric",
    "dem.none_of_the_above_numeric",
    "dem.prefer_not_to_say_numeric"
    )
variables_numeric_0_1


## ----vector numeric values 0_1----------------------------------------------------------------------------------------
values_numeric_0_1 <- 
  c(
    -777,
    0,
    1,
    NA
  )
values_numeric_0_1


## ----imp_check numeric variables 0_1----------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_numeric_0_1,
          values = values_numeric_0_1)
#There are no implausible values in the dataset. Can leave these variables as they are.


## ----creating single highest qualification variable-------------------------------------------------------------------
dat <- dat %>%
  mutate(
    dem.highest_education_numeric =
      case_when(
        dem.none_of_the_above_numeric == "1" ~ 0,
        dem.college_or_university_degree_numeric == "1" ~ 1,
        dem.a_levelsas_levels_or_equivalent_numeric == "1" ~ 2,
        dem.o_levelsgcses_or_equivalent_numeric == "1" ~ 3,
        dem.cses_or_equivalent_numeric == "1" ~ 4,
        dem.nvq_or_hnd_or_hnc_or_equivalent_numeric == "1" ~ 5,
        dem.other_professional_qualifications__numeric == "1" ~ 6,
        dem.prefer_not_to_say_numeric == "1" ~ -999
        )
    )

#recode the numeric version into a factor
dat <- dat %>%
  mutate(
    dem.highest_education =
      recode_factor(
        dem.highest_education_numeric,
        `0` = "None of the above" ,
        `1` = "College or university degree",
        `2` = "A levels/AS levels or equivalent",
        `3` = "O levels/GCSEs or equivalent",
        `4` = "CSEs or equivalent",
        `5` = "NVQ or HND or HNC or equivalent",
        `6` = "Other professional qualifications",
        `-999` = "Prefer not to say"
        )
    )

dat %>%
  freq(dem.highest_education)


## ----Export variables-------------------------------------------------------------------------------------------------
export_variables <- 
  c(
    "ID",
    "sample", 
    "startDate",                                       
    "endDate", 
    "dem.college_or_university_degree",
    "dem.college_or_university_degree_numeric",
    "dem.a_levelsas_levels_or_equivalent",
    "dem.a_levelsas_levels_or_equivalent_numeric",
    "dem.o_levelsgcses_or_equivalent",
    "dem.o_levelsgcses_or_equivalent_numeric",
    "dem.cses_or_equivalent",
    "dem.cses_or_equivalent_numeric",
    "dem.nvq_or_hnd_or_hnc_or_equivalent",
    "dem.nvq_or_hnd_or_hnc_or_equivalent_numeric",
    "dem.other_professional_qualifications_",
    "dem.other_professional_qualifications__numeric",
    "dem.none_of_the_above",
    "dem.none_of_the_above_numeric",
    "dem.prefer_not_to_say",
    "dem.prefer_not_to_say",
    "dem.highest_education",
    "dem.highest_education_numeric"
  )


## ----Save clean data as a .rds file-----------------------------------------------------------------------------------
dat %>% 
  select(all_of(export_variables)) %>% 
  saveRDS(file = 
            paste0(ilovecovidcns, "/data/latest_freeze/baseline/dem_highest_qualification_covidcns_clean.rds"))

