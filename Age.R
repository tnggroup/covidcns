## ----setup, include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      comment=NA,
                      prompt=FALSE,
                      cache=FALSE)


## ----Delete everything in your global environment---------------------------------------------------------------------
remove(list = ls())


## ----Read in functions------------------------------------------------------------------------------------------------
source(file = "scripts/functions/remove_duplicates.R")
source(file = "scripts/functions/sumscores.R")
source(file = "scripts/functions/package_check.R")


## ----Install load dependencies----------------------------------------------------------------------------------------
packages = c(
  "summarytools",
  "sjlabelled",
  "data.table",
  "Amelia",
  "lubridate",
  "tidyverse"
  )
package_check(packages)


## ----Recent date------------------------------------------------------------------------------------------------------
date = Sys.Date()
date


## ----Read in file with path to ilovecovidcns channel on Teams---------------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----read in data-----------------------------------------------------------------------------------------------------
dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/dem_covid_cns.rds")
  ) 
  
# check variable names in dataframe
dat %>%
  colnames()

# Inspect dimensions of dataframe (number of rows and columns)
dat %>%
  dim()


## ----select & rename relevant columns---------------------------------------------------------------------------------
dat_id <- dat %>% # new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  distinct(externalDataReference, .keep_all = TRUE) %>% # Changed to distinct due to NA coercion
  add_column(sample = "COVIDCNS", 
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference,# ID
         sample,
         startDate,
         endDate,
         dem.how_old_are_you_now.txt = dem.required_question_eligibility_criteria.txt, #self-reported age at sign up 
         dem.day, # day of birth
         dem.month, # month of birth
         dem.year # year of birth
         )  %>% 
   
  rename_with( ~ paste(.x, "unc", sep = "_"), starts_with("dem"))
 
# Inspect colnames
dat_id %>%
  colnames()


## ----number excluded--------------------------------------------------------------------------------------------------
# Inspect dimensions of new data set
dat_id %>%
  dim()

# Inspect number of rows dropped
excluded <- dim(dat_id)[1]-dim(dat)[1]
excluded



## ----inspect missingness----------------------------------------------------------------------------------------------
 miss_map <- dat_id %>% 
   missmap()
 miss_map


## ----check values-----------------------------------------------------------------------------------------------------
dat_id %>%
  freq(dem.how_old_are_you_now.txt_unc)


## ----dem.how_old_are_you_now.txt_unc conversion to positive value-----------------------------------------------------
dat_id <- dat_id %>%
  mutate(
    dem.how_old_are_you_now.txt_unc =
      abs(
        as.numeric(
        dem.how_old_are_you_now.txt_unc)
      )
  )
#Check
dat_id %>%
  
  freq(dem.how_old_are_you_now.txt_unc)


## ----current age define limits----------------------------------------------------------------------------------------
age_lower_limit = 16
age_upper_limit = 117


## ----age outlier count------------------------------------------------------------------------------------------------
dat_id %>%
  filter(
    dem.how_old_are_you_now.txt_unc > age_upper_limit | # older than the age limit
      dem.how_old_are_you_now.txt_unc < age_lower_limit # younger than the age limit
    ) %>%
  nrow()


## ----age Recode outliers to -666--------------------------------------------------------------------------------------
dat_id <- dat_id%>%
    mutate(
      dem.how_old_are_you_now.txt = # remove "_unc" from the end to show that it is now cleaned
        if_else(
          dem.how_old_are_you_now.txt_unc > age_upper_limit |
            dem.how_old_are_you_now.txt_unc < age_lower_limit,
          true = -666,
          false = dem.how_old_are_you_now.txt_unc,
          missing = NA_real_
        )
    )


## ----age after recoding to -666---------------------------------------------------------------------------------------
dat_id %>%
  freq(dem.how_old_are_you_now.txt)


## ----Inspect variables------------------------------------------------------------------------------------------------
dat_id %>% 
  freq(dem.day_unc)

dat_id %>% 
  freq(dem.month_unc)

dat_id %>% 
  freq(dem.year_unc)


## ----dem.day.unc conversion to positive values and numeric------------------------------------------------------------
dat_id <- dat_id %>%
  mutate(
    dem.day_unc =
      abs(
        as.numeric(
        dem.day_unc)
      )
  )
#Check
dat_id %>%
  freq(dem.day_unc)


## ----dem.month_unc conversion to positive values and numeric----------------------------------------------------------
dat_id <- dat_id %>%
  mutate(
    dem.month_unc =
      abs(
        as.numeric(
        dem.month_unc)
      )
  )
#Check
dat_id %>%
  freq(dem.month_unc)


## ----dem.year_unc conversion to positive values and numeric-----------------------------------------------------------
dat_id <- dat_id %>%
  mutate(
    dem.year_unc =
      abs(
        as.numeric(
        dem.year_unc)
      )
  )
#Check
dat_id %>%
  freq(dem.year_unc) 


## ----Add years to birthyear-------------------------------------------------------------------------------------------
#add values to birth year
dat_id <- dat_id %>%
  mutate(dem.year_unc = dem.year_unc + 1919) ##add 1919 years

dat_id %>%
  freq(dem.year_unc)

  


## ----set minimum and maximum values dob-------------------------------------------------------------------------------
# Day
day.min.scale = 1
day.max.scale = 31

# Month
month.min.scale = 1
month.max.scale = 12

# Year
year.min.scale = 1899
year.max.scale = 2021 # note max age set later in age_upper_limit


## ----Clean dem.day----------------------------------------------------------------------------------------------------
dat_id <- dat_id %>% 
  mutate(dem.day_unc_clean =
           case_when(dem.day_unc < day.min.scale | dem.day_unc > day.max.scale ~ -666, # implausible value
                     TRUE ~ dem.day_unc)
         ) # leave as is


# Check for implausible values
dat_day_imp_n <- dat_id %>% 
  filter(dem.day_unc_clean == -666) %>% 
  nrow()

# Check
dat_day_imp_n

# If statement
if (dat_day_imp_n == 0) {
  print(paste0("The number of implausible values in the COVID CNS day of birth variable is ", dat_day_imp_n, ". This is fine."))
  setnames(dat_id,
         old = "dem.day_unc_clean",
         new = "dem.day")
} else {
  print(paste0("The number of implausible values in the COVID CNS day of birth variable is ", dat_day_imp_n, ". Please investigate."))
  setnames(dat_id,
         old = "dem.day_unc_clean",
         new = "dem.day")
}

# Check
colnames(dat_id)

# Check cleaned variable
dat_id %>% 
  freq(dem.day) 


## ----Clean dem.month--------------------------------------------------------------------------------------------------
dat_id <- dat_id %>% 
  mutate(dem.month_unc_clean =
           case_when(dem.month_unc < month.min.scale | dem.month_unc > month.max.scale ~ -666, # implausible value
                     TRUE ~ dem.month_unc)
         ) # leave as is

# Check for implausible values
dat_month_imp_n <- dat_id %>% 
  filter(dem.month_unc_clean == -666) %>% 
  nrow()

# Check
dat_month_imp_n

# If statement
if (dat_month_imp_n == 0) {
  print(paste0("The number of implausible values in the COVID CNS month of birth variable is ", dat_month_imp_n, ". This is fine."))
  setnames(dat_id,
         old = "dem.month_unc_clean",
         new = "dem.month")
} else {
  print(paste0("The number of implausible values in the COVID CNS month of birth variable is ", dat_month_imp_n, ". Please investigate."))
  setnames(dat_id,
         old = "dem.month_unc_clean",
         new = "dem.month")
}

# Check
colnames(dat_id)

# Check clean variable
dat_id %>% 
  freq(dem.month)


## ----dem Clean dem.year-----------------------------------------------------------------------------------------------
dat_id <- dat_id %>% 
  mutate(dem.year_unc_clean =
           case_when(dem.year_unc < year.min.scale | dem.year_unc > year.max.scale ~ -666,  # implausible value
                     TRUE ~ dem.year_unc)
         )

# Check for implausible values
year_imp_n <- dat_id %>% 
  filter(dem.year_unc_clean == -666) %>% 
  nrow()

# Check
year_imp_n

# If statement
if (year_imp_n == 0) {
  print(paste0("The number of implausible values in the COVID CNS year of birth variable is ", year_imp_n, ". This is fine."))
  setnames(dat_id,
         old = "dem.year_unc_clean",
         new = "dem.year")
} else {
  print(paste0("The number of implausible values in the COVID CNS year of birth variable is ", year_imp_n, ". Please investigate."))
  setnames(dat_id,
         old = "dem.year_unc_clean",
         new = "dem.year")
}

# Check
dat_id %>% 
  freq(dem.year)




## ----Drop all -666 to NA----------------------------------------------------------------------------------------------
dat_no_imps <- dat_id %>%
  mutate_if(is.numeric, ~na_if(., -666)) # Implausible value


## ----Drop all variables with "_unc" on the end------------------------------------------------------------------------
dat_clean <- dat_no_imps %>% 
  select(!contains("_unc")) # selects ID, sample and drops all uncleaned variables

# Check (there should be no variables with "_unc" in the name now)
colnames(dat_clean)


## ----make birth date--------------------------------------------------------------------------------------------------
dat_clean <- dat_clean %>%
  mutate(dem.dob = make_date(dem.year, dem.month, dem.day))

# check
dat_clean %>% 
  select(dem.day,
         dem.month,
         dem.year,
         dem.dob) %>% 
  head()


## ----calculate age from birth date and startdate----------------------------------------------------------------------
dat_clean$dem.dob_age <- interval(
    start = dat_clean$dem.dob,
    end = dat_clean$startDate) %/% # use modulo to round down by %/%
        duration(num = 1, units = "years")


# check COVID CNS age variables
dat_clean %>%
  select(dem.dob,
         dem.dob_age,
         dem.how_old_are_you_now.txt) %>% 
  head()


## ----difference self-report age and DOB age---------------------------------------------------------------------------
# check COVID CNS 
diff_age_variabl_n <- dat_clean %>%
  filter(
    dem.dob_age != dem.how_old_are_you_now.txt) %>%
  select(ID, 
         dem.dob_age,
         dem.how_old_are_you_now.txt,
         dem.dob) 

diff_age_variabl_n


## ----dem.dob_age conversion to positive values and numeric------------------------------------------------------------
dat_clean <- dat_clean %>%
  mutate(
    dem.dob_age =
      abs(
        as.numeric(
        dem.dob_age)
      )
  )
#Check
dat_clean %>%
  freq(dem.dob_age)


## ----DOB age at sign up outlier count---------------------------------------------------------------------------------
dat_clean %>%
  filter(
    dem.dob_age > age_upper_limit | # older than the age limit
      dem.dob_age < age_lower_limit # younger than the age limit
    ) %>%
  nrow()


## ----DOB age at sign up Recode outliers to -666-----------------------------------------------------------------------
dat_clean <- dat_clean %>%
    mutate(
      dem.dob_age = 
        if_else(
          dem.dob_age > age_upper_limit |
            dem.dob_age < age_lower_limit,
          true = -666,
          false = dem.dob_age,
          missing = NA_real_
        )
    )


## ----DOB age at sign up after recoding to -666------------------------------------------------------------------------
dat_clean %>%
  freq(dem.dob_age)


## ----check colnames---------------------------------------------------------------------------------------------------
colnames(dat_clean)


## ----Save variables for export----------------------------------------------------------------------------------------
export_variables <- dat_clean %>% 
  select(ID,
         startDate,
         endDate,
         sample,
         dem.how_old_are_you_now.txt,
         dem.dob_age) %>%
  colnames()


## ----Write cleaned variables to a .rds file---------------------------------------------------------------------------
dat_clean %>% 
  select(all_of(export_variables)) %>% 
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/age_covidcns_clean.rds")
    )


## ----INTERNAL ONLY Write cleaned variables to a .rds file-------------------------------------------------------------
dat_clean %>% 
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/age_covidcns_clean_INTERNAL_ONLY.rds")
    )

