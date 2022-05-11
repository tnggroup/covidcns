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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/moca/moca_outp_covid_cns.rds")
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
         moca_outp.visuospatialexecutive.txt,
         moca_outp.naming.txt,
         moca_outp.attention,
         moca_outp.attention.1,
         moca_outp.attention.2,
         moca_outp.language,
         moca_outp.language.1,
         moca_outp.abstraction.txt,
         moca_outp.uncued_recall_scored_delayed.txt,
         moca_outp.orientation.txt,
         moca_outp.total_score.txt,
         moca_outp.notes.txt,
         moca_outp.did_the_participant_complete_a_moca
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
  mutate(across(c(moca_outp.visuospatialexecutive.txt,
                  moca_outp.naming.txt,
                  moca_outp.attention,
                  moca_outp.attention.1,
                  moca_outp.attention.2,
                  moca_outp.language,
                  moca_outp.language.1,
                  moca_outp.abstraction.txt,
                  moca_outp.uncued_recall_scored_delayed.txt,
                  moca_outp.orientation.txt,
                  moca_outp.did_the_participant_complete_a_moca),
                ~case_when(
                  . == -55 ~ -555,
                  . == -77 ~ -777,
                  . == -88 ~ -888,
                  . == -99 ~ -999,
                  TRUE ~ .)))


## ----List unique values-----------------------------------------------------------------------------------------------
ulst <- sapply(dat, unique)


## ----Vectors numeric values-------------------------------------------------------------------------------------------
values_1 <- c(
  0,
  1,
  2,
  3,
  4,
  5,
  -777,
  NA
  )

values_2 <- c(
  0,
  1,
  2,
  3,
  -777,
  NA
  )

values_3 <- c(
  0,
  1,
  2,
  -777,
  NA
  )

values_4 <- c(
  0,
  1,
  -777,
  NA
  )

values_5 <- c(
  0,
  1,
  2,
  3,
  -777,
  NA
  )

values_6 <- c(
  0,
  1,
  2,
  -777,
  NA
  )

values_7 <- c(
  0,
  1,
  -777,
  NA
  )

values_8 <- c(
  0,
  1,
  2,
  -777,
  NA
  )

values_9 <- c(
  0,
  1,
  2,
  3,
  4,
  5,
  -777,
  NA
  )

values_10 <- c(
  0,
  1,
  2,
  3,
  4,
  5,
  6,
  -777,
  NA
  )

values_11 <- c(seq(0, 30, 1), c(-777, NA))

values_12 <- c(
  0,
  1,
  -777,
  NA
)


## ----List values vectors----------------------------------------------------------------------------------------------
values_list <- list(
  values_1,
  values_2,
  values_3,
  values_4,
  values_5,
  values_6,
  values_7,
  values_8,
  values_9,
  values_10,
  values_11,
  values_12
)


## ----Vector numeric variables-----------------------------------------------------------------------------------------
variables_numeric <- c(
  "moca_outp.visuospatialexecutive.txt",
  "moca_outp.naming.txt",
  "moca_outp.attention",
  "moca_outp.attention.1",
  "moca_outp.attention.2",
  "moca_outp.language",
  "moca_outp.language.1",
  "moca_outp.abstraction.txt",
  "moca_outp.uncued_recall_scored_delayed.txt",
  "moca_outp.orientation.txt",
  "moca_outp.total_score.txt",
  "moca_outp.did_the_participant_complete_a_moca"
  )
variables_numeric


## ----Name values list-------------------------------------------------------------------------------------------------
names(values_list) <- variables_numeric


## ----Imp_check variables----------------------------------------------------------------------------------------------
# Create empty list
imp_list <- list()
# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(variables_numeric)) {
  imp_list[i] <- imp_check_1(data = dat,
                                     variables = names(values_list)[i],
                                     values = values_list[[i]]) 
}
# Name list with var names to correspond to imp_messages
names(imp_list) <- variables_numeric
# View list of imp_messages with corresponding var names
print(imp_list)


## ----Summary table categorical domains variables----------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(variables_numeric),
    missing_text = "Missing")


## ----Inspect incorrect variable---------------------------------------------------------------------------------------
dat %>%
  select(
    ID,
    moca_outp.attention,
    moca_outp.attention.1,
    moca_outp.attention.2,
    moca_outp.language,
    moca_outp.language.1) %>%
  filter(
    moca_outp.attention == 5|
      moca_outp.attention.1 == 5|
      moca_outp.language == 3
  )


## ----Recode incorrect variable----------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(moca_outp.attention,
                ~case_when(
                  . ==5 ~ -666,
                  TRUE ~ .)))

dat <- dat %>% 
  mutate(across(moca_outp.attention.1,
                ~case_when(
                  . == 5 ~ -666,
                  TRUE ~ .)))

dat <- dat %>% 
  mutate(across(moca_outp.language,
                ~case_when(
                  . == 3 ~ -666,
                  TRUE ~ .)))


## ----Recheck variable coding------------------------------------------------------------------------------------------
dat %>%
  select(
    moca_outp.attention,
    moca_outp.attention.1,
    moca_outp.language) %>%
  tbl_summary(missing_text = "Missing")


## ----Sumscores inputs-------------------------------------------------------------------------------------------------
keys <- c(
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1
  )

sum_vars <- c(
  "moca_outp.visuospatialexecutive.txt",
  "moca_outp.naming.txt",
  "moca_outp.attention",
  "moca_outp.attention.1",
  "moca_outp.attention.2",
  "moca_outp.language",
  "moca_outp.language.1",
  "moca_outp.abstraction.txt",
  "moca_outp.uncued_recall_scored_delayed.txt",
  "moca_outp.orientation.txt"
  )


## ----Generate sumscores-----------------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(
    moca_outp.adj_sum_score = 
         sumscores(input = dat,
                   sum_vars = sum_vars,
                   coding_keys = keys,
                   na_allowed = 0,
                   min_item = 0,
                   max_item = 6,
                   min_score = 0,
                   max_score = 30
                   )[["scores"]]
         )



## ----Numeric binary phenotype-----------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(
    moca_outp.cognition_numeric =
      case_when(
        moca_outp.adj_sum_score >= 26 ~ 1,
        moca_outp.adj_sum_score < 26 ~ 0
      )
  )
dat %>%
  select(moca_outp.cognition_numeric) %>%
  tbl_summary()


## ----Categorical binary phenotype variable----------------------------------------------------------------------------
dat <- dat %>%
  mutate(
    moca_outp.cognition =
      recode_factor(
        moca_outp.cognition_numeric,
        "0" = "Not Normal Cognition",
        "1" = "Normal Cognition"
      )
  )
dat %>%
  select(moca_outp.cognition) %>%
  tbl_summary()


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/moca/moca_outp_covidcns_clean.rds")
    )

