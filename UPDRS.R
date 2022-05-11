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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/updrs_covid_cns.rds")
  )
  
# Check variable names in dataframe
covidcns_dat %>%
  colnames()
# Inspect dimensions of dataframe 
covidcns_dat %>%
  dim()


## ----COVIDCNS specify excluded columns--------------------------------------------------------------------------------
exclude_cols_numeric <- c(
  "ID",
  "sample",
  "startDate",
  "endDate"
  )


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
         updrs.trouble_reading_handwriting_past,
         updrs.hobbies_trouble_past_week,
         updrs.trouble_turning_bed_past,
         updrs.shaking_tremor_past_week,
         updrs.left_arm,
         updrs.right_arm,
         updrs.left_leg,
         updrs.right_leg,
         updrs.whole_body,
         updrs.deep_chair_car_seat,
         updrs.balance_walking_past_week,
         updrs.feet_floor_freeze_stuck,
         updrs.speech_past_week_problems,
         updrs.awake_saliva_sleep_past,
         updrs.made_soft_chopped_avoid,
         updrs.trouble_handling_finger_foods,
         updrs.zippers_putting_problems_dressing,
         updrs.personal_hygiene_hair_slow,
         updrs.start_happening_required_question,
         updrs.start_happening_required_question.1,
         updrs.start_happening_required_question.2,
         updrs.start_happening_required_question.3,
         updrs.start_happening_required_question.4,
         updrs.start_happening_required_question.5,
         updrs.start_happening_required_question.6,
         updrs.start_happening_required_question.7,
         updrs.start_happening_required_question.8,
         updrs.start_happening_required_question.9,
         updrs.start_happening_required_question.10,
         updrs.start_happening_required_question.11,
         updrs.start_happening_required_question.12
         ) %>%
  add_numeric(exclude = exclude_cols_numeric)
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
  mutate(across(ends_with("numeric"),
                ~case_when(
                  . == -55 ~ -555,
                  . == -77 ~ -777,
                  . == -88 ~ -888,
                  . == -99 ~ -999,
                  TRUE ~ .)))


## ----List unique values-----------------------------------------------------------------------------------------------
ulst <- sapply(dat, unique)
llst <- sapply(dat, levels)


## ----Vector categorical values----------------------------------------------------------------------------------------
values_cat_list <- list(
  c(
    "Seen but not answered",
    "Normal - Not at all (no problems).",
    "Slight - My writing is slow, clumsy or uneven, but all words are clear.",
    "Mild - Some words are unclear and difficult to read.",
    "Moderate - Many words are unclear and difficult to read.",
    "Severe - Most or all words cannot be read.",
    NA
  ),
  
  c(
    "Seen but not answered",
    "Normal - Not at all (no problems).",
    "Slight - I am a bit slow but do these activities easily.",
    "Mild - I have some difficulty doing these activities.",
    "Moderate - I have major problems doing these activities, but still do most.",
    "Severe - I am unable to do most or all of these activities.",
    NA
  ),
  
  c(
    "Seen but not answered",
    "Normal - Not at all (no problems).",
    "Slight - I have a bit of trouble turning, but I do not need any help.",
    "Mild - I have a lot of trouble turning and need occasional help from someone else.",
    "Moderate - To turn over I often need help from someone else.",
    "Severe - I am unable to turn over without help from someone else.",
    NA
  ),
  
  c(
    "Seen but not answered",
    "Normal - Not at all. I have no shaking or tremor.",
    "Slight - Shaking or tremor occurs but does not cause problems with any activities.",
    "Mild - Shaking or tremor causes problems with only a few activities.",
    "Moderate - Shaking or tremor causes problems with many of my daily activities.",
    "Severe - Shaking or tremor causes problems with most or all activities.",
    NA
  ),
  
  c(
    "Not Left arm",
    "Left arm",
    NA
  ),
  
  c(
    "Not Right arm",
    "Right arm",
    NA
  ),
  
  c(
    "Not Left leg",
    "Left leg",
    NA
  ),
  
  c(
    "Not Right leg",
    "Right leg",
    NA
  ),
  
  c(
    "Not Whole body",
    "Whole body",
    NA
  ),
  
  c(
    "Seen but not answered",
    "Normal - Not at all (no problems).",
    "Slight - I am slow or awkward, but I usually can do it on my first try.",
    "Mild - I need more than one try to get up or need occasional help.",
    "Moderate - I sometimes need help to get up, but most times I can still do it on my own.",
    "Severe - I need help most or all of the time.",
    NA
  ),
  
  c(
    "Seen but not answered",
    "Normal - Not at all (no problems).",
    "Slight - I am slightly slow or may drag a leg. I never use a walking aid.",
    "Mild - I occasionally use a walking aid, but I do not need any help from another person.",
    "Moderate - I usually use a walking aid (cane, walker) to walk safely without falling. However, I do not usually need the support of another person.",
    "Severe - I usually use the support of another person to walk safely without falling.",
    NA
  ),
  
  c(
    "Seen but not answered",
    "Normal - Not at all (no problems).",
    "Slight - I briefly freeze but I can easily start to walk again. I do not need help from someone else or a walking aid (cane or walker) because of freezing.",
    "Mild - I freeze and have trouble starting to walk again, but I do not need someone's help or a walking aid (cane or walker) because of freezing.",
    "Moderate - When I freeze I have a lot of trouble starting to walk again and, because of freezing, I sometimes need to use a walking aid or need someone else's help.",
    "Severe - Because of freezing, most or all of the time, I need to use a walking aid or someone's help.",
    NA
  ),
  
  c(
    "Seen but not answered",
    "Normal - Not at all (no problems)",
    "Slight - My speech is soft, slurred or uneven, but it does not cause others to ask me to repeat myself.",
    "Mild - My speech causes people to ask me to occasionally repeat myself, but not everyday.",
    "Moderate - My speech is unclear enough that others ask me to repeat myself every day even though most of my speech is understood.",
    "Severe - Most or all of my speech cannot be understood.",
    NA
  ),
  
  c(
    "Seen but not answered",
    "Normal - Not at all (no problems).",
    "Slight - I have too much saliva, but do not drool.",
    "Mild - I have some drooling during sleep, but none when I am awake.",
    "Moderate - I have some drooling when I am awake, but I usually do not need tissues or a handkerchief.",
    "Severe - I have so much drooling that I regularly need to use tissues or a handkerchief to protect my clothes.",
    NA
  ),
  
  c(
    "Seen but not answered",
    "Normal - No problems.",
    "Slight - I am aware of slowness in my chewing or increased effort at swallowing, but I do not choke or need to have my food specially prepared.",
    "Mild - I need to have my pills cut or my food specially prepared because of chewing or swallowing problems, but I have not choked over the past week.",
    "Moderate - I choked at least once in the past week.",
    "Severe - Because of chewing and swallowing problems, I need a feeding tube.",
    NA
  ),
  
  c(
    "Seen but not answered",
    "Normal - Not at all (no problems).",
    "Slight - I am slow, but I do not need any help handling my food and have not had food spills while eating.",
    "Mild - I am slow with my eating and have occasional food spills. I may need help with a few tasks such as cutting meat.",
    "Moderate - I need help with many eating tasks but can manage some alone.",
    "Severe - I need help for most or all eating tasks.",
    NA
  ),
  
  c(
    "Seen but not answered",
    "Normal - Not at all (no problems).",
    "Slight - I am slow but I do not need help.",
    "Mild - I need someone else to help me with some dressing tasks.",
    "Moderate - I need help for many dressing tasks.",
    "Severe - I need help for most or all of my dressing tasks.",
    NA
  ),
  
  c(
    "Seen but not answered",
    "Normal - Not at all (no problems).",
    "Slight - I am slow but I do not need any help.",
    "Mild - I need someone else to help me with some hygiene tasks.",
    "Moderate - I need help for many hygiene tasks.",
    "Severe - I need help for most or all of my hygiene tasks.",
    NA
  ),
  
  c(
    "Don't know",
    "Seen but not answered",
    "No – I had this problem before I had COVID-19",
    "Yes – this problem started after I had COVID-19 or during my infection with it",
    NA
  ),
  c(
    "Don't know",
    "Seen but not answered",
    "No – I had this problem before I had COVID-19",
    "Yes – this problem started after I had COVID-19 or during my infection with it",
    NA
  ),
  
  c(
    "Don't know",
    "Seen but not answered",
    "No – I had this problem before I had COVID-19",
    "Yes – this problem started after I had COVID-19 or during my infection with it",
    NA
  ),
  c(
    "Don't know",
    "Seen but not answered",
    "No – I had this problem before I had COVID-19",
    "Yes – this problem started after I had COVID-19 or during my infection with it",
    NA
  ),
  
  c(
    "Don't know",
    "Seen but not answered",
    "No – I had this problem before I had COVID-19",
    "Yes – this problem started after I had COVID-19 or during my infection with it",
    NA
  ),
  c(
    "Don't know",
    "Seen but not answered",
    "No – I had this problem before I had COVID-19",
    "Yes – this problem started after I had COVID-19 or during my infection with it",
    NA
  ),
  
  c(
    "Don't know",
    "Seen but not answered",
    "No – I had this problem before I had COVID-19",
    "Yes – this problem started after I had COVID-19 or during my infection with it",
    NA
  ),
  c(
    "Don't know",
    "Seen but not answered",
    "No – I had this problem before I had COVID-19",
    "Yes – this problem started after I had COVID-19 or during my infection with it",
    NA
  ),
  
  c(
    "Don't know",
    "Seen but not answered",
    "No – I had this problem before I had COVID-19",
    "Yes – this problem started after I had COVID-19 or during my infection with it",
    NA
  ),
  c(
    "Don't know",
    "Seen but not answered",
    "No – I had this problem before I had COVID-19",
    "Yes – this problem started after I had COVID-19 or during my infection with it",
    NA
  ),
  
  c(
    "Don't know",
    "Seen but not answered",
    "No – I had this problem before I had COVID-19",
    "Yes – this problem started after I had COVID-19 or during my infection with it",
    NA
  ),
  c(
    "Don't know",
    "Seen but not answered",
    "No – I had this problem before I had COVID-19",
    "Yes – this problem started after I had COVID-19 or during my infection with it",
    NA
  ),
  
  c(
    "Don't know",
    "Seen but not answered",
    "No – I had this problem before I had COVID-19",
    "Yes – this problem started after I had COVID-19 or during my infection with it",
    NA
  )
)


## ----Set list names categorical variables-----------------------------------------------------------------------------
names(values_cat_list) <- c(
  "updrs.trouble_reading_handwriting_past",
  "updrs.hobbies_trouble_past_week",
  "updrs.trouble_turning_bed_past",
  "updrs.shaking_tremor_past_week",
  "updrs.left_arm",
  "updrs.right_arm",
  "updrs.left_leg",
  "updrs.right_leg",
  "updrs.whole_body",
  "updrs.deep_chair_car_seat",
  "updrs.balance_walking_past_week",
  "updrs.feet_floor_freeze_stuck",
  "updrs.speech_past_week_problems",
  "updrs.awake_saliva_sleep_past",
  "updrs.made_soft_chopped_avoid",
  "updrs.trouble_handling_finger_foods",
  "updrs.zippers_putting_problems_dressing",
  "updrs.personal_hygiene_hair_slow",
  "updrs.start_happening_required_question",
  "updrs.start_happening_required_question.1",
  "updrs.start_happening_required_question.2",
  "updrs.start_happening_required_question.3",
  "updrs.start_happening_required_question.4",
  "updrs.start_happening_required_question.5",
  "updrs.start_happening_required_question.6",
  "updrs.start_happening_required_question.7",
  "updrs.start_happening_required_question.8",
  "updrs.start_happening_required_question.9",
  "updrs.start_happening_required_question.10",
  "updrs.start_happening_required_question.11",
  "updrs.start_happening_required_question.12"
)


## ----Imp_check categorical variables----------------------------------------------------------------------------------
# Create empty list
imp_list_cat <- list()
# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(names(values_cat_list))) {
  imp_list_cat[i] <- imp_check_1(data = dat,
                                 variables = names(values_cat_list)[i],
                                 values = values_cat_list[[i]]) 
}
# Name list with var names to correspond to imp_messages
names(imp_list_cat) <- names(values_cat_list)
# View list of imp_messages with corresponding var names
print(imp_list_cat)


## ----Summary table categorical variables------------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(names(values_cat_list)),
    missing_text = "Missing")


## ----Vector numeric values--------------------------------------------------------------------------------------------
vals_num_1 <- c(
  0,
  1,
  2,
  3,
  4,
  -777,
  NA
)
vals_num_2 <- c(
  0,
  1,
  -777,
  NA
)
vals_num_3 <- c(
  0,
  1,
  -777,
  -888,
  NA
)


## ----List values vectors----------------------------------------------------------------------------------------------
values_num_list <- list(
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_2,
  vals_num_2,
  vals_num_2,
  vals_num_2,
  vals_num_2,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3,
  vals_num_3
)


## ----Remove values vectors--------------------------------------------------------------------------------------------
rm(
  vals_num_1,
  vals_num_2,
  vals_num_3
)


## ----Set list names numeric variables---------------------------------------------------------------------------------
names(values_num_list) <- c(
  "updrs.trouble_reading_handwriting_past_numeric",
  "updrs.hobbies_trouble_past_week_numeric",
  "updrs.trouble_turning_bed_past_numeric",
  "updrs.shaking_tremor_past_week_numeric",
  "updrs.left_arm_numeric",
  "updrs.right_arm_numeric",
  "updrs.left_leg_numeric",
  "updrs.right_leg_numeric",
  "updrs.whole_body_numeric",
  "updrs.deep_chair_car_seat_numeric",
  "updrs.balance_walking_past_week_numeric",
  "updrs.feet_floor_freeze_stuck_numeric",
  "updrs.speech_past_week_problems_numeric",
  "updrs.awake_saliva_sleep_past_numeric",
  "updrs.made_soft_chopped_avoid_numeric",
  "updrs.trouble_handling_finger_foods_numeric",
  "updrs.zippers_putting_problems_dressing_numeric",
  "updrs.personal_hygiene_hair_slow_numeric",
  "updrs.start_happening_required_question_numeric",
  "updrs.start_happening_required_question.1_numeric",
  "updrs.start_happening_required_question.2_numeric",
  "updrs.start_happening_required_question.3_numeric",
  "updrs.start_happening_required_question.4_numeric",
  "updrs.start_happening_required_question.5_numeric",
  "updrs.start_happening_required_question.6_numeric",
  "updrs.start_happening_required_question.7_numeric",
  "updrs.start_happening_required_question.8_numeric",
  "updrs.start_happening_required_question.9_numeric",
  "updrs.start_happening_required_question.10_numeric",
  "updrs.start_happening_required_question.11_numeric",
  "updrs.start_happening_required_question.12_numeric"
)


## ----Imp_check numeric variables--------------------------------------------------------------------------------------
# Create empty list
imp_list_num <- list()
# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(names(values_num_list))) {
  imp_list_num[i] <- imp_check_1(data = dat,
                                 variables = names(values_num_list)[i],
                                 values = values_num_list[[i]]) 
}
# Name list with var names to correspond to imp_messages
names(imp_list_num) <- names(values_num_list)
# View list of imp_messages with corresponding var names
print(imp_list_num)


## ----Summary table numeric variables----------------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(names(values_num_list)),
    missing_text = "Missing")


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
  "updrs.trouble_reading_handwriting_past_numeric",
  "updrs.hobbies_trouble_past_week_numeric",
  "updrs.trouble_turning_bed_past_numeric",
  "updrs.shaking_tremor_past_week_numeric",
  "updrs.left_arm_numeric",
  "updrs.right_arm_numeric",
  "updrs.left_leg_numeric",
  "updrs.right_leg_numeric",
  "updrs.whole_body_numeric",
  "updrs.deep_chair_car_seat_numeric",
  "updrs.balance_walking_past_week_numeric",
  "updrs.feet_floor_freeze_stuck_numeric",
  "updrs.speech_past_week_problems_numeric",
  "updrs.awake_saliva_sleep_past_numeric",
  "updrs.made_soft_chopped_avoid_numeric",
  "updrs.trouble_handling_finger_foods_numeric",
  "updrs.zippers_putting_problems_dressing_numeric",
  "updrs.personal_hygiene_hair_slow_numeric"
  )


## ----Generate sumscores-----------------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(
    updrs.sum_score = 
         sumscores(input = dat,
                   sum_vars = sum_vars,
                   coding_keys = keys,
                   na_allowed = 0,
                   min_item = 0,
                   max_item = 4,
                   min_score = 0,
                   max_score = 72
                   )[["scores"]]
         )


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/updrs_covidcns_clean.rds")
    )

