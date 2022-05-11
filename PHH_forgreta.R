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
source(file = "scripts/functions/add_numeric_1.R")
source(file = "scripts/functions/remove_duplicates.R")
source(file = "scripts/functions/sumscores.R")
source(file = "scripts/functions/package_check.R")
source(file = "scripts/functions/imp_check_1.R")
source(file = "scripts/functions/cont_clean.R")


## ----Install load dependencies----------------------------------------------------------------------------------------
packages <- c("summarytools", "sjlabelled", "Amelia", "gtsummary", "tidyverse")
package_check(packages)


## ----Read in file with path to ilovecovidcns channel on teams---------------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----COVIDCNS load data-----------------------------------------------------------------------------------------------
covidcns_dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/phh_covid_cns.rds")
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
  "endDate",
  "phh.nbsp",
  "phh.nbsp.1",
  "phh.",
  "phh..1",
  "phh..2",
  "phh.nbsp.2",
  "phh.nbsp.3",
  "phh..3",
  "phh..4",
  "phh..5",
  "phh.nbsp.4",
  "phh.nbsp.5",
  "phh..6",
  "phh..7",
  "phh..8",
  "phh.osteoarthritis.1",
  "phh.rheumatoid_arthritis.1",
  "phh.other_arthritistext.txt"
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
         phh.epilepsy_or_convulsions,
         phh.migraines,
         phh.multiple_sclerosis,
         phh.parkinsons_disease,
         phh.brain_tumour,
         phh.severe_memory_loss_,
         phh.cerebral_palsy,
         phh.dont_know,
         phh.prefer_not_to_answer,
         phh.none_of_the_above,
         phh.steroids,
         phh.immunosuppressants,
         phh.non_steroid_antiinflammatory_drugs,
         phh.thyroid_hormones,
         phh.bronchodilators,
         phh.non_steroid_antiinflammatory_drugs,
         phh.longterm_prescription_medication,
         phh.nbsp,
         phh.nbsp.1,
         phh.,
         phh..1,
         phh..2,
         phh.nbsp.2,
         phh.nbsp.3,
         phh..3,
         phh..4,
         phh..5,
         phh.nbsp.4,
         phh.nbsp.5,
         phh..6,
         phh..7,
         phh..8,
         phh.osteoarthritis,
         phh.rheumatoid_arthritis,
         phh.other_arthritis,
         phh.other_arthritistext.txt,
         phh.osteoarthritis.1,
         phh.rheumatoid_arthritis.1,
         phh.other_arthritis,
         phh.osteoarthritis.2,
         phh.rheumatoid_arthritis.2,
         phh.other_arthritis.2,
         phh.osteoarthritis.3, 
         phh.rheumatoid_arthritis.3,
         phh.other_arthritis.3 
         ) %>%
  add_numeric_1(exclude = exclude_cols_numeric)

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
vals_cat_1 <- c(
  "Not Epilepsy or convulsions",
  "Epilepsy or convulsions",
  NA
)

vals_cat_2 <- c(
  "Not Migraines",
  "Migraines",
  NA
)

vals_cat_3 <- c(
  "Not Multiple sclerosis",
  "Multiple sclerosis",
  NA
)

vals_cat_4 <- c(
  "Not Parkinson's disease",
  "Parkinson's disease",
  NA
)

vals_cat_5 <- c(
  "Not Brain tumour",
  "Brain tumour",
  NA
)

vals_cat_6 <- c(
  "Not Severe memory loss (like Alzheimer's)",
  "Severe memory loss (like Alzheimer's)",
  NA
)

vals_cat_7 <- c(
  "Not Cerebral palsy",
  "Cerebral palsy",
  NA
)

vals_cat_8 <- c(
  "Not Don't know",
  "Don't know",
  NA
)

vals_cat_9 <- c(
  "Not Prefer not to answer",
  "Prefer not to answer",
  NA
)

vals_cat_10 <- c(
  "Not None of the above",
  "None of the above",
  NA
)

vals_cat_yns <- c(
  "Seen but not answered",
  "No",
  "Yes",
  NA
)

vals_cat_ynsp <- c(
  "Prefer not to say",
  "Seen but not answered",
  "No",
  "Yes",
  NA
)

vals_cat_13 <- c(
  "Not Osteoarthritis",
  "Osteoarthritis",
  NA
)

vals_cat_14 <- c(
  "Not Rheumatoid arthritis",
  "Rheumatoid arthritis",
  NA
)

vals_cat_15 <- c(
  "Not Other arthritis:",
  "Other arthritis:",
  NA
)


## ----List categorical values vectors----------------------------------------------------------------------------------
values_cat_list <- list(
  vals_cat_1,
  vals_cat_2,
  vals_cat_3,
  vals_cat_4,
  vals_cat_5,
  vals_cat_6,
  vals_cat_7,
  vals_cat_8,
  vals_cat_9,
  vals_cat_10,
  
  vals_cat_yns,
  vals_cat_yns,
  vals_cat_yns,
  vals_cat_yns,
  vals_cat_yns,
  
  vals_cat_ynsp,
  
  vals_cat_13,
  vals_cat_14,
  vals_cat_15,

  vals_cat_yns,
  vals_cat_yns,
  vals_cat_yns,
  vals_cat_yns,
  vals_cat_yns,
  vals_cat_yns
)


## ----Vector categorical variables-------------------------------------------------------------------------------------
variables_cat <- c(
  "phh.epilepsy_or_convulsions",
  "phh.migraines",
  "phh.multiple_sclerosis",
  "phh.parkinsons_disease",
  "phh.brain_tumour",
  "phh.severe_memory_loss_",
  "phh.cerebral_palsy",
  "phh.dont_know",
  "phh.prefer_not_to_answer",
  "phh.none_of_the_above",
  
  #yns
  "phh.steroids",
  "phh.immunosuppressants",
  "phh.non_steroid_antiinflammatory_drugs",
  "phh.thyroid_hormones",
  "phh.bronchodilators",
  
  #ynsp
  "phh.longterm_prescription_medication",
  
  "phh.osteoarthritis",
  "phh.rheumatoid_arthritis",
  "phh.other_arthritis",
  
  #yns
  "phh.osteoarthritis.2",
  "phh.rheumatoid_arthritis.2",
  "phh.other_arthritis.2",
  "phh.osteoarthritis.3",
  "phh.rheumatoid_arthritis.3",
  "phh.other_arthritis.3"
)


## ----Set list names cat-----------------------------------------------------------------------------------------------
names(values_cat_list) <- variables_cat


## ----Imp_check categorical variables----------------------------------------------------------------------------------
# Create empty list
imp_list_cat <- list()

# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(variables_cat)) {
  imp_list_cat[i] <- imp_check_1(data = dat,
                                 variables = names(values_cat_list)[i],
                                 values = values_cat_list[[i]]) 

}

# Name list with var names to correspond to imp_messages
names(imp_list_cat) <- variables_cat

# View list of imp_messages with corresponding var names
print(imp_list_cat)


## ----Summary table categorical variables------------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(variables_cat),
    missing_text = "Missing")


## ----Vector numeric values--------------------------------------------------------------------------------------------
vals_num_1 <- c(
  0,
  1,
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
  -999,
  NA
)



## ----List numeric values vectors--------------------------------------------------------------------------------------
values_num_list <- list(
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_1,
  
  vals_num_2,
  vals_num_2,
  vals_num_2,
  vals_num_2,
  vals_num_2,
  
  vals_num_3,
  
  vals_num_1,
  vals_num_1,
  vals_num_1,
  
  vals_num_2,
  vals_num_2,
  vals_num_2,
  vals_num_2,
  vals_num_2,
  vals_num_2
)


## ----Vector numeric variables-----------------------------------------------------------------------------------------
variables_num <- c(
  "phh.epilepsy_or_convulsions_numeric",
  "phh.migraines_numeric",
  "phh.multiple_sclerosis_numeric",
  "phh.parkinsons_disease_numeric",
  "phh.brain_tumour_numeric",
  "phh.severe_memory_loss__numeric",
  "phh.cerebral_palsy_numeric",
  "phh.dont_know_numeric",
  "phh.prefer_not_to_answer_numeric",
  "phh.none_of_the_above_numeric",
  
  #yns
  "phh.steroids_numeric",
  "phh.immunosuppressants_numeric",
  "phh.non_steroid_antiinflammatory_drugs_numeric",
  "phh.thyroid_hormones_numeric",
  "phh.bronchodilators_numeric",
  
  #ynsp
  "phh.longterm_prescription_medication_numeric",
  
  "phh.osteoarthritis_numeric",
  "phh.rheumatoid_arthritis_numeric",
  "phh.other_arthritis_numeric",
  
  #yns
  "phh.osteoarthritis.2_numeric",
  "phh.rheumatoid_arthritis.2_numeric",
  "phh.other_arthritis.2_numeric",
  "phh.osteoarthritis.3_numeric",
  "phh.rheumatoid_arthritis.3_numeric",
  "phh.other_arthritis.3_numeric"
)


## ----Set list names---------------------------------------------------------------------------------------------------
names(values_num_list) <- variables_num


## ----Imp_check numeric variables--------------------------------------------------------------------------------------
# Create empty list
imp_list_num <- list()

# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(variables_num)) {
  imp_list_num[i] <- imp_check_1(data = dat,
                                 variables = names(values_num_list)[i],
                                 values = values_num_list[[i]]) 

}

# Name list with var names to correspond to imp_messages
names(imp_list_num) <- variables_num

# View list of imp_messages with corresponding var names
print(imp_list_num)


## ----Summary table numeric variables----------------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(variables_num),
    missing_text = "Missing")


## ----Create cont vars vector------------------------------------------------------------------------------------------
variables_cont <- c(
  "phh.osteoarthritis.1",
  "phh.rheumatoid_arthritis.1"
)


## ----Tnspect cont vars------------------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_cont)) %>%
  tbl_summary(
    missing_text = "Missing",
    statistic = list(all_continuous() ~ "{median} ({p0}, {p100})"))


## ----Cont vars to num-------------------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(
    all_of(variables_cont),
    ~as.numeric(.)
                )
         )


## ----Cont vars remove -77---------------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(
    all_of(variables_cont),
    ~na_if(., -77)
                )
         )


## ----Create matrix limits---------------------------------------------------------------------------------------------
limits_mat <- rbind(
  c(0, 117),
  c(0, 117)
  )


## ----Set lim_mat names------------------------------------------------------------------------------------------------
rownames(limits_mat) <- variables_cont
colnames(limits_mat) <- c("Lower", "Upper")


## ----Cont_clean cont vars---------------------------------------------------------------------------------------------
cont_list <- cont_clean(
  variables = variables_cont,
  limits_mat = limits_mat,
  dat = dat
)

cont_list


## ----Inspect after cleaning-------------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_cont)) %>%
  tbl_summary(
    missing_text = "Missing",
    statistic = list(all_continuous() ~ "{median} ({p0}, {p100})")
  )


## ----Inspect text vars------------------------------------------------------------------------------------------------
variables_text <- c(
  "phh.nbsp",
  "phh.nbsp.1",
  "phh.",
  "phh..1",
  "phh..2",
  "phh.nbsp.2",
  "phh.nbsp.3",
  "phh..3",
  "phh..4",
  "phh..5",
  "phh.nbsp.4",
  "phh.nbsp.5",
  "phh..6",
  "phh..7",
  "phh..8",
  "phh.other_arthritistext.txt"
)

theme_gtsummary_compact()

dat %>%
  select(all_of(variables_text)) %>%
  tbl_summary(
    missing_text = "Missing"
  )


## ----COVIDCNS recode medications 1 variable---------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(phh.nbsp,
                ~case_when(
                  str_detect(phh.nbsp, "Adeport")  ~ "Tacrolimus",
                  str_detect(phh.nbsp, "Adizem")  ~ "Diltiazem",
                  str_detect(phh.nbsp, "ALLOPUR")  ~ "Allopurinol",
                  str_detect(phh.nbsp, "mitr")  ~ "Amitriptyline",
                  str_detect(phh.nbsp, "Amilidopalin")  ~ "Amlodipine",
                  str_detect(phh.nbsp, "mlodip")  ~ "Amlodipine",
                  str_detect(phh.nbsp, "Amrizaprol")  ~ "Omeprazole",
                  str_detect(phh.nbsp, "Artorvostatin")  ~ "Atorvastatin",
                  str_detect(phh.nbsp, "spirin")  ~ "Aspirin",
                  str_detect(phh.nbsp, "Atanolel")  ~ "Atenolol",
                  str_detect(phh.nbsp, "torvastat")  ~ "Atorvastatin",
                  str_detect(phh.nbsp, "Cadesartan")  ~ "Candesartan",
                  str_detect(phh.nbsp, "Candesarten")  ~ "Candesartan",
                  str_detect(phh.nbsp, "Cerazette")  ~ "Desogestrel",
                  str_detect(phh.nbsp, "LOPIDOGR")  ~ "Clopidogrel",
                  str_detect(phh.nbsp, "cocodamol")  ~ "Co-codamol",
                  str_detect(phh.nbsp, "Doxacosin")  ~ "Doxazosin",
                  str_detect(phh.nbsp, "doxaban")  ~ "Edoxaban",
                  str_detect(phh.nbsp, "flecanide")  ~ "Flecainide",
                  str_detect(phh.nbsp, "ostair")  ~ "Beclometasone/Formoterol",
                  str_detect(phh.nbsp, "abape")  ~ "Gabapentin",
                  str_detect(phh.nbsp, "indapamide")  ~ "Indapamide",
                  str_detect(phh.nbsp, "EVETIRC")  ~ "Levetiracetam",
                  str_detect(phh.nbsp, "evothyro")  ~ "Levothyroxine",
                  str_detect(phh.nbsp, "Lipitor")  ~ "Atorvastatin",
                  str_detect(phh.nbsp, "loratadine")  ~ "Loratadine",
                  str_detect(phh.nbsp, "lozartan")  ~ "Lozartan",
                  str_detect(phh.nbsp, "Madopar")  ~ "Benserazide",
                  str_detect(phh.nbsp, "etformi")  ~ "Metformin",
                  str_detect(phh.nbsp, "irena")  ~ "Levonorgestrel",
                  str_detect(phh.nbsp, "nortryptiline")  ~ "Nortriptyline",
                  str_detect(phh.nbsp, "Olanzipine")  ~ "Olanzapine",
                  str_detect(phh.nbsp, "pantoprasole")  ~ "Pantoprazole",
                  str_detect(phh.nbsp, "Pentasa")  ~ "Mesalamine",
                  str_detect(phh.nbsp, "rednis")  ~ "Prednisolone",
                  str_detect(phh.nbsp, "regab")  ~ "Pregabalin",
                  str_detect(phh.nbsp, "Propraolol")  ~ "Propranolol",
                  str_detect(phh.nbsp, "ramapril")  ~ "Ramipril",
                  str_detect(phh.nbsp, "amipr")  ~ "Ramipril",
                  str_detect(phh.nbsp, "albutam")  ~ "Salbutamol",
                  str_detect(phh.nbsp, "teroid")  ~ "Steroids",
                  str_detect(phh.nbsp, "Sukkarto")  ~ "Metformin",
                  str_detect(phh.nbsp, "ymbicor")  ~ "Budesonide/Formoterol",
                  str_detect(phh.nbsp, "amsulos")  ~ "Tamsulosin",
                  str_detect(phh.nbsp, "TEGRETOL")  ~ "Carbamazepine",
                  str_detect(phh.nbsp, "Trelegy")  ~ "Fluticasone/Umeclinidium/Vilanterol",
                  str_detect(phh.nbsp, "nkno")  ~ "-77",
                  str_detect(phh.nbsp, "enlafaxin")  ~ "Venlafaxine",
                  str_detect(phh.nbsp, "Xaretto")  ~ "Rivaroxaban",
                  str_detect(phh.nbsp, "ivarox")  ~ "Rivaroxaban",
                  str_detect(phh.nbsp, "zinc")  ~ "Zinc",
                  str_detect(phh.nbsp, "On medica")  ~ "-77",
                  TRUE ~ .)))


## ----COVIDCNS recheck coding 1----------------------------------------------------------------------------------------
dat %>%
  select(phh.nbsp) %>%
  tbl_summary(missing_text = "Missing")


## ----COVIDCNS recode medications 2 variable---------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(phh.nbsp.1,
                ~case_when(
                  str_detect(phh.nbsp.1, "Benzofibrate")  ~ "Bezafibrate",
                  str_detect(phh.nbsp.1, "Isporol")  ~ "Bisoprolol",
                  str_detect(phh.nbsp.1, "Questran")  ~ "Cholestyramine",
                  str_detect(phh.nbsp.1, "salbutomal")  ~ "Salbutamol",
                  str_detect(phh.nbsp.1, "imvast")  ~ "Simvastatin",
                  str_detect(phh.nbsp.1, "iotrop")  ~ "Tiotropium",
                  str_detect(phh.nbsp.1, "Yacella")  ~ "Ethinylestradiol/Drospirenone",
                  str_detect(phh.nbsp.1, "Statin")  ~ "Statins",
                  str_detect(phh.nbsp.1, "EDOXOBAN")  ~ "Edoxaban",
                  str_detect(phh.nbsp.1, "ESCITALOPRAM")  ~ "Escitalopram",
                  str_detect(phh.nbsp.1, "Flutiform")  ~ "Fluticasone/Formoterol",
                  str_detect(phh.nbsp.1, "Keppra")  ~ "Levetiracetam",
                  str_detect(phh.nbsp.1, "Leveciraterem")  ~ "Levetiracetam",
                  str_detect(phh.nbsp.1, "Levetiractam")  ~ "Levetiracetam",
                  str_detect(phh.nbsp.1, "evothryoxine")  ~ "Levothyroxine",
                  str_detect(phh.nbsp.1, "irtaz")  ~ "Mirtazapine",
                  str_detect(phh.nbsp.1, "ontelu")  ~ "Montelukast",
                  str_detect(phh.nbsp.1, "ycophen")  ~ "Mycophenolate",
                  str_detect(phh.nbsp.1, "mepr")  ~ "Omeprazole",
                  str_detect(phh.nbsp.1, "Bipisporol")  ~ "Bisoprolol",
                  str_detect(phh.nbsp.1, "isop")  ~ "Bisoprolol",
                  str_detect(phh.nbsp.1, "Adizem")  ~ "Diltiazem",
                  str_detect(phh.nbsp.1, "ALLOPUR")  ~ "Allopurinol",
                  str_detect(phh.nbsp.1, "mitr")  ~ "Amitriptyline",
                  str_detect(phh.nbsp.1, "Amilidopalin")  ~ "Amlodipine",
                  str_detect(phh.nbsp.1, "mlodip")  ~ "Amlodipine",
                  str_detect(phh.nbsp.1, "Amrizaprol")  ~ "Omeprazole",
                  str_detect(phh.nbsp.1, "Artorvostatin")  ~ "Atorvastatin",
                  str_detect(phh.nbsp.1, "atorvostatin")  ~ "Atorvastatin",
                  str_detect(phh.nbsp.1, "Atrovarstatin")  ~ "Atorvastatin",
                  str_detect(phh.nbsp.1, "ATORVASTATIN")  ~ "Atorvastatin",
                  str_detect(phh.nbsp.1, "spirin")  ~ "Aspirin",
                  str_detect(phh.nbsp.1, "Atanolel")  ~ "Atenolol",
                  str_detect(phh.nbsp.1, "torvastat")  ~ "Atorvastatin",
                  str_detect(phh.nbsp.1, "Cadesartan")  ~ "Candesartan",
                  str_detect(phh.nbsp.1, "Candersartan")  ~ "Candesartan",
                  str_detect(phh.nbsp.1, "Candesarten")  ~ "Candesartan",
                  str_detect(phh.nbsp.1, "Colafac")  ~ "Mebeverine",
                  str_detect(phh.nbsp.1, "Dueloxitine")  ~ "Duloxetine",
                  str_detect(phh.nbsp.1, "Ditropan")  ~ "Oxybutynin",
                  str_detect(phh.nbsp.1, "erazette")  ~ "Desogestrel",
                  str_detect(phh.nbsp.1, "LOPIDOGR")  ~ "Clopidogrel",
                  str_detect(phh.nbsp.1, "cocodamol")  ~ "Co-codamol",
                  str_detect(phh.nbsp.1, "Doxacosin")  ~ "Doxazosin",
                  str_detect(phh.nbsp.1, "doxaban")  ~ "Edoxaban",
                  str_detect(phh.nbsp.1, "flecanide")  ~ "Flecainide",
                  str_detect(phh.nbsp.1, "ostair")  ~ "Beclometasone/Formoterol",
                  str_detect(phh.nbsp.1, "abape")  ~ "Gabapentin",
                  str_detect(phh.nbsp.1, "indapamide")  ~ "Indapamide",
                  str_detect(phh.nbsp.1, "EVETIRC")  ~ "Levetiracetam",
                  str_detect(phh.nbsp.1, "evothyro")  ~ "Levothyroxine",
                  str_detect(phh.nbsp.1, "Lipitor")  ~ "Atorvastatin",
                  str_detect(phh.nbsp.1, "loratadine")  ~ "Loratadine",
                  str_detect(phh.nbsp.1, "lozartan")  ~ "Lozartan",
                  str_detect(phh.nbsp.1, "Madopar")  ~ "Benserazide",
                  str_detect(phh.nbsp.1, "etformi")  ~ "Metformin",
                  str_detect(phh.nbsp.1, "irena")  ~ "Levonorgestrel",
                  str_detect(phh.nbsp.1, "nortryptiline")  ~ "Nortriptyline",
                  str_detect(phh.nbsp.1, "Olanzipine")  ~ "Olanzapine",
                  str_detect(phh.nbsp.1, "pantoprasole")  ~ "Pantoprazole",
                  str_detect(phh.nbsp.1, "Pentasa")  ~ "Mesalamine",
                  str_detect(phh.nbsp.1, "rednis")  ~ "Prednisolone",
                  str_detect(phh.nbsp.1, "regab")  ~ "Pregabalin",
                  str_detect(phh.nbsp.1, "Propraolol")  ~ "Propranolol",
                  str_detect(phh.nbsp.1, "ramapril")  ~ "Ramipril",
                  str_detect(phh.nbsp.1, "amipr")  ~ "Ramipril",
                  str_detect(phh.nbsp.1, "albutam")  ~ "Salbutamol",
                  str_detect(phh.nbsp.1, "teroid")  ~ "Steroids",
                  str_detect(phh.nbsp.1, "Sukkarto")  ~ "Metformin",
                  str_detect(phh.nbsp.1, "ymbicor")  ~ "Budesonide/Formoterol",
                  str_detect(phh.nbsp.1, "amsulos")  ~ "Tamsulosin",
                  str_detect(phh.nbsp.1, "TEGRETOL")  ~ "Carbamazepine",
                  str_detect(phh.nbsp.1, "Trelegy")  ~ "Fluticasone/Umeclinidium/Vilanterol",
                  str_detect(phh.nbsp.1, "nkno")  ~ "-77",
                  str_detect(phh.nbsp.1, "enlafaxin")  ~ "Venlafaxine",
                  str_detect(phh.nbsp.1, "Xaretto")  ~ "Rivaroxaban",
                  str_detect(phh.nbsp.1, "ivarox")  ~ "Rivaroxaban",
                  str_detect(phh.nbsp.1, "zinc")  ~ "Zinc",
                  str_detect(phh.nbsp.1, "On medica")  ~ "-77",
                  TRUE ~ .)))


## ----COVIDCNS recheck coding 2----------------------------------------------------------------------------------------
dat %>%
  select(phh.nbsp.1) %>%
  tbl_summary(missing_text = "Missing")


## ----COVIDCNS recode medications 3 variable---------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(phh.,
                ~case_when(
                  str_detect(phh., "Accrete")  ~ "Colecalciferol",
                  str_detect(phh., "Amatryptyline")  ~ "Amitriptyline",
                  str_detect(phh., "torvast")  ~ "Atorvastatin",
                  str_detect(phh., "endroflu")  ~ "Bendroflumethiazide",
                  str_detect(phh., "etahis")  ~ "Betahistine",
                  str_detect(phh., "BISOPROLOL")  ~ "Bisoprolol",
                  str_detect(phh., "lopido")  ~ "Clopidogrel",
                  str_detect(phh., "mespro")  ~ "Esomeprazole",
                  str_detect(phh., "zetim")  ~ "Ezetimibe",
                  str_detect(phh., "exof")  ~ "Fexofenadine",
                  str_detect(phh., "Fluox")  ~ "Fluoxetine",
                  str_detect(phh., "Flutiform")  ~ "Fluticasone/Formoterol",
                  str_detect(phh., "ostair")  ~ "Beclometasone/Formoterol",
                  str_detect(phh., "Fultium")  ~ "Colecalciferol",
                  str_detect(phh., "glickasade")  ~ "Gliclazide",
                  str_detect(phh., "glipizide")  ~ "Gliclazide",
                  str_detect(phh., "anzo")  ~ "Lansoprazole",
                  str_detect(phh., "anso")  ~ "Lansoprazole",
                  str_detect(phh., "ANSO")  ~ "Lansoprazole",
                  str_detect(phh., "Lipitor")  ~ "Atorvastatin",
                  str_detect(phh., "formin")  ~ "Metformin",
                  str_detect(phh., "mperazo")  ~ "Omeprazole",
                  str_detect(phh., "amip")  ~ "Ramipril",
                  str_detect(phh., "Rebar")  ~ "Fluticasone/Vilanterol",
                  str_detect(phh., "ymbicor")  ~ "Budesonide/Formoterol",
                  str_detect(phh., "Tamsolosin")  ~ "Tamsulosin",
                  str_detect(phh., "agife")  ~ "Estradiol",
                  TRUE ~ .)))


## ----COVIDCNS recheck coding 3----------------------------------------------------------------------------------------
dat %>%
  select(phh.) %>%
  tbl_summary(missing_text = "Missing")


## ----COVIDCNS recode medications 4 variable---------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(phh..1,
                ~case_when(
                  str_detect(phh..1, "Ad-")  ~ "Colecalciferol",
                  str_detect(phh..1, "Adcal")  ~ "Colecalciferol",
                  str_detect(phh..1, "llopur")  ~ "Allopurinol",
                  str_detect(phh..1, "pixab")  ~ "Apixaban",
                  str_detect(phh..1, "spirin")  ~ "Aspirin",
                  str_detect(phh..1, "torv")  ~ "Atorvastatin",
                  str_detect(phh..1, "TORV")  ~ "Atorvastatin",
                  str_detect(phh..1, "torav")  ~ "Atorvastatin",
                  str_detect(phh..1, "tropin")  ~ "Atropine",
                  str_detect(phh..1, "ispro")  ~ "Bisoprolol",
                  str_detect(phh..1, "buprenorphine")  ~ "Buprenorphine",
                  str_detect(phh..1, "dorzolamide")  ~ "Dorzolamide",
                  str_detect(phh..1, "Dutesterdide")  ~ "Dutasteride",
                  str_detect(phh..1, "Furosmide")  ~ "Furosemide",
                  str_detect(phh..1, "insulin")  ~ "Insulin",
                  str_detect(phh..1, "ansopr")  ~ "Lansoprazole",
                  str_detect(phh..1, "Levetractam")  ~ "Levetiracetam",
                  str_detect(phh..1, "ormin")  ~ "Metformin",
                  str_detect(phh..1, "icrogyn")  ~ "Ethinylestradiol/Levonorgestrel",
                  str_detect(phh..1, "Mirtazipine")  ~ "Mirtazapine",
                  str_detect(phh..1, "fenolate")  ~ "Mycophenolate",
                  str_detect(phh..1, "Oxybutinin")  ~ "Oxybutynin",
                  str_detect(phh..1, "aracetamol")  ~ "Paracetamol",
                  str_detect(phh..1, "uinin")  ~ "Quinine",
                  str_detect(phh..1, "entoli")  ~ "Salbutamol",
                  str_detect(phh..1, "enna")  ~ "Senna",
                  str_detect(phh..1, "ertral")  ~ "Sertraline",
                  str_detect(phh..1, "it D")  ~ "Colecalciferol",
                  str_detect(phh..1, "eletran")  ~ "Buprenorphine",
                  TRUE ~ .)))


## ----COVIDCNS recheck coding 4----------------------------------------------------------------------------------------
dat %>%
  select(phh..1) %>%
  tbl_summary(missing_text = "Missing")


## ----COVIDCNS recode medications 5 variable---------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(phh..2,
                ~case_when(
                  str_detect(phh..2, "Advograf")  ~ "Tacrolimus",
                  str_detect(phh..2, "statin")  ~ "Atorvastatin",
                  str_detect(phh..2, "pril")  ~ "Ramipril",
                  str_detect(phh..2, "PRIL")  ~ "Ramipril",
                  str_detect(phh..2, "lipti")  ~ "Sitagliptin",
                  str_detect(phh..2, "B12")  ~ "Cobalamin",
                  str_detect(phh.nbsp.1, "ostair")  ~ "Beclometasone/Formoterol",
                  str_detect(phh..2, "obala")  ~ "Cobalamin",
                  str_detect(phh..2, "odein")  ~ "Codeine",
                  str_detect(phh..2, "Famitidine")  ~ "Famotidine",
                  str_detect(phh..2, "Gaba")  ~ "Gabapentin",
                  str_detect(phh..2, "nsuli")  ~ "Insulin",
                  str_detect(phh..2, "Jardiance")  ~ "Empagliflozin",
                  str_detect(phh..2, "antan")  ~ "Latanoprost",
                  str_detect(phh..2, "Letrazole")  ~ "Letrozole",
                  str_detect(phh..2, "Ocrevus")  ~ "Ocrelizumab",
                  str_detect(phh..2, "stexerol")  ~ "Colecalciferol",
                  str_detect(phh..2, "sumatriptan")  ~ "Sumatriptan",
                  str_detect(phh..2, "Waferin")  ~ "Warfarin",
                  str_detect(phh..2, "Zapain")  ~ "Co-codamol",
                  TRUE ~ .)))


## ----COVIDCNS recheck coding 5----------------------------------------------------------------------------------------
dat %>%
  select(phh..2) %>%
  tbl_summary(missing_text = "Missing")


## ----COVIDCNS recode medications 6 variable---------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(phh.nbsp.2,
                ~case_when(
                  str_detect(phh.nbsp.2, "abl")  ~ "Tablets",
                  str_detect(phh.nbsp.2, "aps")  ~ "Capsules",
                  str_detect(phh.nbsp.2, "ABL")  ~ "Tablets",
                  str_detect(phh.nbsp.2, "nha")  ~ "Inhaler",
                  
                  TRUE ~ .)))


## ----COVIDCNS recheck coding 6----------------------------------------------------------------------------------------
dat %>%
  select(phh.nbsp.2) %>%
  tbl_summary(missing_text = "Missing")


## ----COVIDCNS recode medications 7 variable---------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(phh.nbsp.3,
                ~case_when(
                  str_detect(phh.nbsp.3, "abl")  ~ "Tablets",
                  str_detect(phh.nbsp.3, "ABL")  ~ "Tablets",
                  str_detect(phh.nbsp.3, "nha")  ~ "Inhaler",
                  
                  TRUE ~ .)))


## ----COVIDCNS recheck coding 7----------------------------------------------------------------------------------------
dat %>%
  select(phh.nbsp.3) %>%
  tbl_summary(missing_text = "Missing")


## ----COVIDCNS recode medications 8 variable---------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(phh..3,
                ~case_when(
                  str_detect(phh..3, "ab")  ~ "Tablets",
                  str_detect(phh..3, "ABL")  ~ "Tablets",
                  str_detect(phh..3, "nha")  ~ "Inhaler",
                  
                  TRUE ~ .)))


## ----COVIDCNS recheck coding 8----------------------------------------------------------------------------------------
dat %>%
  select(phh..3) %>%
  tbl_summary(missing_text = "Missing")


## ----COVIDCNS recode medications 9 variable---------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(phh..4,
                ~case_when(
                  str_detect(phh..4, "ab")  ~ "Tablets",
                  str_detect(phh..4, "ABL")  ~ "Tablets",
                  str_detect(phh..4, "nha")  ~ "Inhaler",
                  str_detect(phh..4, "atc")  ~ "Patch",
                  TRUE ~ .)))


## ----COVIDCNS recheck coding 9----------------------------------------------------------------------------------------
dat %>%
  select(phh..4) %>%
  tbl_summary(missing_text = "Missing")


## ----COVIDCNS recode medications 10 variable--------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(phh..5,
                ~case_when(
                  str_detect(phh..5, "abl")  ~ "Tablets",
                  str_detect(phh..5, "ABL")  ~ "Tablets",
                  str_detect(phh..5, "nha")  ~ "Inhaler",
                  
                  TRUE ~ .)))


## ----COVIDCNS recheck coding 10---------------------------------------------------------------------------------------
dat %>%
  select(phh..5) %>%
  tbl_summary(missing_text = "Missing")


## ----COVIDCNS recheck coding final------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_text)) %>%
  tbl_summary(missing_text = "Missing")


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/PHH_forgreta_covidcns_clean.rds")
    )

