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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/core_neuro/nis_outp_covid_cns.rds")
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
  "nis_outp.other_observations.txt"
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
         nis_outp.left_upper_limb,
         nis_outp.right_upper_limb,
         nis_outp.left_lower_limb,
         nis_outp.right_lower_limb,
         nis_outp.trunk,
         nis_outp.motor__impairment_type,
         nis_outp.tonejoint_range,
         nis_outp.spasticity_abnormal_muscle_tightness,
         nis_outp.tendons_leading_deformity_hardening,
         nis_outp.sensation,
         nis_outp.somatic___bodily_sensation_of_touch,
         nis_outp.body_parts_awareness_position,
         nis_outp.dysesthesia__abnormal_unpleasant_sensation_felt_when_touched,
         nis_outp.perceptual_function,
         nis_outp.body_inability_body_process,
         nis_outp.process_side_external_space,
         nis_outp.speech_and_language,
         nis_outp.partial_loss_ability_expressive,
         nis_outp.comprehend_language_intact_production,
         nis_outp.unclear_articulation_speech_dysarthria,
         nis_outp.cognitive_impairment_aphasia_cognitive,
         nis_outp.cognitive_function,
         nis_outp.consciousness_cognitive_problems_related,
         nis_outp.time_place_orientation_person,
         nis_outp.memory_problems_recalling_past,
         nis_outp.attention_choose_concentrate_ability,
         nis_outp.initiate_start_initiation_action,
         nis_outp.executive_function,
         nis_outp.behaviour,
         nis_outp.threaten_offensive_language_meant,
         nis_outp.physical_aggression_behaviour_causing,
         nis_outp.social_convention_impulsivity_restraint,
         nis_outp.mood,
         nis_outp.dejection_depressionlow_mood_feelings,
         nis_outp.tension_feelings_anxiety_emotion,
         nis_outp.strong_emotions_feelings_occur,
         nis_outp.seeing_and_vision,
         nis_outp.visual_field_area_lost,
         nis_outp.corrected_lost_vision_spectacles,
         nis_outp.images_overlapping_double_vision,
         nis_outp.not_applicable,
         nis_outp.hearing,
         nis_outp.damage_ear_auditory_nerve,
         nis_outp.outer_sounds_conductive_hearing,
         nis_outp.not_applicable.1,
         nis_outp.pain,
         nis_outp.burning_electrical_disease_lesion,
         nis_outp.muscles_ligaments_tendons_constant,
         nis_outp.spasticity_pain_occurring_spasticity,
         nis_outp.fatigue,
         nis_outp.perform_physical_exercise_physical,
         nis_outp.muscle_fatigability_decline_muscles,
         nis_outp.cognitive_fatigue_extreme_capable,
         nis_outp.other,
         nis_outp.loss_consciousness_andor_rhythmic,
         nis_outp.skin_pressure_sores_injuries,
         nis_outp.other_observations.txt
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


## ----Vectors categorical domains values-------------------------------------------------------------------------------
values_cat_domains_1 <- c( # same for first 5 variables
  "0 (None - No weakness/loss of motor control/coordination)",
  "1 (Mild - Affecting higher level motor control only)",
  "2 (Moderate - Significant impact on function but some useful active movement)",
  "3 (Severe - Severe loss of motor control, with little or no active movement)",
  "Seen but not answered",
  NA
)

values_cat_domains_2 <- c( # second 'mild' is not capitalised in level '1'
  "0 (None - No spasticity/contractures)",
  "1 (Mild - Mild tone problems - can achieve full range of movement)",
  "2 (Moderate - Significant spasticity with mild restriction of some joint range: 1-2 joints only)",
  "3 (Severe - Severe spasticity/contracture with marked restriction of joint range - limiting function)",
  "Seen but not answered",
  NA
)

values_cat_domains_3 <- c(
  "0 (None - No dysaethesia, loss of sensation or joint position sense)",
  "1 (Mild - Mild or patchy loss - minimal interference with sensory function)",
  "2 (Moderate - Partial sensory loss with significant impact on ability to feel the limb and where it is)",
  "3 (Severe - Complete/near-complete loss of sensation - all modalities - in one or more limbs)",
  "Seen but not answered",
  NA
)

values_cat_domains_4 <- c(
  "0 (None - No neglect of body or external space: i.e. does not ignore body parts or bump into things)",
  "1 (Mild - Mild neglect, but compensates effectively - minimal interference with function)",
  "2 (Moderate - Bumps into things or ignores body parts some of the time with significant impact on function)",
  "3 (Severe - Total neglect of body part or field effectively limiting independence/rehabilitation)",
  "Seen but not answered",
  NA
)

values_cat_domains_5 <- c(
  "0 (None - No deficit in expression / comprehension or articulation of language)",
  "1 (Mild - Mild deficit affecting high level communication only)",
  "2 (Moderate - Moderate deficit with significant impact on functional communication/listener burden)",
  "3 (Severe - Severe deficit - communication through language effectively not possible)",
  "Seen but not answered",
  NA
)

values_cat_domains_6 <- c(
  "0 (None - No cognitive deficit)",
  "1 (Mild - Mild deficit affecting higher level cognitive function only)",
  "2 (Moderate - Moderate deficit with significant impact on carryover and engagement in rehabilitation)",
  "3 (Severe - Severe cognitive effectively limiting carryover and engagement in rehabilitation)",
  "Seen but not answered",
  NA
)

values_cat_domains_7 <- c(
  "0 (None - No problems with aggression or disinhibtion)",
  "1 (Mild - Occasional mild outbursts/disinhibition, with minimal impact on function / rehabilitation)",
  "2 (Moderate - Moderate behavioural problems with significant impact / interference with rehab)",
  "3 (Severe - Severe behaviours / hitting / biting / scratching / etc. which effectively limit rehab)",
  "Seen but not answered",
  NA
)

values_cat_domains_8 <- c(
  "0 (None - No mood disturbance: depressive or anxiety)",
  "1 (Mild - Mild mood problems not interfering with daily function)",
  "2 (Moderate - Significant mood disturbance with some impact on function / engagement in rehab)",
  "3 (Severe - Severe mood disorder effectively limiting engagement in rehab)",
  "Seen but not answered",
  NA
)

values_cat_domains_9 <- c(
  "0 (None - No visual deficit)",
  "1 (Mild - Mild visual deficit correctable with compensatory techniques)",
  "2 (Moderate - Moderate visual disturbance with significant limitation on function: partially sighted)",
  "3 (Severe - Effectively blind: little or no useful vision)",
  "Seen but not answered",
  NA
)

values_cat_domains_10 <- c(
  "0 (None - No hearing deficit)",
  "1 (Mild - Mild hearing deficit correctable with hearing aid)",
  "2 (Moderate - Moderate hearing disturbance with significant limitation on function: partially deaf)",
  "3 (Severe - Effectively deaf: little or no useful hearing)",
  "Seen but not answered",
  NA
)

values_cat_domains_11 <- c(
  "0 (None - No pain)",
  "1 (Mild - Mild pain, easily controlled with little impact on function / rehab)",
  "2 (Moderate - Moderate pain, incompletely controlled with significant impact on function / rehab)",
  "3 (Severe - Severe pain: effectively limits function / engagement in rehabilitation)",
  "Seen but not answered",
  NA
)

values_cat_domains_12 <- c(
  "0 (None - No fatigue)",
  "1 (Mild - Mild fatigue, easily controlled through pacing with little impact on function / rehab)",
  "2 (Moderate - Moderate fatigue, incompletely controlled - significant interference with function / rehab)",
  "3 (Severe - Effectively limits function / engagement in rehabilitation)",
  "Seen but not answered",
  NA
)

values_cat_domains_13 <- c(
  "0 (None - No seizures / sores)",
  "1 (Minimal - Minimal / occasional: little interference with function / rehab)",
  "2 (Marked - Interference with function / rehabilitation)",
  "Unknown",
  "Seen but not answered",
  NA
)


## ----List categorical domains values vectors--------------------------------------------------------------------------
values_cat_domains_list <- list(
  values_cat_domains_1,
  values_cat_domains_1,
  values_cat_domains_1,
  values_cat_domains_1,
  values_cat_domains_1,
  values_cat_domains_2,
  values_cat_domains_3,
  values_cat_domains_4,
  values_cat_domains_5,
  values_cat_domains_6,
  values_cat_domains_7,
  values_cat_domains_8,
  values_cat_domains_9,
  values_cat_domains_10,
  values_cat_domains_11,
  values_cat_domains_12,
  values_cat_domains_13
)


## ----Vector categorical domains variables-----------------------------------------------------------------------------
variables_cat_domains <- c(
  "nis_outp.left_upper_limb", #1
  "nis_outp.right_upper_limb", #1
  "nis_outp.left_lower_limb", #1
  "nis_outp.right_lower_limb", #1
  "nis_outp.trunk", #1
  "nis_outp.tonejoint_range", #2
  "nis_outp.sensation", #3
  "nis_outp.perceptual_function",#4
  "nis_outp.speech_and_language", #5
  "nis_outp.cognitive_function", #6
  "nis_outp.behaviour", #7
  "nis_outp.mood", #8
  "nis_outp.seeing_and_vision", #9
  "nis_outp.hearing", #10
  "nis_outp.pain", #11
  "nis_outp.fatigue", #12
  "nis_outp.other" #13
)


## ----Set list names cat domains---------------------------------------------------------------------------------------
names(values_cat_domains_list) <- variables_cat_domains


## ----Imp_check categorical domains variables--------------------------------------------------------------------------
# Create empty list
imp_list_cat_domains <- list()

# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(variables_cat_domains)) {
  imp_list_cat_domains[i] <- imp_check_1(data = dat,
                                     variables = names(values_cat_domains_list)[i],
                                     values = values_cat_domains_list[[i]]) 

}

# Name list with var names to correspond to imp_messages
names(imp_list_cat_domains) <- variables_cat_domains

# View list of imp_messages with corresponding var names
print(imp_list_cat_domains)


## ----Summary table categorical domains variables----------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(variables_cat_domains),
    missing_text = "Missing")


## ----Vectors categorical impairments values---------------------------------------------------------------------------
values_cat_impairms_1 <- c(
  "R Hemiparesis - weakness to the right side of the body (arm and leg)",
  "L Hemiparesis - weakness to the left side of the body (arm and leg)",
  "Tetraparesis - weakness to all limbs (both arms and both legs)",
  "Paraparesis - weakness to either both arms or both legs",
  "Monoparesis - weakness of one limb (one arm or leg)",
  "Ataxia - loss of control of movement / uncoordinated movements",
  "Seen but not answered",
  NA
)

values_cat_impairms_2 <- c(
  "Not Spasticity - abnormal muscle tightness and weakness (but it is still possible to move the muscle passively",
  "Spasticity - abnormal muscle tightness and weakness (but it is still possible to move the muscle passively",
  NA
)

values_cat_impairms_3 <- c(
  "Not Contractures - shortening and hardening of the muscle and tendons leading to a deformity and reduced range of movements (the muscle is fixed in place)",
  "Contractures - shortening and hardening of the muscle and tendons leading to a deformity and reduced range of movements (the muscle is fixed in place)",
  NA
)

values_cat_impairms_4 <- c(
  "Not Somatic (e.g. touch) - bodily sensation of touch",
  "Somatic (e.g. touch) - bodily sensation of touch",
  NA
)

values_cat_impairms_5 <- c(
  "Not Proprioception - perception or awareness of the position and movement of body parts",
  "Proprioception - perception or awareness of the position and movement of body parts",
  NA
)

values_cat_impairms_6 <- c(
  "Not Dysesthesia - abnormal unpleasant sensation felt when touched",
  "Dysesthesia - abnormal unpleasant sensation felt when touched",
  NA
)

values_cat_impairms_7 <- c(
  "Not Neglect of body - inability to process and perceive stimuli on one side of the body",
  "Neglect of body - inability to process and perceive stimuli on one side of the body",
  NA
)

values_cat_impairms_8 <- c(
  "Not Neglect of external space - inability to process and perceive stimuli on one side of the room / space",
  "Neglect of external space - inability to process and perceive stimuli on one side of the room / space",
  NA
)

values_cat_impairms_9 <- c(
  "Not Expressive - complete or partial loss of the ability to produce language (spoken, manual, or written) with intact comprehension",
  "Expressive - complete or partial loss of the ability to produce language (spoken, manual, or written) with intact comprehension",
  NA
)

values_cat_impairms_10 <- c(
  "Not Receptive - complete or partial loss of the ability to comprehend language (spoken, manual, or written) with intact production of language",
  "Receptive - complete or partial loss of the ability to comprehend language (spoken, manual, or written) with intact production of language",
  NA
)

values_cat_impairms_11 <- c(
  "Not Dysarthria - difficult or unclear articulation of speech that is otherwise linguistically normal",
  "Dysarthria - difficult or unclear articulation of speech that is otherwise linguistically normal",
  NA
)

values_cat_impairms_12 <- c(
  "Not Cognitive speech - speech problems related to cognitive impairment more than to aphasia",
  "Cognitive speech - speech problems related to cognitive impairment more than to aphasia",
  NA
)

values_cat_impairms_13 <- c(
  "Not Consciousness - cognitive problems related to reduced consciousness (e.g. when drowsy, sleepy)",
  "Consciousness - cognitive problems related to reduced consciousness (e.g. when drowsy, sleepy)",
  NA
)

values_cat_impairms_14 <- c(
  "Not Orientation - problems related to orientation in time, place and person",
  "Orientation - problems related to orientation in time, place and person",
  NA
)
     
values_cat_impairms_15 <- c(
  "Not Memory - Problems recalling past events (either short or long term)",
  "Memory - Problems recalling past events (either short or long term)",
  NA
)

values_cat_impairms_16 <- c(
  "Not Attention - the ability to choose and concentrate on relevant stimuli",
  "Attention - the ability to choose and concentrate on relevant stimuli",
  NA
)

values_cat_impairms_17 <- c(
  "Not Initiation - the ability to initiate / start a certain action (when abnormal a person might e.g. not respond or respond with a delay)",
  "Initiation - the ability to initiate / start a certain action (when abnormal a person might e.g. not respond or respond with a delay)",
  NA
)

values_cat_impairms_18 <- c(
  "Not Executive function (e.g. insight, planning, flexible thought) - a set of mental skills that include working memory, flexible thinking, and self-control (when abnormal a person might be unable to plan an action)",
  "Executive function (e.g. insight, planning, flexible thought) - a set of mental skills that include working memory, flexible thinking, and self-control (when abnormal a person might be unable to plan an action)",
  NA
)
 
values_cat_impairms_19 <- c(
  "Not Verbal aggression - the use of offensive language meant to threaten someone",
  "Verbal aggression - the use of offensive language meant to threaten someone",
  NA
)

values_cat_impairms_20 <- c(
  "Not Physical aggression - behaviour causing or threatening physical harm towards others",
  "Physical aggression - behaviour causing or threatening physical harm towards others",
  NA
)

values_cat_impairms_21 <- c(
  "Not Disinhibition - lack of restraint manifested in disregard of social convention, impulsivity, and poor risk assessment",
  "Disinhibition - lack of restraint manifested in disregard of social convention, impulsivity, and poor risk assessment",
  NA
)

values_cat_impairms_22 <- c(
  "Not Depression/Low mood - feelings of severe despondency and dejection",
  "Depression/Low mood - feelings of severe despondency and dejection",
  NA
)

values_cat_impairms_23 <- c(
  "Not Anxiety - emotion characterised by feelings of tension and worries thoughts",
  "Anxiety - emotion characterised by feelings of tension and worries thoughts",
  NA
)

values_cat_impairms_24 <- c(
  "Not Emotional liability - rapid, often exaggerated changes in mood, where strong emotions or feelings occur",
  "Emotional liability - rapid, often exaggerated changes in mood, where strong emotions or feelings occur",
  NA
)

values_cat_impairms_25 <- c(
  "Not Visual field loss/inattention - an area of the visual field is lost",
  "Visual field loss/inattention - an area of the visual field is lost",
  NA
)

values_cat_impairms_26 <- c(
  "Not Uncorrectable acuity - sharpness of vision is lost and cannot be corrected by e.g. spectacles",
  "Uncorrectable acuity - sharpness of vision is lost and cannot be corrected by e.g. spectacles",
  NA
)

values_cat_impairms_27 <- c(
  "Not Double vision - simultaneous perception of two images, usually overlapping, of a single scene or object",
  "Double vision - simultaneous perception of two images, usually overlapping, of a single scene or object",
  NA
)

values_cat_impairms_28 <- c(
  "Not Not applicable",
  "Not applicable",
  NA
)

values_cat_impairms_29 <- c(
  "Not Sensorineural - hearing loss occurring from damage to the inner ear, the auditory nerve, or the brain",
  "Sensorineural - hearing loss occurring from damage to the inner ear, the auditory nerve, or the brain",
  NA
)

values_cat_impairms_30 <- c(
  "Not Conductive - hearing loss occurring when sounds cannot get through the outer and middle ear (e.g. the ear canal is blocked)",
  "Conductive - hearing loss occurring when sounds cannot get through the outer and middle ear (e.g. the ear canal is blocked)",
  NA
)

values_cat_impairms_31 <- c(
  "Not Not applicable",
  "Not applicable",
  NA
)

values_cat_impairms_32 <- c(
  "Not Neuropathic pain - pain caused by a lesion or disease of the somatosensory nervous system, often described as burning, electrical, or shooting pain",
  "Neuropathic pain - pain caused by a lesion or disease of the somatosensory nervous system, often described as burning, electrical, or shooting pain",
  NA
)

values_cat_impairms_33 <- c(
  "Not Musculoskeletal pain - pain affecting the muscles. ligaments, tendons, and bones, often described as constant, pulling or 'muscle ache'",
  "Musculoskeletal pain - pain affecting the muscles. ligaments, tendons, and bones, often described as constant, pulling or 'muscle ache'",
  NA
)

values_cat_impairms_34 <- c(
  "Not Pain due to spasticity - pain occurring as part of spasticity",
  "Pain due to spasticity - pain occurring as part of spasticity",
  NA
)

values_cat_impairms_35 <- c(
  "Not Reduced cardiovascular fitness - inability to perform physical exercise or physical work",
  "Reduced cardiovascular fitness - inability to perform physical exercise or physical work",
  NA
)

values_cat_impairms_36 <- c(
  "Not Muscle fatigability - the decline in ability of muscles to generate force or sustain activity",
  "Muscle fatigability - the decline in ability of muscles to generate force or sustain activity",
  NA
)

values_cat_impairms_37 <- c(
  "Not Cognitive fatigue (ICF??) - extreme and persistent sense of tiredness, but when required a person is capable of performing physical exercise or work",
  "Cognitive fatigue (ICF??) - extreme and persistent sense of tiredness, but when required a person is capable of performing physical exercise or work",
  NA
)

values_cat_impairms_38 <- c(
  "Not Seizures (ICF ??) - sudden attacks of loss of consciousness and/or rhythmic motor activity that cannot be voluntarily controlled",
  "Seizures (ICF ??) - sudden attacks of loss of consciousness and/or rhythmic motor activity that cannot be voluntarily controlled",
  NA
)

values_cat_impairms_39 <- c(
  "Not Pressure sores - injuries to the skin and underlying tissue, primarily caused by prolonged pressure on the skin",
  "Pressure sores - injuries to the skin and underlying tissue, primarily caused by prolonged pressure on the skin",
  NA
)



## ----List categorical impairments values vectors----------------------------------------------------------------------
values_cat_impairms_list <- list(
  values_cat_impairms_1,
  values_cat_impairms_2,
  values_cat_impairms_3,
  values_cat_impairms_4,
  values_cat_impairms_5,
  values_cat_impairms_6,
  values_cat_impairms_7,
  values_cat_impairms_8,
  values_cat_impairms_9,
  values_cat_impairms_10,
  values_cat_impairms_11,
  values_cat_impairms_12,
  values_cat_impairms_13,
  values_cat_impairms_14,
  values_cat_impairms_15,
  values_cat_impairms_16,
  values_cat_impairms_17,
  values_cat_impairms_18,
  values_cat_impairms_19,
  values_cat_impairms_20,
  values_cat_impairms_21,
  values_cat_impairms_22,
  values_cat_impairms_23,
  values_cat_impairms_24,
  values_cat_impairms_25,
  values_cat_impairms_26,
  values_cat_impairms_27,
  values_cat_impairms_28,
  values_cat_impairms_29,
  values_cat_impairms_30,
  values_cat_impairms_31,
  values_cat_impairms_32,
  values_cat_impairms_33,
  values_cat_impairms_34,
  values_cat_impairms_35,
  values_cat_impairms_36,
  values_cat_impairms_37,
  values_cat_impairms_38,
  values_cat_impairms_39
)


## ----Vector categorical impairments variables-------------------------------------------------------------------------
variables_cat_impairms <- c(
  "nis_outp.motor__impairment_type",
  "nis_outp.spasticity_abnormal_muscle_tightness",
  "nis_outp.tendons_leading_deformity_hardening",
  "nis_outp.somatic___bodily_sensation_of_touch",
  "nis_outp.body_parts_awareness_position",
  "nis_outp.dysesthesia__abnormal_unpleasant_sensation_felt_when_touched",
  "nis_outp.body_inability_body_process",
  "nis_outp.process_side_external_space",
  "nis_outp.partial_loss_ability_expressive",
  "nis_outp.comprehend_language_intact_production",
  "nis_outp.unclear_articulation_speech_dysarthria",
  "nis_outp.cognitive_impairment_aphasia_cognitive",
  "nis_outp.consciousness_cognitive_problems_related",
  "nis_outp.time_place_orientation_person",
  "nis_outp.memory_problems_recalling_past",
  "nis_outp.attention_choose_concentrate_ability",
  "nis_outp.initiate_start_initiation_action",
  "nis_outp.executive_function",
  "nis_outp.threaten_offensive_language_meant",
  "nis_outp.physical_aggression_behaviour_causing",
  "nis_outp.social_convention_impulsivity_restraint",
  "nis_outp.dejection_depressionlow_mood_feelings",
  "nis_outp.tension_feelings_anxiety_emotion",
  "nis_outp.strong_emotions_feelings_occur",
  "nis_outp.visual_field_area_lost",
  "nis_outp.corrected_lost_vision_spectacles",
  "nis_outp.images_overlapping_double_vision",
  "nis_outp.not_applicable",
  "nis_outp.damage_ear_auditory_nerve",
  "nis_outp.outer_sounds_conductive_hearing",
  "nis_outp.not_applicable.1",
  "nis_outp.burning_electrical_disease_lesion",
  "nis_outp.muscles_ligaments_tendons_constant",
  "nis_outp.spasticity_pain_occurring_spasticity",
  "nis_outp.perform_physical_exercise_physical",
  "nis_outp.muscle_fatigability_decline_muscles",
  "nis_outp.cognitive_fatigue_extreme_capable",
  "nis_outp.loss_consciousness_andor_rhythmic",
  "nis_outp.skin_pressure_sores_injuries"
)


## ----Set list names cat impairms--------------------------------------------------------------------------------------
names(values_cat_impairms_list) <- variables_cat_impairms


## ----Imp_check categorical impairments variables----------------------------------------------------------------------
# Create empty list
imp_list_cat_impairms <- list()

# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(variables_cat_impairms)) {
  imp_list_cat_impairms[i] <- imp_check_1(data = dat,
                                     variables = names(values_cat_impairms_list)[i],
                                     values = values_cat_impairms_list[[i]]) 

}

# Name list with var names to correspond to imp_messages
names(imp_list_cat_impairms) <- variables_cat_impairms

# View list of imp_messages with corresponding var names
print(imp_list_cat_impairms)


## ----Summary table categorical impairments variables------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(variables_cat_impairms),
    missing_text = "Missing")


## ----Vector numeric domain values-------------------------------------------------------------------------------------
values_num_domains <- c(
  0,
  1,
  2,
  3,
  -777,
  NA
)


## ----Vector numeric domain variables----------------------------------------------------------------------------------
variables_num_domains <- c(
  "nis_outp.left_upper_limb_numeric",
  "nis_outp.right_upper_limb_numeric",
  "nis_outp.left_lower_limb_numeric",
  "nis_outp.right_lower_limb_numeric",
  "nis_outp.trunk_numeric",
  "nis_outp.tonejoint_range_numeric",
  "nis_outp.sensation_numeric",
  "nis_outp.perceptual_function_numeric",
  "nis_outp.speech_and_language_numeric",
  "nis_outp.cognitive_function_numeric",
  "nis_outp.behaviour_numeric",
  "nis_outp.mood_numeric",
  "nis_outp.seeing_and_vision_numeric",
  "nis_outp.hearing_numeric",
  "nis_outp.pain_numeric",
  "nis_outp.fatigue_numeric"
)


## ----Imp_check numeric domain variables-------------------------------------------------------------------------------
# Create empty list
imp_list_num_domains <- list()

# Loop over each variable
# Check if any implausible values as listed in values vector
# Add imp_message to list
for (i in 1:length(variables_num_domains)) {
  imp_list_num_domains[i] <- imp_check_1(data = dat,
                                     variables = variables_num_domains[i],
                                     values = values_num_domains) 

}

# Set names of list of imp_messages to var names
names(imp_list_num_domains) <- variables_num_domains

# View named list of imp_messages
print(imp_list_num_domains)


## ----Summary table numeric domain variables---------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(variables_num_domains),
    missing_text = "Missing")


## ----Vector numeric impairments values--------------------------------------------------------------------------------
values_num_impairms <- c(
  0,
  1,
  -777,
  NA
)


## ----Vector numeric impairments variables-----------------------------------------------------------------------------
variables_num_impairms <- c(
  "nis_outp.spasticity_abnormal_muscle_tightness_numeric",
  "nis_outp.tendons_leading_deformity_hardening_numeric",
  "nis_outp.somatic___bodily_sensation_of_touch_numeric",
  "nis_outp.body_parts_awareness_position_numeric",
  "nis_outp.dysesthesia__abnormal_unpleasant_sensation_felt_when_touched_numeric",
  "nis_outp.body_inability_body_process_numeric",
  "nis_outp.process_side_external_space_numeric",
  "nis_outp.partial_loss_ability_expressive_numeric",
  "nis_outp.comprehend_language_intact_production_numeric",
  "nis_outp.unclear_articulation_speech_dysarthria_numeric",
  "nis_outp.cognitive_impairment_aphasia_cognitive_numeric",
  "nis_outp.consciousness_cognitive_problems_related_numeric",
  "nis_outp.time_place_orientation_person_numeric",
  "nis_outp.memory_problems_recalling_past_numeric",
  "nis_outp.attention_choose_concentrate_ability_numeric",
  "nis_outp.initiate_start_initiation_action_numeric",
  "nis_outp.executive_function_numeric",
  "nis_outp.threaten_offensive_language_meant_numeric",
  "nis_outp.physical_aggression_behaviour_causing_numeric",
  "nis_outp.social_convention_impulsivity_restraint_numeric",
  "nis_outp.dejection_depressionlow_mood_feelings_numeric",
  "nis_outp.tension_feelings_anxiety_emotion_numeric",
  "nis_outp.strong_emotions_feelings_occur_numeric",
  "nis_outp.visual_field_area_lost_numeric",
  "nis_outp.corrected_lost_vision_spectacles_numeric",
  "nis_outp.images_overlapping_double_vision_numeric",
  "nis_outp.not_applicable_numeric",
  "nis_outp.damage_ear_auditory_nerve_numeric",
  "nis_outp.outer_sounds_conductive_hearing_numeric",
  "nis_outp.not_applicable.1_numeric",
  "nis_outp.burning_electrical_disease_lesion_numeric",
  "nis_outp.muscles_ligaments_tendons_constant_numeric",
  "nis_outp.spasticity_pain_occurring_spasticity_numeric",
  "nis_outp.perform_physical_exercise_physical_numeric",
  "nis_outp.muscle_fatigability_decline_muscles_numeric",
  "nis_outp.cognitive_fatigue_extreme_capable_numeric",
  "nis_outp.loss_consciousness_andor_rhythmic_numeric",
  "nis_outp.skin_pressure_sores_injuries_numeric"
)


## ----Imp_check numeric impairments variables--------------------------------------------------------------------------
# Create empty list
imp_list_num_impairms <- list()

# Loop over each variable
# Check if any implausible values as listed in values vector
# Add imp_message to list
for (i in 1:length(variables_num_impairms)) {
  imp_list_num_impairms[i] <- imp_check_1(data = dat,
                                     variables = variables_num_impairms[i],
                                     values = values_num_impairms) 

}

# Set names of list of imp_messages to var names
names(imp_list_num_impairms) <- variables_num_impairms

# View named list of imp_messages
print(imp_list_num_impairms)


## ----Summary table numeric impairments variables----------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(variables_num_impairms),
    missing_text = "Missing")


## ----Vectors numeric values-------------------------------------------------------------------------------------------
values_num_1 <- c(
  1,
  2,
  3,
  4,
  5,
  6,
  -777,
  NA
)

values_num_2 <- c(
  0,
  1,
  -777,
  -888,
  NA
)


## ----List numeric values vectors--------------------------------------------------------------------------------------
values_num_list <- list(
  values_num_1,
  values_num_2
)


## ----Vector numeric variables-----------------------------------------------------------------------------------------
variables_num <- c(
  "nis_outp.motor__impairment_type_numeric",
  "nis_outp.other_numeric"
)


## ----Set list names num-----------------------------------------------------------------------------------------------
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
  1
  )

sum_vars <- c(
  "nis_outp.left_upper_limb_numeric",
  "nis_outp.right_upper_limb_numeric",
  "nis_outp.left_lower_limb_numeric",
  "nis_outp.right_lower_limb_numeric",
  "nis_outp.trunk_numeric",
  "nis_outp.tonejoint_range_numeric",
  "nis_outp.sensation_numeric",
  "nis_outp.perceptual_function_numeric",
  "nis_outp.speech_and_language_numeric",
  "nis_outp.cognitive_function_numeric",
  "nis_outp.behaviour_numeric",
  "nis_outp.mood_numeric",
  "nis_outp.seeing_and_vision_numeric",
  "nis_outp.hearing_numeric",
  "nis_outp.pain_numeric",
  "nis_outp.fatigue_numeric",
  "nis_outp.other_numeric"
  )


## ----Generate sumscores-----------------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(
    nis.sum_score = 
         sumscores(input = dat,
                   sum_vars = sum_vars,
                   coding_keys = keys,
                   na_allowed = 0,
                   min_item = 0,
                   max_item = 3,
                   min_score = 0,
                   max_score = 50
                   )[["scores"]]
         )



## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/core_neuro/nis_outp_covidcns_clean.rds")
    )

