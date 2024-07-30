## COVID-CNS cognition analysis
## Univariate script and modelling script #authors Kukatharmini Tharmaratnam and Greta Wood
## Input: Output from Script 1 (dataframe including NeuroCOVID, COVID and normative data including biomarkers), imaging z score composites
## Outputs:
# Table 1: Demographics of cohort, comparing NeuroCOVID and COVID groups. 
# Table 2: Univariate associations, clinical linear regression model and multifaceted linear regression models for Global DfE (GDfE) Score in NeuroCOVID and COVID groups
# Extended Data Table 2" Recovery
# Supplementary Tables 4, 5 and 6

#clear environment
rm(list = ls())

#set WD
setwd("/Users/GretaKWood/Documents/0- Covid-CNS/Neurocog/Data analysis/Final code and figures")

#load packages
library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(tidyverse)
library(reshape2)
library(plyr)
library(MASS)
library(gtsummary)
library(misty)
library(caret)
library(metan)

##Start####

#load data
data_Almost<-readRDS("Dummycogbiomerg_norms3_190723.rds")
neuroimcomps<-readRDS("dummyneuroimcomps.rds") #neuroimaging composites from Brendan Sargent

#Explore data and assign levels ####
#check dim
dim(neuroimcomps) 
dim(data_Almost)

#check correlation between neuroimaging composites
neurocor<-cor(neuroimcomps[,c("fsidps_volume_anteriorcingulatecortex_composite_Zscore","fsidps_volume_superiortemporalgyrus_composite_Zscore","fsidps_volume_insula_composite_Zscore", "fsidps_thickness_superiortemporalgyrus_composite_Zscore","fsidps_thickness_orbitofrontalcortex_composite_Zscore")])
print(neurocor)

#merge data
data_All<- merge(data_Almost, neuroimcomps, by = "ID", all.x = TRUE)
dim(data_All)

#time between COVID-19 and post-acute assessment (cognitive testing, brain injury markers and imaging)
summary(as.numeric(data_All$days_covid_baseline))

#how many people have a valid score for Cognitron at baseline (post-acute appointment), online follow-up 1 and follow-up 2
response_counts <- data_All %>%
  group_by(as.factor(case_control_vaccine)) %>%
  summarise(
    valid_responses_Composite_global = sum(!is.na(Composite_global)),
    valid_responses_Composite_global_f1 = sum(!is.na(Composite_global_f1)),
    valid_responses_Composite_global_f2 = sum(!is.na(Composite_global_f2))
  )

# Print the response counts
print(response_counts)

#time since COVID-19 visualise
hist(as.numeric(data_All$days_covid_baseline),main="Number of days from COVID test to Assessment date for all patients")

#Explore Diagnostic.Group
table(data_All$diagnostic_group)
table(data_All$case_control_vaccine)

##subset to only include NeuroCOVID and COVID
data_case_positiveControls=subset(data_All,(data_All$case_control_vaccine=="Case: COVID-19 positive (i.e. neurological or psychiatric complication)"|data_All$case_control_vaccine=="Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)"))

#convert characters to factors
data_case_positiveControls$dem.sex_at_birth<-as.factor(data_case_positiveControls$dem.sex_at_birth)
data_case_positiveControls$primary_site_name<-as.factor(data_case_positiveControls$primary_site_name)

#Group recruitment sites by region
data_case_positiveControls$site_group[data_case_positiveControls$primary_site_name=="Birmingham"]="South"
data_case_positiveControls$site_group[data_case_positiveControls$primary_site_name=="Cambridge"]="South"
data_case_positiveControls$site_group[data_case_positiveControls$primary_site_name=="Cardiff"]="South"
data_case_positiveControls$site_group[data_case_positiveControls$primary_site_name=="GSTT"]="London"
data_case_positiveControls$site_group[data_case_positiveControls$primary_site_name=="KCH"]="London"
data_case_positiveControls$site_group[data_case_positiveControls$primary_site_name=="Liverpool"]="North"
data_case_positiveControls$site_group[data_case_positiveControls$primary_site_name=="Newcastle"]="North"
data_case_positiveControls$site_group[data_case_positiveControls$primary_site_name=="Nottingham"]="South"
data_case_positiveControls$site_group[data_case_positiveControls$primary_site_name=="Oxford"]="South"
data_case_positiveControls$site_group[data_case_positiveControls$primary_site_name=="Salford Royal"]="North"
data_case_positiveControls$site_group[data_case_positiveControls$primary_site_name=="Sheffield"]="North"
data_case_positiveControls$site_group[data_case_positiveControls$primary_site_name=="SLaM"]="London"
data_case_positiveControls$site_group[data_case_positiveControls$primary_site_name=="Southampton"]="South"
data_case_positiveControls$site_group[data_case_positiveControls$primary_site_name=="UCLH"]="London"

#Explore site data
table(data_case_positiveControls$site_group)
table(data_case_positiveControls$primary_site_name)
table(data_case_positiveControls$site_group)

# WHO Covid severity
summary(data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19)
table(data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric)

#create new grouping and assign NA
dim(data_case_positiveControls)
data_case_positiveControls$WHO_Covid_severity_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$WHO_Covid_severity_group[data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==0]="Ambulatory mild disease"
data_case_positiveControls$WHO_Covid_severity_group[data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==1|data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==2|data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==3]="Ambulatory mild disease"
data_case_positiveControls$WHO_Covid_severity_group[data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==4|data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==5]="Hospitalised: moderate"
data_case_positiveControls$WHO_Covid_severity_group[data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==6|data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==7|data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==8|data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==9]="Hospitalised: severe"
data_case_positiveControls$WHO_Covid_severity_group[data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==10]="Dead"
data_case_positiveControls$WHO_Covid_severity_group[data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric==-777]=NA
data_case_positiveControls$WHO_Covid_severity_group[is.na(data_case_positiveControls$ncrf1_vital.admission_severity_worst_covid19_numeric)]=NA

table(data_case_positiveControls$WHO_Covid_severity_group)

##Pre-morbid Rockwell Clinical Frailty Score
summary(as.factor(data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale))
table(data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric)

data_case_positiveControls$clinical_frailty_scale_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$clinical_frailty_scale_group[data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==1|data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==2|data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==3]="Managing well"
data_case_positiveControls$clinical_frailty_scale_group[data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==4|data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==5]="Mild"
data_case_positiveControls$clinical_frailty_scale_group[data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==6|data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==7|data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==8]="Moderate-severe"
data_case_positiveControls$clinical_frailty_scale_group[data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==9]="Terminally ill "
data_case_positiveControls$clinical_frailty_scale_group[data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==-777]=NA
data_case_positiveControls$clinical_frailty_scale_group[is.na(data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric)]=NA
data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric[data_case_positiveControls$ncrf1_comorbid.clinical_frailty_scale_numeric==-777]=NA
table(data_case_positiveControls$clinical_frailty_scale_group)

##Timeframe of admission in 6 month windows starting in March 2020.
summary(data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission) 

###define how many cases vs Controls were recruited in the first COVID-19 wave in every 6 months from March 2020
data_case_positiveControls$time_window_covid19=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$time_window_covid19[data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission>="2020-03-01 00:00:00" & data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission<"2020-09-01 00:00:00"]=1
data_case_positiveControls$time_window_covid19[data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission>="2020-09-01 00:00:00" & data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission<"2021-03-01 00:00:00"]=2
data_case_positiveControls$time_window_covid19[data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission>="2021-03-01 00:00:00" & data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission<"2021-09-01 00:00:00"]=3
data_case_positiveControls$time_window_covid19[data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission>="2021-09-01 00:00:00" & data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission<"2022-03-01 00:00:00"]=4
data_case_positiveControls$time_window_covid19[data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission>="2022-03-01 00:00:00" & data_case_positiveControls$ncrf1_admission.date_of_inpatient_admission<"2022-09-01 00:00:00"]=5

table(data_case_positiveControls$time_window_covid19)

##"dem.concerned_memory"  self-reported memory concerns
data_case_positiveControls$dem.concerned_memory_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$dem.concerned_memory_group[data_case_positiveControls$dem.concerned_memory=="Yes"]=1
data_case_positiveControls$dem.concerned_memory_group[data_case_positiveControls$dem.concerned_memory=="No"]=0

##"dem.concerned_memory_before_covid" 
data_case_positiveControls$dem.concerned_memory_before_covid_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$dem.concerned_memory_before_covid_group[data_case_positiveControls$dem.concerned_memory_before_covid=="Yes"]=1
data_case_positiveControls$dem.concerned_memory_before_covid_group[data_case_positiveControls$dem.concerned_memory_before_covid=="No"]=0

## memory after covid
data_case_positiveControls$dem.concerned_memory_after_covid_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$dem.concerned_memory_after_covid_group[data_case_positiveControls$dem.concerned_memory_after_covid=="Yes"]=1
data_case_positiveControls$dem.concerned_memory_after_covid_group[data_case_positiveControls$dem.concerned_memory_after_covid=="No"]=0

## memory after covid progressively worse
data_case_positiveControls$dem.has_your_memory_got_progressively_worse_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$dem.has_your_memory_got_progressively_worse_group[data_case_positiveControls$dem.has_your_memory_got_progressively_worse=="Yes"]=1
data_case_positiveControls$dem.has_your_memory_got_progressively_worse_group[data_case_positiveControls$dem.has_your_memory_got_progressively_worse=="No"]=0

##had vaccination grouping
data_case_positiveControls$ncrf1_pre_med.months_presentation.had_covid19_vaccine_group[data_case_positiveControls$ncrf1_pre_med.months_presentation.had_covid19_vaccine=="Yes"]=1
data_case_positiveControls$ncrf1_pre_med.months_presentation.had_covid19_vaccine_group[data_case_positiveControls$ncrf1_pre_med.months_presentation.had_covid19_vaccine=="No"]=0

#vaccine by time of post-acute vaccine.have_you_had_your_second_dose
table(data_case_positiveControls$vaccine.have_you_had_a_covid19_vaccine) #274/306 vaccinated
table(data_case_positiveControls$vaccine.have_you_had_your_second_dose) #257 two doses

###had steroids grouping
data_case_positiveControls$ncrf2_med.corticosteroid_group=rep(NA,nrow(data_case_positiveControls))
data_case_positiveControls$ncrf2_med.corticosteroid_group[data_case_positiveControls$ncrf2_med.corticosteroid=="Yes"]=1
data_case_positiveControls$ncrf2_med.corticosteroid_group[data_case_positiveControls$ncrf2_med.corticosteroid=="No"]=0

#make low wbc count normal
data_case_positiveControls$ncrf2_lab.wbc_count_level_numeric[data_case_positiveControls$ncrf2_lab.wbc_count_level_numeric==1]<-0

##recategorise education for the sake of modelling
# Create a new variable "education_category" with three categories
data_case_positiveControls$education_category <- ifelse(data_case_positiveControls$dem.highest_education %in% c("College or university degree"), "College or university degree",
                                                        ifelse(data_case_positiveControls$dem.highest_education == "None of the above", "None of the above", "Other"))

# Convert "education_category" to a factor variable
data_case_positiveControls$education_category <- factor(data_case_positiveControls$education_category, levels = c("College or university degree", "None of the above", "Other"))

# Print the summary of the new variable "education_category"
summary(data_case_positiveControls$education_category)

#make days since COVID-19 numeric variable and window of illness a factor
data_case_positiveControls$days_covid_baseline<-as.numeric(data_case_positiveControls$days_covid_baseline)
data_case_positiveControls$time_window_covid19.factor<-as.factor(data_case_positiveControls$time_window_covid19)

#assign intuitive names to levels in time window variable and case status for table
levels(data_case_positiveControls$time_window_covid19.factor)=c("01/03/2020 – 01/09/2020", "01/09/2020 – 01/03/2021","01/03/2021 – 01/09/2021",
                                                                "01/09/2021 – 01/03/2022","01/03/2022 - 01/09/2022")
data_case_positiveControls$case_control_vaccine.factor<-as.factor(data_case_positiveControls$case_control_vaccine)
levels(data_case_positiveControls$case_control_vaccine.factor)=c("NeuroCOVID","COVID")

table(data_case_positiveControls$language)

#Table 1 ####
tablevarrev<-data_case_positiveControls %>% select (dem.dob_age,
                                                               case_control_vaccine.factor,
                                                               dem.sex_at_birth,
                                                               language,
                                                               dem.highest_education,
                                                               clinical_frailty_scale_group,
                                                               WHO_Covid_severity_group,
                                                               days_covid_baseline,
                                                               time_window_covid19.factor,
                                                               ncrf1_pre_med.months_presentation.had_covid19_vaccine_group,
                                                               ncrf2_med.corticosteroid_group,
                                                               dem.concerned_memory_group,
                                                               phq9.sum_score,
                                                               gad7.sum_score,
                                                               pcl5.sum_score,
                                                               cfs.physical_subscale,
                                                               cfs.mental_subscale,
                                                               Composite_global,
                                                               Composite_acc,
                                                               Composite_rt)

#Assign names to columns
colnames(tablevarrev)<-c("Age","Group","Sex","First language","Level of education","Pre-morbid Clinical Frailty Scale",
                      "WHO COVID-19 Severity","Days since admission","Admission date",
                      "Prior COVID-19 vaccination","Acute steroid treatment",
                      "Memory concerns", "PHQ-9 score", "GAD-7 score", "PCL-5 score", "CFQ physical subscale", "CHQ mental subscale", "Cognitron Global Score","Cognitron Accuracy","Cognitron RT")

#create table grouped by NeuroCOVID and COVID
tablevarrev %>%
  tbl_summary(by='Group') %>%
  add_p() %>%
  add_overall %>%
  bold_labels()

#Table of task scores for supplementary ####
#Extended Data Table 1 ####
tablevartasks<-data_case_positiveControls %>% select (case_control_vaccine.factor,
                                                    Composite_global,
                                                    Composite_acc,
                                                    Composite_rt,rs_prospectiveMemoryWords_1_immediate_dfe,
                                                    rs_spatialSpan_dfe,
                                                    rs_manipulations2D_dfe, 
                                                    rs_verbalAnalogies_dfe,
                                                    rs_prospectiveMemoryWords_1_delayed_dfe,
                                                    rs_TOL_dfe,
                                                    rs_prospectiveMemoryWords_1_immediate_RT_dfe,
                                                    rs_spatialSpan_RT_dfe,                       
                                                    rs_manipulations2D_RT_dfe,                  
                                                    rs_verbalAnalogies_RT_dfe,                   
                                                    rs_prospectiveMemoryWords_1_delayed_RT_dfe,  
                                                    rs_TOL_RT_dfe,                             
                                                    rs_motorControl_RT_dfe)

colnames(tablevartasks)<-c("Group","Cognitron Global Score","Cognitron Accuracy","Cognitron RT",
                         "Recognition memory (immediate) Accuracy","Spatial Span Accuracy", "2D Manipulations Accuracy",
                         "Verbal Analogies Accuracy","Recognition memory (delayed) Accuracy", "Tower of London Accuracy",
                         "Recognition memory (immediate) RT","Spatial Span RT", "2D Manipulations RT",
                         "Verbal Analogies RT","Recognition memory (delayed) RT", "Tower of London RT",
                         "Motor Control RT"
                         )

#create table grouped by NeuroCOVID and COVID
tablevartasks %>%
  tbl_summary(by='Group') %>%
  add_overall %>%
  bold_labels()

#days since COVID-19 - split into quarters of fixed duration- median in cases 341 days
data_case_positiveControls$daysquarters[data_case_positiveControls$days_covid_baseline<170]<-1
data_case_positiveControls$daysquarters[data_case_positiveControls$days_covid_baseline>=170&
                                          data_case_positiveControls$days_covid_baseline<341]<-2
data_case_positiveControls$daysquarters[data_case_positiveControls$days_covid_baseline>=341&
                                          data_case_positiveControls$days_covid_baseline<511]<-3
data_case_positiveControls$daysquarters[data_case_positiveControls$days_covid_baseline>=511]<-4
table(data_case_positiveControls$daysquarters,data_case_positiveControls$case_control_vaccine)

# Normality test for continuous variables #######

#### Median(IQR) for continuous variables
Median_IQR=function(x,y,k){
  Median=median(x[which(y==k)],na.rm = TRUE)
  IQR=IQR(x[which(y==k)],na.rm = TRUE)
  return(c(Median=Median,IQR=IQR))}

###### M: Mann-Whitney test 
Mtest_continuous=function(x,y,alpha){
  Missing=sum(is.na(x))
  Missing_percentage=sum(is.na(x))/length(x)*100
  Mtest=wilcox.test(x~y,conf.int = TRUE, conf.level = (1-alpha)) 
  p_value=Mtest$p.value
  difference= Mtest$estimate
  CI=Mtest$conf.int
  return(c(Missing=Missing,Missing_percentage=Missing_percentage,p_value=p_value,difference=difference,CI=CI))
}

#Subset data by NeuroCOVID (case) /COVID (positive control)####
#### subset of cases 
data_case=subset(data_case_positiveControls,(data_case_positiveControls$case_control_vaccine=="Case: COVID-19 positive (i.e. neurological or psychiatric complication)"))
dim(data_case) 
### subset of positive controls 
data_positiveControls=subset(data_case_positiveControls,(data_case_positiveControls$case_control_vaccine=="Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)"))
dim(data_positiveControls) 

#Distribution of time since COVID-19 by NeuroCOVID/COVID
summary(as.numeric(data_case$days_covid_baseline))
hist(as.numeric(data_case$days_covid_baseline),main="Number of days from COVID test to Assessment date for case patients")

summary(as.numeric(data_positiveControls$days_covid_baseline))
hist(as.numeric(data_positiveControls$days_covid_baseline),main="Number of days from COVID test to Assessment date for control patients")

# Table 2 effect sizes ####
cor(data_case$Composite_global, data_case$dem.dob_age, method = "spearman")
wilcox_effsize(data_case, Composite_global~education_category)
wilcox_effsize(data_case, Composite_global~clinical_frailty_scale_group)
wilcox_effsize(data_case, Composite_global~time_window_covid19.factor)
cor(data_case$Composite_global, data_case$days_covid_baseline, use="complete.obs", method = "spearman")
wilcox_effsize(data_case, Composite_global~WHO_Covid_severity_group)
wilcox_effsize(data_case, Composite_global~ncrf1_pre_med.months_presentation.had_covid19_vaccine_group)
wilcox_effsize(data_case, Composite_global~diagnostic_group)
wilcox_effsize(data_case, Composite_global~On_antidepress)
cor(data_case$Composite_global, data_case$phq9.sum_score, use="complete.obs", method = "spearman")
cor(data_case$Composite_global, data_case$gad7.sum_score, use="complete.obs", method = "spearman")
cor(data_case$Composite_global, data_case$pcl5.sum_score, use="complete.obs", method = "spearman")
cor(data_case$Composite_global, data_case$cfs.mental_subscale, use="complete.obs", method = "spearman")
cor(data_case$Composite_global, data_case$cfs.physical_subscale, use="complete.obs", method = "spearman")
cor(data_case$Composite_global, data_case$Total_ACB_Score, use="complete.obs", method = "spearman")
cor(data_case$Composite_global, data_case$Total_Multimorbidity, use="complete.obs", method = "spearman")
wilcox_effsize(data_case, Composite_global~ncrf2_lab.crp_level_numeric)
wilcox_effsize(data_case, Composite_global~ncrf2_lab.wbc_count_level_numeric)

table(data_positiveControls$ncrf2_lab.wbc_count_level_numeric)

wilcox_effsize(data_case, Composite_global~ncrf2_med.corticosteroid_group)
wilcox_effsize(data_case, Composite_global~dem.concerned_memory_group)
cor(data_case$Composite_global, data_case$NfL, use="complete.obs", method = "spearman")
cor(data_case$Composite_global, data_case$GFAP, use="complete.obs", method = "spearman")
cor(data_case$Composite_global, data_case$Tau, use="complete.obs", method = "spearman")
cor(data_case$Composite_global, data_case$UCHL1, use="complete.obs", method = "spearman")

#now same for controls
cor(data_positiveControls$Composite_global, data_positiveControls$dem.dob_age, method = "spearman")
wilcox_effsize(data_positiveControls, Composite_global~education_category)
wilcox_effsize(data_positiveControls, Composite_global~clinical_frailty_scale_group)
wilcox_effsize(data_positiveControls, Composite_global~time_window_covid19.factor)
cor(data_positiveControls$Composite_global, data_positiveControls$days_covid_baseline, use="complete.obs", method = "spearman")
wilcox_effsize(data_positiveControls, Composite_global~WHO_Covid_severity_group)
wilcox_effsize(data_positiveControls, Composite_global~ncrf1_pre_med.months_presentation.had_covid19_vaccine_group)
wilcox_effsize(data_positiveControls, Composite_global~On_antidepress)
cor(data_positiveControls$Composite_global, data_positiveControls$phq9.sum_score, use="complete.obs", method = "spearman")
cor(data_positiveControls$Composite_global, data_positiveControls$gad7.sum_score, use="complete.obs", method = "spearman")
cor(data_positiveControls$Composite_global, data_positiveControls$pcl5.sum_score, use="complete.obs", method = "spearman")
cor(data_positiveControls$Composite_global, data_positiveControls$cfs.mental_subscale, use="complete.obs", method = "spearman")
cor(data_positiveControls$Composite_global, data_positiveControls$cfs.physical_subscale, use="complete.obs", method = "spearman")
cor(data_positiveControls$Composite_global, data_positiveControls$Total_ACB_Score, use="complete.obs", method = "spearman")
cor(data_positiveControls$Composite_global, data_positiveControls$Total_Multimorbidity, use="complete.obs", method = "spearman")
wilcox_effsize(data_positiveControls, Composite_global~ncrf2_lab.crp_level_numeric)
wilcox_effsize(data_positiveControls, Composite_global~ncrf2_lab.wbc_count_level_numeric)
wilcox_effsize(data_positiveControls, Composite_global~ncrf2_med.corticosteroid_group)
wilcox_effsize(data_positiveControls, Composite_global~dem.concerned_memory_group)
cor(data_positiveControls$Composite_global, data_positiveControls$NfL, use="complete.obs", method = "spearman")
cor(data_positiveControls$Composite_global, data_positiveControls$GFAP, use="complete.obs", method = "spearman")
cor(data_positiveControls$Composite_global, data_positiveControls$Tau, use="complete.obs", method = "spearman")
cor(data_positiveControls$Composite_global, data_positiveControls$UCHL1, use="complete.obs", method = "spearman")

#PPV of memory concerns ####
#Compositeglobal by subjective cognitive impairment (memory concerns)
#First for NeuroCOVID
#Mann Whitney U
Mtest_continuous(data_case$Composite_global,data_case$dem.concerned_memory_group,alpha=0.05)
wilcox_test(data_case, Composite_global~dem.concerned_memory_group, detailed=T)
#effect size
wilcox_effsize(data_case, Composite_global~dem.concerned_memory_group)
#then same for COVID
Mtest_continuous(data_positiveControls$Composite_global,data_positiveControls$dem.concerned_memory_group,alpha=0.05)
wilcox_test(data_positiveControls, Composite_global~dem.concerned_memory_group, detailed=T)
wilcox_effsize(data_positiveControls, Composite_global~dem.concerned_memory_group)

#Descriptive data of memory concerns
#case GDfE by subj cog
tapply(data_case$Composite_global, data_case$dem.concerned_memory_group, summary)
tapply(data_case$Composite_global, data_case$dem.concerned_memory_group, IQR)

#control GDfE by subj cog
tapply(data_positiveControls$Composite_global, data_positiveControls$dem.concerned_memory_group, summary)
tapply(data_positiveControls$Composite_global, data_positiveControls$dem.concerned_memory_group, IQR)

#Create binary variable GDfE less than expected (<0)
data_case$poorcog[data_case$Composite_global>=0]<-0
data_case$poorcog[data_case$Composite_global<0]<-1

# Create a 2x2 contingency table
contingency_table <- table(data_case$poorcog, data_case$dem.concerned_memory_group)
contingency_table
# Calculate sensitivity (True Positive Rate)
sensitivity <- contingency_table[2, 2] / sum(contingency_table[2, ])

# Calculate specificity (True Negative Rate)
specificity <- contingency_table[1, 1] / sum(contingency_table[1, ])

# Calculate Positive Predictive Value (PPV)
ppv <- contingency_table[2, 2] / sum(contingency_table[, 2])

# Calculate Negative Predictive Value (NPV)
npv <- contingency_table[1, 1] / sum(contingency_table[, 1])

# Print the results
cat("Sensitivity (True Positive Rate):", sensitivity, "\n")
cat("Specificity (True Negative Rate):", specificity, "\n")
cat("Positive Predictive Value (PPV):", ppv, "\n")
cat("Negative Predictive Value (NPV):", npv, "\n")

#same for COVID
data_positiveControls$poorcog[data_positiveControls$Composite_global>=0]<-0
data_positiveControls$poorcog[data_positiveControls$Composite_global<0]<-1

# Create a 2x2 contingency table
contingency_table <- table(data_positiveControls$poorcog, data_positiveControls$dem.concerned_memory_group)
contingency_table
# Calculate sensitivity (True Positive Rate)
sensitivity <- contingency_table[2, 2] / sum(contingency_table[2, ])

# Calculate specificity (True Negative Rate)
specificity <- contingency_table[1, 1] / sum(contingency_table[1, ])

# Calculate Positive Predictive Value (PPV)
ppv <- contingency_table[2, 2] / sum(contingency_table[, 2])

# Calculate Negative Predictive Value (NPV)
npv <- contingency_table[1, 1] / sum(contingency_table[, 1])

# Print the results
cat("Sensitivity (True Positive Rate):", sensitivity, "\n")
cat("Specificity (True Negative Rate):", specificity, "\n")
cat("Positive Predictive Value (PPV):", ppv, "\n")
cat("Negative Predictive Value (NPV):", npv, "\n")

#Revision - additionally report with GDfE score < -1 
data_case$poorcogR[data_case$Composite_global>=-1]<-0
data_case$poorcogR[data_case$Composite_global< (-1)]<-1

table(data_case$dem.concerned_memory_group, data_case$poorcogR)

# Create a 2x2 contingency table
contingency_tableR <- table(data_case$poorcogR, data_case$dem.concerned_memory_group)
contingency_tableR
# Calculate sensitivity (True Positive Rate)
sensitivity <- contingency_tableR[2, 2] / sum(contingency_tableR[2, ])

# Calculate specificity (True Negative Rate)
specificity <- contingency_tableR[1, 1] / sum(contingency_tableR[1, ])

# Calculate Positive Predictive Value (PPV)
ppv <- contingency_tableR[2, 2] / sum(contingency_tableR[, 2])

# Calculate Negative Predictive Value (NPV)
npv <- contingency_tableR[1, 1] / sum(contingency_tableR[, 1])

#in controls
data_positiveControls$poorcogR[data_positiveControls$Composite_global >= -1] <- 0
data_positiveControls$poorcogR[data_positiveControls$Composite_global < -1] <- 1

table(data_positiveControls$dem.concerned_memory_group, data_positiveControls$poorcogR)

# Create a 2x2 contingency table
contingency_table <- table(data_positiveControls$poorcogR, data_positiveControls$dem.concerned_memory_group)
contingency_table
# Calculate sensitivity (True Positive Rate)
sensitivity <- contingency_table[2, 2] / sum(contingency_table[2, ])

# Calculate specificity (True Negative Rate)
specificity <- contingency_table[1, 1] / sum(contingency_table[1, ])

# Calculate Positive Predictive Value (PPV)
ppv <- contingency_table[2, 2] / sum(contingency_table[, 2])

# Calculate Negative Predictive Value (NPV)
npv <- contingency_table[1, 1] / sum(contingency_table[, 1])

# Print the results
cat("Sensitivity (True Positive Rate):", sensitivity, "\n")
cat("Specificity (True Negative Rate):", specificity, "\n")
cat("Positive Predictive Value (PPV):", ppv, "\n")
cat("Negative Predictive Value (NPV):", npv, "\n")

#Subsetting of data to create multiple regression data ####
#First check correlations
#Supplementary Figure 8: Correlations ####
# select continuous variables included in modelling
correlationsupp<-data_case_positiveControls[ ,c("days_covid_baseline",
                                                "dem.dob_age",
                                                "Total_Multimorbidity",
                                                "Composite_global",
                                                "phq9.sum_score",
                                                "NfL",
                                                "GFAP",
                                                "fsidps_volume_anteriorcingulatecortex_composite_Zscore")]
#Rename columns
colnames(correlationsupp)<-c("Days since COVID-19", "Age", "Multimorbidity", "Global DfE", "PHQ-9", "NfL", "GFAP", "ACC Volume")
#calculate correlations
cor(correlationsupp, method="spearman", use="pairwise.complete.obs")
#plot these
corrplot<-corr_coef(correlationsupp, method="spearman", use="pairwise.complete.obs")
plot(corrplot)

#NeuroCOVID modelling dataset ####
attach(data_case)
data_case_multipleReg <-data.frame(
  ID,
  primary_site_name,
  site_group,
  dem.dob_age,
  dem.sex_at_birth,
  WHO_Covid_severity_group,
  diagnostic_group,
  education_category,
  clinical_frailty_scale_group,
  phq9.sum_score,
  gad7.sum_score,
  pcl5.sum_score,
  cfs.mental_subscale,
  cfs.physical_subscale,
  Total_ACB_Score,
  Total_Multimorbidity,
  ncrf1_pre_med.months_presentation.had_covid19_vaccine_group,
  ncrf2_med.corticosteroid_group,
  ncrf2_lab.crp_level_numeric,
  ncrf2_lab.wbc_count_level_numeric,
  dem.concerned_memory_group,
  days_covid_baseline,
  daysquarters,
  time_window_covid19,
  NfL,
  GFAP,
  Tau,
  UCHL1,
  fsidps_volume_anteriorcingulatecortex_composite_Zscore,
  Composite_global,
  Composite_acc,
  Composite_rt
)

#check dimensions and names
dim(data_case_multipleReg)
names(data_case_multipleReg)

# Visualize missingness in the dataset
visdat::vis_miss(data_case_multipleReg)

####Model 1 variables - to visualise missingness
data_case_model1_includedvariables<-data_case_multipleReg[,c("site_group",
                                                      "dem.dob_age",
                                                      "WHO_Covid_severity_group",
                                                      "diagnostic_group",
                                                     "education_category",
                                                      "clinical_frailty_scale_group",
                                                      "phq9.sum_score",
                                                     "Total_Multimorbidity",
                                                     "days_covid_baseline",
                                                     "time_window_covid19",
                                                     "ncrf2_med.corticosteroid_group"
                                                    )]
visdat::vis_miss(data_case_model1_includedvariables)
dim(data_case_model1_includedvariables)

####Model 3 variables - to visualise missingness
data_case_model3_includedvariables<-data_case_multipleReg[,c("NfL",
                                                             "GFAP",
                                                             "fsidps_volume_anteriorcingulatecortex_composite_Zscore",
                                                             "site_group",
                                                             "dem.dob_age",
                                                             "diagnostic_group",
                                                             "phq9.sum_score",
                                                             "Total_Multimorbidity",
                                                             "days_covid_baseline",
                                                             "time_window_covid19"
                                                             
)]
dim(data_case_model3_includedvariables)
visdat::vis_miss(data_case_model3_includedvariables)

#COVID modelling dataset ####
attach(data_positiveControls)
data_control_multipleReg <-data.frame(
  ID,
  dem.dob_age,
  primary_site_name,
  site_group,
  dem.sex_at_birth,
  WHO_Covid_severity_group,
  diagnostic_group,
  education_category,
  clinical_frailty_scale_group,
  phq9.sum_score,
  gad7.sum_score,
  pcl5.sum_score,
  cfs.mental_subscale,
  cfs.physical_subscale,
  Total_ACB_Score,
  Total_Multimorbidity,
  ncrf1_pre_med.months_presentation.had_covid19_vaccine_group,
  ncrf1_comorbid.clinical_frailty_scale_numeric,
  ncrf2_med.corticosteroid_group,
  ncrf2_lab.crp_level_numeric,
  ncrf2_lab.wbc_count_level_numeric,
  dem.concerned_memory_group,
  days_covid_baseline,
  daysquarters,
  time_window_covid19,
  NfL,
  GFAP,
  Tau,
  UCHL1,
  fsidps_volume_anteriorcingulatecortex_composite_Zscore,
  Composite_global,
  Composite_acc,
  Composite_rt
)

#check names and dimensions
dim(data_control_multipleReg)
names(data_control_multipleReg)

####Model 2: Included variables
data_control_model2_includedvariables<-data_control_multipleReg[,c("site_group",
                                                                "dem.dob_age",
                                                                "WHO_Covid_severity_group",
                                                                "education_category",
                                                                "clinical_frailty_scale_group",
                                                                "phq9.sum_score",
                                                                "Total_Multimorbidity",
                                                                "days_covid_baseline",
                                                                "time_window_covid19",
                                                                "ncrf2_med.corticosteroid_group"
)]
dim(data_control_model2_includedvariables)
visdat::vis_miss(data_control_model2_includedvariables)

####Model 4 data missingness
data_control_model4_includedvariables<-data_control_multipleReg[,c("NfL",
                                                             "GFAP",
                                                             "fsidps_volume_anteriorcingulatecortex_composite_Zscore",
                                                             "site_group",
                                                             "dem.dob_age",
                                                             "WHO_Covid_severity_group",
                                                             "phq9.sum_score",
                                                             "Total_Multimorbidity",
                                                             "days_covid_baseline",
                                                             "time_window_covid19"
                                                             
)]
dim(data_control_model4_includedvariables)
visdat::vis_miss(data_control_model4_includedvariables)


#Table 2 NeuroCOVID univariate ####
summary(as.numeric(dem.dob_age))
fit1 = lm(Composite_global ~ as.numeric(dem.dob_age), data = data_case_multipleReg)
summary(fit1)

summary(as.factor(WHO_Covid_severity_group))
fit1 <- lm(Composite_global ~ relevel(as.factor(WHO_Covid_severity_group), ref = "Ambulatory mild disease"), data = data_case_multipleReg)
summary(fit1)

fit1 <- lm(Composite_global ~ relevel(as.factor(diagnostic_group), ref="Peripheral"), data = data_case_multipleReg)
summary(fit1) 

fit1 <- lm(Composite_global ~ relevel(as.factor(education_category), ref="None of the above"), data = data_case_multipleReg)
summary(fit1) 

summary(as.factor(clinical_frailty_scale_group))
fit1 <- lm(Composite_global ~ as.factor(clinical_frailty_scale_group), data = data_case_multipleReg)
summary(fit1) 

fit1 <- lm(Composite_global ~ as.numeric(phq9.sum_score), data = data_case_multipleReg)
summary(fit1) 

fit1 <- lm(Composite_global ~ as.numeric(gad7.sum_score), data = data_case_multipleReg)
summary(fit1) 

summary(as.numeric(pcl5.sum_score))
fit1 <- lm(Composite_global ~ as.numeric(pcl5.sum_score), data = data_case_multipleReg)
summary(fit1)

fit1 <- lm(Composite_global ~ as.numeric(cfs.mental_subscale), data = data_case_multipleReg)
summary(fit1) 

fit1 <- lm(Composite_global ~ as.numeric(cfs.physical_subscale), data = data_case_multipleReg)
summary(fit1)

fit1 <- lm(Composite_global ~ as.numeric(Total_ACB_Score), data = data_case_multipleReg)
summary(fit1) 

fit1 <- lm(Composite_global ~ as.numeric(Total_Multimorbidity), data = data_case_multipleReg)
summary(fit1) 

summary(as.factor(ncrf1_pre_med.months_presentation.had_covid19_vaccine))
fit1 <- lm(Composite_global ~ as.factor(ncrf1_pre_med.months_presentation.had_covid19_vaccine_group), data = data_case_multipleReg)
summary(fit1) 

summary(as.factor(ncrf2_med.corticosteroid))
fit1 <- lm(Composite_global ~ as.factor(ncrf2_med.corticosteroid_group), data = data_case_multipleReg)
summary(fit1) 

fit1 <- lm(Composite_global ~ as.factor(dem.concerned_memory_group), data = data_case_multipleReg)
summary(fit1) 

fit1 <- lm(Composite_global ~ relevel(as.factor(time_window_covid19), ref=5), data = data_case_multipleReg)
summary(fit1) 

fit1 <- lm(Composite_global ~ relevel(as.factor(time_window_covid19), ref=5)+ as.numeric(days_covid_baseline), data = data_case_multipleReg)
summary(fit1)

# time since covid-19
fit1 <- lm(Composite_global ~  as.numeric(days_covid_baseline), data = data_case_multipleReg)
summary(fit1)

# brain injury biomarkers
fit1 <- lm(Composite_global ~  as.numeric(NfL), data = data_case_multipleReg)
summary(fit1) 

fit1 <- lm(Composite_global ~  as.numeric(GFAP), data = data_case_multipleReg)
summary(fit1) 

fit1 <- lm(Composite_global ~  as.numeric(Tau), data = data_case_multipleReg)
summary(fit1)  

fit1 <- lm(Composite_global ~  as.numeric(UCHL1), data = data_case_multipleReg)
summary(fit1) 

#Table 2 COVID univariate ####
summary(as.numeric(dem.dob_age))
fit1 = lm(Composite_global ~ as.numeric(dem.dob_age), data = data_control_multipleReg)
summary(fit1) 

summary(as.factor(dem.sex_at_birth))
fit1 = lm(Composite_global ~ as.factor(dem.sex_at_birth), data = data_control_multipleReg)
summary(fit1)

summary(as.factor(WHO_Covid_severity_group))
fit1 = lm(Composite_global ~ relevel(as.factor(WHO_Covid_severity_group), ref = "Ambulatory mild disease"), data = data_control_multipleReg) # Exclude due to missing data
summary(fit1)

fit1 <- lm(Composite_global ~ relevel(as.factor(education_category), ref="None of the above"), data = data_control_multipleReg)
summary(fit1) 

summary(as.factor(clinical_frailty_scale_group))
fit1 = lm(Composite_global ~ as.factor(clinical_frailty_scale_group), data = data_control_multipleReg) # Exclude due to missing data
summary(fit1)

fit1 <- lm(Composite_global ~ relevel(as.factor(time_window_covid19), ref=5), data = data_control_multipleReg)
summary(fit1) 

# time since covid-19
fit1 <- lm(Composite_global ~  as.numeric(days_covid_baseline), data = data_control_multipleReg)
summary(fit1)

#with interaction term between window and time since covid-19
fit1 <- lm(Composite_global ~ relevel(as.factor(time_window_covid19), ref = 4) + as.numeric(days_covid_baseline) + 
             relevel(as.factor(time_window_covid19), ref = 4):as.numeric(days_covid_baseline), data = data_control_multipleReg)

summary(fit1)

fit1 = lm(Composite_global ~ as.numeric(phq9.sum_score), data = data_control_multipleReg)
summary(fit1)

fit1 = lm(Composite_global ~ as.numeric(gad7.sum_score), data = data_control_multipleReg)
summary(fit1)

summary(as.numeric(pcl5.sum_score))
fit1 = lm(Composite_global ~ as.numeric(pcl5.sum_score), data = data_control_multipleReg) # Exclude due to missing data
summary(fit1)

fit1 = lm(Composite_global ~ as.numeric(cfs.mental_subscale), data = data_control_multipleReg)
summary(fit1)

fit1 = lm(Composite_global ~ as.numeric(cfs.physical_subscale), data = data_control_multipleReg)
summary(fit1)

fit1 <- lm(Composite_global ~ as.numeric(Total_ACB_Score), data = data_control_multipleReg)
summary(fit1) 

fit1 <- lm(Composite_global ~ as.numeric(Total_Multimorbidity), data = data_control_multipleReg)
summary(fit1) 

summary(as.factor(ncrf1_pre_med.months_presentation.had_covid19_vaccine))
fit1 = lm(Composite_global ~ as.factor(ncrf1_pre_med.months_presentation.had_covid19_vaccine_group), data = data_control_multipleReg) # Exclude due to missing data
summary(fit1)

summary(as.factor(ncrf2_med.corticosteroid_group))
fit1 = lm(Composite_global ~ as.factor(ncrf2_med.corticosteroid_group), data = data_control_multipleReg) # Exclude due to missing data
summary(fit1)

fit1 = lm(Composite_global ~ as.factor(ncrf2_med.corticosteroid_group)+as.factor(WHO_Covid_severity_group), data = data_control_multipleReg) # Exclude due to missing data
summary(fit1) 

fit1 = lm(Composite_global ~ as.factor(dem.concerned_memory_group), data = data_control_multipleReg)
summary(fit1)

# brain injury biomarkers
fit1 <- lm(Composite_global ~  as.numeric(NfL), data = data_control_multipleReg)
summary(fit1) 

# brain injury biomarkers
fit1 <- lm(Composite_global ~  as.numeric(GFAP), data = data_control_multipleReg)
summary(fit1) 

# brain injury biomarkers
fit1 <- lm(Composite_global ~  as.numeric(Tau), data = data_control_multipleReg)
summary(fit1) 

# brain injury biomarkers
fit1 <- lm(Composite_global ~  as.numeric(UCHL1), data = data_control_multipleReg)
summary(fit1) 

#Table 2 Imaging linear regression ####
#NeuroCOVID
#MRI - original thickness/ volume
fit1_22=lm(Composite_global ~ fsidps_thickness_total_composite_Zscore, data = data_case)
summary(fit1_22) 
fit1_22=lm(Composite_global ~ fsidps_volume_total_composite_Zscore, data = data_case)
summary(fit1_22) 
fit1_22=lm(Composite_global ~ fsidps_volume_total_composite_Zscore+fsidps_thickness_total_composite_Zscore, data = data_case)
summary(fit1_22) 

#MRI - specific areas
fit1_22=lm(Composite_global ~ fsidps_volume_anteriorcingulatecortex_composite_Zscore, data = data_case)
summary(fit1_22)
fit1_22=lm(Composite_global ~ fsidps_volume_superiortemporalgyrus_composite_Zscore, data = data_case)
summary(fit1_22)
fit1_22=lm(Composite_global ~ fsidps_volume_insula_composite_Zscore, data = data_case)
summary(fit1_22)
fit1_22=lm(Composite_global ~ fsidps_thickness_superiortemporalgyrus_composite_Zscore, data = data_case)
summary(fit1_22)
fit1_22=lm(Composite_global ~ fsidps_thickness_orbitofrontalcortex_composite_Zscore, data = data_case)
summary(fit1_22)

#COVID
#MRI - original thickness/ volume
fit1_22=lm(Composite_global ~ fsidps_thickness_total_composite_Zscore, data = data_positiveControls)
summary(fit1_22) 
fit1_22=lm(Composite_global ~ fsidps_volume_total_composite_Zscore, data = data_positiveControls)
summary(fit1_22)
fit1_22=lm(Composite_global ~ fsidps_volume_total_composite_Zscore+fsidps_thickness_total_composite_Zscore, data = data_positiveControls)
summary(fit1_22) 

#specific areas
fit1_22=lm(Composite_global ~ fsidps_volume_anteriorcingulatecortex_composite_Zscore, data = data_positiveControls)
summary(fit1_22)
fit1_22=lm(Composite_global ~ fsidps_volume_superiortemporalgyrus_composite_Zscore, data = data_positiveControls)
summary(fit1_22)
fit1_22=lm(Composite_global ~ fsidps_volume_insula_composite_Zscore, data = data_positiveControls)
summary(fit1_22)
fit1_22=lm(Composite_global ~ fsidps_thickness_superiortemporalgyrus_composite_Zscore, data = data_positiveControls)
summary(fit1_22)
fit1_22=lm(Composite_global ~ fsidps_thickness_orbitofrontalcortex_composite_Zscore, data = data_positiveControls)
summary(fit1_22)

# Table 2 - Models ####
#Model 1: NeuroCOVID clinical model
fit1_1 <- lm(Composite_global ~ 
               #first model foundation
               as.factor(site_group)+
               relevel(as.factor(time_window_covid19), ref = 4)+
               as.numeric(days_covid_baseline)+
               as.numeric(days_covid_baseline):as.factor(time_window_covid19)+ #this intis interaction term indicates recovery may not be linear and may differ by wave
               #diagnostic group
               relevel(as.factor(diagnostic_group), ref = "Peripheral") +
               #then variables from hypothesis 3
               as.numeric(dem.dob_age)+
               relevel(as.factor(WHO_Covid_severity_group), ref="Ambulatory mild disease")+
               as.numeric(Total_Multimorbidity)+
               as.numeric(phq9.sum_score) +
               relevel (as.factor(clinical_frailty_scale_group),ref ="Managing well")+
               # and hypothesis 4
               relevel(as.factor(education_category), ref="None of the above")+
               as.factor(ncrf2_med.corticosteroid_group),
             data = data_case)
summary(fit1_1)

#Model 2: COVID clinical model
#with site
fit1_2 <- lm(Composite_global ~ 
               #first model foundation
               as.factor(site_group)+
               relevel(as.factor(time_window_covid19), ref = 4)+
               as.numeric(days_covid_baseline)+
               as.numeric(days_covid_baseline):as.factor(time_window_covid19)+ #this interaction term indicates recovery may not be linear and may differ by wave
               #then variables from hypothesis 3
               as.numeric(dem.dob_age)+
               relevel(as.factor(WHO_Covid_severity_group), ref="Ambulatory mild disease")+
               as.numeric(Total_Multimorbidity)+
               as.numeric(phq9.sum_score) +
               relevel (as.factor(clinical_frailty_scale_group),ref ="Managing well")+
               # and hypothesis 4
               relevel(as.factor(education_category), ref="None of the above")+
               as.factor(ncrf2_med.corticosteroid_group),
             data = data_positiveControls)
summary(fit1_2)

#Model 3: NeuroCOVID multimodal model
fit1_3 <- lm(Composite_global ~ 
               #first model foundation
               as.factor(site_group)+
               relevel(as.factor(time_window_covid19), ref = 4)+
               as.numeric(days_covid_baseline)+
               as.numeric(days_covid_baseline):as.factor(time_window_covid19)+ #this interaction term indicates recovery may not be linear and may differ by wave
               #diagnostic group
               relevel(as.factor(diagnostic_group), ref = "Peripheral") +
               #then core clinical variables
               as.numeric(Total_Multimorbidity)+
               as.numeric(phq9.sum_score) +
               #BIB
               as.numeric(GFAP)+
               as.numeric(NfL)+
               #neuroimaging - ACC
               as.numeric(fsidps_volume_anteriorcingulatecortex_composite_Zscore),
             data = data_case)
summary(fit1_3)

###add age
fit1_3_age <- lm(Composite_global ~ 
                   #first model foundation
                   as.numeric(dem.dob_age)+
                   as.factor(site_group)+
                   relevel(as.factor(time_window_covid19), ref = 4)+
                   as.numeric(days_covid_baseline)+
                   as.numeric(days_covid_baseline):as.factor(time_window_covid19)+ #this interaction term indicates recovery may not be linear and may differ by wave
                   #diagnostic group
                   relevel(as.factor(diagnostic_group), ref = "Peripheral") +
                   #then core clinical variables
                   as.numeric(Total_Multimorbidity)+
                   as.numeric(phq9.sum_score) +
                   #BIB
                   as.numeric(GFAP)+
                   as.numeric(NfL)+
                   #neuroimaging - ACC
                   as.numeric(fsidps_volume_anteriorcingulatecortex_composite_Zscore),
                 data = data_case)
summary(fit1_3_age)

#Model 4 COVID multimodal model
fit1_4 <- lm(Composite_global ~ 
               #first model foundation
               as.factor(site_group)+
               relevel(as.factor(time_window_covid19), ref = 4)+
               as.numeric(days_covid_baseline)+
               as.numeric(days_covid_baseline):as.factor(time_window_covid19)+ #this interaction term indicates recovery may not be linear and may differ by wave
               #then clinical group
               as.factor(WHO_Covid_severity_group)+
               #then core clinical variables
               as.numeric(Total_Multimorbidity)+
               as.numeric(phq9.sum_score) +
               #BIB
               as.numeric(GFAP)+
               as.numeric(NfL)+
               #neuroimaging - ACC
               as.numeric(fsidps_volume_anteriorcingulatecortex_composite_Zscore),
             data = data_positiveControls)
summary(fit1_4)

fit1_4_age <- lm(Composite_global ~ 
                   #first model foundation
                   as.factor(site_group)+
                   relevel(as.factor(time_window_covid19), ref = 4)+
                   as.numeric(days_covid_baseline)+
                   as.numeric(days_covid_baseline):as.factor(time_window_covid19)+ #this interaction term indicates recovery may not be linear and may differ by wave
                   #then clinical group
                   as.factor(WHO_Covid_severity_group)+
                   #then core clinical variables
                   as.numeric(dem.dob_age)+
                   as.numeric(Total_Multimorbidity)+
                   as.numeric(phq9.sum_score) +
                   #BIB
                   as.numeric(GFAP)+
                   as.numeric(NfL)+
                   #neuroimaging - ACC
                   as.numeric(fsidps_volume_anteriorcingulatecortex_composite_Zscore),
                 data = data_positiveControls)
summary(fit1_4_age)

#Secondary analysis n=66 matched for time since COVID-19 ####
#Supplementary Table 6 ####
#Univariate associations, clinical linear regression model and multimodal linear regression models for Global DfE (GDfE) Score in COVID group (n=161) and subsampled COVID group (n=66, matched to NeuroCOVID group for days since COVID-19). 
set.seed(42)  # Set seed for reproducibility

# Randomly select patients from each group to give same proportion in each group as in NeuroCOVID group
table(data_control_multipleReg$daysquarters)
selected_patients_group1 <- sample(which(data_control_multipleReg$daysquarters == 1), 15)
selected_patients_group2 <- which(data_control_multipleReg$daysquarters == 2)  # Include all patients from group 2
selected_patients_group3 <- which(data_control_multipleReg$daysquarters == 3)  # Include all patients from group 3
selected_patients_group4 <- sample(which(data_control_multipleReg$daysquarters == 4), 12)

# Combine selected patients from all groups
selected_patients <- c(selected_patients_group1, selected_patients_group2, selected_patients_group3, selected_patients_group4)

# Create a new data frame with selected patients
selected_data_control_multipleReg <- data_control_multipleReg[selected_patients, ]
#check dimensions
dim(selected_data_control_multipleReg)

#summarise days since COVID-19
summary(selected_data_control_multipleReg$days_covid_baseline)
summary(data_case$days_covid_baseline)

#correlation between days since COVID-19 and GDfE
cor(selected_data_control_multipleReg$days_covid_baseline,selected_data_control_multipleReg$Composite_global)

#Univariate effect sizes to create comparator for Table 2
cor(selected_data_control_multipleReg$Composite_global, selected_data_control_multipleReg$dem.dob_age, method = "spearman")
wilcox_effsize(selected_data_control_multipleReg, Composite_global~education_category)
wilcox_effsize(selected_data_control_multipleReg, Composite_global~clinical_frailty_scale_group)
wilcox_effsize(selected_data_control_multipleReg, Composite_global~time_window_covid19)
cor(selected_data_control_multipleReg$Composite_global, selected_data_control_multipleReg$days_covid_baseline, use="complete.obs", method = "spearman")
wilcox_effsize(selected_data_control_multipleReg, Composite_global~WHO_Covid_severity_group)
cor(selected_data_control_multipleReg$Composite_global, selected_data_control_multipleReg$phq9.sum_score, use="complete.obs", method = "spearman")
cor(selected_data_control_multipleReg$Composite_global, selected_data_control_multipleReg$Total_Multimorbidity, use="complete.obs", method = "spearman")
wilcox_effsize(selected_data_control_multipleReg, Composite_global~ncrf2_med.corticosteroid_group)
wilcox_effsize(selected_data_control_multipleReg, Composite_global~dem.concerned_memory_group)
cor(selected_data_control_multipleReg$Composite_global, selected_data_control_multipleReg$NfL, use="complete.obs", method = "spearman")
cor(selected_data_control_multipleReg$Composite_global, selected_data_control_multipleReg$GFAP, use="complete.obs", method = "spearman")
cor(selected_data_control_multipleReg$Composite_global, selected_data_control_multipleReg$fsidps_volume_anteriorcingulatecortex_composite_Zscore, use="complete.obs", method = "pearson")

#Univariate coefficients for GDfE in subgroup
# Fit a linear model for Composite_global by age
fit1 = lm(Composite_global ~ as.numeric(dem.dob_age), data = selected_data_control_multipleReg)
summary(fit1)

# Summary of WHO Covid severity group (as a factor)
summary(as.factor(WHO_Covid_severity_group))

# Fit a linear model for Composite_global by WHO Covid severity group
fit1 = lm(Composite_global ~ relevel(as.factor(WHO_Covid_severity_group), ref = "Ambulatory mild disease"), data = selected_data_control_multipleReg) 
summary(fit1)

# Fit a linear model for Composite_global by education category
fit1 = lm(Composite_global ~ relevel(as.factor(education_category), ref = "None of the above"), data = selected_data_control_multipleReg)
summary(fit1)

# Summary of clinical frailty scale group (as a factor)
summary(as.factor(clinical_frailty_scale_group))

# Fit a linear model for Composite_global by clinical frailty scale group
fit1 = lm(Composite_global ~ as.factor(clinical_frailty_scale_group), data = selected_data_control_multipleReg)
summary(fit1)

# Fit a linear model for Composite_global by time window since COVID-19
fit1 = lm(Composite_global ~ relevel(as.factor(time_window_covid19), ref = 4), data = selected_data_control_multipleReg)
summary(fit1)

# Fit a linear model for Composite_global by days since COVID-19 baseline
fit1 = lm(Composite_global ~ as.numeric(days_covid_baseline), data = selected_data_control_multipleReg)
summary(fit1)

# Fit a linear model for Composite_global by PHQ-9 sum score
fit1 = lm(Composite_global ~ as.numeric(phq9.sum_score), data = selected_data_control_multipleReg)
summary(fit1)

# Fit a linear model for Composite_global by total multimorbidity
fit1 = lm(Composite_global ~ as.numeric(Total_Multimorbidity), data = selected_data_control_multipleReg)
summary(fit1)

# Summary of corticosteroid group (as a factor)
summary(as.factor(ncrf2_med.corticosteroid_group))

# Summary of corticosteroid usage (as a factor)
summary(as.factor(ncrf2_med.corticosteroid))

# Fit a linear model for Composite_global by corticosteroid group
fit1 = lm(Composite_global ~ as.factor(ncrf2_med.corticosteroid_group), data = selected_data_control_multipleReg)
summary(fit1)

fit1 = lm(Composite_global ~ as.numeric(NfL), data = selected_data_control_multipleReg)
summary(fit1)

# Fit a linear model for Composite_global by corticosteroid group
fit1 = lm(Composite_global ~ as.numeric(GFAP), data = selected_data_control_multipleReg)
summary(fit1)

fit1 = lm(Composite_global ~ as.numeric(fsidps_volume_anteriorcingulatecortex_composite_Zscore), data = selected_data_control_multipleReg)
summary(fit1)

# Recovery trajectories ####
#### Create supplementary tables of follow-up demographics and GDfE scores
#summarise time from post-acute assessment to follow-up 1 for NeuroCOVID and COVID
summary(as.numeric(data_case$days_f1_baseline))
summary(as.numeric(data_positiveControls$days_f1_baseline))

#create summary tables on demographics of these patients
FU1<-data_case_positiveControls[(!is.na(data_case_positiveControls$Composite_global_f1)),]
FU2<-data_case_positiveControls[(!is.na(data_case_positiveControls$Composite_global_f2)),]

FUtablevar<-FU1 %>% select (dem.dob_age,
                            case_control_vaccine.factor,
                            dem.sex_at_birth,
                            language,
                            dem.highest_education,
                            clinical_frailty_scale_group,
                            WHO_Covid_severity_group,
                            time_window_covid19.factor,
                            days_f1_baseline,
                            Composite_global,
                            Composite_acc,
                            Composite_rt,
                            Composite_global_f1,
                            Composite_acc_f1,
                            Composite_rt_f1)
#assign names for table
colnames(FUtablevar)<-c("Age","Group","Sex","First language","Level of education","Clinical Frailty Scale",
                      "WHO COVID-19 Severity","Admission date",
                      "Days since PAA",
                      "PAA Cognitron Global Score","PAA Cognitron Accuracy","PAA Cognitron RT",
                      "FU1 Cognitron Global Score","FU1 Cognitron Accuracy","FU1 Cognitron RT")

#create table grouped by NeuroCOVID and COVID
####Supplementary Table 4 ####
FUtablevar %>%
  tbl_summary(by='Group') %>%
  add_overall %>%
  add_p() %>%
  bold_labels()

#Follow up 2
FU2tablevar<-FU2 %>% select (dem.dob_age,
                            case_control_vaccine.factor,
                            dem.sex_at_birth,
                            language,
                            dem.highest_education,
                            clinical_frailty_scale_group,
                            WHO_Covid_severity_group,
                            time_window_covid19.factor,
                            days_f2_baseline,
                            Composite_global,
                            Composite_acc,
                            Composite_rt,
                            Composite_global_f2,
                            Composite_acc_f2,
                            Composite_rt_f2)
#assign names for table
colnames(FU2tablevar)<-c("Age","Group","Sex","First language","Level of education","Clinical Frailty Scale",
                        "WHO COVID-19 Severity","Admission date",
                        "Days since PAA",
                        "PAA Cognitron Global Score","PAA Cognitron Accuracy","PAA Cognitron RT",
                        "FU2 Cognitron Global Score","FU2 Cognitron Accuracy","FU2 Cognitron RT")

#create table grouped by NeuroCOVID and COVID
####Supplementary Table 5 ####
FU2tablevar %>%
  tbl_summary(by='Group') %>%
  add_overall %>%
  add_p %>%
  bold_labels()

# Recovery modelling for controls (COVID) ####
attach(data_positiveControls)
data_control_recovery1 <-data.frame(
  primary_site_name,
  ID,
  dem.dob_age,
  dem.sex_at_birth,
  WHO_Covid_severity_group,
  education_category,
  phq9.sum_score,
  Total_Multimorbidity,
  ncrf1_pre_med.months_presentation.had_covid19_vaccine_group,
  ncrf2_med.corticosteroid_group,
  dem.concerned_memory_group,
  days_covid_baseline,
  time_window_covid19,
  NfL,
  GFAP,
  Tau,
  UCHL1,
  fsidps_volume_total_composite_Zscore,
  fsidps_thickness_total_composite_Zscore,
  Composite_global,
  days_f1_baseline,
  Composite_global_f1
)
#check dim
dim(data_control_recovery1)
# check missingness
visdat::vis_miss(data_control_recovery1)
#check correlation
data_control_recovery_num<-data_control_recovery1[,c("dem.dob_age","phq9.sum_score","NfL","GFAP","Tau","UCHL1")]
cor_rec_cont<-cor(data_control_recovery_num, use = "complete.obs")
print(cor_rec_cont) #all grand
#create outcome variable
data_control_recovery1$recovery<-Composite_global_f1-Composite_global #i.e. positive means more recovery
dim(data_control_recovery1)
summary(data_control_recovery1$recovery)
#get rid of those without FU1
data_control_recovery<-subset(data_control_recovery1, (!is.na(data_control_recovery1[,"recovery"])))
dim(data_control_recovery) 
## who completes follow up - demographics of recovery cohort
#post-acute DfE score
summary(data_control_recovery$Composite_global)
#Age
summary(data_control_recovery$dem.dob_age) 
#Sex
table(data_control_recovery$dem.sex_at_birth) 
#COVID severity
table(data_control_recovery$WHO_Covid_severity_group) 
#Education
table(data_control_recovery$education_category)
#PHQ9
summary(data_control_recovery$phq9.sum_score)
#pre-COVID vaccination
table(data_control_recovery$ncrf1_pre_med.months_presentation.had_covid19_vaccine_group) 
#steroid treatment
table(data_control_recovery$ncrf2_med.corticosteroid_group) 
#memory concerns
table(data_control_recovery$dem.concerned_memory_group) 
#time window of illness
table(data_control_recovery$time_window_covid19) 

#Univariate associations with recovery controls ####
#Extended data Table 2 ####
#age
fit1 = lm(recovery ~ as.numeric(dem.dob_age), data = data_control_recovery) 
summary(fit1) 
#COVID severity
fit1 = lm(recovery ~ relevel(as.factor(WHO_Covid_severity_group),ref="Ambulatory mild disease"), data = data_control_recovery)  
summary(fit1)
#Education
fit1 = lm(recovery ~ relevel(as.factor(education_category), ref ="None of the above"), data = data_control_recovery)  
summary(fit1) 
#PHQ9
fit1 = lm(recovery ~ as.numeric(phq9.sum_score), data = data_control_recovery)  
summary(fit1) 
#steroids
fit1 = lm(recovery ~ as.factor(ncrf2_med.corticosteroid_group), data = data_control_recovery)  
summary(fit1) 
#multimorbidity
fit1 = lm(recovery ~ as.numeric(Total_Multimorbidity), data = data_control_recovery)  
summary(fit1) 
#COVID vaccination
fit1 = lm(recovery ~ as.factor(ncrf1_pre_med.months_presentation.had_covid19_vaccine_group), data = data_control_recovery)
summary(fit1)
#Date of admission (time window)
fit1 = lm(recovery ~ relevel(as.factor(time_window_covid19), ref="4"), data = data_control_recovery)  
summary(fit1)
#window and time since COVID-19
fit1 = lm(recovery ~ relevel(as.factor(time_window_covid19), ref="4")+as.numeric(days_covid_baseline):as.factor(time_window_covid19)+as.numeric(days_covid_baseline), data = data_control_recovery)  
summary(fit1) 
#all for BIB together

# each individual brain injury marker
fit1 = lm(recovery ~ as.numeric(NfL), data = data_control_recovery)  
summary(fit1)
fit1 = lm(recovery ~ as.numeric(GFAP), data = data_control_recovery)  
summary(fit1)
fit1 = lm(recovery ~ as.numeric(Tau), data = data_control_recovery)  
summary(fit1)
fit1 = lm(recovery ~ as.numeric(UCHL1), data = data_control_recovery)  
summary(fit1)

#Neuroimaging#
fit1 = lm(recovery ~ as.numeric(fsidps_volume_total_composite_Zscore), data = data_control_recovery)  
summary(fit1)
fit1 = lm(recovery ~ as.numeric(fsidps_thickness_total_composite_Zscore), data = data_control_recovery)  
summary(fit1)

#Recovery modelling for cases (NeuroCOVID) ####
# Modeling dataset for cases
attach(data_case)
data_case_recovery1 <- data.frame(
  primary_site_name,
  ID,
  dem.dob_age,
  dem.sex_at_birth,
  WHO_Covid_severity_group,
  diagnostic_group,
  education_category,
  phq9.sum_score,
  Total_Multimorbidity,
  Total_ACB_Score,
  cfs.physical_subscale,
  dem.concerned_memory_group,
  days_covid_baseline,
  time_window_covid19,
  NfL,
  GFAP,
  Tau,
  UCHL1,
  fsidps_volume_total_composite_Zscore,
  fsidps_thickness_total_composite_Zscore,
  Composite_global,
  days_f1_baseline,
  Composite_global_f1
)
# Check dim
dim(data_case_recovery1)
# Check missingness
visdat::vis_miss(data_case_recovery1)
# Create outcome variable
data_case_recovery1$recovery <- Composite_global_f1 - Composite_global # i.e. positive means more recovery
summary(data_case_recovery1$recovery)
dim(data_case_recovery1)
summary(data_case_recovery1$recovery)
# Get rid of those without FU1
data_case_recovery <- subset(data_case_recovery1, (!is.na(data_case_recovery1[,"recovery"])))
dim(data_case_recovery) #51 as expected

# Summary Statistics for data_case_recovery
# Composite Global
summary(data_case_recovery$Composite_global)

# Age
summary(data_case_recovery$dem.dob_age) 

# Sex
table(data_case_recovery$dem.sex_at_birth) 

# COVID Severity
table(data_case_recovery$WHO_Covid_severity_group) 

# Education
table(data_case_recovery$education_category)

# PHQ9
summary(data_case_recovery$phq9.sum_score)

# Pre-COVID Vaccination
table(data_case_recovery$ncrf1_pre_med.months_presentation.had_covid19_vaccine_group) 

# Steroid Treatment
table(data_case_recovery$ncrf2_med.corticosteroid_group) 

# Memory Concerns
table(data_case_recovery$dem.concerned_memory_group) 

# Time Window of Illness
table(data_case_recovery$time_window_covid19) 

# Diagnostic group
table(data_case_recovery$diagnostic_group) 

# Univariate Associations with Recovery Controls ####

# Age
fit1 <- lm(recovery ~ as.numeric(dem.dob_age), data = data_case_recovery) 
summary(fit1) 

# COVID Severity
fit1 <- lm(recovery ~ relevel(as.factor(WHO_Covid_severity_group), ref = "Ambulatory mild disease"), data = data_case_recovery)  
summary(fit1)

# Education
fit1 <- lm(recovery ~ relevel(as.factor(education_category), ref = "None of the above"), data = data_case_recovery)  
summary(fit1) 

# PHQ9
fit1 <- lm(recovery ~ as.numeric(phq9.sum_score), data = data_case_recovery)  
summary(fit1) 

# Steroids
fit1 <- lm(recovery ~ as.factor(ncrf2_med.corticosteroid_group), data = data_case_recovery)  
summary(fit1) 

# Multimorbidity
fit1 <- lm(recovery ~ as.numeric(Total_Multimorbidity), data = data_case_recovery)  
summary(fit1) 

# COVID Vaccination
fit1 <- lm(recovery ~ as.factor(ncrf1_pre_med.months_presentation.had_covid19_vaccine_group), data = data_case_recovery)
summary(fit1)

# Date of Admission (Time Window)
fit1 <- lm(recovery ~ relevel(as.factor(time_window_covid19), ref = "4"), data = data_case_recovery)  
summary(fit1)

# Window and Time Since COVID-19
fit1 <- lm(recovery ~ relevel(as.factor(time_window_covid19), ref = "4") + as.numeric(days_covid_baseline):as.factor(time_window_covid19) + as.numeric(days_covid_baseline), data = data_case_recovery)  
summary(fit1) 

# Diagnostic group
fit1 <- lm(recovery ~ relevel(as.factor(diagnostic_group), ref = "Peripheral"), data = data_case_recovery)  
summary(fit1) 

# Each Individual Brain Injury Marker
fit1 <- lm(recovery ~ as.numeric(NfL), data = data_case_recovery)  
summary(fit1)

fit1 <- lm(recovery ~ as.numeric(GFAP), data = data_case_recovery)  
summary(fit1)

fit1 <- lm(recovery ~ as.numeric(Tau), data = data_case_recovery)  
summary(fit1)

fit1 <- lm(recovery ~ as.numeric(UCHL1), data = data_case_recovery)  
summary(fit1)

# Neuroimaging #
fit1 <- lm(recovery ~ as.numeric(fsidps_volume_total_composite_Zscore), data = data_case_recovery)  
summary(fit1)

fit1 <- lm(recovery ~ as.numeric(fsidps_thickness_total_composite_Zscore), data = data_case_recovery)  
summary(fit1)

# Extended Data Table 2 - Recovery models ####
#Model 5: Recovery NeuroCOVID
fit1_5 <- lm(recovery ~ 
               as.numeric(days_covid_baseline)+
               #diagnostic group
               relevel(as.factor(diagnostic_group), ref = "Peripheral") +
               #then core clinical variables
               as.numeric(dem.dob_age)+
               as.numeric(Total_Multimorbidity)+
               as.numeric(phq9.sum_score) +
               #BIB
               as.numeric(GFAP)+
               as.numeric(NfL),
             data = data_case_recovery)
summary(fit1_5)

#Model 6: Recovery COVID
fit1_6 <- lm(recovery ~ 
               as.numeric(days_covid_baseline)+
               #clinical group
               as.factor(WHO_Covid_severity_group)+
               #then core clinical variables
               as.numeric(dem.dob_age)+
               as.numeric(Total_Multimorbidity)+
               as.numeric(phq9.sum_score) +
               #BIB
               as.numeric(GFAP)+
               as.numeric(NfL),
             data = data_control_recovery)
summary(fit1_6)

######## End #############
