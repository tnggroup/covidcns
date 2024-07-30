## COVID-CNS
## Author Greta Wood 
## Script 1: Code for merging all data for COVID-CNS cognition analysis (excluding neuroimaging)
## All subsequent script will load rds output from this script 
## Numbers for Figure 1 derived from this script

#This script uses Dummy data

# Subsequent Scripts are:
## Table 2: Univariate script and multivariate script #authors Kukatharmini Tharmaratnam and Greta Wood 
# Figure 2A: Violin plot - author Kukatharmini Tharmaratnam, edits Greta Wood
## Figure 2B: Boxplots of effect size -author Kukatharmini Tharmaratnam, edits Greta Wood 
## Figure 2C: Recovery trajectories -author Greta Wood
## Fig 3A: Biomarkers -author Greta Wood
## Figure 4: Heatmap script - author Greta Wood using ComplexHeatmap Gu, Z. (2022) Complex Heatmap Visualization, iMeta. DOI: 10.1002/imt2.43

## Neuroimaging scripts - author Brendan Sargent

# load packages
rm(list = ls())
getwd()
library(tidyr)
library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(tidyverse)
library(reshape2)
library(plyr)
library(ggrepel)

# load individual datasets ####
Cogdata1<-readRDS('dummy_Cogdata1.rds') #questionnaires, cognitive and FU1/2
Biom<-readRDS("Cognitive_dummyBrainInjury.rds") #biomarkers 
ACB_mm<-readRDS("dummyACBmm.rds") # ACB and multimorbidity
Biocontrols<-readRDS("DummyBiocontrols.rds") #biomarker controls
cognitron_normative_tasks<-readRDS("dummy_cognitive_normative_tasks.rds") #cognitive normative task data
cognitron_normative_comps<-readRDS("dummy_cognitron_normative_comps.rds") #cognitive normative comps

# Script starts - first explore numbers with cognitive testing
Cogdatainc<- Cogdata1 %>% drop_na(Composite_global) #only include those with global score (outcome variable)
Cogdataincoverall<- Cogdata1 %>% drop_na(timeStamp) #only include those who started
CogdataincoFU1<- Cogdata1 %>% drop_na(timeStamp_f1) #only FU1 attempts 
CogdataincoFU1<- Cogdata1 %>% drop_na(Composite_global_f1) #only FU1 complete
CogdataincoFU2<- Cogdata1 %>% drop_na(timeStamp_f2) #only FU2 attempts 
CogdataincoFU2<- Cogdata1 %>% drop_na(Composite_global_f2) #with global score

table(Cogdata1$case_control_vaccine) #case/control (NeuroCOVID/COVID) breakdown in COVID CNS cohort #Fig 1 row 1
table(Cogdataincoverall$case_control_vaccine) #who attempted cognitron #Fig 1 row 2
table(Cogdatainc$case_control_vaccine) #who completed Cognitron with valid score #Fig 1 row 3 # (Global Deviation from Expected score) 

table(Cogdata1$moca_outp.did_the_participant_complete_a_moca) #MOCA total
table(Cogdataincoverall$moca_outp.did_the_participant_complete_a_moca) #MOCA + cognitron

#remove vaccine complications and COVID negative controls
CacoCogdata<- Cogdatainc[Cogdatainc$case_control_vaccine %in% c('Case: COVID-19 positive (i.e. neurological or psychiatric complication)', 'Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)'), ] 
table(CacoCogdata$diagnostic_group) #diagnostic groups
table(CacoCogdata$case_control_vaccine) #overall numbers

#diagnostic group updates in Cogdata 1 ####
#add new diagnoses and tidy diagnostic groups based upon Clinical Case Evaluation Panel and subsequent Consultant neurological review
CacoCogdata["74","diagnostic_group"]<-"Cerebrovascular"
CacoCogdata["74","diagnosis"]<-"Ischaemic Stroke"
CacoCogdata["218","diagnostic_group"]<-"Peripheral"
CacoCogdata["218","diagnosis"]<-"Myositis/Myopathy/Myalgia"
CacoCogdata["220","diagnostic_group"]<-"Encephalopathy/Delirium"
CacoCogdata["220","diagnosis"]<-"Encephalopathy/Delirium"
CacoCogdata["221","diagnostic_group"]<-"Neuropsychiatric"
CacoCogdata["221","diagnosis"]<-"Panic disorder"
CacoCogdata["222","diagnostic_group"]<-"Unknown"
CacoCogdata["222","diagnosis"]<-"Unknown"
CacoCogdata["225","diagnostic_group"]<-"Cerebrovascular"
CacoCogdata["225","diagnosis"]<-"Transient Ischaemic Attack"
CacoCogdata["273","diagnostic_group"]<-"Neuropsychiatric"
CacoCogdata["273","diagnosis"]<-"Cognitive Decline"
CacoCogdata["376","diagnostic_group"]<-"Other"
CacoCogdata["376","diagnosis"]<-"Fatigue"
Recodereview<-select(CacoCogdata, ID, case_control_vaccine, diagnostic_group, diagnosis) #check results - recoded as intended
table(CacoCogdata$case_control_vaccine) #none of above changes case status
#98 cns03011 should be control
CacoCogdata["98","case_control_vaccine"]<-"Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)"
#197 cns01119 should be case
CacoCogdata["197","case_control_vaccine"]<-"Case: COVID-19 positive (i.e. neurological or psychiatric complication)"
#the other patients with 'NA' for their specific diagnosis are controls
CacoCogdata$diagnostic_group <- CacoCogdata$diagnostic_group %>% replace_na("Control")
CacoCogdata$diagnosis <- CacoCogdata$diagnosis %>% replace_na("Control")

#Review patients who were 'cases'(NeuroCOVID) due to loss of smell or taste alone
anos<-CacoCogdata[CacoCogdata$diagnosis== "Anosmia/Ageusia",]
#recode ageusia/anosmia as control in diagnostic group and case/control
CacoCogdata$diagnostic_group[CacoCogdata$diagnosis== "Anosmia/Ageusia" ] <- "Control"
CacoCogdata$case_control_vaccine[CacoCogdata$diagnostic_group== "Control" ] <- "Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)"
CacoCogdata$diagnostic_group[CacoCogdata$case_control_vaccine== "Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)" ] <- "Control"
table(CacoCogdata$diagnosis, CacoCogdata$diagnostic_group) #checks
table(CacoCogdata$diagnostic_group) #check updated diagnostic groups
sum(!is.na(CacoCogdata$diagnostic_group)) #how many valid diagnostic groups
table(CacoCogdata$case_control_vaccine)
CacoCogdata<-CacoCogdata[CacoCogdata$diagnostic_group!="Unknown",] #remove unknown cases

table(CacoCogdata$case_control_vaccine)
table(CacoCogdata$diagnostic_group)

#PMH removals #### 
#exclude those with past medical history of brain tumour or multiple sclerosis 
CCCogDataInc<-CacoCogdata[(CacoCogdata$phh.brain_tumour=="Not Brain tumour"|is.na(CacoCogdata$phh.brain_tumour))&(CacoCogdata$phh.multiple_sclerosis=="Not Multiple sclerosis"|is.na(CacoCogdata$phh.multiple_sclerosis)),]
table(CCCogDataInc$case_control_vaccine)

#exclude specific ones (VP shunt and stem cell brain tumour)
CCCogDataInc2<-CCCogDataInc[CCCogDataInc$ID!="CNS05047"&CCCogDataInc$ID!="CNS07037",]
table(CCCogDataInc2$case_control_vaccine)

#exclude strokes as PMH (i.e. when this is a previous stroke and their complication is not a stroke)
Cogdata<-CCCogDataInc2[CCCogDataInc2$phh.stroke=="Not Stroke"|is.na(CCCogDataInc2$phh.stroke)|(CCCogDataInc2$phh.stroke=="Stroke"&CCCogDataInc2$diagnostic_group=="Cerebrovascular"),]
table(Cogdata$case_control_vaccine)

#exclude epilepsy as PMH (i.e. when history of epilepsy and their complication is not associated with seizures)
table(CCCogDataInc2$phh.epilepsy_or_convulsions)
EpilepsyRV<-Cogdata[,c('ID','case_control_vaccine','phh.epilepsy_or_convulsions','diagnostic_group','diagnosis')]
data_case_positiveControls<-Cogdata[Cogdata$phh.epilepsy_or_convulsions=="Not Epilepsy or convulsions"|is.na(Cogdata$phh.epilepsy_or_convulsions)|
                    Cogdata$phh.epilepsy_or_convulsions=="Epilepsy or convulsions"&Cogdata$diagnostic_group=="Cerebrovascular"|
                    Cogdata$phh.epilepsy_or_convulsions=="Epilepsy or convulsions"&Cogdata$diagnosis=="Seizures"|
                    Cogdata$phh.epilepsy_or_convulsions=="Epilepsy or convulsions"&Cogdata$diagnostic_group=="Inflammatory",]
table(data_case_positiveControls$case_control_vaccine)
dim(data_case_positiveControls)

#add time stamp data and format ####
# this code is specific to the observed data 
### timeStamp
data_case_positiveControls$timeStamp_split=strsplit(data_case_positiveControls$timeStamp,split=" ")
for (i in 1:nrow(data_case_positiveControls)){
data_case_positiveControls$timeStamp_merge[i]=paste(data_case_positiveControls$timeStamp_split[[i]][3],"/", data_case_positiveControls$timeStamp_split[[i]][2],"/",data_case_positiveControls$timeStamp_split[[i]][4], sep="")}
data_case_positiveControls$timeStamp_format=as.Date(strptime(data_case_positiveControls$timeStamp_merge,format='%d/%b/%Y'))
summary(data_case_positiveControls$timeStamp_format)
### timeStamp at follow up 1   
data_case_positiveControls$timeStamp_f1_split=strsplit(data_case_positiveControls$timeStamp_f1,split=" ")
for (i in 1:nrow(data_case_positiveControls)){
data_case_positiveControls$timeStamp_f1_merge[i]=paste(data_case_positiveControls$timeStamp_f1_split[[i]][3],"/", data_case_positiveControls$timeStamp_f1_split[[i]][2],"/",data_case_positiveControls$timeStamp_f1_split[[i]][4], sep="")}
data_case_positiveControls$timeStamp_f1_format=as.Date(strptime(data_case_positiveControls$timeStamp_f1_merge,format='%d/%b/%Y'))
summary(data_case_positiveControls$timeStamp_f1_format)
### timeStamp at follow up 2
data_case_positiveControls$timeStamp_f2_split=strsplit(data_case_positiveControls$timeStamp_f2,split=" ")
for (i in 1:nrow(data_case_positiveControls)){
data_case_positiveControls$timeStamp_f2_merge[i]=paste(data_case_positiveControls$timeStamp_f2_split[[i]][3],"/", data_case_positiveControls$timeStamp_f2_split[[i]][2],"/",data_case_positiveControls$timeStamp_f2_split[[i]][4], sep="")}
data_case_positiveControls$timeStamp_f2_format=as.Date(strptime(data_case_positiveControls$timeStamp_f2_merge,format='%d/%b/%Y'))
summary(data_case_positiveControls$timeStamp_f2_format)

#### Define the variable number of days between baseline and follow-up1
data_case_positiveControls$days_f1_baseline=difftime(data_case_positiveControls$timeStamp_f1_format,data_case_positiveControls$timeStamp_format) 
summary(as.numeric(data_case_positiveControls$days_f1_baseline))
hist(as.numeric(data_case_positiveControls$days_f1_baseline),main="Number of days between baseline and follow-up 1")

#### Define the variable number of days between baseline and follow-up2
data_case_positiveControls$days_f2_baseline=difftime(data_case_positiveControls$timeStamp_f2_format,data_case_positiveControls$timeStamp_format) 
summary(as.numeric(data_case_positiveControls$days_f2_baseline))
hist(as.numeric(data_case_positiveControls$days_f2_baseline),main="Number of days between baseline and follow-up 2")


#### Define the variable number of days between follow-up1 and follow-up2
data_case_positiveControls$days_f2_f1=difftime(data_case_positiveControls$timeStamp_f2_format,data_case_positiveControls$timeStamp_f1_format) 
summary(as.numeric(data_case_positiveControls$days_f2_f1))
hist(as.numeric(data_case_positiveControls$days_f2_f1),main="Number of days between follow-up 1 and follow-up 2")

####Days from covid to Cognitron/baseline. variable: ncrf1_admission.date_of_positive_covid19_test.txt
data_case_positiveControls$days_covid_baseline<-difftime(data_case_positiveControls$timeStamp_format,data_case_positiveControls$ncrf1_admission.date_of_positive_covid19_test.txt)
summary(as.numeric(data_case_positiveControls$days_covid_baseline)) #summarise data
#check case/control status
table(data_case_positiveControls$case_control_vaccine)

hist(as.numeric(data_case_positiveControls$days_covid_baseline))
dim(data_case_positiveControls)

### negative time since COVID-19
summary(as.numeric(data_case_positiveControls$days_covid_baseline))
#who has negative
# Filter observations with a negative value for days_covid_baseline
negative_days_covid <- subset(data_case_positiveControls, days_covid_baseline < 0)

# Print the "ID" variable for those observations
print(negative_days_covid$ID)

#replace with NA
# Replace negative values in days_covid_baseline with "N/A"
data_case_positiveControls$days_covid_baseline <- ifelse(data_case_positiveControls$days_covid_baseline < 0, "N/A", data_case_positiveControls$days_covid_baseline)

### merge ####
#make all as numeric
cols.num <- c("NfL","GFAP","Tau","UCHL1") 
Biom[cols.num] <- sapply(Biom[cols.num],as.numeric)
Cogbiomerg<-merge(data_case_positiveControls, Biom, by="ID") #merge by ID
table(Cogbiomerg$case_control_vaccine) #check case grouping
dim(Cogbiomerg)

# biomarker normative data
print(Biocontrols[0,]) #column nates from Biomarker control data
Cogbiomerg_norms <- bind_rows(Cogbiomerg, Biocontrols) #merge
dim(Cogbiomerg_norms) #check dimensions after merge
table(Cogbiomerg_norms$diagnostic_group)
Cogbiomerg_norms$case_control_vaccine[Cogbiomerg_norms$diagnostic_group== "Biomarker_control" ] <- "Biomarker_control" #give biomarker controls case status
table(Cogbiomerg_norms$case_control_vaccine)

# cognitive normative data
#reorder tasks - specific to observed data
#Cogbiomerg_norms <- Cogbiomerg_norms[, c(1:3767, 3772, 3773, 3768, 3774, 3771, 3770, 3778, 3779, 3775, 3780, 3777, 3776, 3769, 3781:3809)] #specific to original data
colnames(Cogbiomerg_norms) #check col names in dummy data
Cogbiomerg_norms <- Cogbiomerg_norms[, c(1:7,12, 13, 8, 14, 11, 10, 18, 19, 15, 20, 17, 16, 9,21:77)] #equivalent rearrangement in dummy data 
normative_tasks<-cognitron_normative_tasks[,-c(7,14)] #remove target detection in normative data (for consistency with real data in this analysis)
cog_norms<-cbind(normative_tasks,cognitron_normative_comps) #join task scores and composites
Cogbiomerg_norms2<-rbind.fill(Cogbiomerg_norms,cog_norms) #join to rest of dataset
dim(Cogbiomerg_norms2)
Cogbiomerg_norms2$case_control_vaccine[Cogbiomerg_norms2$diagnostic_group=="Normative"]<-"Normative" #call cognitive controls "Normative" group in case_control_vaccine
dim(Cogbiomerg_norms2)
table(Cogbiomerg_norms2$case_control_vaccine)

colnames(Cogbiomerg_norms)

# ACB and multimorbidity
##recode dichotmous for 2+ comorbidities
ACB_mm$Multimorbidity_Dichotomised <- ifelse(ACB_mm$Total_Multimorbidity >= 2, "Yes", ACB_mm$Multimorbidity_Dichotomised)
head(ACB_mm)
## merge these variables to data all
Cogbiomerg_norms3 <- merge(Cogbiomerg_norms2, ACB_mm, by = "ID", all.x = TRUE)
dim(Cogbiomerg_norms3)

#Cogbiomerg_norms3<-Cogbiomerg_norms3[,-c(3803:3809)] #remove added unneccessary columns (specific to observed data)
dim(Cogbiomerg_norms3) 

#Merge complete
#Create rds to be loaded at start of analysis scripts
write_rds(Cogbiomerg_norms3, "Dummycogbiomerg_norms3_190723.rds")
data<-read_rds("Dummycogbiomerg_norms3_190723.rds")

#End ####