## Figure 2B: Boxplots of effect size -author Kukatharmini Tharmaratnam, edits Greta Wood 
## Input: Output from Script 1 (dataframe including NeuroCOVID, COVID and normative data including biomarkers)
## Output: Figure 2B: Pattern of deficits in clinical groups by median DfE accuracy and responsive time minus matched community controls across six cognitive tasks

#load packages
library(ggplot2)
library(ggpattern)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(tidyverse)
library(plyr)
library(rcompanion)

#clear environment
rm(list = ls())

#load data
covidcns_community_controls_cases<-readRDS("Dummycogbiomerg_norms3_190723.rds")
dim(covidcns_community_controls_cases)
#subset cases (NeuroCOVID)
data_case=subset(covidcns_community_controls_cases,covidcns_community_controls_cases$case_control_vaccine=="Case: COVID-19 positive (i.e. neurological or psychiatric complication)")
dim(data_case)
## community_control (normative)
data_community_control=subset(covidcns_community_controls_cases,covidcns_community_controls_cases$case_control_vaccine=="Normative")
dim(data_community_control)

###### Univariate tests ###
############## Normality test for continuous variables ############

#### Median(IQR) for continuous variables #####
Median_IQR=function(x,y,k){
  Median=median(x[which(y==k)],na.rm = TRUE)
  IQR=IQR(x[which(y==k)],na.rm = TRUE)
  return(c(Median=Median,IQR=IQR))}

###### M: Mann-Whitney test #####
Mtest_continuous=function(x,y,alpha){
  Missing=sum(is.na(x))
  Missing_percentage=sum(is.na(x))/length(x)*100
  Mtest=wilcox.test(x~y,conf.int = TRUE, conf.level = (1-alpha)) 
  p_value=Mtest$p.value
  difference= Mtest$estimate
  CI=Mtest$conf.int
  return(c(Missing=Missing,Missing_percentage=Missing_percentage,p_value=p_value,difference=difference,CI=CI))
}
######################
#### Effect size for Mann-Whitney test - for reference
#wilcoxonR(x = values,g = group)
#small:  0.10     -   < 0.30
#medium: 0.30     -   < 0.50
#large:>= 0.50
######################
### univariate test for Case_controls p-value and adj p-value and effect size for all patients ####
### define the binary variable - NeuroCOVID vs normative
covidcns_community_controls_cases$binary[covidcns_community_controls_cases$case_control_vaccine=="Case: COVID-19 positive (i.e. neurological or psychiatric complication)"]=1
covidcns_community_controls_cases$binary[covidcns_community_controls_cases$case_control_vaccine=="Normative"]=0
#check numbers
table(covidcns_community_controls_cases$binary)
MWT_p_value=MWT_adj_p_value=effect_size=significant=median_effect_size=rep(NA,12)

colnames(covidcns_community_controls_cases)
# create a dataframe with columns Task, Mann Whitney U p value, adjusted p value, 
# effect size, significance and median effect size (as in existing literature)
for (i in 8:19){ #indexing of task scores (excluding motor control as no normative data available)
  M=Mtest_continuous(covidcns_community_controls_cases[,i],covidcns_community_controls_cases$binary,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 12)
  effect_size[i]=wilcoxonR(x = covidcns_community_controls_cases[,i],g = covidcns_community_controls_cases$binary)
  median_effect_size[i]=median(covidcns_community_controls_cases[which(covidcns_community_controls_cases$binary==1),i],na.rm=T)-median(covidcns_community_controls_cases[which(covidcns_community_controls_cases$binary==0),i],na.rm=T)
if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
  if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
    if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
#create dataframe containing task names, p value, adjusted p value, effect size, significance marker and effect size by median value
dummyuni_P_value=cbind(colnames(covidcns_community_controls_cases[,8:19]),MWT_p_value[8:19],MWT_adj_p_value[8:19],effect_size[8:19],significant[8:19],median_effect_size[8:19])
dummyuni_P_value
write.csv(dummyuni_P_value, file="dummycase_control_uni_P_value_with_median_15DEc2022.csv")

#####################
df=data.frame(dummyuni_P_value)
names(df)

## a factor with the levels in the correct order
df$X1<- factor(df$X1, levels=unique(df$X1))
#Create factor with 12 levels - tasks
Tasks=df$X1
Tasks
Tasks=c("Recognition memory (immediate)","Spatial Span","2D Manipulations","Verbal Analogies","Recognition memory (delayed)","Tower of London","Recognition memory (immediate) RT","Spatial Span RT","2D Manipulations RT","Verbal Analogies RT","Recognition memory (delayed) RT","Tower of London RT")            
df
Effect_size=median_effect_size[8:19]
dev.off()
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Create a bar plot of median effect sizes by diagnostic group, starting with NeuroCOVID
#Significance markers for Mann Whitney U on each bar
#Colour code so that is consistent with other tiles in the same figure
dummycasespattern<-ggplot(df, aes(x =factor(Tasks, levels=unique(Tasks)), y =Effect_size)) +
  geom_bar(stat="identity",color=c("azure3", "azure3", "azure3", "azure3", "azure3", "azure3", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff"),
           fill=c("#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442","#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442"),size=1.4)+
  ggtitle("NeuroCOVID") + ylim(-1.60,1.25)+ 
    ylab("Median effect size (SD units)")+
    geom_text(aes(label = df$X5), colour = "black", size=5, check_overlap = TRUE, nudge_y = -0.055)+  
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=18), 
        plot.title = element_text(size = 24))#+geom_col()
dummycasespattern
ggsave("dummycases_pattern.pdf")

## Split dataframe to have separate datagrame for each group containing each diagnostic group and normative data only
table(covidcns_community_controls_cases$diagnostic_group)

Cerebrovascular_data<-covidcns_community_controls_cases[covidcns_community_controls_cases$diagnostic_group=="Normative"|covidcns_community_controls_cases$diagnostic_group=="Cerebrovascular",]
dim(Cerebrovascular_data)
table(covidcns_community_controls_cases$diagnostic_group)
Encephalopathy_data=covidcns_community_controls_cases[covidcns_community_controls_cases$diagnostic_group=="Normative"|covidcns_community_controls_cases$diagnostic_group=="Encephalopathy/Delirium",]
dim(Encephalopathy_data)
Inflammatory_data=covidcns_community_controls_cases[covidcns_community_controls_cases$diagnostic_group=="Normative"|covidcns_community_controls_cases$diagnostic_group=="Inflammatory",]
Neuropsychiatric_data=covidcns_community_controls_cases[covidcns_community_controls_cases$diagnostic_group=="Normative"|covidcns_community_controls_cases$diagnostic_group=="Neuropsychiatric",]
Peripheral_data=covidcns_community_controls_cases[covidcns_community_controls_cases$diagnostic_group=="Normative"|covidcns_community_controls_cases$diagnostic_group=="Peripheral",]
Other_data=covidcns_community_controls_cases[covidcns_community_controls_cases$diagnostic_group=="Normative"|covidcns_community_controls_cases$diagnostic_group=="Other",]
COVID_positive_control_data=covidcns_community_controls_cases[covidcns_community_controls_cases$diagnostic_group=="Normative"|covidcns_community_controls_cases$diagnostic_group=="Control",]

#Cerebrovascular_data ####
#Repeat process of creating statistics and figure but for cerebrovascular group only 
### univariate test for diagnosis_controls p-value and adj p-value and effect size for cerebrovasucular data
table(Cerebrovascular_data$diagnostic_group)
### define the binary variable
Cerebrovascular_data$binary<-NA
Cerebrovascular_data$binary[Cerebrovascular_data$diagnostic_group=="Cerebrovascular"]=1
Cerebrovascular_data$binary[Cerebrovascular_data$diagnostic_group=="Normative"]=0
#check numbers
table(Cerebrovascular_data$binary)

#Create same dataframe for Cerebrovascular 
MWT_p_value=MWT_adj_p_value=effect_size=significant=  median_effect_size=rep(NA,12)
for (i in 8:19){ 
  M=Mtest_continuous(Cerebrovascular_data[,i],Cerebrovascular_data$diagnostic_group,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 12)
  effect_size[i]=wilcoxonR(x = Cerebrovascular_data[,i],g = Cerebrovascular_data$diagnostic_group)
  median_effect_size[i]=median(Cerebrovascular_data[which(Cerebrovascular_data$binary==1),i],na.rm=T)-median(Cerebrovascular_data[which(Cerebrovascular_data$binary==0),i],na.rm=T)
    if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
dummyuni_P_value_Cerebrovascular_data=cbind(colnames(Cerebrovascular_data[,8:19]),MWT_p_value[8:19],MWT_adj_p_value[8:19],effect_size[8:19],significant[8:19],median_effect_size[8:19])
dummyuni_P_value_Cerebrovascular_data

#####################
df2=data.frame(dummyuni_P_value_Cerebrovascular_data)
Effect_size1=median_effect_size[8:19]
dummyCerebrovascularpattern<-ggplot(df2, aes(x =factor(Tasks, levels=unique(Tasks)), y =Effect_size1)) +
  geom_bar(stat="identity",color=c("azure3", "azure3", "azure3", "azure3", "azure3", "azure3", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff"),
           fill=c("#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442","#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442"),size=1.4)+
  ggtitle("Cerebrovascular") + ylim(-1.60,1.25)+ 
  ylab("Median effect size (SD units)")+
  geom_text(aes(label = df2$X5), colour = "black", size=5, check_overlap = TRUE, nudge_y = -0.055)+  
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                    axis.text.y = element_text(size=18), 
                    plot.title = element_text(size = 24))
#show plot
dummyCerebrovascularpattern

#save plot
ggsave("dummyCerebrovascularpattern.pdf")

####################
### univariate test for diagnosis_controls p-value and adj p-value and effect size for Encephalopathy_data ####
table(Encephalopathy_data$diagnostic_group)
### define the binary variable
Encephalopathy_data$binary[Encephalopathy_data$diagnostic_group=="Encephalopathy/Delirium"]=1
Encephalopathy_data$binary[Encephalopathy_data$diagnostic_group=="Normative"]=0
table(Encephalopathy_data$binary)
MWT_p_value=MWT_adj_p_value=effect_size=significant=  median_effect_size=rep(NA,12)
for (i in 8:19){
  M=Mtest_continuous(Encephalopathy_data[,i],Encephalopathy_data$diagnostic_group,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 12)
  effect_size[i]=wilcoxonR(x = Encephalopathy_data[,i],g = Encephalopathy_data$diagnostic_group)
  median_effect_size[i]=median(Encephalopathy_data[which(Encephalopathy_data$binary==1),i],na.rm=T)-median(Encephalopathy_data[which(Encephalopathy_data$binary==0),i],na.rm=T)
  if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
dummyuni_P_value_Encephalopathy_data=cbind(colnames(Encephalopathy_data[,8:19]),MWT_p_value[8:19],MWT_adj_p_value[8:19],effect_size[8:19],significant[8:19],median_effect_size[8:19])
dummyuni_P_value_Encephalopathy_data
write.csv(dummyuni_P_value_Encephalopathy_data, file="dummyUni_P_value_enceph.csv")

#####################
df3=data.frame(dummyuni_P_value_Encephalopathy_data)
df3
Effect_size2=median_effect_size[8:19]
dummyEncephpattern<-ggplot(df3, aes(x =factor(Tasks, levels=unique(Tasks)), y =Effect_size2)) +
           geom_bar(stat="identity",color=c("azure3", "azure3", "azure3", "azure3", "azure3", "azure3", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff"),
           fill=c("#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442","#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442"),size=1.4)+
  ggtitle("Encephalopathy") + ylim(-1.60,1.25)+ 
  ylab("Median effect size (SD units)")+
  geom_text(aes(label = df3$X5), colour = "black", size=5, check_overlap = TRUE, nudge_y = -0.055)+  
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                    axis.text.y = element_text(size=18), 
                    plot.title = element_text(size = 24))
dummyEncephpattern
ggsave("dummyEncephpattern.pdf")

### univariate test for diagnosis_controls p-value and adj p-value and effect size for Inflammatory_data ####
### define the binary variable
Inflammatory_data$binary[Inflammatory_data$diagnostic_group=="Inflammatory"]=1
Inflammatory_data$binary[Inflammatory_data$diagnostic_group=="Normative"]=0
#check numbers
table(Inflammatory_data$binary)
#create dataframe
MWT_p_value=MWT_adj_p_value=effect_size=significant=  median_effect_size=rep(NA,12)
for (i in 8:19){
  M=Mtest_continuous(Inflammatory_data[,i],Inflammatory_data$diagnostic_group,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 12)
  effect_size[i]=wilcoxonR(x = Inflammatory_data[,i],g = Inflammatory_data$diagnostic_group)
  median_effect_size[i]=median(Inflammatory_data[which(Inflammatory_data$binary==1),i],na.rm=T)-median(Inflammatory_data[which(Inflammatory_data$binary==0),i],na.rm=T)
  if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
dummyuni_P_value_Inflammatory_data=cbind(colnames(Inflammatory_data[,8:19]),MWT_p_value[8:19],MWT_adj_p_value[8:19],effect_size[8:19],significant[8:19],median_effect_size[8:19])
write.csv(dummyuni_P_value_Inflammatory_data, "dummyuni_P_value_Inflammatory_data.csv")

#####################
df4=data.frame(dummyuni_P_value_Inflammatory_data)
Effect_size3=median_effect_size[8:19]
df4
dummyInflammpattern<-ggplot(df4, aes(x =factor(Tasks, levels=unique(Tasks)), y =Effect_size3)) +
           geom_bar(stat="identity",color=c("azure3", "azure3", "azure3", "azure3", "azure3", "azure3", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff"),
           fill=c("#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442","#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442"),size=1.4)+
  ggtitle("Inflammatory") + ylim(-1.60,1.25)+ 
  ylab("Median effect size (SD units)")+
  geom_text(aes(label = df4$X5), colour = "black", size=5, check_overlap = TRUE, nudge_y = -0.055)+  
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                    axis.text.y = element_text(size=18), 
                    plot.title = element_text(size = 24))
dummyInflammpattern
ggsave("dummyInflammpattern.pdf")

### univariate test for diagnosis_controls p-value and adj p-value and effect size for Neuropsychiatric_data ####
table(Neuropsychiatric_data$diagnostic_group)
### define the binary variable
Neuropsychiatric_data$binary[Neuropsychiatric_data$diagnostic_group=="Neuropsychiatric"]=1
Neuropsychiatric_data$binary[Neuropsychiatric_data$diagnostic_group=="Normative"]=0
#check numbers
table(Neuropsychiatric_data$binary)

MWT_p_value=MWT_adj_p_value=effect_size=significant= median_effect_size=rep(NA,12)
for (i in 8:19){
  M=Mtest_continuous(Neuropsychiatric_data[,i],Neuropsychiatric_data$diagnostic_group,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 12)
  effect_size[i]=wilcoxonR(x = Neuropsychiatric_data[,i],g = Neuropsychiatric_data$diagnostic_group)
  median_effect_size[i]=median(Neuropsychiatric_data[which(Neuropsychiatric_data$binary==1),i],na.rm=T)-median(Neuropsychiatric_data[which(Neuropsychiatric_data$binary==0),i],na.rm=T)
  if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
dummyuni_P_value_Neuropsychiatric_data=cbind(colnames(Neuropsychiatric_data[,8:19]),MWT_p_value[8:19],MWT_adj_p_value[8:19],effect_size[8:19],significant[8:19],median_effect_size[8:19])
write.csv(dummyuni_P_value_Neuropsychiatric_data, file="dummyuni_P_value_Neuropsychiatric_data.csv")

#####################
df5=data.frame(dummyuni_P_value_Neuropsychiatric_data)
df5
Effect_size4=median_effect_size[8:19]
dummyNeuropsychpattern<-ggplot(df5, aes(x =factor(Tasks, levels=unique(Tasks)), y =Effect_size4)) +
           geom_bar(stat="identity",color=c("azure3", "azure3", "azure3", "azure3", "azure3", "azure3", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff"),
           fill=c("#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442","#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442"),size=1.4)+
  ggtitle("Neuropsychiatric") + ylim(-1.60,1.25)+ 
  ylab("Median effect size (SD units)")+
  geom_text(aes(label = df5$X5), colour = "black", size=5, check_overlap = TRUE, nudge_y = -0.055)+  
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                    axis.text.y = element_text(size=18), 
                    plot.title = element_text(size = 24))
dummyNeuropsychpattern
ggsave("dummyNeuropsychpattern.pdf")

### univariate test for diagnosis_controls p-value and adj p-value and effect size for Peripheral_data ####
table(Peripheral_data$diagnostic_group)
### defind the binary variable
Peripheral_data$binary[Peripheral_data$diagnostic_group=="Peripheral"]=1
Peripheral_data$binary[Peripheral_data$diagnostic_group=="Normative"]=0
table(Peripheral_data$binary)
MWT_p_value=MWT_adj_p_value=effect_size=significant=  median_effect_size=rep(NA,12)
for (i in 8:19){
  M=Mtest_continuous(Peripheral_data[,i],Peripheral_data$diagnostic_group,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 12)
  effect_size[i]=wilcoxonR(x = Peripheral_data[,i],g = Peripheral_data$diagnostic_group)
  median_effect_size[i]=median(Peripheral_data[which(Peripheral_data$binary==1),i],na.rm=T)-median(Peripheral_data[which(Peripheral_data$binary==0),i],na.rm=T)
  if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
dummyuni_P_value_Peripheral_data=cbind(colnames(Peripheral_data[,8:19]),MWT_p_value[8:19],MWT_adj_p_value[8:19],effect_size[8:19],significant[8:19],median_effect_size[8:19])
write.csv(dummyuni_P_value_Peripheral_data, file="dummyuni_P_value_Peripheral_data.csv")

#####################
df6=data.frame(dummyuni_P_value_Peripheral_data)
df6
Effect_size5=median_effect_size[8:19]
dummyPeripheralpattern<-ggplot(df6, aes(x =factor(Tasks, levels=unique(Tasks)), y =Effect_size5)) +
           geom_bar(stat="identity",color=c("azure3", "azure3", "azure3", "azure3", "azure3", "azure3", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff"),
           fill=c("#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442","#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442"),size=1.4)+
  ggtitle("Peripheral") + ylim(-1.60,1.25)+ 
  ylab("Median effect size (SD units)")+
  geom_text(aes(label = df6$X5), colour = "black", size=5, check_overlap = TRUE, nudge_y = -0.055)+  
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                    axis.text.y = element_text(size=18), 
                    plot.title = element_text(size = 24))
dummyPeripheralpattern
ggsave("dummyPeripheralpattern.pdf")

### univariate test for diagnosis_controls p-value and adj p-value and effect size for Other_data ####
table(Other_data$diagnostic_group)
### defind the binary variable
Other_data$binary[Other_data$diagnostic_group=="Other"]=1
Other_data$binary[Other_data$diagnostic_group=="Normative"]=0
table(Other_data$binary)
MWT_p_value=MWT_adj_p_value=effect_size=significant=  median_effect_size=rep(NA,12)
for (i in 8:19){
  M=Mtest_continuous(Other_data[,i],Other_data$diagnostic_group,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 12)
  effect_size[i]=wilcoxonR(x = Other_data[,i],g = Other_data$diagnostic_group)
  median_effect_size[i]=median(Other_data[which(Other_data$binary==1),i],na.rm=T)-median(Other_data[which(Other_data$binary==0),i],na.rm=T)
  if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
dummyuni_P_value_Other_data=cbind(colnames(Other_data[,8:19]),MWT_p_value[8:19],MWT_adj_p_value[8:19],effect_size[8:19],significant[8:19],median_effect_size[8:19])
write.csv(dummyuni_P_value_Other_data, file="dummyuni_P_value_Other_data.csv")

#####################
df7=data.frame(dummyuni_P_value_Other_data)
names(df)
df7
Effect_size6=median_effect_size[8:19]
dummyOtherpattern<-ggplot(df7, aes(x =factor(Tasks, levels=unique(Tasks)), y =Effect_size6)) +
  geom_bar(stat="identity",color=c("azure3", "azure3", "azure3", "azure3", "azure3", "azure3", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff"),
           fill=c("#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442","#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442"),size=1.4)+
  ggtitle("Other") + ylim(-1.60,1.25)+ 
  ylab("Median effect size (SD units)")+
  geom_text(aes(label = df7$X5), colour = "black", size=5, check_overlap = TRUE, nudge_y = -0.055)+  
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                    axis.text.y = element_text(size=18), 
                    plot.title = element_text(size = 24))
dummyOtherpattern
ggsave("dummyOtherpattern.pdf")

####################
### univariate test for diagnosis_controls p-value and adj p-value and effect size for COVID_positive_control_data ####
table(COVID_positive_control_data$diagnostic_group)
### defind the binary variable
COVID_positive_control_data$binary[COVID_positive_control_data$diagnostic_group=="Control"]=1
COVID_positive_control_data$binary[COVID_positive_control_data$diagnostic_group=="Normative"]=0
table(COVID_positive_control_data$binary)
MWT_p_value=MWT_adj_p_value=effect_size=significant=  median_effect_size=rep(NA,12)
for (i in 8:19){
  M=Mtest_continuous(COVID_positive_control_data[,i],COVID_positive_control_data$diagnostic_group,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 12)
  effect_size[i]=wilcoxonR(x = COVID_positive_control_data[,i],g = COVID_positive_control_data$diagnostic_group)
  median_effect_size[i]=median(COVID_positive_control_data[which(COVID_positive_control_data$binary==1),i],na.rm=T)-median(COVID_positive_control_data[which(COVID_positive_control_data$binary==0),i],na.rm=T)
  if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
dummyuni_P_value_COVID_positive_control_data=cbind(colnames(COVID_positive_control_data[,8:19]),MWT_p_value[8:19],MWT_adj_p_value[8:19],effect_size[8:19],significant[8:19],median_effect_size[8:19])
write.csv(dummyuni_P_value_COVID_positive_control_data, file="dummyuni_P_value_COVID_positive_control_data.csv")
#####################
df8=data.frame(dummyuni_P_value_COVID_positive_control_data)
df8
Effect_size7=median_effect_size[8:19]
dummyCOVIDpattern<-ggplot(df8, aes(x =factor(Tasks, levels=unique(Tasks)), y =Effect_size7)) +
  geom_bar(stat="identity",color=c("azure3", "azure3", "azure3", "azure3", "azure3", "azure3", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff", "peachpuff"),
           fill=c("#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442","#56B4E9","#CC79A7","#E69F00", "#009E73","#0072B2","#F0E442"),size=1.4)+
  ggtitle("COVID") +ylim(-1.60,1.25)+ 
  ylab("Median effect size (SD units)")+
  geom_text(aes(label = df8$X5), colour = "black", size=5, check_overlap = TRUE, nudge_y = -0.055)+  
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                    axis.text.y = element_text(size=18), 
                    plot.title = element_text(size = 24))
dummyCOVIDpattern
ggsave("dummyCOVIDpattern.pdf")

## Subjective comparison plots - put all boxplots in a grid to allow comparison

dummyFig3grid<-ggarrange(dummycasespattern+ rremove("xlab")+ rremove("ylab")+ theme(axis.text.x=element_blank()), dummyCerebrovascularpattern+ rremove("xlab")+ rremove("ylab")+ theme(axis.text.x=element_blank()), dummyEncephpattern+ rremove("xlab")+ rremove("ylab")+ theme(axis.text.x=element_blank()),
          dummyInflammpattern+ rremove("xlab")+ rremove("ylab")+ theme(axis.text.x=element_blank()),dummyCOVIDpattern+ rremove("xlab")+rremove("ylab")+ theme(axis.text.x=element_blank()), dummyNeuropsychpattern+ rremove("xlab")+ rremove("ylab")+ theme(axis.text.x=element_blank()),dummyPeripheralpattern+ rremove("xlab")+ rremove("ylab")+ theme(axis.text.x=element_blank()),dummyOtherpattern+ rremove("xlab")+ rremove("ylab")+ theme(axis.text.x=element_blank()),
          ncol=4, nrow=2, legend="top"+ theme(legend.text=element_text(size=30)))
#view this plot
dummyFig3grid
#save this
ggsave("dummyFig3grid.pdf", width = 350, height = 200, units = "mm")
#########################
####################
###############  end ### 

