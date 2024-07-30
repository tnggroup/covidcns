#Authors: Kukatharmini Tharmaratnam and Greta Wood
#Input: Merged dataset from Script 1 (cognition, biomarkers, normative)
#Output: Fig 2A - violin plot of Composite, accuracy, RT by diagnostic group
#Associated p values corrected for multiple comparisons

#clear environment
rm(list = ls())

#load scripts
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(wesanderson)
library(gridExtra)
library(ggpattern)
library(tidyr)
library(dplyr)
library(rstatix)
library(ggpubr)
library(tidyverse)

#load data
data_All<-readRDS("Dummycogbiomerg_norms3_190723.rds")
dim(data_All)

#Script start
#### Diagnostic.Group #### Check all diagnostic groups correctly assigned
table(data_All$diagnostic_group)
data_All$diagnostic_group[data_All$diagnostic_group=="Other "]="Other"
data_All$diagnostic_group[data_All$diagnostic_group=="Encephalopathy/Delirium"]="Encephalopathy"
data_All$diagnostic_group[data_All$diagnostic_group=="Control"]="COVID"
data_All$diagnosis_case_control[data_All$diagnostic_group=="Cerebrovascular"|data_All$diagnostic_group=="Encephalopathy"|data_All$diagnostic_group=="Inflammatory"|
                            data_All$diagnostic_group=="Neuropsychiatric"|data_All$diagnostic_group=="Other"|data_All$diagnostic_group=="Peripheral"]="NeuroCOVID"
data_All$diagnosis_case_control[data_All$diagnostic_group=="COVID"]="COVID"
data_All$diagnosis_case_control[data_All$diagnostic_group=="Normative"]="Normative"

table(data_All$diagnosis_case_control)

summary(data_All$Composite_global)
tapply(data_All$Composite_global, data_All$diagnosis_case_control, summary)

table(data_All$diagnostic_group)

########## Violin plot ##############
### combining all diagnostic groups as cases and controls, normative and each diagnostic group 

#create colour schemes
colourb9<-c("lightskyblue4","darkred","cornflowerblue","chocolate","mediumpurple3","palegreen3","orange1","lightpink","navy")

colourb8<-c("lightskyblue4","darkred","mediumpurple3","palegreen3","orange1","lightpink","navy")

display.brewer.all(colorblindFriendly = TRUE)
# Function to display sample size (n) for each violin.
#give.n <- function(x){return(c(y = min(Composite), label = length(x)))}
pl1=bind_rows(NeuroCOVID = data_All %>% filter(diagnostic_group %in% c("Cerebrovascular", "Encephalopathy","Inflammatory","Neuropsychiatric","Peripheral","Unknown","Other")),
              COVID =  data_All%>% filter(diagnostic_group== "COVID"),
              Normative =  data_All %>% filter(diagnostic_group == "Normative"),
              Cerebrovascular = data_All %>% filter(diagnostic_group == "Cerebrovascular"),
              Encephalopathy = data_All %>% filter(diagnostic_group == "Encephalopathy"),
              Inflammatory = data_All %>% filter(diagnostic_group == "Inflammatory"),
              Neuropsychiatric = data_All %>% filter(diagnostic_group == "Neuropsychiatric"),
              Peripheral = data_All %>% filter(diagnostic_group == "Peripheral"),
              Other = data_All %>% filter(diagnostic_group == "Other"),
              .id = "group2"
) %>% 
  mutate(group2 = factor(group2, levels = c("Normative","COVID","NeuroCOVID","Cerebrovascular", "Encephalopathy", "Inflammatory","Neuropsychiatric","Peripheral","Other"))) %>% 
  ggplot(aes(x = group2, y = Composite_global, fill=group2))# + 
  #geom_violin(trim = F)

# violin plot
pl1 <- pl1 + geom_violin(trim = F)
pl1

# add summary stat line and create plot for Composite Global Score
pl1 <- pl1 + stat_summary(fun.data=median_hilow, # can use mean_sdl instead
                          geom="pointrange",
                          color="black")
pl1 <- pl1 + ylab("GDfE score (SD units)")
pl1 <- pl1 + labs(fill = "Diagnostic group")
pl1 <- pl1 + scale_fill_manual(values=colourb9)
pl1 <- pl1 + theme_bw()
pl1 <- pl1 + 
  ggtitle("Composite Global Score") + 
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=18),
        axis.text.x = element_text(size = 16,
                                   angle = 45,
                                   hjust = 1),
        axis.text.y=element_text(size = 18))+
  theme(legend.position = "none")
pl1
ggsave("DummyCognitronglobalviolin.png")

#```{r violin plot accuracy cognitron}
pl2=bind_rows(NeuroCOVID = data_All %>% filter(diagnostic_group %in% c("Cerebrovascular", "Encephalopathy","Inflammatory","Neuropsychiatric","Peripheral","Unknown","Other")),
              COVID =  data_All%>% filter(diagnostic_group== "COVID"),
              Normative =  data_All %>% filter(diagnostic_group == "Normative"),
              Cerebrovascular = data_All %>% filter(diagnostic_group == "Cerebrovascular"),
              Encephalopathy = data_All %>% filter(diagnostic_group == "Encephalopathy"),
              Inflammatory = data_All %>% filter(diagnostic_group == "Inflammatory"),
              Neuropsychiatric = data_All %>% filter(diagnostic_group == "Neuropsychiatric"),
              Peripheral = data_All %>% filter(diagnostic_group == "Peripheral"),
              Other = data_All %>% filter(diagnostic_group == "Other"),
              .id = "group2"
) %>% 
  mutate(group2 = factor(group2, levels = c("Normative","COVID","NeuroCOVID","Cerebrovascular", "Encephalopathy", "Inflammatory","Neuropsychiatric","Peripheral","Other"))) %>% 
  ggplot(aes(x = group2, y = Composite_acc, fill=group2))# + 

pl2 <- pl2 + geom_violin(trim = F)
pl2
# violin plot
pl2 <- pl2 + stat_summary(fun.data=median_hilow, # can use mean_sdl instead
                          geom="pointrange",
                          color="black")
pl2 <- pl2 + ylab("DfE Accuracy (SD units)")
pl2 <- pl2 + labs(fill = "Diagnostic group")
pl2 <- pl2 + scale_fill_manual(values=colourb9)
pl2 <- pl2 + theme_bw()
pl2 <- pl2 + 
  ggtitle("Accuracy") + 
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=18),
        axis.text.x = element_text(size = 16,
                                   angle = 45,
                                   hjust = 1),
        axis.text.y=element_text(size = 18))+
  theme(legend.position = "none")
pl2
ggsave("DummyCognitronaccuracyviolin.png")

#Violin plot cognitron RT by diagnostic group
#```{r violin plot RT cognitron}
pl3=bind_rows(NeuroCOVID = data_All %>% filter(diagnostic_group %in% c("Cerebrovascular", "Encephalopathy","Inflammatory","Neuropsychiatric","Peripheral","Other")),
              COVID =  data_All%>% filter(diagnostic_group== "COVID"),
              Normative =  data_All %>% filter(diagnostic_group == "Normative"),
              Cerebrovascular = data_All %>% filter(diagnostic_group == "Cerebrovascular"),
              Encephalopathy = data_All %>% filter(diagnostic_group == "Encephalopathy"),
              Inflammatory = data_All %>% filter(diagnostic_group == "Inflammatory"),
              Neuropsychiatric = data_All %>% filter(diagnostic_group == "Neuropsychiatric"),
              Peripheral = data_All %>% filter(diagnostic_group == "Peripheral"),
              Other = data_All %>% filter(diagnostic_group == "Other"),
              .id = "group2"
) %>% 
  mutate(group2 = factor(group2, levels = c("Normative","COVID","NeuroCOVID","Cerebrovascular", "Encephalopathy", "Inflammatory","Neuropsychiatric","Peripheral","Other"))) %>% 
  ggplot(aes(x = group2, y = Composite_rt, fill=group2))# + 

pl3 <- pl3 + geom_violin(trim = F)
pl3
# violin plot
pl3 <- pl3 + stat_summary(fun.data=median_hilow, # can use mean_sdl instead
                          geom="pointrange",
                          color="black")
pl3 <- pl3 + ylab("DfE Response Time (SD units)")
pl3 <- pl3 + labs(fill = "Diagnostic group")
pl3 <- pl3 + scale_fill_manual(values=colourb9)
pl3 <- pl3 + theme_bw()
pl3 <- pl3 + 
  ggtitle("Response Time") + 
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=18),
        axis.text.x = element_text(size = 16,
                                   angle = 45,
                                   hjust = 1),
        axis.text.y=element_text(size = 18))+
  theme(legend.position = "none")
pl3

ggsave("DummyCognitronRTviolin.png")

#Prepare plots to be in single figure 
dummyviolingrid<-ggarrange(pl1+ rremove("xlab"), pl2+ rremove("xlab"), pl3+ rremove("xlab"), ncol=3, nrow=1, common.legend = TRUE, legend="top")
#view this
dummyviolingrid
#save joint figure
ggsave("dummyviolingrid.pdf", width = 297, height = 140, units = "mm")

#calculate significance levels to be added to figure above
#compare cases (NeuroCOVID) to normative
#create binary variable with only cases and normative
data_All$binary<-NA
data_All$binary[data_All$case_control_vaccine=="Case: COVID-19 positive (i.e. neurological or psychiatric complication)"]=1
data_All$binary[data_All$case_control_vaccine=="Normative"]=0
#Check numbers are as expected
table(data_All$binary)
data_All$binary<-as.factor(data_All$binary)

#Mann Whitney U function
Mtest_continuous=function(x,y,alpha){
  Missing=sum(is.na(x))
  Missing_percentage=sum(is.na(x))/length(x)*100
  Mtest=wilcox.test(x~y,conf.int = TRUE, conf.level = (1-alpha)) 
  p_value=Mtest$p.value
  difference= Mtest$estimate
  CI=Mtest$conf.int
  return(c(Missing=Missing,Missing_percentage=Missing_percentage,p_value=p_value,difference=difference,CI=CI))
}

#check col names for indexing
colnames(data_All)
#summarise DfE composite data
summary(data_All[,5:7])

#Function for effect sizes and p values, first applied to cases
MWT_p_value=MWT_adj_p_value=effect_size=significant=median_effect_size=rep(NA,3)
for (i in c(5:7)){ #indexing of tasks with matched normative data
  M=Mtest_continuous(data_All[,i],data_All$binary,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 8) 
  median_effect_size[i]=median(data_All[which(data_All$binary==1),i],na.rm=T)-median(data_All[which(data_All$binary==0),i],na.rm=T) #median effect size
  if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}} #column with significant marker
#create dataframe with tasks, p value, adjusted p value, significance marker, effect size
uni_P_value_case=cbind(colnames(data_All[5:7]),MWT_p_value[5:7],MWT_adj_p_value[5:7],significant[5:7],median_effect_size[5:7])
uni_P_value_case

#now do for controls
#replace binary variable with NA 
data_All$binary<-NA
#recode as control vs normative
data_All$binary[data_All$case_control_vaccine=="Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)"]=1
data_All$binary[data_All$case_control_vaccine=="Normative"]=0
#check numbers as expected
table(data_All$binary)

MWT_p_value=MWT_adj_p_value=effect_size=significant=median_effect_size=rep(NA,3)
for (i in 5:7){
  M=Mtest_continuous(data_All[,i],data_All$binary,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 8)
  median_effect_size[i]=median(data_All[which(data_All$binary==1),i],na.rm=T)-median(data_All[which(data_All$binary==0),i],na.rm=T)
  if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
uni_P_value_control=cbind(colnames(data_All[5:7]),MWT_p_value[5:7],MWT_adj_p_value[5:7],significant[5:7],median_effect_size[5:7])
uni_P_value_control

#now do for cerebrovascular
data_All$binary<-NA
data_All$binary[data_All$diagnostic_group=="Cerebrovascular"]=1
data_All$binary[data_All$case_control_vaccine=="Normative"]=0
table(data_All$binary)

MWT_p_value=MWT_adj_p_value=effect_size=significant=median_effect_size=rep(NA,3)
for (i in 5:7){
  M=Mtest_continuous(data_All[,i],data_All$binary,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 8)
  median_effect_size[i]=median(data_All[which(data_All$binary==1),i],na.rm=T)-median(data_All[which(data_All$binary==0),i],na.rm=T)
  if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
uni_P_value_cerebro=cbind(colnames(data_All[5:7]),MWT_p_value[5:7],MWT_adj_p_value[5:7],significant[5:7],median_effect_size[5:7])
uni_P_value_cerebro

#now do for encephalopathy
data_All$binary<-NA
data_All$binary[data_All$diagnostic_group=="Encephalopathy"]=1
data_All$binary[data_All$case_control_vaccine=="Normative"]=0
table(data_All$binary)

MWT_p_value=MWT_adj_p_value=effect_size=significant=median_effect_size=rep(NA,3)
for (i in 5:7){
  M=Mtest_continuous(data_All[,i],data_All$binary,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 8)
  median_effect_size[i]=median(data_All[which(data_All$binary==1),i],na.rm=T)-median(data_All[which(data_All$binary==0),i],na.rm=T)
  if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
uni_P_value_enceph=cbind(colnames(data_All[5:7]),MWT_p_value[5:7],MWT_adj_p_value[5:7],significant[5:7],median_effect_size[5:7])
uni_P_value_enceph

#now do for Inflammatory
data_All$binary<-NA
data_All$binary[data_All$diagnostic_group=="Inflammatory"]=1
data_All$binary[data_All$case_control_vaccine=="Normative"]=0
table(data_All$binary)

MWT_p_value=MWT_adj_p_value=effect_size=significant=median_effect_size=rep(NA,3)
for (i in 5:7){
  M=Mtest_continuous(data_All[,i],data_All$binary,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 8)
  median_effect_size[i]=median(data_All[which(data_All$binary==1),i],na.rm=T)-median(data_All[which(data_All$binary==0),i],na.rm=T)
  if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
uni_P_value_inflamm=cbind(colnames(data_All[5:7]),MWT_p_value[5:7],MWT_adj_p_value[5:7],significant[5:7],median_effect_size[5:7])
uni_P_value_inflamm

#now do for neuropsych
data_All$binary<-NA
data_All$binary[data_All$diagnostic_group=="Neuropsychiatric"]=1
data_All$binary[data_All$case_control_vaccine=="Normative"]=0
table(data_All$binary)

MWT_p_value=MWT_adj_p_value=effect_size=significant=median_effect_size=rep(NA,3)
for (i in 5:7){
  M=Mtest_continuous(data_All[,i],data_All$binary,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 8)
  median_effect_size[i]=median(data_All[which(data_All$binary==1),i],na.rm=T)-median(data_All[which(data_All$binary==0),i],na.rm=T)
  if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
uni_P_value_neuropsych=cbind(colnames(data_All[5:7]),MWT_p_value[5:7],MWT_adj_p_value[5:7],significant[5:7],median_effect_size[5:7])
uni_P_value_neuropsych
#Neuropsych *** RT

#now do for peripheral
data_All$binary<-NA
data_All$binary[data_All$diagnostic_group=="Peripheral"]=1
data_All$binary[data_All$case_control_vaccine=="Normative"]=0
table(data_All$binary)

MWT_p_value=MWT_adj_p_value=effect_size=significant=median_effect_size=rep(NA,3)
for (i in 5:7){
  M=Mtest_continuous(data_All[,i],data_All$binary,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 8)
  median_effect_size[i]=median(data_All[which(data_All$binary==1),i],na.rm=T)-median(data_All[which(data_All$binary==0),i],na.rm=T)
  if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
uni_P_value_peripheral=cbind(colnames(data_All[5:7]),MWT_p_value[5:7],MWT_adj_p_value[5:7],significant[5:7],median_effect_size[5:7])
uni_P_value_peripheral
#peripheral RT is actually **

#now do for other
data_All$binary<-NA
data_All$binary[data_All$diagnostic_group=="Other"]=1
data_All$binary[data_All$case_control_vaccine=="Normative"]=0
table(data_All$binary)

MWT_p_value=MWT_adj_p_value=effect_size=significant=median_effect_size=rep(NA,3)
for (i in 5:7){
  M=Mtest_continuous(data_All[,i],data_All$binary,alpha=0.05) 
  MWT_p_value[i]=as.vector(M)[3]
  ### Adjusted p-value based on false discovery rate approach
  MWT_adj_p_value[i]=p.adjust(MWT_p_value[i], method = "fdr", n = 8)
  median_effect_size[i]=median(data_All[which(data_All$binary==1),i],na.rm=T)-median(data_All[which(data_All$binary==0),i],na.rm=T)
  if(MWT_adj_p_value[i]<0.001) {significant[i]="***" } else {
    if(MWT_adj_p_value[i]>0.001&MWT_adj_p_value[i]<0.01) {significant[i]="**" }else {
      if(MWT_adj_p_value[i]>0.01&MWT_adj_p_value[i]<0.05) {significant[i]="*" }else {significant[i]=" "}}}}
uni_P_value_other=cbind(colnames(data_All[5:7]),MWT_p_value[5:7],MWT_adj_p_value[5:7],significant[5:7],median_effect_size[5:7])
uni_P_value_other

#End ####