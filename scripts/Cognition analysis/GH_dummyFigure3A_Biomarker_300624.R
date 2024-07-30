## Figure 3A: Brain injury marker boxplot (measured at post-acute appointment)
## Input: Output from Script 1 (dataframe including NeuroCOVID, COVID and normative data including biomarkers)
## Output: Figure 3A: Brain injury markers in pg/mL by diagnostic group. Lower limit of quantification (LLOQ marked (dashed)) if included in scale. Centre line, median; box limits, upper and lower quartiles; whiskers, 1.5x interquartile range. Normative values from n=60 healthy controls. * p < 0.05, **p < 0.01, ***p < 0.001, ns= non-significant.

#clear environment
rm(list = ls())

#load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(tidyverse)
library(reshape2)
library(plyr)
library(ggrepel)

# create colours
colourb9<-c("cornflowerblue","darkred","lightskyblue4","chocolate","mediumpurple3","palegreen3","orange1","lightpink","navy")
colourb8<-c("darkred","lightskyblue4","chocolate","mediumpurple3","palegreen3","orange1","lightpink","navy")

#load data
Cogbiomerg<-readRDS("Dummycogbiomerg_norms3_190723.rds") #master data

#Biomarkers ####
##describe biomarkers - explore data ####
## mann whitney u function
Mtest_continuous=function(x,y,alpha){
  Missing=sum(is.na(x))
  Missing_percentage=sum(is.na(x))/length(x)*100
  Mtest=wilcox.test(x~y,conf.int = TRUE, conf.level = (1-alpha)) 
  p_value=Mtest$p.value
  difference= Mtest$estimate
  CI=Mtest$conf.int
  return(c(Missing=Missing,Missing_percentage=Missing_percentage,p_value=p_value,difference=difference,CI=CI))}
###
##NfL case vs control
#create table of descriptive statistics by case group
tapply(Cogbiomerg$NfL, Cogbiomerg$case_control_vaccine, summary)

#normative is NA as expected - this is the cognitive normative data

#by diagnostic group
tapply(Cogbiomerg$NfL, Cogbiomerg$diagnostic_group, summary)

##check NfL distribution
shapiro.test(Cogbiomerg$NfL) 

#GFAP case vs control
tapply(Cogbiomerg$GFAP, Cogbiomerg$case_control_vaccine, summary)
tapply(Cogbiomerg$GFAP, Cogbiomerg$diagnostic_group, summary)

#check GFAP distribution
shapiro.test(Cogbiomerg$GFAP) 

#Tau descriptive statistics 
tapply(Cogbiomerg$Tau, Cogbiomerg$case_control_vaccine, summary)
tapply(Cogbiomerg$Tau, Cogbiomerg$diagnostic_group, summary)

#Tau distribution
shapiro.test(Cogbiomerg$Tau) 

# UCHL1 descriptive statistics
tapply(Cogbiomerg$UCHL1, Cogbiomerg$case_control_vaccine, summary)
tapply(Cogbiomerg$UCHL1, Cogbiomerg$diagnostic_group, summary)

#UCHL1 distribution
shapiro.test(Cogbiomerg$UCHL1)

####boxplot biomarkers - prepare data ####
#Subset cases (NeuroCOVID) and control (COVID) data
Casesbio<-Cogbiomerg[Cogbiomerg$case_control_vaccine=="Case: COVID-19 positive (i.e. neurological or psychiatric complication)",]
Controlbio<-Cogbiomerg[Cogbiomerg$case_control_vaccine=="Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)",]

#make case status a factor variable 
Cogbiomerg$case_control_vaccine<- as.factor(Cogbiomerg$case_control_vaccine)

#remove cognition normative data
Cogbiomerg <- Cogbiomerg[Cogbiomerg$case_control_vaccine != "Normative", ]

#check
table(Cogbiomerg$case_control_vaccine)

#Recode levels of case/control status for ease of graphing
levels(Cogbiomerg$case_control_vaccine)[levels(Cogbiomerg$case_control_vaccine) == "Case: COVID-19 positive (i.e. neurological or psychiatric complication)"]  <- "NeuroCOVID"
levels(Cogbiomerg$case_control_vaccine)[levels(Cogbiomerg$case_control_vaccine) == "Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)"]  <- "COVID"
levels(Cogbiomerg$case_control_vaccine)[levels(Cogbiomerg$case_control_vaccine) == "Biomarker_control"]  <- "Normative"

#make diagnostic group variable a factor
Cogbiomerg$diagnostic_group<- as.factor(Cogbiomerg$diagnostic_group)

table(Cogbiomerg$case_control_vaccine)

#rename diagnostic groups
levels(Cogbiomerg$diagnostic_group)[levels(Cogbiomerg$diagnostic_group) == "Control"]  <- "COVID"
levels(Cogbiomerg$diagnostic_group)[levels(Cogbiomerg$diagnostic_group) == "Encephalopathy/Delirium"]  <- "Encephalopathy"

#subset data required for boxplot
Bioboxplot<-select(Cogbiomerg, ID, case_control_vaccine, diagnostic_group, NfL,Tau,GFAP,UCHL1)
#check dim
dim(Bioboxplot)
#case status
table(Bioboxplot$case_control_vaccine)

#STATS
biomarker_comparisons <- list( c("Normative", "COVID"), c("Normative", "NeuroCOVID"), c("COVID", "NeuroCOVID") )

#order diagnostic groups - for consistent colours
Bioboxplot$diagnostic_group <- factor(Bioboxplot$diagnostic_group, levels = c("COVID", "Normative", 'Cerebrovascular', 'Encephalopathy', 'Inflammatory', 'Neuropsychiatric', 'Peripheral','Other'))
Bioboxplot$case_control_vaccine <- factor(Bioboxplot$case_control_vaccine, levels = c("Normative", "COVID", 'NeuroCOVID'))

#check subset
head(Bioboxplot)

##Create the boxplots ####
#NfL boxplot
#0.317 is LLOQ for NfL
NfLboxplot<-ggplot(Bioboxplot, aes(x=case_control_vaccine, y=NfL))+ #x axis is split by case status, y is NfL
  geom_boxplot(outlier.shape = NA)+ scale_y_continuous(trans='log10',limits = quantile(Bioboxplot$GFAP, c(0.000, 0.99), na.rm=TRUE))+ #log scale, stop outliers skewing entire plot
  geom_jitter(aes(colour=diagnostic_group))+theme_minimal(base_size = 20)+ #add individual points coloured by diagnostic group
  ggtitle("Nfl-L")+xlab("Group")+ylab("pg/mL")+ #axis titles
  labs(colour="Diagnostic group")+ #dots are coloured by diagnostic group
  geom_hline(yintercept = 0.317, color="darkslategrey", lty=5) #add LLOQ to plot if included in y axis
dummyNfLboxplot<-NfLboxplot +scale_colour_manual(values=colourb8) #add colours consistent with other plots
dummyNfLboxplot #display plot
ggsave("dummyNfLboxplot.pdf") #save plot in working directory
#GFAP LLOQ 0.933 - same process for GFAP but with updated LLOQ
head(Bioboxplot)
GFAPboxplot<-ggplot(Bioboxplot, aes(x=case_control_vaccine, y=GFAP))+
  geom_boxplot(outlier.shape = NA)+ scale_y_continuous(trans='log10',limits = quantile(Bioboxplot$GFAP, c(0.001, 0.995), na.rm=TRUE))+
  geom_jitter(aes(colour=diagnostic_group))+theme_minimal(base_size = 20)+
  ggtitle("GFAP")+xlab("Group")+ylab("pg/mL")+
  labs(colour="Diagnostic group")+
  geom_hline(yintercept = 0.933, color="darkslategrey", lty=5)
dummyGFAPboxplot<-GFAPboxplot +scale_colour_manual(values=colourb8)
dummyGFAPboxplot
ggsave("dummyGFAPboxplot.pdf")
#Tau LLOQ 0.114 - same process for Tau
Tauboxplot<-ggplot(Bioboxplot, aes(x=case_control_vaccine, y=Tau))+
  geom_boxplot(outlier.shape = NA)+ scale_y_continuous(trans='log10')+
  geom_jitter(aes(colour=diagnostic_group))+theme_minimal(base_size = 20)+
  ggtitle("Tau")+xlab("Group")+ylab("pg/mL")+
  labs(colour="Diagnostic group")+
  geom_hline(yintercept = 0.114, color="darkslategrey", lty=5)
dummyTauboxplot<-Tauboxplot +scale_colour_manual(values=colourb8)
dummyTauboxplot
ggsave("dummyTauboxplot.pdf")

#UCHL1 LLOQ 9.6 - and then for UCHL1
UCHL1boxplot<-ggplot(Bioboxplot, aes(x=case_control_vaccine, y=UCHL1))+
  geom_boxplot(outlier.shape = NA)+ scale_y_continuous(trans='log10',limits = quantile(Bioboxplot$GFAP, c(0.000, 0.99), na.rm=TRUE))+
  geom_jitter(aes(colour=diagnostic_group))+theme_minimal(base_size =20)+
  ggtitle("UCH-L1")+xlab("Group")+ylab("pg/mL")+
  labs(colour="Diagnostic group")+
  geom_hline(yintercept = 9.6, color="darkslategrey", lty=5)
dummyUCHL1boxplot<-UCHL1boxplot +scale_colour_manual(values=colourb8)
dummyUCHL1boxplot
ggsave("dummyUCHL1boxplot.pdf")

###put all together
#arrange into a single plot with a shraed legend
dummybioboxplots<-ggarrange(dummyNfLboxplot+ rremove("xlab"), dummyGFAPboxplot+ rremove("xlab"), dummyTauboxplot+ rremove("xlab"), dummyUCHL1boxplot+ rremove("xlab"), ncol=2, nrow=2,common.legend = TRUE, legend="bottom")
dummybioboxplots
ggsave("dummybioboxplots.pdf",width = 250, height = 210, units = "mm")

##boxplots stats ####
#Kruskal wallis comparing 3 groups for each biomarker
kruskal.test(NfL ~ case_control_vaccine, data=Bioboxplot) 
kruskal.test(GFAP ~ case_control_vaccine, data=Bioboxplot) 
kruskal.test(Tau ~ case_control_vaccine, data=Bioboxplot) 
kruskal.test(UCHL1 ~ case_control_vaccine, data=Bioboxplot) 

#Mann Whitney U
#NeuroCOVID vs COVID
#To compare NeuroCOVID vs COVID, remove normative data
BioboxplotCasecontrol<-Bioboxplot[Bioboxplot$case_control_vaccine!="Normative",]
#Drop empty factor levels
BioboxplotCasecontrol <- droplevels(BioboxplotCasecontrol)
#check
table(BioboxplotCasecontrol$case_control_vaccine)

#Mann Whitney U
wilcox.test(NfL ~ case_control_vaccine, data=BioboxplotCasecontrol)
wilcox.test(GFAP ~ case_control_vaccine, data=BioboxplotCasecontrol) 
wilcox.test(Tau ~ case_control_vaccine, data=BioboxplotCasecontrol) 
wilcox.test(UCHL1 ~ case_control_vaccine, data=BioboxplotCasecontrol) 

#Nfl- get test statistic, estimate (CI), and effect size for main text 
wilcox_test(BioboxplotCasecontrol, NfL ~ case_control_vaccine, detailed=T)
wilcox_effsize(BioboxplotCasecontrol, NfL ~ case_control_vaccine)

#GFAP get test statistic, estimate (CI), and effect size for main text 
wilcox_test(BioboxplotCasecontrol, GFAP ~ case_control_vaccine, detailed=T)
wilcox_effsize(BioboxplotCasecontrol, GFAP ~ case_control_vaccine)

#Tau get test statistic, estimate (CI), and effect size for main text 
wilcox_test(BioboxplotCasecontrol, Tau ~ case_control_vaccine, detailed=T)
wilcox_effsize(BioboxplotCasecontrol, Tau ~ case_control_vaccine)

#Compare NeuroCOVID vs normative
#remove COVID patients
BioboxplotNeuronorm<-Bioboxplot[Bioboxplot$case_control_vaccine!="COVID",]
#check
table(BioboxplotNeuronorm$case_control_vaccine)
#drop empty levels
BioboxplotNeuronorm<-droplevels(BioboxplotNeuronorm)

#Mann Whitney U for figure
wilcox.test(NfL ~ case_control_vaccine, data=BioboxplotNeuronorm)
wilcox.test(GFAP ~ case_control_vaccine, data=BioboxplotNeuronorm)
wilcox.test(Tau ~ case_control_vaccine, data=BioboxplotNeuronorm) 
wilcox.test(UCHL1 ~ case_control_vaccine, data=BioboxplotNeuronorm) 

#COVID vs normative - same process
BioboxplotCovidnorm<-Bioboxplot[Bioboxplot$case_control_vaccine!="NeuroCOVID",]
table(BioboxplotCovidnorm$case_control_vaccine)
BioboxplotCovidnorm<-droplevels(BioboxplotCovidnorm)

#Mann Whitney U
wilcox.test(NfL ~ case_control_vaccine, data=BioboxplotCovidnorm) 
wilcox.test(GFAP ~ case_control_vaccine, data=BioboxplotCovidnorm) 
wilcox.test(Tau ~ case_control_vaccine, data=BioboxplotCovidnorm) 
wilcox.test(UCHL1 ~ case_control_vaccine, data=BioboxplotCovidnorm) 

#Nfl- COVID vs normative - get test statistic, estimate (CI), and effect size for main text 
wilcox_test(BioboxplotCovidnorm, NfL ~ case_control_vaccine, detailed=T)
wilcox_effsize(BioboxplotCovidnorm, NfL ~ case_control_vaccine)

#GFAP- COVID vs normative - get test statistic, estimate (CI), and effect size for main text 
wilcox_test(BioboxplotCovidnorm, GFAP ~ case_control_vaccine, detailed=T)
wilcox_effsize(BioboxplotCovidnorm, GFAP ~ case_control_vaccine)

###End####

