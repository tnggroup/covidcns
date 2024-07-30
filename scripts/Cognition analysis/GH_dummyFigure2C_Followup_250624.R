## COVID-CNS cognition analysis
## Author Greta Wood 
## Input: Output from Script 1 (dataframe including NeuroCOVID, COVID and normative data including biomarkers)
## Output: Figure 2B: Recovery trajectories - graphs underlying this tile
#(Change in Global Deviation from Expected scores between 3 assessment) 

# clear environment
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

#load data
Cogbiomerg<- readRDS("Dummycogbiomerg_norms3_190723.rds") #master data

#Follow-up data ####
summary(Cogbiomerg$Composite_global)
summary(Cogbiomerg$Composite_global_f1)
summary(Cogbiomerg$Composite_global_f2)

data_case_positiveControls=subset(Cogbiomerg,(Cogbiomerg$case_control_vaccine=="Case: COVID-19 positive (i.e. neurological or psychiatric complication)"|Cogbiomerg$case_control_vaccine=="Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)"))

# Rename the levels in the 'case_control_vaccine' column
data_case_positiveControls$case_control_vaccine <- factor(data_case_positiveControls$case_control_vaccine, 
                                                          levels = c("Case: COVID-19 positive (i.e. neurological or psychiatric complication)", 
                                                                     "Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)"),
                                                          labels = c("NeuroCOVID", "COVID"))

#check renaming
table(data_case_positiveControls$case_control_vaccine)

#subset data required for graphs
Followupdata<-select(data_case_positiveControls, ID,case_control_vaccine,diagnostic_group,Composite_global,Composite_global_f1,Composite_global_f2)

#rename columns
colnames(Followupdata)<-c("ID","Case group","Diagnostic group","Post-acute assessment","Follow-up 1","Follow-up 2")

table(Followupdata$`Case group`)
###FU graph separate case/control
##subset
Followupcases<-Followupdata[(Followupdata$`Case group`=="NeuroCOVID"),]
Followupcontrols<-Followupdata[(Followupdata$`Case group`=="COVID"),]

#NeuroCOVID group ####
#NeuroCOVID - comparison between Post-acute assessment and follow-up 1
FUcasespairedB1<-ggpaired(Followupcases, cond1 = "Post-acute assessment", cond2 = "Follow-up 1", id="ID",
                          color = "black", #colour of dots
                          width = 0.5,
                          point.size = 1.2,
                          line.size = 0.4,
                          line.color = "Case group", 
                          palette=c("cornflowerblue"), #line colour
                          linetype = "solid",
                          xlab = "Timepoint",
                          ylab = "Global Composite DfE score (SD units)",
                          #ylim=c(-6,1.6), y limit specific to real data
                          short.panel.labs = TRUE,
                          font.label = list(size = 11, color = "black"),)
#Display graph
FUcasespairedB1
#Add y intercept and change y scale breaks
dummyFUcasespairedB1<-FUcasespairedB1+geom_hline(yintercept = 0, color="darkslategrey", lty=5)+ scale_y_continuous(breaks=c(-6,-5,-4,-3, -2,-1, 0,1,2))
#redisplay
dummyFUcasespairedB1
#save the graph
ggsave("dummyFUcasespairedB1.pdf")

####Now comoparing F1 to F2
FUcasespaired12<-ggpaired(Followupcases, cond1 = "Follow-up 1", cond2 = "Follow-up 2", id="ID",
                          color = "black",
                          width = 0.5,
                          point.size = 1.2,
                          line.size = 0.4,
                          line.color = "Case group",
                          palette=c("cornflowerblue"),
                          linetype = "solid",
                          ylab = "Global Composite DfE score (SD units)",
                          #ylim=c(-6,1.6), 
                          facet.by = NULL,
                          panel.labs = NULL,
                          short.panel.labs = TRUE,
                          font.label = list(size = 11, color = "black"),)
dummyFUcasespaired12<-FUcasespaired12+geom_hline(yintercept = 0, color="darkslategrey", lty=5)+ scale_y_continuous(breaks=c(-6,-5,-4,-3, -2,-1, 0,1,2))
dummyFUcasespaired12
ggsave("dummyFUcasespaired12.pdf", width = 297, height = 210, units = "mm")

##casestogether
dummyFollowupcases3time<-ggarrange(dummyFUcasespairedB1,dummyFUcasespaired12,ncol=2, nrow=1,common.legend = TRUE, legend="top")
dummyFollowupcases3time
ggsave("Followupcases3time.pdf",width = 297, height = 150, units = "mm")

#COVID group ####
#Post acute assessment vs follow-up 1
FUcontrolspairedB1<-ggpaired(Followupcontrols, cond1 = "Post-acute assessment", cond2 = "Follow-up 1", id="ID",
                          color = "black",
                          width = 0.5,
                          point.size = 1.2,
                          line.size = 0.4,
                          line.color = "Case group",
                          palette=c("darkred"),
                          linetype = "solid",
                          xlab = c("Timepoint"),
                          ylab = "Global Composite DfE score (SD units)",
                          #ylim=c(-6,1.6),
                          short.panel.labs = TRUE,
                          font.label = list(size = 11, color = "black"),)
dummyFUcontrolspairedB1<-FUcontrolspairedB1+geom_hline(yintercept = 0, color="darkslategrey", lty=5)+ scale_y_continuous(breaks=c(-6,-5,-4,-3, -2,-1, 0,1,2))

ggsave("dummyFUcontrolspairedB1.pdf")

####F1 to F2
FUcontrolspaired12<-ggpaired(Followupcontrols, cond1 = "Follow-up 1", cond2 = "Follow-up 2", id="ID",
                          color = "black",
                          width = 0.5,
                          point.size = 1.2,
                          line.size = 0.4,
                          line.color = "Case group",
                          palette=c("darkred"),
                          linetype = "solid",
                          ylab = "Global Composite DfE score (SD units)",
                          #ylim=c(-6,1.6),
                          facet.by = NULL,
                          panel.labs = NULL,
                          short.panel.labs = TRUE,
                          font.label = list(size = 11, color = "black"),)
dummyFUcontrolspaired12<-FUcontrolspaired12+geom_hline(yintercept = 0, color="darkslategrey", lty=5)+ scale_y_continuous(breaks=c(-6,-5,-4,-3, -2,-1, 0,1,2))
ggsave("dummyFUcontrolspaired12.pdf", width = 297, height = 210, units = "mm")

#controls together
dummyFollowupcontrols3time<-ggarrange(dummyFUcontrolspairedB1,dummyFUcontrolspaired12,ncol=2, nrow=1,common.legend = TRUE, legend="top")
dummyFollowupcontrols3time
ggsave("Followupcontrols3time.pdf",width = 297, height = 150, units = "mm")

print(colnames(Followupdata))

#Statistics for graphs ####

#Mann Whitney U (i.e. treating samples as independent)
###recovery all together
wilcox.test(Followupdata$`Post-acute assessment`, Followupdata$`Follow-up 1`) 
wilcox.test(Followupdata$`Follow-up 1`, Followupdata$`Follow-up 2`) 
wilcox.test(Followupdata$`Post-acute assessment`, Followupdata$`Follow-up 2`) 

### Cases Recovery
wilcox.test(Followupcases$`Post-acute assessment`, Followupcases$`Follow-up 1`) 
wilcox.test(Followupcases$`Follow-up 1`, Followupcases$`Follow-up 2`) 
wilcox.test(Followupcases$`Post-acute assessment`, Followupcases$`Follow-up 2`) 

### Controls Recovery
wilcox.test(Followupcontrols$`Post-acute assessment`, Followupcontrols$`Follow-up 1`) 
wilcox.test(Followupcontrols$`Follow-up 1`, Followupcontrols$`Follow-up 2`) 
wilcox.test(Followupcontrols$`Post-acute assessment`, Followupcontrols$`Follow-up 2`) 

#Paired data only
###recovery all together
wilcox.test(Followupdata$`Post-acute assessment`, Followupdata$`Follow-up 1`, paired=T) 
wilcox.test(Followupdata$`Follow-up 1`, Followupdata$`Follow-up 2`, paired=T) 
wilcox.test(Followupdata$`Post-acute assessment`, Followupdata$`Follow-up 2`, paired=T) 

### Cases Recovery
wilcox.test(Followupcases$`Post-acute assessment`, Followupcases$`Follow-up 1`, paired=T) 
wilcox.test(Followupcases$`Follow-up 1`, Followupcases$`Follow-up 2`, paired=T) 
wilcox.test(Followupcases$`Post-acute assessment`, Followupcases$`Follow-up 2`, paired =T) 

### Controls Recovery
wilcox.test(Followupcontrols$`Post-acute assessment`, Followupcontrols$`Follow-up 1`, paired=T) 
wilcox.test(Followupcontrols$`Follow-up 1`, Followupcontrols$`Follow-up 2`, paired=T) 
wilcox.test(Followupcontrols$`Post-acute assessment`, Followupcontrols$`Follow-up 2`, paired=T) 

#Graphs cropped to overlap and then statistical markers added outside of R

# End ####