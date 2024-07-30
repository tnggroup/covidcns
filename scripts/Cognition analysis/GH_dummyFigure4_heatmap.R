## Figure 4: Heatmap author Greta Wood 
## Input: Output from Script 1 (dataframe including NeuroCOVID, COVID and normative data including biomarkers), imaging z score composites
## Output: Figure 4: Heatmap and unsupervised cluster analysis (Euclidean, complete) in full cohort (n=351) of cognitive tasks shaded by correlation (Spearman), including cognition (accuracy and inverse RT), clinical variables, biomarkers and neuroimaging. * p < 0.05, **p < 0.01, ***p < 0.001 adjusted for multiple comparisons (37x37 matrix, n=1,369) using false discovery rate approach. 
## DOI: 10.18129/B9.bioc.ComplexHeatmap

#load packages
library(tidyverse)
library(ComplexHeatmap)
library(rstatix)
library(circlize)
library(grid)
library(caret)
library(heatmaply)
##

#clear environment
rm(list = ls())

#load data
covidcns_cognitive_data <- readRDS("Dummycogbiomerg_norms3_190723.rds") #master data
neuroimcomps<-readRDS("dummyneuroimcomps.rds")  #neuroimaging composites

#merge these files
bigcorrdata<- merge(covidcns_cognitive_data, neuroimcomps, by = "ID", all.x = TRUE)

#check dimensions
dim(bigcorrdata)

# Diagnostic group
table(covidcns_cognitive_data$diagnostic_group)

####### task scores ##########
dim(covidcns_cognitive_data)

#Subset to include only NeuroCOVID and COVID patients
bigcorrdata=subset(bigcorrdata,(bigcorrdata$case_control_vaccine=="Case: COVID-19 positive (i.e. neurological or psychiatric complication)"|covidcns_cognitive_data$case_control_vaccine=="Control: COVID-19 positive (i.e. NO neurological or psychiatric complication)"))

#select tasks, BIB, neuroimaging and selfreport
# Define the list of columns to include
columns_to_include <- c(
  "rs_prospectiveMemoryWords_1_immediate_dfe",
  "rs_spatialSpan_dfe",
  "rs_manipulations2D_dfe",
  "rs_verbalAnalogies_dfe",
  "rs_prospectiveMemoryWords_1_delayed_dfe",
  "rs_TOL_dfe",
  "rs_prospectiveMemoryWords_1_immediate_RT_dfe",
  "rs_spatialSpan_RT_dfe",
  "rs_manipulations2D_RT_dfe",
  "rs_verbalAnalogies_RT_dfe",
  "rs_prospectiveMemoryWords_1_delayed_RT_dfe",
  "rs_TOL_RT_dfe",
  "rs_motorControl_RT_dfe",
  "NfL",
  "GFAP",
  "Tau",
  "UCHL1",
  "Total_Multimorbidity",
  "Total_ACB_Score",
  "fsidps_volume_total_composite_Zscore",
  "fsidps_thickness_total_composite_Zscore",
  "fsidps_volume_anteriorcingulatecortex_composite_Zscore",
  "fsidps_volume_entorhinalcortex_composite_Zscore",
  "fsidps_volume_parahippocampalgyrus_composite_Zscore",
  "fsidps_volume_superiortemporalgyrus_composite_Zscore",
  "fsidps_volume_insula_composite_Zscore",
  "fsidps_thickness_anteriorcingulatecortex_composite_Zscore",
  "fsidps_thickness_entorhinalcortex_composite_Zscore",
  "fsidps_thickness_orbitofrontalcortex_composite_Zscore",
  "fsidps_thickness_parahippocampalgyrus_composite_Zscore",
  "fsidps_thickness_superiortemporalgyrus_composite_Zscore",
  "fsidps_thickness_insula_composite_Zscore",
  "phq9.sum_score",
  "gad7.sum_score",
  "pcl5.sum_score",
  "dem.concerned_memory",
  "ncrf2_med.corticosteroid"
)

# Subset the dataset to include only the specified columns
task_score_data <- bigcorrdata %>% select(all_of(columns_to_include))

# Display the first few rows of the new dataset to verify the result
head(task_score_data)

#check dimensions
dim(task_score_data)

#check names
names(task_score_data)

colnames(task_score_data)

#inverse RT variables so that high score is good for accuracy and RT
task_score_data[,7:13]<- 0-(task_score_data[,7:13]) 

#make binary variables numeric
task_score_data$dem.concerned_memory_numeric[task_score_data$dem.concerned_memory=="Yes"]<-1 #make numeric
task_score_data$dem.concerned_memory_numeric[task_score_data$dem.concerned_memory=="No"]<-0
task_score_data$steroid_numeric[task_score_data$ncrf2_med.corticosteroid=="Yes"]<-1 #make numeric
task_score_data$steroid_numeric[task_score_data$ncrf2_med.corticosteroid=="No"]<-0

#remove original non-numeric columns
task_score_data<-task_score_data[,-(36:37)]

#recheck names
names(task_score_data)

#create vector of variable names for figure with prefix
Colnames=c("COG - Recognition memory (immediate)","COG - Spatial Span","COG - 2D Manipulations","COG - Verbal Analogies","COG - Recognition memory (delayed)","COG - Tower of London","COG - Recognition memory (immediate) RT","COG - Spatial Span RT","COG - 2D Manipulations RT","COG - Verbal Analogies RT","COG - Recognition memory (delayed) RT","COG - Tower of London RT", "COG - Motor Control RT",
           "BIB - NfL","BIB - GFAP","BIB - Tau", "BIB - UCHL1", 
           "Multimorbidity", "ACB Score",
           "MRI - Volume Composite",
           "MRI - Thickness Composite",
           "MRI - ACC volume",
           "MRI - ERC volume",
           "MRI - PHG volume",
           "MRI - STG volume",
           "MRI - Insula volume",
           "MRI - ACC thickness",
           "MRI - ERC thickness",
           "MRI - OFC thickness",
           "MRI - PHG thickness",
           "MRI - STG thickness",
           "MRI - Insula thickness",
           "SR - PHQ-9", "SR - GAD-7", "SR - PCL-5","SR - Memory concerns", "Steroid treatment"
)           
names(task_score_data) #check original names
colnames(task_score_data)<-Colnames #overwrite with new names

#check this has worked
names(task_score_data)

#all patients together - check correlation
cor1=cor(task_score_data, use = "pairwise.complete.obs")

#correlation matrix
cor_mat(
  task_score_data[,c(1:37)],
  vars = NULL,
  method = "spearman",
  alternative = "two.sided",
  conf.level = 0.95
);
# p value matrix
p_values_mat<-cor_pmat(
  task_score_data[,c(1:37)],
  vars = NULL,
  method = "spearman",
  alternative = "two.sided",
  conf.level = 0.95
);

#adjust p values
dim(p_values_mat)
#this turns p value matrix into vector, adjusts p values using p.adjust, then turns back into matrix
adjusted_p_val_mat<-matrix(p.adjust(as.vector(as.matrix(p_values_mat[,c(2:38)])), method='fdr'),ncol=37);
#heatmap with both
Heatmap(cor1);
col_fun = colorRamp2(c(-1, 0, 1), c("blue", "white", "red")) #colour spectrum
Heatmap(cor1, col=col_fun); #adjust colour spectrum
Heatmap(cor1, col=col_fun, rect_gp = gpar(col = "white", lwd = 2))  #adjust cell border
Heatmap(cor1, col=col_fun, rect_gp = gpar(col = "white", lwd = 2), 
        column_title = "Task scores", column_title_side = "bottom") #add column name
# add adjusted p values
Heatmap(cor1, col=col_fun, rect_gp = gpar(col = "white", lwd = 2), 
        column_title = "Task scores", column_title_side = "bottom",
        cell_fun = function(j, i, x, y, w, h, fill) {
          if (adjusted_p_val_mat[i, j] < 0.001) {
            grid.text("***", x, y)
          } else if (adjusted_p_val_mat[i, j] < 0.01) {
            grid.text("**", x, y)
          } else if (adjusted_p_val_mat[i, j] < 0.05) {
            grid.text("*", x, y)
          }
        });
# remove *** from correlations ==1
Heatmap(cor1, col=col_fun, rect_gp = gpar(col = "white", lwd = 2), 
        column_title = "Task scores, clinical data, biomarkers and neuroimaging", column_title_side = "bottom",
        cell_fun = function(j, i, x, y, w, h, fill) {
          if (cor1[i, j] ==1.0) {
            grid.text("", x, y)
          } 
            else if (adjusted_p_val_mat[i, j] < 0.001) {
              grid.text("***", x, y)
            } else if (adjusted_p_val_mat[i, j] < 0.01) {
              grid.text("**", x, y)
            } else if (adjusted_p_val_mat[i, j] < 0.05) {
              grid.text("*", x, y)
            }
          });
## make column names bigger
Heatmap(cor1, col=col_fun, rect_gp = gpar(col = "white", lwd = 2), 
        column_title = "Task scores, clinical data, biomarkers and neuroimaging", column_title_side = "bottom",
        column_names_gp = grid::gpar(fontsize = 10),
        row_names_gp = grid::gpar(fontsize = 10),
        cell_fun = function(j, i, x, y, w, h, fill) {
          if (cor1[i, j] ==1.000000000) {
            grid.text("", x, y)
          } 
          else if (adjusted_p_val_mat[i, j] < 0.001) {
            grid.text("***", x, y)
          } else if (adjusted_p_val_mat[i, j] < 0.01) {
            grid.text("**", x, y)
          } else if (adjusted_p_val_mat[i, j] < 0.05) {
            grid.text("*", x, y)
          }
        });

#save it
png(file="/Users/GretaKWood/Dropbox/My Mac (MacBook Air (2))/Documents/0- Covid-CNS/Neurocog/Data analysis/Final code and figures/dummycolours.png", )

## end #########

