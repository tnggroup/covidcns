# COVIDCNS README #

This is a repository for data cleaning scripts for the COVIDCNS study. <br>
Libraries used: sjlabelled 1.2.0, gtsummary 2.0.0, Amelia 1.8.2, tidyverse 2.0.0, rstatix 0.7.2, ggpubr 0.6.0, reshape2 1.4.4, ggrepel 0.9.5, RColorBrewer 1.1-3, wesanderson 0.3.7, gridExtra 2.3, ggpattern 1.1.1, rcompanion 2.4.36, ComplexHeatmap 1.10.2, circlize 0.4.16, caret 6.0-94, heatmaply 1.5.0, MASS 7.3-61, misty 0.6.5, metan 1.18.0 (these are detailed in the scripts at length). <br>
Requires R 4.1 or newer (R 3.4 or newer if excluding code blocks using gtsummary) <br>
All user-defined functions required to run the script are included within the repository. <br>
This is not compiled software so there are no dependencies and it does not need to be installed. <br>
Typical run-time on a desktop computer for the wrapper script is around 300 seconds. <br>

## Structure ##

The repository is laid out as follows: <br>

- Cognition analysis
- assessment_status
- baseline
- clin_neuro
- cognitron_pipeline
- core_neuro
- fbc
- functions
- joining_wrapper
- knitted_scripts
- mh_case_report
- moca
- other_data_requests
- ref_ranges
- standardised


The folders baseline, clin_neuro, core_neuro, fbc, followups, mh_case_report, and moca contain cleaning scripts for data from each of the surveys sent out to participants, divided into one script per questionnaire.

The assessment_status folder contains a cleaning script for the data taken from the REDCAP website database.

The cognitron_pipeline folder contains scripts to clean each of the cognitron datasets. For a detailed explanation of the cleaning steps, please refer to the scripts themselves, as they are commented to explain the process throughout.

The standardised folder contains a template script used to create cleaning scripts for each of the surveys. The functions folder contains the functions that these scripts are dependent on, and the ref_ranges folder contains data on references ranges used in the cleaning process. For a detailed explanation of each of these, please refer to the template script or any of the individual cleaning scripts, they are in Rmd format and commented extensively to explain the entire process. In addition, each of the functions contains Roxygen2 format documentation.

The joining_wrapper folder contains scripts to automatically join cleaned data from multiple surveys into a single survey, to check this data for joining errors and to save it to an external location. It also contains scripts for variable extraction from joined, cleaned datasets. Again, all of these scripts are commented extensively. The wrapper script runs all of the individual cleaning scripts with a single execution.

The other_data_requests folder contains variable extraction scripts for papers other than the main paper, using the variable extraction script.

The Cognition analysis folder contains all analysis scripts used for the final analysis in the main paper: these scripts are commented in detail and at length. They can be run on a dummy dataset provided on request.

## Usage ##


Each of the cleaning scripts can be run individually, or they can all be run using the wrapper scripts.

In order to perform either of these, you will need to create a credentials folder (untracked by Git) in the scripts folder of the repository.

This folder must contain a credentials .R file called paths.R

This file must contain an object assignment for one object: ilovecovidcns, which is a character string of the path pointing to the directory where you wish to store your data (it will not default to the current working directory).

You must also ensure that your data structure for raw and clean data storage matches the original structure (you can check this by looking at the paths for import and export at the top and bottom of the cleaning scripts).

To use the variable extraction, you need a .txt file of the required variables, with one variable per line in quotation marks.


## Contributors ##
Zain Ahmad <br>
Christopher Huebel <br>
Valentina Giunchiglia <br>
Chelsea Malouf <br>
Saakshi Kakar <br>
