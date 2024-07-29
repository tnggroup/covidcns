# COVIDCNS README #

This is a repository for data cleaning scripts for the COVIDCNS study. <br>


## Structure ##

The repository is laid out as follows: <br>

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


The folders baseline, clin_neuro, core_neuro, fbc, mh_case_report, and moca contain cleaning scripts for data from each of the surveys sent out to participants, divided into one script per questionnaire.

The assessment_status folder contains a cleaning script for the data taken from the REDCAP website database.

The cognition_pipeline folder contains scripts to clean each of the cognition datasets. For a detailed explanation of the cleaning steps, please refer to the scripts themselves, as they are commented to explain the process throughout.

The standardised folder contains a template script used to create cleaning scripts for each of the surveys. The functions folder contains the functions that these scripts are dependent on, and the ref_ranges folder contains data on references ranges used in the cleaning process. For a detailed explanation of each of these, please refer to the template script or any of the individual cleaning scripts, they are in Rmd format and commented extensively to explain the entire process. In addition, each of the functions contains Roxygen2 format documentation.

The joining_wrapper folder contains scripts to automatically join cleaned data from multiple surveys into a single survey, to check this data for joining errors and to save it to an external location. It also contains scripts for variable extraction from joined, cleaned datasets. Again, all of these scripts are commented extensively. The wrapper script runs all of the individual cleaning scripts with a single execution.

The other_data_requests folder contains variable extraction scripts for papers other than the main paper, using the variable extraction script.

## Usage ##

Using the repository: <br>

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
