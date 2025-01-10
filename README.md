# Work-related-autonomy-and-mental-health-replication-package
Replicates the results in "work-related autonomy and mental health"

_Data_

Understanding Society (UKHLS) data can be downloaded from the UK data service: https://ukdataservice.ac.uk/find-data/. You will need study number 6614. 

O*NET data are available at: https://www.onetcenter.org/db_releases.html. I used the August 2018 vintage Work Context File.

_R_scripts_

Run the scripts in order:
01_select_UKHLS_data.R operates on UKHLS files and constructs the main data set from them
02_bootstrap_oster_corrections.R draws from the data and caclulates a distribution of oster corrected coefficients. These are used to calculate standard errors
03_calculate_results.R estimates all the results and robustness tests in the paper

_Before_running_scripts_

You will need to check that all packages are installed. All the packages that need to be installed are named in the code at the start of each script which says "library(packagename)". You will need to install all packages by entering "install.packages("packagename")" into the console.
You will need to define the repositories by going to the part of the code that says:

#####################
#Define repositories#
#####################

and replacing underneath the file paths with the path to the repository where you store your data, and the repository where you want the graphs to be saved to.
