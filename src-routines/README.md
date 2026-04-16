# E.Chung, 2026
This folder contains the code to get transition entropy (TE) scores. It contains 
three scripts.

## functions_to_compute_routine_scores.R
This script contains three functions, which are use to generate a counts matrix,
a transition probability matrix and to calculate entropy.

## run_routines.R
This is a wrapper script used to execute the functions required to get TE. It 
generates csv file with the TE scores for each task and each subject. To run this 
script, you will need to have generated a dataframe containing the event data
(i.e., recording every door selection a participant has made) from the run_wrangling.R
script in the src folder of this repository.

## plot_entropy_average.R
This script generates a boxplot to visualise TE scores for each participant, 
averaged over condition (i.e., task). To run this script, you will need to use 
the dataframe generated from run_routines.R in this folder (src-routines).