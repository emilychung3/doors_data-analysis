# doors-data-analysis

2025
Contributors to this repo: Emily Chung, Kelly Garner, Lydia Barnes

This repository contains data analysis code for the project: Investigating the 
influence of practice in stable contexts on learning transfer
In this project, we examine how the stability of task contexts during learning 
influence learning transfer.
Participants learn to search for targets behind 20 small grey circles, arranged 
in a concentric circle with two layers. Depending on the border colour of the 
display, participants learn that animals are limited to appear behind 4 different 
locations, and hence, learn two distinct task-sets.

To manipulate the stability of task contexts, participants then practice searching 
for animals while switching between the two task-sets. Participants experience 
either a 5% or 30% chance of switching to the other house on each trial. Importantly, 
border colours are removed during this practice.

Participants then undergo a transfer test where they are assessed in their ability to: 
1. learn a task with completely novel task locations (transfer = 1)
2. transfer two doors from each of the previously learned task-set (transfer = 2)
3. transfer a full set of target locations from one of the two previously learned 
  task-sets (transfer = 3)


Data analysis code is split across the following folders:
src                         | script for getting task-jumps and general-errors during training 
                            | and accuracy, setting error during transfer test
src-learn                   | script for getting learning onset using the Maggi(2024) algorithm 
                            | during transfer test
src-routines                | script for getting entropy (routine variability) scores during 
                            | training
src-individual-differences  | script for getting individual difference scores on
                            | the explicit target memory test, spatial and verbal
                            | working memory tests and self-report responses to 
                            | questions about search strategy.

Each folder contains its own README.md
