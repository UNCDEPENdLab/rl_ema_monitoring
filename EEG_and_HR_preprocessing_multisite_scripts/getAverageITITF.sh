#!/bin/bash

module load matlab/R2021a

# Define the base path
base_path="/ix1/adombrovski/ix/DNPL_DataMesh/Data/Momentum_EMA/Data_Raw"
outputPath="/ix1/adombrovski/DNPL_DataMesh/Data/MMTM/TF_Analysis"

# Get the list of files
participant_list=$(find $base_path -mindepth 1 -maxdepth 1 -type d)

# Initialize a counter
# counter=0

# Loop through each file
for file in $participant_list; do
    
    # ((counter++))

    # # Check if we've already processed one participant
    # if [ $counter -gt 1 ]; then
    #     break  # Exit the loop after processing the first participant
    # fi
   echo "Processing participant $file"

   # Submit a batch job with SLURM
    sbatch  -p htc -N 1 --mem 40g -n 1 -t 72:00:00 -c 1 --wrap " \
    matlab -nodisplay -nodesktop -nosplash -batch \" \
    addpath('/ix1/adombrovski/lab_resources/fieldtrip-20240110'); \
    addpath('/ix1/adombrovski/lab_resources/rl_ema_monitoring/EEG_and_HR_preprocessing_multisite_scripts'); \
    addpath('/ix1/adombrovski/lab_resources/eeglab2024.2'); \
    getITIaverage('$file','$outputPath'); \
    exit\""

done
