#!/bin/bash

module load matlab/R2021a

# Define the base path
base_path="/ix1/adombrovski/DNPL_DataMesh/Data/MUSE_Validation"

# Get the list of subdirectories in the mat_files_csv_and_MUSE_db directory
subj_list=$(find $base_path/mat_files_csv_and_MUSE_db -mindepth 1 -maxdepth 1 -type d)

# pendingSubjects=("440774" "440790" "440807" "440812" "440847" "440869" "440871" "440883")  
# pendingSubjects=("440754")  
# pendingSubjects=("440883")  
# pendingSubjects=("440812")  

# Initialize a counter
#counter=0

# Loop through each subject directory
for subj_dir in ${subj_list[@]}
do
   # Extract the subject directory name
   subj=$(basename $subj_dir)

   # Increment the counter
   #((counter++))

    # Skip the first participant
   #if [ $counter -eq 1 ]; then
   #  continue 
   #fi

   # Check if the subject ID is in the pending subjects array
    # if [[ ! " ${pendingSubjects[@]} " =~ " ${subj} " ]]; then
    #     continue  # Skip this directory if it's not in the list
    # fi

   
   echo "Processing subject $subj"

   # Submit a batch job with SLURM

    sbatch  -p htc -N 1 --mem 40g -n 1 -t 8:00:00 -c 1 --wrap " \
    matlab -nodisplay -nodesktop -nosplash -batch \" \
    addpath('/bgfs/adombrovski/lab_resources/fieldtrip-20240110'); \
    addpath('/bgfs/adombrovski/lab_resources/rl_ema_monitoring/TF_Scripts'); \
    addpath('/bgfs/adombrovski/lab_resources/eeglab2024.2'); \
    EEGanalysis_test_Validation('$subj', '$base_path',true); \
    exit\""

done