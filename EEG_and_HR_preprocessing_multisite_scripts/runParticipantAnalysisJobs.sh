#!/bin/bash

module load matlab/R2021a

# Define the directories
# base_path="/ix1/adombrovski/ix/DNPL_DataMesh/Data/Momentum_EMA/Data_Raw"
# preprocessedDir="/ix1/adombrovski/ix/DNPL_DataMesh/Data/Momentum_EMA"

# Validation only: 
base_path="/ix1/adombrovski/DNPL_DataMesh/Data/MUSE_Validation/BIOSEMI_bdf"
preprocessedDir="/ix1/adombrovski/DNPL_DataMesh/Data/MUSE_Validation"

# Get the list of subdirectories in the mat_files_csv_and_MUSE_db directory
subj_list=$(find $base_path -mindepth 1 -maxdepth 1 -type d)

# Directory for logs
# logdir="./Logs/2025_06_17_RRI_feedback_150cHz_PanTompkins_run2Sorted"
# logdir="./Logs/2025_06_16_choice_eeg_only_25tp"
logdir="./Logs/2025_07_28_BiosemiCorrectedAlignment_NoPreprocCleaning"
jobName="Momentum_Validation"

# Create the log directory 
mkdir -p "$logdir"

# Initialize a counter
# counter=0

# RRI pending: 
# pendingSubjects=(540212)

# Muse validation errors
# pendingSubjects=(221595)

# Loop through each subject directory
for subj_dir in ${subj_list[@]}
do
   # Extract the subject directory name
   subj=$(basename $subj_dir)

   # Increment the counter
#    ((counter++))

    # Skip the first participant
#    if [ $counter -eq 1 ]; then
#     continue 
#    fi

   # Check if the subject ID is in the pending subjects array
    # if [[ ! " ${pendingSubjects[@]} " =~ " ${subj} " ]]; then
    #     continue  # Skip this directory if it's not in the list
    # fi
   
   echo "Processing subject $subj"

   # Submit a batch job with SLURM

    sbatch -N 1 --mem 40g -n 1 -t 5:00:00 -c 1 \
    --output="${logdir}/id_${subj}_j_%j.out" --error="${logdir}/id_${subj}_j_%j.err" \
    --job-name="id${subj}_${jobName}" \
    --wrap "matlab -nodisplay -nodesktop -nosplash -batch \" \
    addpath('/ix1/adombrovski/lab_resources/fieldtrip-20240110'); \
    addpath(genpath('./ObjectOriented')); \
    addpath('/ix1/adombrovski/lab_resources/rl_ema_monitoring/EEG_and_HR_preprocessing_multisite_scripts'); \
    addpath('/ix1/adombrovski/lab_resources/eeglab2024.2'); \
    runMomentumParticipantAnalysis('$subj', '$base_path','$preprocessedDir'); \
    exit\""
    
done