#!/bin/bash

module load matlab/R2021a

# Define the base path
base_path="/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA"

# Get the list of subdirectories in the Data_Raw directory
subj_list=$(find $base_path/Data_Raw -mindepth 1 -maxdepth 1 -type d)

# Initialize a counter
# counter=0

# Loop through each subject directory
for subj_dir in ${subj_list[@]}
do

   # Increment the counter
   # ((counter++))

   # Check if we've already processed one subject
   # if [ $counter -gt 1 ]; then
   #     break  # Exit the loop after processing the first subject
   # fi

   # Extract the subject directory name
   subj=$(basename $subj_dir)

   echo "Processing subject $subj"

   # Construct the paths using the base path variable
   pathToPhysioFiles="$base_path/Data_Raw/${subj}/physio/"
   pathToScheduleFile=$(find $subj_dir/schedule -maxdepth 1 -type f -name "*_${subj}_schedule.db")
   savePath="$base_path/Data_Processed/"

   # echo "matlab -nodisplay -nodesktop -nosplash -batch \"alignRRI('$pathToPhysioFiles', '$pathToScheduleFile', '$savePath'); exit\""


   # Submit a batch job with SLURM
   sbatch -p htc -N 1 --mem 40g -n 1 -t 8:00:00 -c 1 --wrap "matlab -nodisplay -nodesktop -nosplash -batch \"alignRRI('$pathToPhysioFiles', '$pathToScheduleFile', '$savePath'); exit\""
done