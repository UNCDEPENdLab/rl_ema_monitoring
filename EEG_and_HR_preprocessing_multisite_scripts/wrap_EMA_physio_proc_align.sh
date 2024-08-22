#!/bin/bash

module load matlab/R2021a

# Define the base path
base_path="/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA/"

# Get the list of subdirectories in the Data_Raw directory
subj_list=$(ls $base_path/Data_Raw/*/)

# Loop through each subject directory
for subj_dir in ${subj_list[@]}
do
   # Extract the subject directory name
   subj=$(basename $subj_dir)

   echo "Processing subject $subj"

   # Construct the paths using the base path variable
   pathToPhysioFile="$base_path/Data_Processed/subject_${subj}/${subj}_physio.db"
   pathToScheduleFile=$(find $subj_dir/schedule -type f -name "*_${subj}_schedule.db")
   savePath="$base_path/Data_Processed/subject_${subj}/"

   # Submit a batch job with SLURM
   sbatch \
   -p htc \             # Partition name
   -N 1 \               # Nodes
   --mem 40g \          # Memory
   -n 1 \               # Nb of tasks
   -t 16:00:00 \        # Time limit
   -c 1 \               # CPUs
   --wrap "matlab -nodisplay -nodesktop -nosplash -batch \"alignRRI('$pathToPhysioFile', '$pathToScheduleFile', '$savePath'); exit\""
done