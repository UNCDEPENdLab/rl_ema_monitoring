#!/bin/bash

module load matlab/R2023a

# Paths
# processedPath="/ix1/adombrovski/ix/DNPL_DataMesh/Data/Momentum_EMA/DataProcessed/AlignedEvent/TF/RestingState/ByParticipant"
# processedPath="/ix1/adombrovski/ix/DNPL_DataMesh/Data/Momentum_EMA/DataProcessed/AlignedEvent/TF/feedback_time/ByParticipant"
# processedPath="/ix1/adombrovski/ix/DNPL_DataMesh/Data/Momentum_EMA/DataProcessed/AlignedEvent/TF_Induced/feedback_time/ByParticipant"
processedPath="/ix1/adombrovski/ix/DNPL_DataMesh/Data/Momentum_EMA/DataProcessed/AlignedEvent/TF/feedback_time/ByParticipant"
# processedPath="/ix1/adombrovski/ix/DNPL_DataMesh/Data/Momentum_EMA/DataProcessed/AlignedEvent/EEG/choice_time/ByParticipant"
# processedPath="/ix1/adombrovski/ix/DNPL_DataMesh/Data/Momentum_EMA/DataProcessed/AlignedEvent/RRI/feedback_time/ByParticipant"
# processedPath="/ix1/adombrovski/DNPL_DataMesh/Data/MUSE_Validation/DataProcessed/Validation/EEG/feedback_time/ByParticipant"
logdir="./Logs/2025_11_05_mergeFeedbackTF_DTW_part2"

mkdir -p "$logdir"

#------TF ------#
# Total bins
freqMax=30
timeMax=69
# time_bin=0
session_bin=0
# sessionMax=15 
sectionsToMerge="{'temp', 'front'}" #{''} # 
eventName='feedback_time'
jobName="MmtmTF_dtwMerge"

# pendingTBins=(69 74 78 65)


for (( freq_bin=19; freq_bin<=freqMax; freq_bin++ )); do
# for freq_bin in alpha beta theta delta; do

    for (( time_bin=1; time_bin<=timeMax; time_bin++ )); do
    # for (( session_bin=1; session_bin<=sessionMax; session_bin++ )); do

        # Check if the tBin is in the pending array
        # if [[ ! " ${pendingTBins[@]} " =~ " ${time_bin} " ]]; then
        #     continue  # Skip this bin if it's not in the list
        # fi

        echo "Processing bins freq: $freq_bin time: $time_bin"

        # Submit a batch job with SLURM
        sbatch -N 1 --mem 80g -n 1 -t 1:00:00 -c 1 \
        --output="${logdir}/fb${freq_bin}_tb${time_bin}_%j.out" --error="${logdir}/fb${freq_bin}_tb${time_bin}_%j.err" \
        --job-name="${jobName}_fb${freq_bin}_tb${time_bin}" \
        --wrap "matlab -nodisplay -nodesktop -nosplash -batch \" \
        addpath(genpath(fullfile(fileparts(pwd),'MATLAB'))); \
        mergeParticipantFiles('$processedPath','$eventName', $time_bin,'byTimepoints',$freq_bin,$session_bin,${sectionsToMerge}); \
        exit\""   
         
    done
    
done


# ------- feedback_time--------#
## Runtime ~15 mins (141 bins 40g 1 core) 
# processedPath="/ix1/adombrovski/ix/DNPL_DataMesh/Data/Momentum_EMA/DataProcessed/EEG/feedback_time/ByParticipant"

# timeMax=141 

# for (( time_bin=1; time_bin<=timeMax; time_bin++ )); do

#     echo "Processing time bin: $time_bin"

#     # Submit a batch job with SLURM
#     sbatch  -p htc -N 1 --mem 40g -n 1 -t 0:40:00 -c 1 \
#     --output="${logdir}/%j_tb${time_bin}.out" --error="${logdir}/%j_tb${time_bin}.err" \
#     --wrap "matlab -nodisplay -nodesktop -nosplash -batch \" \
#     addpath('/ix1/adombrovski/lab_resources/fieldtrip-20240110'); \
    # addpath(genpath(fullfile(fileparts(pwd),'MATLAB'))),\
#     addpath('/ix1/adombrovski/lab_resources/rl_ema_monitoring/EEG_and_HR_preprocessing_multisite_scripts'); \
#     addpath('/ix1/adombrovski/lab_resources/eeglab2024.2'); \
#     mergeParticipantFiles('$processedPath','feedback_time',$time_bin,'byTime'); \
#     exit\""
# done