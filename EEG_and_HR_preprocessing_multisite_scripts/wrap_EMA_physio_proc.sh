#!/bin/bash

module load matlab/R2021a

subj_list=$(ls '/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA/Data_Raw/')
echo $subj_list

for i in ${subj_list[@]}
do
   subj=$i
   echo "Merging physio for subject"
   echo $subj
   sbatch -p htc -N 1 --mem 40g -n 1 -t 4:00:00 -c 1 --wrap 'matlab -nodisplay -nodesktop -nosplash -batch "subj=int2str('$subj'); disp(subj); rehash toolboxcache; delete_duplicates_and_merge_physio(subj); EEGanalysis_test(subj); exit"'
done
