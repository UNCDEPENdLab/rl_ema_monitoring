#!/bin/bash

module load matlab/R2021a

subj_list=$(ls '/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA/Data_Raw/')
echo $subj_list
#subj_list=('440529' '540068')
#echo $subj_list
for i in ${subj_list[@]}
do
   subj=$i
   echo "Merging physio for subject"
   echo $subj
   sbatch -p smp -N 1 --mem 30g -n 1 -t 16:00:00 -c 1 --wrap 'matlab -nodisplay -nodesktop -nosplash -batch "subj=int2str('$subj'); disp(subj); trials_reward_pun(subj); exit"'
done
