#!/bin/bash

module load matlab/R2021a

subj_list=('540239' '540227' '540220' '540215' '540185' '540184' '540178' '440750' '440738' '440713' '440712' '440701' '440667' '440570' '440567')

#subj_list=$(ls '/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA/Data_Raw/')
echo $subj_list

for i in ${subj_list[@]}
do
   subj=$i
   echo "Generating outcome trials for subject"
   echo $subj
   sbatch -p htc -N 1 --mem 50g -n 1 -t 25:00:00 -c 1 --wrap 'matlab -nodisplay -nodesktop -nosplash -batch "subj=int2str('$subj'); disp(subj); trials_reward_pun(subj); exit"'
done