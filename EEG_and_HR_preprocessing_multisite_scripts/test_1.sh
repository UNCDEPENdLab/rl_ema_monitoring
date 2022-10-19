#!/bin/bash

subj_list=$(ls '/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA/Data_Raw/')
echo $subj_list
for i in ${subj_list[@]}
do
   subj=$i
   echo $subj
done
