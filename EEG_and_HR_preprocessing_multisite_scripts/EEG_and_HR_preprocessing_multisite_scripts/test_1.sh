#!/bin/bash

subj_list=('440529' '440522' '440366' '440554' '440570' '440571' '440594' '540060' '540068' '540085' '540091')
echo $subj_list
for i in ${subj_list[@]}
do
   subj=$i
   echo $subj
done
