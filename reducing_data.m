function [DATA]=reducing_data(name, DATA, first_block, last_block)
%% read schedule file
filename = fullfile(pwd,'Data_Raw',['subject_' name],[name '_schedule.db']);
db = sqlite(filename);   
temp = cell2mat(fetch(db, 'SELECT stim_time, block FROM trials'));
%indentify the feedback time in the first trial of the first block and the last trial of the last block
%(+- 15000 ms)
first_trial_onset=temp(find(temp(:,2) == first_block, 1, 'first'),1)-15000;
last_trial_onset=temp(find(temp(:,2) == last_block, 1, 'last'),1)+15000;
% deletes rows before and after chosen onsets
DATA(find(((((cell2mat(DATA(:,1)))))<first_trial_onset)==1),:)=[];
DATA(find(((((cell2mat(DATA(:,1)))))>last_trial_onset)==1),:)=[];

