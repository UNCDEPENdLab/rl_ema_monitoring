%varibles that are required for single bins functions
clear; clc; close all

%data_folder='E:\EEG_HR_new_scripts\Momentum_EMA_bigger_window';
%names = string(xlsread('E:\EEG_HR_new_scripts\second_50.xlsx')); 

% name is going to come from the bash script
output_folder = '/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA';      

[data_TF_left_temp, data_TF_left_front, data_TF_right_front, data_TF_right_temp] = load_epoch_TF_US(name);

sampling_rate = 256.03;
feedback_time = (sampling_rate)*1.5; %assuming epoch data contains 769 samples

%subject_names = subject_names(1:(end-1));
save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_data_TF_left_temp']), 'data_TF_left_temp','sampling_rate','feedback_time', '-v7.3')
save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_data_TF_left_front']), 'data_TF_left_front','sampling_rate','feedback_time', '-v7.3')
save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_data_TF_right_temp']), 'data_TF_right_temp','sampling_rate','feedback_time', '-v7.3')
save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_data_TF_right_front']), 'data_TF_right_front','sampling_rate','feedback_time', '-v7.3')
