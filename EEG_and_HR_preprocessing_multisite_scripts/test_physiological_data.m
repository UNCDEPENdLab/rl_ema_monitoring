%these are preprocessings scripts for HR and EEG data. Please place all of the unproccessed data in a
%folder named 'Data_Raw' in the same location where all of the Matlab
%functions are.
% 2022-10-05 AndyP added site
clear; clc; close all
site = 'HUJI';
%output folder-the folder where all of the results will be saved.
if strcmp(site,'HUJI')
    output_folder = 'G:\My Drive\Lab\EEG_for_nadav\single';
    %read subjects codes from an excel file (each subject code in a seperate row):
    names = string(xlsread('G:\My Drive\Lab\EEG_HR_new_scripts\single.xlsx')); 
%elseif strcmp(site,'unc')
%    output_folder = '';
%    names = '';
%elseif strcmp(site,'pitt')
%    output_folder = '~/Momentum';
%    names = {'440366'};
end
%loop over all subjects, run preprocessing, and save the relvant EEG and HR data of
%each subject in the output folder:
for i = 1:length(names)
    close all;
    name = names{i} ;
    out_dir = fullfile(output_folder, 'Data_Processed',['subject_' name]);
    if exist(out_dir,'dir')==0
        mkdir(out_dir) %the mereged physio file will be saved in this folder
    end
    %find indices of reward, punishment and neutral cotcomes and save them such that they correspond with the EEG and HR indices:
    trials_reward_pun(output_folder, name, site);
    %merge all physio to one file:
    delete_duplicates_and_merge_physio(output_folder,name,site);
    %EEGanalysis_test: this function performs preprocessing for each subject
    %and saves a mat file (a seprate file for each subject) that contains "epoch data" (the signal during each trial in each electrode)
    %;classifications of trials (good/bad signal) across different electrodes configurations and more information we might use in the future.
    EEGanalysis_test(output_folder, name, site);
    %getHRperOutcome outputs: HRoutcome_filtered-HR signal during good trials,
    %HRoutcome_all-HR signal during all trials, stats-summary of good/bad
    %trials, HR_percent-percentage of trials with good signal;HR-more
    %information we might use in the future:
    getHRperOutcome(output_folder, name, site);
    %optional: subset and keep only clean trials (for example, only for trials with good signal in all of the electrodes):
    %reward_trials = setdiff(reward_trials,find(ind_na==1));
    %pun_trials = setdiff(pun_trials,find(ind_na==1));
    %neu_trials = setdiff(neu_trials,find(ind_na==1));
end



