% Run this function only once, to prepare a subject's physio files for further processing
% INPUT: name - character string indicating subejct (e.g., '101')
% 2022-10-05 AndyP modified
function delete_duplicates_and_merge_physio(name)
	output_folder = '/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA';
	site = 'pitt';
	addpath('/bgfs/adombrovski/lab_resources/rl_ema_monitoring/EEG_and_HR_preprocessing_multisite_scripts');
	out_dir = fullfile(output_folder,'Data_Processed',['subject_' name]);
	if exist(out_dir,'dir')==0
		mkdir(out_dir);
	end
    path = fullfile(output_folder,'Data_Processed',['subject_' name]);
    if strcmp(site,'isr')
        Utilities.deleteDuplicates(path, name); 
        Utilities.replaceHebrewCharacters(path, name);
    end
    Utilities.mergeDatabases(output_folder, path, name, 'physio');
end
