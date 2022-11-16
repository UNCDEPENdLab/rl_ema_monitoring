% Run this function only once, to prepare a subject's physio files for further processing
% INPUT: name - character string indicating subejct (e.g., '101')
% 2022-10-05 AndyP modified
function delete_duplicates_and_merge_physio(output_folder,name,site)
    if nargin<3 %if output folder wasn't an output
       output_folder = '/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA';
    end
    %site = 'pitt';
    if ~strcmp(site,'HUJI')
	addpath('/bgfs/adombrovski/lab_resources/rl_ema_monitoring/EEG_and_HR_preprocessing_multisite_scripts');
	out_dir = fullfile(output_folder,'Data_Processed',['subject_' name]);
        if exist(out_dir,'dir')==0
            mkdir(out_dir);
        end
    end
    if ~strcmp(site,'HUJI')
        path = fullfile(output_folder,'Data_Processed',['subject_' name]);
    else
        path = fullfile(pwd,'Data_Raw',['subject_' name]);
    end
    if strcmp(site,'HUJI')
        Utilities.deleteDuplicates(path, name); 
        Utilities.replaceHebrewCharacters(path, name);
    end
    if ~strcmp(site,'HUJI')
        Utilities.mergeDatabases(output_folder, path, name, 'physio');
    else
        Utilities.mergeDatabasesHUJI(path, name, 'physio');
    end
end
