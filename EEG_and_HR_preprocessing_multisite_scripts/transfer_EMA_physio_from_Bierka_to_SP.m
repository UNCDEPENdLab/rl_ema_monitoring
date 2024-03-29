% 2023-06-21 AndyP
% copy Momentum EMA files from Bierka to SP, remove raw data

data_in = '/Volumes/bierka_root/datamesh/PROC/MMTM/Momentum_EMA/Data_Processed';
data_out = '/Users/andypapale/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Documents - Momentum/Momentum_EMA/2023-09-06-EEG';

cd(data_in);
subs = dir('*');
nS = length(subs);
A = [];
subj = [];
for iS = 1:nS
    if regexp(subs(iS).name,'subject_\d\d\d\d\d\d')
        disp(subs(iS).name);
        cd(subs(iS).name);
        outcome_trials = dir('*outcome_trials.mat');
        %HR = dir('*HR.mat');
        EEG = dir('*EEG1.mat');
        if length(EEG) > 0 & length(outcome_trials) > 0 %#ok<AND2,ISMT>
            if (~isempty(EEG.name) & ~isempty(outcome_trials.name)) %#ok<AND2> %& ~isempty(HR.name)) %#ok<AND2>
                %load(EEG.name);
                %save_str = strsplit(subs(iS).name,'_');
                %save(strcat(save_str{2},'_','EEG1.mat'),'best_single','best_three_config','best_two_config','EEG_percen_all','EEG_percen_best_three',...
                %    'EEG_percen_best_two','epoch_data','ind_na_all','ind_na_best_single','ind_na_best_three','ind_na_best_two',...
                %    'sampling_rate');
                %load(HR.name);
                %save(strcat(save_str{2},'_','HR1.mat'),'HRoutcome_all','HRoutcome_filtered','ind_na_HR','stats','HR_percen');
                if ~exist(strcat(data_out,'/',subs(iS).name))
                    mkdir(strcat(data_out,'/',subs(iS).name));
                end
                [status,msg] = copyfile(outcome_trials.name, strcat(data_out,'/',subs(iS).name));
                [status,msg] = copyfile(strcat(save_str{2},'_','EEG1.mat'),strcat(data_out,'/',subs(iS).name));
                %[status,msg] = copyfile(EEG.name,strcat(data_out,'/',subs(iS).name));
                %[status,msg] = copyfile(strcat(save_str{2},'_','HR1.mat'),strcat(data_out,'/',subs(iS).name));
                load(EEG.name);
                Ntotal = size(ind_na_all_feedback,1);
                Ngood_all = sum(~ind_na_all_feedback);
                perc_good=(Ngood_all/Ntotal)*100;
                A = cat(1,A,perc_good);
                subj0 = strsplit(subs(iS).name,'_');
                subj = cat(1,subj,subj0{2});
            else
                warning('No EEG file or no outcome_trials file %s',subs(iS).name);
            end
        end
    end
    cd(data_in);
end
