data_in = '/Volumes/bierka_root/datamesh/PROC/MMTM/Momentum_EMA/Data_Processed';


cd(data_in);
subs = dir('*');
nS = length(subs);
A = {};
subj = [];
perc_good = [];
for iS = 1:nS
    if regexp(subs(iS).name,'subject_\d\d\d\d\d\d')
        disp(subs(iS).name);
        cd(subs(iS).name);
        outcome_trials = dir('*outcome_trials.mat');
        %HR = dir('*HR.mat');
        EEG = dir('*EEG1.mat');
        if length(EEG) > 0 & length(outcome_trials) > 0 %#ok<AND2,ISMT>
            if (~isempty(EEG.name) & ~isempty(outcome_trials.name)) %#ok<AND2> %& ~isempty(HR.name)) %#ok<AND2>
                load(EEG.name);
                Ntotal = size(ind_na_all_feedback,1);
                Ngood_all = sum(~ind_na_all_feedback);
                perc_good0=(Ngood_all/Ntotal)*100;
                subj0 = strsplit(subs(iS).name,'_');
                subj = cat(1,subj,subj0{2});
                perc_good = cat(1,perc_good,perc_good0);
            else
                warning('No EEG file or no outcome_trials file %s',subs(iS).name);
            end
        end
    end
    cd(data_in);
end