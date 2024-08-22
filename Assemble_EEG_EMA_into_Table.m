% 2023-06-22 AndyP
% Assemble Momentum EMA EEG into table


data_in = '/Volumes/bierka_root/datamesh/PROC/MMTM/Momentum_EMA/Data_Processed';

cd(data_in);
subs = dir('*');
nS = length(subs);
subj = [];
trial_data = [];
neu_fb0 = [];
pun_fb0 = [];
rew_fb0 = [];
EEG_data1 = [];
EEG_data2 = [];
EEG_data3 = [];
EEG_data4 = [];
ind_na_all0 = [];
ind_na_best_single0 = [];
ind_na_best_two0 = [];
ind_na_best_three0 = [];
EEG_percen_all0 = [];
EEG_percen_best_three0 = [];
EEG_percen_best_two0 = [];
best_single0 = [];
best_three0 = [];
best_two0 = [];
times0 = [];
stim10 = [];
stim20 = [];
for iS = 152:202
    if ~isempty(regexp(subs(iS).name,'subject_\d\d\d\d\d\d', 'once')) & isempty(regexp(subs(iS).name,'bug', 'once')) %#ok<AND2>
        disp(subs(iS).name);
        cd(subs(iS).name);
        subj_str = strsplit(subs(iS).name,'_');
        % outcome_trials = dir('*outcome_trials.mat'); often doesn't match
        filename = dir(strcat(fullfile('/Volumes/bierka_root/datamesh/RAW/Momentum_App/data/Subjects',subj_str{2},'schedule'),'/*schedule.db'));
        if ~isempty(filename)
            db = sqlite(strcat(filename(1).folder,'/',filename(1).name));
            trial_block_outcome_feedback = cell2mat(fetch(db, 'SELECT * FROM trials WHERE choice_time IS NOT NULL AND stim1>=0 AND stim2>=0 ORDER BY choice_time ASC'));
            % block, trial, stim1, stim2, feedback, choice, outcome,
            % stim_time, choice_time, feedback_time, scheduled_time
            stimuli = double(cell2mat(fetch(db, 'SELECT number, image,rank FROM stimuli')));
            stimrewpun = cell2mat(fetch(db,'SELECT reward, punishment FROM stimuli'));
            stimuli = cat(2,stimuli,stimrewpun);
            stim1temp = trial_block_outcome_feedback(:,3);
            stim2temp = trial_block_outcome_feedback(:,4);
            nT = size(trial_block_outcome_feedback,1);
            for iT=1:nT
                stim1_idx = find(stimuli(:,1)==stim1temp(iT,1),1,'first');
                stim10 = cat(1,stim10,stimuli(stim1_idx,:));
                stim2_idx = find(stimuli(:,1)==stim2temp(iT,1),1,'first');
                stim20 = cat(1,stim20,stimuli(stim2_idx,:));
            end
            reward_feedback_trials= find(trial_block_outcome_feedback(:,7)==1 & trial_block_outcome_feedback(:,5)==1);
            pun_feedback_trials= find(trial_block_outcome_feedback(:,7)==-1 & trial_block_outcome_feedback(:,5)==1);
            neu_feedback_trials= find(trial_block_outcome_feedback(:,7)==0 & trial_block_outcome_feedback(:,5)==1);
            EEG = dir('*EEG1.mat');
            if length(EEG) > 0  %#ok<ISMT>
                if ~isempty(EEG.name)
                    load(EEG.name);
                    nT_EEG = size(epoch_data,1);
                    if nT==nT_EEG
                        trial_data = cat(1,trial_data,trial_block_outcome_feedback);
                        subj = cat(1,subj,repmat(subj_str{1},[nT,1]));
                        EEG_data1 = cat(1,EEG_data1,epoch_data(:,:,1));
                        EEG_data2 = cat(1,EEG_data2,epoch_data(:,:,2));
                        EEG_data3 = cat(1,EEG_data3,epoch_data(:,:,3));
                        EEG_data4 = cat(1,EEG_data4,epoch_data(:,:,4));
                        times0 = cat(1,times0, repmat(linspace(-500,1500,513),[nT,1]));
                        neu_fb = zeros(nT,1);
                        pun_fb = zeros(nT,1);
                        rew_fb = zeros(nT,1);
                        neu_fb(neu_feedback_trials) = 1;
                        pun_fb(pun_feedback_trials) = 1;
                        rew_fb(reward_feedback_trials) = 1;
                        neu_fb0 = cat(1,neu_fb0,neu_fb);
                        pun_fb0 = cat(1,pun_fb0,pun_fb);
                        rew_fb0 = cat(1,rew_fb0,rew_fb);
                        ind_na_all0 = cat(1,ind_na_all0,ind_na_all);
                        ind_na_best_single0 = cat(1,ind_na_best_single0,repmat(ind_na_best_single,[nT,1]));
                        ind_na_best_two0 = cat(1,ind_na_best_two0,repmat(ind_na_best_two,[nT,1]));
                        ind_na_best_three0 = cat(1,ind_na_best_three0,repmat(ind_na_best_three,[nT,1]));
                        EEG_percen_all0 = cat(1,EEG_percen_all0,repmat(EEG_percen_all0,[nT,1]));
                        EEG_percen_best_three0 = cat(1,EEG_percen_best_three0,repmat(EEG_percen_best_three,[nT,1]));
                        EEG_percen_best_two0 = cat(1,EEG_percen_best_two0,repmat(EEG_percen_best_two,[nT,1]));
                    else
                        warning('nT=%d nT_EEG=%d subj %s',nT,nT_EEG,subj_str{2});
                    end
                end
            end
        end
        cd(data_in);
    end
end

writetable(table(EEG_data1),'EEG1_batch4.csv');
writetable(table(EEG_data2),'EEG2_batch4.csv');
writetable(table(EEG_data3),'EEG3_batch4.csv');
writetable(table(EEG_data4),'EEG4_batch4.csv');
writetable(table(times0),'times_batch4.csv');
writetable(table(trial_data),'trial_data_batch4.csv');
writetable(table(subj),'subj_batch4.csv');
misc_stats = cat(2,EEG_percen_all0,EEG_percen_best_three0,EEG_percen_best_two0,rew_fb0, pun_fb0, neu_fb0);
writetable(table(misc_stats),'misc_stats_batch4.csv');
