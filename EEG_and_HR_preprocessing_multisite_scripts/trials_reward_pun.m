function trials_reward_pun(name)
%this function finds the trial numbers of reward, punishment and neutral outcomes.
%Trials are aggregated acorss all blocks.
output_folder = '/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA';
site = 'pitt';
if strcmp(site,'HUJI')
    filename = dir(strcat(fullfile(pwd,'Data_Raw',['subject_' name]),'/*schedule.db'));
    %filename = fullfile(pwd,'Data_Raw',['subject_' name],[name '_schedule.db']);
else
    filename = dir(strcat(fullfile(output_folder,'Data_Raw',name,'schedule'),'/*schedule.db'));
end
if length(filename) > 1
    error(sprintf('multiple schedule files found for subject',name,'%s'));
end
db = sqlite(strcat(filename(1).folder,'/',filename(1).name));   
%trial_block_outcome_feedback = cell2mat(fetch(db, 'SELECT trial, block,
%outcome, feedback FROM trials WHERE choice==0 OR choice==1')); %
%2023-06-26 AndyP, this was returning a different number of trials than the
%epoch_data variable
trial_block_outcome_feedback = cell2mat(fetch(db, 'SELECT * FROM trials WHERE choice_time IS NOT NULL AND stim1>=0 AND stim2>=0 ORDER BY choice_time ASC'));
reward_feedback_trials= find(trial_block_outcome_feedback(:,3)==1 & trial_block_outcome_feedback(:,4)==1);
pun_feedback_trials= find(trial_block_outcome_feedback(:,3)==-1 & trial_block_outcome_feedback(:,4)==1);
neu_feedback_trials= find(trial_block_outcome_feedback(:,3)==0 & trial_block_outcome_feedback(:,4)==1);
save(fullfile(output_folder, 'Data_Processed', ['subject_' name], [name '_outcome_trials.mat']), 'reward_feedback_trials','pun_feedback_trials', 'neu_feedback_trials', 'trial_block_outcome_feedback')
end
