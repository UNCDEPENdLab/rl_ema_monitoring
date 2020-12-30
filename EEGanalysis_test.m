function [Ntotal, Ngood, epoch_data]=EEGanalysis_test(name, first_block, last_block)
    
    %% read modeled prediction errors
    %DATA = readtable(fullfile(fileparts(fileparts(pwd)),'Data_Processed',['subject_' name],['PE2_Heir_' name '.csv']));    
    %Trial.fastPE = table2array(DATA(:,2));
    %Trial.slowPE = table2array(DATA(:,3));
    %if iscell(Trial.fastPE); ind_na = cellfun(@(x)strcmp(x,'NA'),Trial.fastPE); Trial.fastPE(ind_na) = {NaN}; Trial.fastPE = cellfun(@str2double,Trial.fastPE); end
    %if iscell(Trial.slowPE); ind_na = cellfun(@(x)strcmp(x,'NA'),Trial.slowPE); Trial.slowPE(ind_na) = {NaN}; Trial.slowPE = cellfun(@str2double,Trial.slowPE); end
    
    %% read trial data
    filename = fullfile(pwd,'Data_Raw',['subject_' name],[name '_schedule.db']);
    db = sqlite(filename);
    
    temp = cell2mat(fetch(db, 'SELECT feedback_time, feedback, block FROM trials WHERE choice_time IS NOT NULL AND stim1>=0 AND stim2>=0 ORDER BY choice_time ASC'));
    %temp = cell2mat(fetch(db, 'SELECT feedback_time, feedback FROM trials WHERE choice_time IS NOT NULL AND stim1>17 AND stim2>17 AND stim1<150 AND stim2<150'));
    Trial.feedback = temp(find((first_block<=temp(:,3)&(last_block>=temp(:,3)))),2);
    Trial.feedbackTimes = temp(find((first_block<=temp(:,3)&(last_block>=temp(:,3)))),1);
    db.close;
    
    %% read EEG and remove trials with NaN
    [EEG, sampling_rate] = readEEG(name, first_block, last_block);
    epoch_data_2 = Utilities.epoch(EEG.times, EEG.data, Trial.feedbackTimes, 500, 1500, sampling_rate);
    ind_na = any(any(isnan(epoch_data_2),2),3);
    epoch_data_2 = epoch_data_2(~ind_na,:,:);
    Ntotal = length(Trial.feedbackTimes);
    Ngood= size(epoch_data_2,1);
    
%% load EEG file
filename = fullfile(pwd,'Data_Processed',['subject_' name],[name '_EEG.mat']);
load(filename)

%% infer real times of samples
sampling_rate = 256.03;
times = double(EEG.times);
% check if times are in milli or nanoseconds
diff_times = diff(times);
median_diff_times = median(diff_times(diff_times>0));
if median_diff_times > 50; times = times/1000;
elseif median_diff_times > 1; error('Neither here nor there, check time data');
end
% first pass with pre-specified sampling rate
real_recording_time = Utilities.createRealTime(times, sampling_rate);
[corrected_times, break_indices] = Utilities.correctRealTime(real_recording_time, times, sampling_rate);
EEG.times = corrected_times;
epoch_data = Utilities.epoch(EEG.times, EEG.data, Trial.feedbackTimes, 500, 1500, 256.03)
    %%correspondence check
    %if any(isnan(Trial.fastPE(Trial.feedback==1))) || any(isnan(Trial.slowPE(Trial.feedback==1))) || any(~isnan(Trial.fastPE(Trial.feedback==0)) & Trial.fastPE(Trial.feedback==0)~=0) || any(~isnan(Trial.slowPE(Trial.feedback==0)) & Trial.slowPE(Trial.feedback==0)~=0)
     %   error('Incompatibility');
    %end
    
    %% isolate feedback trials
    %Trial.feedbackTimes = Trial.feedbackTimes(Trial.feedback==1); 
    %Trial.fastPE = Trial.fastPE(Trial.feedback==1);
    %Trial.slowPE = Trial.slowPE(Trial.feedback==1);
    
    %% read EEG and remove trials with NaN
    %[EEG, sampling_rate] = readEEG(name);
    %epoch_data = Utilities.epoch(EEG.times, EEG.data, Trial.feedbackTimes, 500, 1500, sampling_rate);
    %ind_na = any(any(isnan(epoch_data),2),3);
    %epoch_data = epoch_data(~ind_na,:,:);
    %Ntotal = length(Trial.feedbackTimes);
    %Ngood= size(epoch_data,1);
    %TF = EEGtimefreq(epoch_data, sampling_rate);
    %TF.fastPE = Trial.fastPE(~ind_na);
    %TF.slowPE = Trial.slowPE(~ind_na);       
    %TF.times = Trial.feedbackTimes(~ind_na);       
    
    %% decode
    %[Slow.decodability, Slow.pval, Slow.predicted, Slow.regs] = EEGdecode(TF.slowPE, zscore(TF.data));
    %[Fast.decodability, Fast.pval, Fast.predicted, Fast.regs] = EEGdecode(TF.fastPE, zscore(TF.data));
    %Slow.times = TF.times;
    %Fast.times = TF.times;
    %save(fullfile(fileparts(fileparts(pwd)),'Data_Processed',['subject_' name],'PE_decoding'),'Slow','Fast');
    %Slow
    %Fast
   
    %% figures before cleaning data: figure 1 all data, figure 2-missing data

    figure
for i=1:4
    subplot(4,1,i); imagesc(epoch_data(:,:,i));colorbar;
end
figure
for i=1:4
    subplot(4,1,i); imagesc(isnan(epoch_data(:,:,i)));colorbar;
end

end   
