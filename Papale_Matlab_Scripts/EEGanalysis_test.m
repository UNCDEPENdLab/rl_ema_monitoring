function [Ntotal, Ngood, epoch_data,bad]=EEGanalysis_test(name,path,curD,date_range,block)
    
    %% read modeled prediction errors
    %DATA = readtable(fullfile(fileparts(fileparts(pwd)),'Data_Processed',['subject_' name],['PE2_Heir_' name '.csv']));    
    %Trial.fastPE = table2array(DATA(:,2));
    %Trial.slowPE = table2array(DATA(:,3));
    %if iscell(Trial.fastPE); ind_na = cellfun(@(x)strcmp(x,'NA'),Trial.fastPE); Trial.fastPE(ind_na) = {NaN}; Trial.fastPE = cellfun(@str2double,Trial.fastPE); end
    %if iscell(Trial.slowPE); ind_na = cellfun(@(x)strcmp(x,'NA'),Trial.slowPE); Trial.slowPE(ind_na) = {NaN}; Trial.slowPE = cellfun(@str2double,Trial.slowPE); end
    
    %% read trial data
    filename = strcat(path,'/schedule');
    cd(filename) % already checked
    D = dir('*.db');
    if ~isempty(D)
        db = sqlite(D(1).name);
    else
        cd(curD);
        error('No schedule file found in %s',filename);
    end
    temp = cell2mat(fetch(db, 'SELECT block, feedback_time, feedback FROM trials WHERE choice_time IS NOT NULL ORDER BY choice_time ASC'));
    tempT = fetch(db, 'SELECT printf("%f", feedback_time) FROM trials WHERE choice_time IS NOT NULL');
    Trial.feedback = temp(:,3);
    %Trial.feedbackTimes = temp(:,2);
    
    Trial.feedbackTimes = nan(size(tempT));
    for iT=1:length(tempT)
        Trial.feedbackTimes(iT) = str2double(tempT{iT});
    end
    Trial.block = temp(:,1);
    date = datetime(Trial.feedbackTimes,'ConvertFrom','epochtime','TicksPerSecond',1e3,'TimeZone','UTC','Format','dd-MM-yyyy HH:mm:ss.SSS');
    date.TimeZone = 'America/New_York';
    idx = ones(length(date),1);
    if length(date_range)>1
        idx(date < date_range{1})=0;
        idx(date > date_range{2})=0;
    elseif length(date_range)==1 && ~isempty(date_range{1})
        idx(date~=date_range{1})=0;
    elseif isempty(date_range)
    end
    if length(block)>=1
        idx(block~=Trial.block)=0;
    end
    backup = Trial.feedbackTimes;
    Trial.feedbackTimes(~idx)=[];
    
    if isempty(Trial.feedbackTimes)
        cd(curD);
        error('No feedback times are left, range of dates is %s to %s',min(backup),max(backup));
    end
    db.close;
    
%     %% read EEG and remove trials with NaN
%     epoch_data_2 = Utilities.epoch(EEG.times, EEG.data, Trial.feedbackTimes, 500, 1500, sampling_rate);
%     ind_na = any(any(isnan(epoch_data_2),2),3);
%     epoch_data_2 = epoch_data_2(~ind_na,:,:);
%     Ntotal = length(Trial.feedbackTimes);
%     Ngood= size(epoch_data_2,1);
    
%% load EEG file
proc_dir = strcat(curD,'/Data_Processed/subject_',name);
if exist(proc_dir,'dir')>0
    cd(proc_dir);
else
    mkdir(proc_dir);
    disp('creating directory %s',proc_dir);
    cd(proc_dir);
end

D = dir('*_EEG.mat');
if ~isempty(D)
    load(D(1).name)
else
    [EEG, ~, bad] = readEEG(name,0,curD);
end
%% infer real times of samples
sampling_rate = 256.03;
times = double(EEG.times);
% check if times are in milli or nanoseconds
diff_times = diff(times);
if ~isempty(D)
    median_diff_times = median(diff_times(diff_times>0));
    if median_diff_times > 50; times = times/1000;
    elseif median_diff_times > 1; error('Neither here nor there, check time data');
    end
else
end
% first pass with pre-specified sampling rate
real_recording_time = Utilities.createRealTime(times, sampling_rate);
[corrected_times, break_indices] = Utilities.correctRealTime(real_recording_time, times, sampling_rate);
EEG.times = corrected_times;
EEG.isgood = logical(EEG.isgood==1 | EEG.isgood==2);
if ~isempty(D)
    bad = [];
    for channel = 1:4
        indGood = EEG.isgood(:,channel);
        mn = nanmedian(EEG.data(indGood,channel));
        sd = nanstd(EEG.data(indGood,channel));
        %nogood = EEG.data(:,channel) > mn + 10*sd | EEG.data(:,channel) < mn - 10*sd | EEG.data(:,channel)<=0 |  EEG.data(:,channel)<1650/20  |  EEG.data(:,channel)>19*1650/20;
        nogood_10sd = EEG.data(:,channel)>mn+10*sd | EEG.data(:,channel) < mn-10*sd;
        nogood_neg = EEG.data(:,channel)<=0;
        nogood_range = EEG.data(:,channel)<1650/20 | EEG.data(:,channel)>19*1650/20;
        nogood = nogood_10sd | nogood_neg | nogood_range;
        bad = cat(1,bad, [sum(nogood_10sd),sum(nogood_neg),sum(nogood_range)]);
        EEG.data(nogood,channel) = nan;
    end
end
epoch_data = Utilities.epoch(EEG.times, EEG.data, Trial.feedbackTimes, 500, 1500, 256.03);
ind_na = any(any(isnan(epoch_data),2),3);
epoch_data_2 = epoch_data(~ind_na,:,:);
Ntotal = length(Trial.feedbackTimes);
Ngood= size(epoch_data_2,1);

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
EEG_percen=(Ngood/Ntotal)*100;

F = figure('units','normalized','outerposition',[0 0 1 1]);
for i=1:4
    subplot(4,1,i); pcolor(epoch_data(:,:,i));colorbar; shading flat; axis xy;
    xlabel(EEG_percen, 'fontsize',24);
    caxis([0 1650]);
end

saveas(F,strcat(name,'-EEG1-block-',mat2str(block),'.jpg'));
saveas(F,strcat(name,'-EEG1-block-',mat2str(block),'.fig'));
clf;
figure('units','normalized','outerposition',[0 0 1 1]); clf;
for i=1:4
    subplot(4,1,i); pcolor(isnan(epoch_data(:,:,i)));colorbar; shading flat; axis xy;
    caxis([0 1]);
end
saveas(F,strcat(name,'-EEG2-block-',mat2str(block),'.jpg'));
saveas(F,strcat(name,'-EEG2-block-',mat2str(block),'.fig'));
clf;

end   
