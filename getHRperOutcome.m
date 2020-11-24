function [HRoutcome, stats, HR] = getHRperOutcome(name, resetFlag)   
    if nargin<2 || isempty(resetFlag); resetFlag = 0; end

    %% get HR data
    HR = readHR(name, resetFlag);
    
    %% read trial data
    filename = fullfile(pwd,'Data_Raw',['subject_' name],[name '_schedule.db']);
    db = sqlite(filename);   
    temp = cell2mat(fetch(db, 'SELECT feedback_time, feedback FROM trials WHERE choice_time IS NOT NULL ORDER BY choice_time ASC'));
    Trial.feedback = temp(:,2);
    Trial.feedbackTimes = temp(:,1);
    db.close;
    
    %% get HR for each outcome
    epoch_data = Utilities.epoch(HR.times, HR.rate, Trial.feedbackTimes, 1000, 10000, 100);
    
    %% determine how many are missing and noisy
    stats.Ntrials = size(epoch_data,1);
    stats.Ntrials_missing = sum(sum(isnan(epoch_data),3) > 0);
    stats.Ntrials_noisy = sum(nanstd(epoch_data,[],3) > 5*nanmedian(nanstd(epoch_data,[],3))) ;
    
    %% filter missing and noisy
    inc = sum(isnan(epoch_data),3)==0 & nanstd(epoch_data,[],3) < 5*nanmedian(nanstd(epoch_data,[],3)) ;
    HRoutcome = epoch_data(inc,:,:);    
    figure
    plot(squeeze(mean(HRoutcome)))
end
