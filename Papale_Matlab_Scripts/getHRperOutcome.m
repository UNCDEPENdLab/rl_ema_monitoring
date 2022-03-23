function [HRoutcome, stats, HR] = getHRperOutcome(name, path,resetFlag,curD,date_range,block)   
    if nargin<2 || isempty(resetFlag); resetFlag = 0; end

    %% get HR data
    HR = readHR(name, resetFlag,curD);
    
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
    
    %% get HR for each outcome
    epoch_data = Utilities.epoch(HR.times, HR.rate, Trial.feedbackTimes, 1000, 10000, 100);
    
    %% determine how many are missing and noisy
    stats.Ntrials = size(epoch_data,1);
    stats.Ntrials_missing = sum(sum(isnan(epoch_data),3) > 0);
    stats.Ntrials_noisy = sum(nanstd(epoch_data,[],3) > 5*nanmedian(nanstd(epoch_data,[],3))) ;
    
    %% filter missing and noisy
    inc = sum(isnan(epoch_data),3)==0 & nanstd(epoch_data,[],3) < 5*nanmedian(nanstd(epoch_data,[],3)) ;
    HRoutcome = epoch_data(inc,:,:); 
    
    
    HR_percen=((stats.Ntrials-stats.Ntrials_missing-stats.Ntrials_noisy)/stats.Ntrials)*100;
    
    cd(strcat('~/Momentum/Data_Processed/subject_',name));
    F=figure('visible','off'); clf;
    plot(squeeze(mean(HRoutcome)))
    xlabel(HR_percen,'fontsize',24);
    saveas(F,strcat(name,'-HR-block',mat2str(block),'.jpg'));
end
