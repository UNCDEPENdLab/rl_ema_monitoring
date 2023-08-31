function [HRoutcome_filtered,HRoutcome_all ,stats, HR, HR_percen] = getHRperOutcome(name)
    output_folder = '/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA';
	site = 'pitt';
	resetFlag = 1;    
	%if nargin<4 || isempty(resetFlag); resetFlag = 0; end

    %% get HR data
    HR = readHR(output_folder,name, site, resetFlag);
    
    %% read trial data
    if strcmp(site,'HUJI')
        filename = dir(strcat(fullfile(pwd,'Data_Raw',['subject_' name]),'/*schedule.db'));
    else
        filename = dir(strcat(fullfile(output_folder,'Data_Raw',[name],'schedule'),'/*schedule.db'));
    end
    if length(filename) > 1
        error(sprintf('multiple schedule files found for subject',name,'%s'));
    end
    db = sqlite(strcat(filename(1).folder,'/',filename(1).name));
    temp = cell2mat(fetch(db, 'SELECT feedback_time, feedback, block FROM trials WHERE choice_time IS NOT NULL ORDER BY choice_time ASC'));
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
    HRoutcome_all = epoch_data;  
    HRoutcome_filtered = epoch_data(inc,:,:);   
    HR_percen=((stats.Ntrials-stats.Ntrials_missing-stats.Ntrials_noisy)/stats.Ntrials)*100;
    ind_na_HR = ~inc;
    save(fullfile(output_folder, 'Data_Processed', ['subject_' name] , [name '_HR.mat']), 'HRoutcome_filtered','HRoutcome_all' ,'stats', 'HR', 'HR_percen', 'ind_na_HR','-v7.3')
    figure
    plot(squeeze(mean(HRoutcome_filtered)))
end
