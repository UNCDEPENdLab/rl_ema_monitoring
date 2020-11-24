function [EEG, sampling_rate] = readEEG(name, refine_sampling_rate)   
    if nargin<2 || isempty(refine_sampling_rate); refine_sampling_rate = false; end

    filename = fullfile(pwd,'Data_Processed',['subject_' name],[name '_EEG.mat']);
    if exist(filename, 'file') 
        load(filename);
    else
        %% read from sqlite database
        dbname = fullfile(pwd,'Data_Raw',['subject_' name],[name '_physio.db']);
        
        db = sqlite(dbname);
        DATA = fetch(db, 'SELECT recording_time,EEG1,EEG2,EEG3,EEG4 FROM EEG_muse ORDER BY recording_time ASC');
        db.close();
        if any(ischar(DATA{1,1})); EEG.times = cellfun(@str2double,DATA(:,1));
        else EEG.times = cellfun(@double,DATA(:,1));
        end
        EEG.data(:,1) =  cellfun(@str2double,DATA(:,2));
        EEG.data(:,2) =  cellfun(@str2double,DATA(:,3));
        EEG.data(:,3) =  cellfun(@str2double,DATA(:,4));
        EEG.data(:,4) =  cellfun(@str2double,DATA(:,5));
        clear DATA

        db = sqlite(dbname);
        DATA = fetch(db, 'SELECT ISGOOD1,ISGOOD2,ISGOOD3,ISGOOD4 FROM EEG_muse ORDER BY recording_time ASC');
        db.close();
        EEG.isgood(:,1) = cellfun(@str2double,DATA(:,1));
        EEG.isgood(:,2) = cellfun(@str2double,DATA(:,2));
        EEG.isgood(:,3) = cellfun(@str2double,DATA(:,3));
        EEG.isgood(:,4) = cellfun(@str2double,DATA(:,4));
        clear DATA
                        
        %%  save
        save(filename, 'EEG');
    end
    
    %% convert isgood to logical
    EEG.isgood = logical(EEG.isgood==1 | EEG.isgood==2);
    
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
    
    % refine sampling rate
    if refine_sampling_rate
        [~, longest_breaks] = sort(diff(break_indices));
        for i = 1:10
            start_ind = break_indices(longest_breaks(end+1-i)) + 100;
            end_ind = break_indices(longest_breaks(end+1-i)+1) - 100;
            estimated_sampling_rate(i) = Utilities.adjustSamplingRate(corrected_times(start_ind:end_ind), times(start_ind:end_ind), sampling_rate);
        end

        % second pass with refined sampling rate
        sampling_rate = median(estimated_sampling_rate);
        real_recording_time = Utilities.createRealTime(times, sampling_rate);
        corrected_times = Utilities.correctRealTime(real_recording_time, times, sampling_rate);
    end
    EEG.times = corrected_times;
        
    %% remove zero values, those that are more than 10 SDs from median, and those that are within 5% of the limits of the sensor's range (0 to 1650)  
    for channel = 1:4
        indGood = EEG.isgood(:,channel);
        mn = nanmedian(EEG.data(indGood,channel));
        sd = nanstd(EEG.data(indGood,channel));
        nogood = EEG.data(:,channel) > mn + 10*sd | EEG.data(:,channel) < mn - 10*sd | EEG.data(:,channel)<=0 |  EEG.data(:,channel)<1650/20  |  EEG.data(:,channel)>19*1650/20;
        EEG.data(nogood,channel) = nan;
    end
       
end
