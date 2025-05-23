function [EEG, sampling_rate] = readEEG(output_folder,name, site, refine_sampling_rate)
if nargin<4 || isempty(refine_sampling_rate); refine_sampling_rate = false; end
filename = fullfile(output_folder,'Data_Processed',['subject_' name],[name '_EEG.mat']);
sampling_rate = 256.03;
if exist(filename, 'file')
    load(filename,'EEG');
else
    %% read EEG from physio.db
    if strcmp(site,'HUJI')
        filename = dir(strcat(fullfile(pwd,'Data_Raw',['subject_' name]),'/*physio.db'));
    else
        filename = dir(strcat(fullfile(output_folder,'Data_Processed',['subject_' name]),'/*physio.db'));
    end
    if length(filename) > 1
        error(sprintf('multiple physio files found for subject',name,'%s'));
    end
    if strcmp(site,'HUJI')
        dbname = fullfile(pwd,'Data_Raw',['subject_' name],[name '_physio.db']);
        db = sqlite(dbname);
    else
        db = sqlite(strcat(filename(1).folder,'/',filename(1).name));
    end
    
    % DATA = fetch(db, 'SELECT recording_time,EEG1,EEG2,EEG3,EEG4 FROM EEG_muse ORDER BY recording_time ASC');
    DATA = fetch(db, 'SELECT * FROM EEG_muse ORDER BY recording_time ASC');
    db.close();
    
    % Added for compatibility 
    if istable(DATA)
        % Extract the time column (first column)
        col1 = DATA{:,1};
        % Handle text or numeric time columns
        if iscellstr(col1) || isstring(col1)
            % If the data are in a cell array of character vectors or string array,
            % convert each element to a double.
            EEG.times = cellfun(@str2double, cellstr(col1));
        else
            EEG.times = double(col1);
        end
    
        % Process columns 2 to 5 for EEG.data
        for colIdx = 2:5
            currCol = DATA{:,colIdx};
            if iscellstr(currCol) || isstring(currCol)
                % Convert using str2double
                EEG.data(:,colIdx-1) = cellfun(@str2double, cellstr(currCol));
            else
                EEG.data(:,colIdx-1) = double(currCol);
            end
        end
    
        % Process columns 6 to 9 for EEG.isgood
        for colIdx = 6:9
            currCol = DATA{:,colIdx};
            if iscellstr(currCol) || isstring(currCol)
                EEG.isgood(:,colIdx-5) = cellfun(@str2double, cellstr(currCol));
            else
                EEG.isgood(:,colIdx-5) = double(currCol);
            end
        end
    
    else
        % Assume DATA is a cell array
        if any(ischar(DATA{1,1}))
            EEG.times = cellfun(@str2double, DATA(:,1));
        else
            EEG.times = cellfun(@double, DATA(:,1));
        end
        EEG.data(:,1) = cellfun(@str2double, DATA(:,2));
        EEG.data(:,2) = cellfun(@str2double, DATA(:,3));
        EEG.data(:,3) = cellfun(@str2double, DATA(:,4));
        EEG.data(:,4) = cellfun(@str2double, DATA(:,5));
        EEG.isgood(:,1) = cellfun(@str2double, DATA(:,6));
        EEG.isgood(:,2) = cellfun(@str2double, DATA(:,7));
        EEG.isgood(:,3) = cellfun(@str2double, DATA(:,8));
        EEG.isgood(:,4) = cellfun(@str2double, DATA(:,9));
    end
    clear DATA
    
    
    %% convert isgood to logical
    EEG.isgood = logical(EEG.isgood==1 | EEG.isgood==2);
    
    %% infer real times of samples
    times = double(EEG.times);
    
    % check if times are in milli or nanoseconds
    diff_times = diff(times);
    median_diff_times = median(diff_times(diff_times>0));
    %if median_diff_times > 50; times = times/1000;
    %elseif median_diff_times > 5; error('Neither here nor there, check time data');
    %end
    
    % first pass with pre-specified sampling rate
    real_recording_time = Utilities.createRealTime(times, sampling_rate);
    [corrected_times, break_indices] = Utilities.correctRealTime(real_recording_time, times, sampling_rate);
    
    % refine sampling rate
    if (refine_sampling_rate) && (length(break_indices)>10)
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
    EEG.sampling_rate = sampling_rate;
    %% remove zero values, those that are more than 10 SDs from median, and those that are within 5% of the limits of the sensor's range (0 to 1650)
    EEG.remove = zeros(size(EEG.data));
    for channel = 1:4
        indGood = EEG.isgood(:,channel);
        mn = nanmedian(EEG.data(indGood,channel)); %#ok<NANMEDIAN>
        sd = nanstd(EEG.data(indGood,channel)); %#ok<NANSTD>
        %nogood = EEG.data(:,channel) > mn + 10*sd | EEG.data(:,channel) < mn - 10*sd | EEG.data(:,channel)<=0 |  EEG.data(:,channel)<1650/20  |  EEG.data(:,channel)>19*1650/20;
        %nogood_10sd = EEG.data(:,channel)>mn+10*sd | EEG.data(:,channel) < mn-10*sd;
        nogood_neg = EEG.data(:,channel)<=0;
        nogood_range = EEG.data(:,channel)<1650/20 | EEG.data(:,channel)>19*1650/20;
        %EEG.remove(nogood_10sd,channel) = 1;
        EEG.remove(nogood_neg,channel) = 2;
        EEG.remove(nogood_range,channel) = 3;
        EEG.remove(~EEG.isgood(:,channel),channel) = 4; % 2022-10-20 AndyP
    end
    
    %%  now save with remove computation and corrected times
    if strcmp(site,'HUJI')
        if ~exist(fullfile(output_folder,'Data_Processed',['subject_' name],[name '_EEG.mat']), 'file')
            save(fullfile(output_folder, '\Data_Processed', ['subject_' name] , [name '_EEG.mat']), 'EEG');
        end
    else
        %if ~exist(fullfile(output_folder,'Data_Processed',['subject_' name],[name '_EEG.mat']), 'file')
        save(fullfile(output_folder, 'Data_Processed', ['subject_' name] , [name '_EEG.mat']), 'EEG','-v7.3');
        %end
    end
end
end
