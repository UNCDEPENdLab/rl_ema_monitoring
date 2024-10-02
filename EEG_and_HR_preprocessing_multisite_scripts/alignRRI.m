function [alignedRRI,timings] = alignRRI(pathToPhysioFiles,pathToScheduleFile,savePath)
    % Processes the ECG of a merged file containing all the sessions of a
    % participant. Computes the RRI timeseries and aligns it with the
    % feedback times. Optionally saves the resulting array.
    %
    % Parameters:
    % pathToPhysioFiles   - [String] Path to the database file containing the physio sessions
    % pathToScheduleFile - [String] Path to where the schedule file is located 
    % savePath - [String] Path to where the alignedRRI will be saved, if
    %               empty no data is saved.
    % 
    % Returns:
    % alignedRRI - [Table] Contains the aligned trials.
    % timings - [Cell Array] Contains the timings or deltas corresponding
    %           to the feedback time as time zero.
    
    %% Generate paths
    % [pathToPhysioFiles, pathToScheduleFile, savePath] = generatePaths(base_path, participantId);

    %% Process the schedule file
    participantId = extractParticipantId(pathToScheduleFile);
    feedbackEventTimes = getFeedbackTimings(pathToScheduleFile);

    %% Merge the physiofiles
    tempDbFile = createTemporalMergedDatabase(pathToPhysioFiles);

    %% Process the RRI
    [~,RRI] = readSession(tempDbFile,false,false,false);
    
    % Delete the temporary database file
    if exist(tempDbFile, 'file')
        delete(tempDbFile);
    end

    %% Align the RRI
    % [alignedRRI,timings] = getAlignedTrials(RRI,feedbackEventTimes,'spline'); % Interpolate
    [alignedRRI,timings] = getAlignedTrials(RRI,feedbackEventTimes,[]); % Fill with nans
    
    %% Save the data
    saveAlignedRRI(savePath, participantId, alignedRRI);
end

function datetime_array = convert_to_datetime(datenum_array_ms)
    % Converts an array of date numbers in milliseconds to a datetime array with
    % a specified format and timezone.
    %
    % Parameters:
    % datenum_array_ms - [Array of Doubles] Contains date numbers in milliseconds since the Unix epoch (01-Jan-1970).
    %
    % Returns:
    % datetime_array - [Datetime Array] Contains the converted date and time values
    %                  in the 'dd-MMM-yyyy HH:mm:ss.SSSSSSSSS' format, adjusted to the
    %                  'America/New_York' timezone.
    
    datetime_array = double(datenum_array_ms)/1000; % Convert to seconds
    datetime_array = datetime(datetime_array, 'ConvertFrom', 'posixtime', 'Format', 'dd-MMM-yyyy HH:mm:ss.SSSSSSSSS', 'TimeZone', 'America/New_York');
end

function tempDbFile = createTemporalMergedDatabase(pathToPhysioFile)
    % Creates a temporary SQLite database by merging specific tables from multiple database files
    % 
    % Parameters:
    % pathToPhysioFile - [String] Path to database files
    % 
    % Returns:
    %   tempDbFile - [String] Path to the temporary merged database file
    
    % Generate a list of all database files
    physioFiles = findDatabaseFiles(pathToPhysioFile);

    % Generate a unique temporary file name for the SQLite database
    tempDbFile = [tempname, '.db'];

    % Open a connection to the temporary database
    db = sqlite(tempDbFile, 'create');
    
    % For older versions of matlab
    % Create the 'Polar_heartrate' table with desired columns
    createTableQuery = ['CREATE TABLE Polar_heartrate (' ...
                        'time_ms INTEGER, ' ...
                        'heartrate INTEGER, ' ...
                        'rr_intervals TEXT, ' ...
                        'contact INTEGER);'];
    exec(db, createTableQuery);
    
    % Create the 'Polar_ECG' table with desired columns
    createTableQuery = ['CREATE TABLE Polar_ECG (' ...
                        'time_ms INTEGER, ' ...
                        'polar_timestamp INTEGER, ' ...
                        'ECG REAL);'];
    exec(db, createTableQuery);

    % Loop through each file in physioFiles to read and merge the data
    for i = 1:length(physioFiles)

        % Open each database file individually
        newdb = sqlite(physioFiles{i}, 'connect');
        
         try
            % Fetch available tables
            tables = fetch(newdb, 'SELECT name FROM sqlite_master WHERE type="table" AND name IN ("Polar_heartrate", "Polar_ECG")');

            for t = 1:height(tables)
                
                % Fetch the data 
                table_name = tables{t,:};
                fetch_str = sprintf('SELECT * FROM %s',table_name);
                data = fetch(newdb, fetch_str);
                
                % Check if there is data
                if ~isempty(data)
                     % Determine the action based on MATLAB version
                    if verLessThan('matlab', '9.11')  % Check if MATLAB version is older than 9.11
                        % Define column names based on the table name
                        if strcmp(table_name, 'Polar_heartrate')
                            colnames = {'time_ms', 'heartrate', 'rr_intervals', 'contact'};
                        elseif strcmp(table_name, 'Polar_ECG')
                            colnames = {'time_ms', 'polar_timestamp', 'ECG'};
                        else
                            error('Unknown table. Cannot determine column names.');
                        end
            
                        % Perform the insert operation with the specified column names
                        insert(db, table_name, colnames, data);
                    else
                        % Use sqlwrite for newer MATLAB versions
                        sqlwrite(db, table_name, data);
                    end
                end
            end
        catch ME 
            fprintf('An error occurred while fetching tables in file %s: %s\n',physioFiles{i}, ME.message);
        end
        newdb.close();
    end
    db.close();

end

function dataTable = extractAndConcatenateDataTable(processed_session, ecg)
    % Extracts the tables from a struct and concatenates them into a single
    % table.
    %
    % Parameters:
    % processed_session - [Cell Array] 1xN contains the processed sessions.
    % ecg - [Logical] Determines whether to extract the ECG or the RRI
    %               table from the struct
    %
    % Returns:
    % dataTable - [Table] Mx2 Contains all the merged data from the
    %                   sessions

    tableList = cell(size(processed_session));  % Preallocate a cell array for tables
    if ecg; fieldName = 'ecg_data'; else; fieldName='rri_data';end % Choose the field name based on ecg flag
    
    % Remove the empty values
    nonEmptyIndex = ~cellfun(@isempty, processed_session);
    processed_session = processed_session(nonEmptyIndex);    % Filter out empty cells

    % Merge into a new table
    for i = 1:length(processed_session)
        tableList{i} = processed_session{i}.(fieldName);
    end

    % Concatenating all extracted tables into one and assigning to the appropriate field
    dataTable = vertcat(tableList{:});
end

function participantId = extractParticipantId(filePath)
    % Extract the participant id from the schedule file name
    % 
    % Parameters:
    % filePath - [String] Path to the schedule file
    % 
    % Returns:
    % participantId - [String] Id of the participant

    match = regexp(filePath, '_\d+_schedule\.db', 'match');

    if ~isempty(match)
        % Extract the numeric part from the match
        % The match will be something like '_######_schedule.db'
        % So we remove the leading '_' and the trailing '_schedule.db'
        participantId = regexp(match{1}, '\d+', 'match', 'once');
    else
        % If no match is found, return an empty string or handle the error
        participantId = '';
        warning('No participantId found in the given file path.');
    end
end

function idxArray = findClosestIndices(longArray, rawArray)
    % Returns the indices of the longArray where each element of the
    % rawArray is closest to.
    % 
    % Parameters:
    % longArray - [Array] Where the elements will be searched 
    % rawArray - [Array] The elements to search 
    %
    % Returns:
    % idxArray - [Array] Contains the indices where the values were found
    
    % Ensure both arrays are sorted
    longArray = sort(longArray);
    rawArray = sort(rawArray);
    
    % Calculate the differences matrix
    differences = rawArray - longArray;
    
    % Create a mask where differences are non-positive (second date is before or at the first date)
    validMask = differences <= 0;
    
    % Find the first value where the rawArray was negative and return the indices
    [~, idxArray] = max(validMask, [], 2);
    
end

function filteredECG = filterECG(ecgData, fs)
    % Applies a bandpass FIR filter to ECG data to reduce noise and enhance signal quality.
    %
    % The filter is designed using a FIR (Finite Impulse Response) approach with a 
    % specified filter order and cutoff frequencies calculated from the Nyquist frequency.
    % The bandpass filter passes frequencies between 9 Hz and 50 Hz, which are typical
    % cutoff frequencies for ECG data processing.
    %
    % Parameters:
    % ecgData - [Array] Raw ECG data to be filtered.
    % fs - [Double] Sampling frequency of the ECG data in Hz.
    %
    % Returns:
    % filteredECG - [Array] ECG data after being processed by the bandpass filter.
    %
    Nyquist = fs / 2;                       % Calculate the Nyquist frequency
    lowFreq = 9 / Nyquist;                  % Normalize the low cutoff frequency
    highFreq = 50 / Nyquist;                % Normalize the high cutoff frequency
    
    filterOrder = 100;                      % Define the order of the FIR filter
    b = fir1(filterOrder, [lowFreq highFreq], 'bandpass'); % Design the FIR bandpass filter
    filteredECG = filtfilt(b, 1, ecgData);  % Apply the filter using zero-phase filtering
end

function endIdx = findEndIdx(times, i, currentIdx)
    % Recursively determines the last consecutive index in an array where 
    % the value matches that at a specified starting index.
    %
    % The function navigates through the 'times' array starting from a given index,
    % and recursively identifies the last index where the value at the starting
    % index ('i') matches the values in subsequent indices. The search stops when 
    % a mismatch is found or the end of the array is reached.
    %
    % Parameters:
    % times - [Array of Numbers] The array being searched.
    % i - [Integer] The index of the initial value being matched in the array.
    % currentIdx - [Integer] The current index being checked in the recursive search.
    %
    % Returns:
    % endIdx - [Integer] The index of the last consecutive element that matches
    %          the value at index 'i', or the index immediately before a mismatch.
    %
    if currentIdx <= length(times) && times(i) == times(currentIdx)
        % If the current index is within the array bounds and the values match,
        % recursively call findEndIdx to check the next index.
        endIdx = findEndIdx(times, i, currentIdx + 1);
    else
        % If there is no match or the end of the array is reached, return the previous
        % index, as it is the last index where the values matched.
        endIdx = currentIdx - 1;
    end
end

function dbFiles = findDatabaseFiles(directory)
    % Returns a list of all database files in the specified directory.
    % Parameters:
    % directory - [String] The directory to search
    % 
    % Returns:
    % dbFiles - [Cell Array] Contains all paths found to database files

    % Validate if the input is a directory
    if ~isfolder(directory)
        error('The provided path is not a valid directory.');
    end

    % Get a list of all '.db' files in the directory
    filePattern = fullfile(directory, '*.db');
    files = dir(filePattern);

    % Extract the names and create full paths
    dbFiles = {files.name};
    dbFiles = cellfun(@(x) fullfile(directory, x), dbFiles, 'UniformOutput', false);

end

function [t_flat,rri_flat] = flattenTimeAndRri(timestamps,rri)
    % Flattens arrays of timestamps and corresponding RRI (R-R Interval) data,
    % handling irregularities such as empty cells.
    %
    % The function iterates over the cells in the rri which contain packets and duplicates the timestamps
    % for the same number of values in the packet. If the rri cell is empty
    % (no data in the packet) then a 0 is appended in the rri.
    %
    % Parameters:
    % timestamps - [Array] An array of timestamps, where each timestamp corresponds
    %              to a data packet in the 'rri' array.
    % rri - [Cell Array] A data packet where each packet may contain none or any number of RRI measurements .
    %
    % Returns:
    % t_flat - [Array] A flattened array of timestamps, where each timestamp is repeated
    %          to correspond to each RRI value in the 'rri' cell array.
    % rri_flat - [Array] A flattened array of RRI values corresponding to each timestamp.
    
    % Initialize arrays for collection
    rri_flat = [];
    t_flat = [];

    % Loop through each element of the raw data
    for i = 1:length(rri)
        if ~isempty(rri{i})
            % Number of elements in the current cell
            k = length(rri{i});
            
            % Append current RRI data to the flat array
            rri_flat = [rri_flat, rri{i}];
            
            % Repeat the timestamp k times and append to the flat timestamp array
            t_flat = [t_flat, repmat(timestamps(i), 1, k)];
        else
            % Handle empty cells 
            rri_flat = [rri_flat, 0];  % or NaN
            t_flat = [t_flat, timestamps(i)];     % Keep the timestamp of the missing data
        end
    end

    % Transpose
    rri_flat = rri_flat';
    t_flat = t_flat';

end

function [pathToPhysioFiles, pathToScheduleFile, savePath] = generatePaths(base_path, participantId)
    % Generates paths similar to a shell script for a given subject
    % 
    % Parameters:
    %   base_path - [String] Base directory for data paths
    %   participantId - [String] Participant identifier as a string
    %
    % Returns:
    %   pathToPhysioFiles - Path to the physio files for the subject
    %   pathToScheduleFile - Path to the schedule file for the subject
    %   savePath - Path to save processed data for the subject

    % Path to physio files
    pathToPhysioFiles = fullfile(base_path, 'Data_Raw', participantId, 'physio');

    % Path to schedule file
    subj_dir = fullfile(base_path, 'Data_Raw', participantId, 'schedule');
    files = dir(fullfile(subj_dir, ['*_' participantId '_schedule.db']));
    if isempty(files)
        error('No schedule file found for subject %s in directory %s', participantId, subj_dir);
    end
    pathToScheduleFile = fullfile(files(1).folder, files(1).name);

    % Path for saving processed data
    savePath = fullfile(base_path, 'Data_Processed');
end

function rri_accum = getAccumulatedRRI(rri)
    % Calculates the cumulative sum of R-R Interval (RRI) data and converts the sum to seconds.
    %
    % This function iterates over an array of RRI values, accumulating the sum of intervals.
    % Each subsequent entry is the sum of all previous RRI values up to that point, starting
    % from the first value. The final accumulated values are converted from milliseconds
    % to seconds by dividing by the sample rate factor of 1024.
    %
    % Parameters:
    % rri - [Array] An array of RRI measurements in milliseconds.
    %
    % Returns:
    % rri_accum - [Array] An array where each element is the cumulative sum of RRI values
    %              up to that index, converted to seconds.
    
    rri_accum = zeros(length(rri),1);
    rri_accum(1) = rri(1);
    for i = 2:length(rri)
        rri_accum(i) = rri_accum(i - 1) + rri(i); %Carry forward the last valid accumulation
    end
    rri_accum = rri_accum*1/1024; % Convert to s

end

function [alignedRRI, timings]= getAlignedTrials(RRI,events,interpolation_method)
    % Aligns the RRI trials to the events. The data is upsampled by the
    % interpolation_method.
    %
    % Parameters:
    % RRI - [Table] Contains the Timestamp column as datetime object and
    %               the RRI values.
    % events - [Array] Contains the events to which the RRI will be
    %               aligned.
    % interpolation_method - [String] The desired interpolation method for
    %               the upsampling of the RRI in the window surrounding the event times.
    %
    % Returns:
    % alignedRRI - [Numeric Array] Contains the aligned trials.
    % timings - [Cell Array] Contains the timings or deltas corresponding
    %               to the feedback time as time zero.

    % Make sure there are no duplicated rows
    % disp(height(RRI));
    % RRI = removeDuplicates(RRI);
    % disp(height(RRI));

    % Define parameters
    pre_window = 1000; % milliseconds before the event
    post_window = 10000; % milliseconds after the event
    pre_duration = seconds(pre_window*1e-3);
    post_duration = seconds(post_window*1e-3);

    % Extend the window slightly to include at least one RRI before and after
    buffer_time = seconds(5);  % 5 seconds before and after
    extended_pre_duration = pre_duration + buffer_time;
    extended_post_duration = post_duration + buffer_time;

    newSamplingRate = 10; % Hz
    newSamplingStep = seconds(1/newSamplingRate);
    
    %Initialize array to gather aligned trials
    alignedRRI = cell(length(events), 1);  % Initialize a cell array to hold interpolated RRIs
    
    for i = 1:length(events)

        event_time = events(i);

        % Define new times 
        new_times = event_time - pre_duration:newSamplingStep:event_time + post_duration;
        
        % If it's empty then just fill the timebins with NaN values
        if isempty(interpolation_method)
            % Find the RRI data in the specified window
            indices = find(RRI.Timestamp >= event_time - pre_duration & RRI.Timestamp <= event_time + post_duration);
            
            % Get the indices closest to the new times
            idxArray = findClosestIndices(new_times, RRI.Timestamp(indices));
            
            % Create a new array with nans
            result = NaN(size(new_times,2), 1);
            
            % Fill in the RRI values
            result(idxArray) = RRI.RRI(indices);

            % Append the result
            alignedRRI{i} = result';

        else
            % Find RRI data around the event within the specified window
            indices = find(RRI.Timestamp >= event_time - extended_pre_duration & RRI.Timestamp <= event_time + extended_post_duration);
        
            % If data was found between the bounds
            if size(indices,1)>1
    
                relevant_times = RRI.Timestamp(indices);
                relevant_rris = RRI.RRI(indices);
                
                % Interpolate RRI data at new times
                try
                    interpolated_rris = interp1(relevant_times, double(relevant_rris), new_times, interpolation_method); 
                    alignedRRI{i} = interpolated_rris;
                catch ex
                    disp(ex.message);
                end
            end
        end
    end

    alignedRRI = alignedRRI(~cellfun('isempty', alignedRRI));  % Removes any empty cells resulting from the try-catch block
    timings =  - pre_duration:newSamplingStep: post_duration;
    
    % Convert to mat
    alignedRRI = cell2mat(alignedRRI);
end

function ECG = getECG(raw_data,optimized_alignment)
    % Takes in a table with 3 columns named (time_ms, polar_timestamp, ECG)
    % Filters the ECG and adjusts the timestamps to be at a nanosecond
    % resolution. Computes the RRI from the processed ECG.
    % 
    % Parameters:
    % raw_data - [Table] Nx3 Contains the streamed raw data from the recording.
    % optimized_alignment - [Logical] Determines wether to use an
    %                   optimization technique to find shifts in the phone timestamps. When
    %                   set to false it uses the first phone timestamp as the initial value.
    %                   (default: false)
    % 
    % Returns:
    % ECG - [Struct] Contains the sampling frequency and 2 tables. ecg_data
    %               with the processed ecg and rri_data with the computed RRI from the
    %               ECG.

    ECG.fs = 130; %Hz Sampling rate of Polar H10
    original_size = height(raw_data);

    % Exit early if no data is present
    if original_size==0
        ECG.ecg_data = table([],[],'VariableNames',{'Timestamp','ECG'});
        ECG.rri_data = table([],[],'VariableNames',{'Timestamp','RRI'});
        return;
    end

    %% ECG processing
    raw_data.ECG = cellfun(@(x) str2num(strrep(strrep(x, '[', ''), ']', '')), raw_data.ECG, 'UniformOutput', false);
    datapoints_per_timestamp = size(raw_data.ECG{1},2); % Assuming all packets have same size because it streams at constant rate   
    raw_data.time_ms_timestamp =  convert_to_datetime(double(raw_data.time_ms));
    
    % Flatten and filter
    ECG.ecg_data.ECG = filterECG(cell2mat(raw_data.ECG.').',ECG.fs);
    
    %Get interpolated times
    interpolated_interval = getInterpolatedIntervals(datapoints_per_timestamp,ECG.fs);

    %% Process polar timestamps containing higher precision
    T_from_ns = 1e-9*(double(raw_data.polar_timestamp)-double(raw_data.polar_timestamp(1))); % Convert to seconds and as elapsed time
    
    %% Align Phone timestamps
    if optimized_alignment
        start_timestamp = double(raw_data.time_ms(1)); % it's actually the arrival of the last sample of the first data packet
        T_from_ms = (double(raw_data.time_ms)-start_timestamp)*1e-3; % Elapsed time from timestamps in seconds
        % fitness = getTimestampFitness(T_from_ns,T_from_ms);
        % disp('Fitness before: '+string(fitness));
        
        % Optimize alignment
        [tau_opt, ~] = optimizeAlignment(T_from_ms,T_from_ns);
        
        % Adjusting the original timestamps
        T_from_ms = seconds(T_from_ms + tau_opt) + convert_to_datetime(start_timestamp); % Add the correction and shift back to the original time
        T_from_ms = repmat(T_from_ms,1,datapoints_per_timestamp)+seconds(interpolated_interval);
        T_from_ms = reshape(T_from_ms',1,numel(T_from_ms))'; % Flatten row-wise
    else
        % Considers the first timestamp of the phone as precise and uses
        % the sensor timestamps for the rest

        % Convert sensor data to timedate using shift described in the
        % manufacturer's documentation
        raw_data.polar_timedate = datetime(double(946684800000000000+raw_data.polar_timestamp)*1e-9,'ConvertFrom', 'posixtime', 'Format', 'dd-MMM-yyyy HH:mm:ss.SSSSSSSSS', 'TimeZone', 'America/New_York');
        
        % Get the difference 
        timestamp_diff = raw_data.time_ms_timestamp(1)-raw_data.polar_timedate(1);
        
        % Add the difference to the sensor data
        T_from_ms = timestamp_diff + raw_data.polar_timedate;
        
        % Add the interpolated values and reshape to a single column
        T_from_ms = repmat(T_from_ms,1,datapoints_per_timestamp)+seconds(interpolated_interval);
        T_from_ms = reshape(T_from_ms',1,numel(T_from_ms))'; % Flatten row-wise
    end

    %% Organizing into table
    ECG.ecg_data.Timestamp = T_from_ms;
    ECG.ecg_data = struct2table(ECG.ecg_data);
    ECG.ecg_data = ECG.ecg_data(:, {'Timestamp','ECG'});

    % For analyzing distribution only
    % tau_opt = repmat(tau_opt,1,datapoints_per_timestamp);
    % ECG.ecg_data.Tau = reshape(tau_opt',1,numel(tau_opt))';

    % For evaluating optimality of fmincon only
    % elapsed_time = seconds(T_from_ms-T_from_ms(1));
    % fitness = getTimestampFitness(T_from_ns,elapsed_time);
    % disp('Fitness: '+ string(fitness)+' max_tau '+string(max(tau_opt))+' Median tau: '+string(median(tau_opt)));

    %% Convert to RRI 

    % Detect peaks
    [~, ind_locs] = findpeaks(ECG.ecg_data.ECG,  ... % Only using the datapoints with no reference to return the peak indices
                               'MinPeakDistance',50,...
                               'MinPeakHeight', 100, ...
                               'MinPeakProminence',10);

    locs = ECG.ecg_data.Timestamp(ind_locs); % Using the indices to get the actual locations
    rri = seconds(diff(locs))*1e3; % in ms

    % Organizing into table
    rri_timestamp = locs(2:end);
    ECG.rri_data = table(rri_timestamp,rri,'VariableNames',{'Timestamp','RRI'});
    
end

function timings = getFeedbackTimings(filePath)
    % Obtain the feedback times 
    %
    % Parameters:
    % filepath - [String] Path to the schedule file
    %
    % Returns:
    % timings - [Cell Array] Contains the feedback times as datetime object

    db = sqlite(filePath);
    feedbackTimeTable = fetch(db,'SELECT feedback_time, stim_time, choice_time, feedback, block FROM trials WHERE choice_time IS NOT NULL AND stim1>=0 AND stim2>=-1000 ORDER BY choice_time ASC');
    db.close();
    if strcmp(class(feedbackTimeTable),'table')
        feedbackTimes = feedbackTimeTable.feedback_time;
    elseif strcmp(class(feedbackTimeTable),'cell')
        feedbackTimes = feedbackTimeTable(:,1);
        feedbackTimes = cell2mat(feedbackTimes);
    end
    timings = convert_to_datetime(feedbackTimes);
end

function [interpolated_interval] = getInterpolatedIntervals(datapoints_per_timestamp,fs)
    % Compute the timestamps going back in time by interpolating a number
    % of datapoints using a given sampling frequency.
    %
    % Parameters:
    % datapoints_per_timestamp - [Double] the length of the resulting
    %                           interval.
    % fs - [Double] the sampling frequency
    %
    % Returns:
    % interpolated_interval - [Double] The interval of interpolated values

    currentTs = 0;
    previousTs = -(datapoints_per_timestamp-1)./fs;% datapoints_per_timestamp-1 because we'll use the currentTs
    interpolated_interval = linspace(previousTs, currentTs, datapoints_per_timestamp); % Extrapolate from current timestamp back a number of datapoints 
end

function RRI = getRRI(DATA)
        
    original_size = height(DATA);
    % Exit early if no data is present
    if original_size==0
        RRI.data = table([], [],'VariableNames', {'Timestamp','RRI'});
        return;
    end
    %% Flatten the data
    DATA.rr_intervals = cellfun(@(x) str2num(strrep(strrep(x, '[', ''), ']', '')), DATA.rr_intervals, 'UniformOutput', false); % Convert from string to numeric and process missing data
    [t_flat,rri_flat] = flattenTimeAndRri(DATA.time_ms,DATA.rr_intervals);

    %% Process timestamps
    t_flat = double(t_flat) / 1000;
    
    % Interpolate time
    t_flat = interpolateRRITimestamps(t_flat);
    t_flat = datetime(t_flat, 'ConvertFrom', 'posixtime', 'Format', 'dd-MMM-yyyy HH:mm:ss.SSS', 'TimeZone', 'America/New_York');

    %% Compute differences 
    % t_elapsed = seconds(t_flat - t_flat(1));
    % rri_accum = getAccumulatedRRI(rri);
    % t_delta = t_elapsed - rri_accum;
    
    %% Organize data
    rri_flat = rri_flat*1000/1024; % Convert to s

    % RRI.data = table(t_flat, t_elapsed, rri_flat,rri_accum,t_delta, 'VariableNames', {'Timestamp', 'Elapsed','RRI','RRI_accum','Time_delta'});
    RRI.rri_data = table(t_flat, rri_flat,'VariableNames', {'Timestamp','RRI'});
    % RRI.raw_data = DATA;

end 

function sessions = getSeparatedSessions(filepath,ecg)
    % Reads a database and splits it into multiple databases if the data
    % are too separated from each other
    %
    % Parameters:
    % filepath - [String] Path to the session or merged physio file.
    % ecg - [Logical] Determines whether an ECG or an RRI stream will be read
    %
    % Returns:
    % sessions - [Cell Array] 1xN Contains the N subsessions split from the
    %             database.

    %% Read data from database
    db = sqlite(filepath);
    if ecg
        sessionData = fetch(db,'SELECT * FROM Polar_ECG'); % Read timestamp, polar_timestamp and raw ECG
    else
        sessionData = fetch(db,'SELECT time_ms, rr_intervals, heartrate, contact FROM Polar_heartrate ORDER BY time_ms ASC');
    end

    db.close();
    %% Make sure it's in table format
    if iscell(sessionData)
        colNames = {'time_ms','polar_timestamp','ECG'};
        sessionData = cell2table(sessionData, 'VariableNames', colNames);
    end

    %% Sort the table by timestamps first
    [~, idx] = unique(sessionData(:, {'polar_timestamp', 'ECG'}), 'rows', 'stable'); % Sometimes the phone records the exact same packet twice
    sessionData = sessionData(idx, :);

    sessionData = sortrows(sessionData,{'time_ms','polar_timestamp'});

    %% Check differences 
    timestamps = sessionData.time_ms;
    timestamps = convert_to_datetime(timestamps);
    
    % Calculate differences in timestamps
    timeDiffs = [seconds(diff(timestamps)); 0]; % Append a zero for the last element to maintain array size
    
    % Identify splits when time difference is greater than x seconds
    splitPoints = find(timeDiffs > 3);
    
    % Split data at these points
    sessions = {};
    startIdx = 1;
    for i = 1:length(splitPoints)
        endIdx = splitPoints(i);
        sessions{end+1} = sessionData(startIdx:endIdx, :);
        startIdx = endIdx + 1;
    end

    % Add the last segment if it wasn't included
    if startIdx <= size(sessionData, 1)
        sessions{end+1} = sessionData(startIdx:end, :);
    end
end

function fitnessValue = getTimestampFitness(nsTimestamp,msTimestamp)
    % Calculates a fitness of nanosecond timestamps compared to millisecond timestamps
    % by assessing the absolute differences after converting milliseconds to seconds.
    %
    % Parameters:
    % nsTimestamp - [Array] Nanosecond timestamps.
    % msTimestamp - [Array] Millisecond timestamps.
    %
    % Returns:
    % fitnessValue - [Numeric] The sum of the absolute differences between the nanosecond
    %                timestamps and the elapsed time from the first millisecond timestamp
    %                converted to seconds. Lower values indicate better fitness similarity.

    if size(nsTimestamp,2)>1
        nsTimestamp = reshape(nsTimestamp',1,numel(nsTimestamp))'; % Flatten row-wise
    end
    if size(msTimestamp,2)>1
        msTimestamp = reshape(msTimestamp',1,numel(msTimestamp))'; % Flatten row-wise
    end
    
    elapsed = seconds(msTimestamp-msTimestamp(1));
    fitnessValue = sum(abs(nsTimestamp - elapsed));

end

function [newTimes] = interpolateRRITimestamps(times)
    % Linearly interpolates repeated timestamps within an array to produce
    % unique, evenly distributed timestamp values. It's aimed at processing RRI data 
    % where timestamps are duplicated.
    %
    % Parameters:
    % times - [Array] Original array of timestamps, possibly containing duplicates.
    %
    % Returns:
    % newTimes - [Array] Modified array of timestamps with interpolated values replacing
    %            repeated entries.
    
    % Initialize the new times array with the original times
    newTimes = times;
    
    % if the first value is repeated fill with placeholder until the endIdx
    % and replace in the end 
    if times(1)  == times(2)
        endFirstRepeatedIdx = findEndIdx(times, 1, 1); % Find the last repeated data
        newTimes(1:endFirstRepeatedIdx - 1) = NaN;
        % When correcting the rest of the values MATLAB interprets NaN as
        % unique values so we start after them
        i = endFirstRepeatedIdx; 
    else
        % Otherwise start the interpolation of segments from the beginning
        i = 1;
    end

    % Find indices of unique timestamps
    [~, uniqueIdx, ~] = unique(times, 'stable');

    % Unique gets the first index, we'll interpolate the earlier values and
    % keep the latest repeated value. Documentation states the timestamp is
    % for the latest value. Subtracting 1 gets the earlier values and
    % leaves the last instance of the repeated value intact.
    repeatedIdx = setdiff(1:length(times),uniqueIdx)' - 1;  

    % Iterate over each repeated timestamp
    while i <= length(repeatedIdx)

        % Find the first index
        startIdx = repeatedIdx(i) - 1; % Start interpolation from latest unrepeated value
        
        % Find the last index (or last time the repeated value appears)
        endIdx = findEndIdx(times, repeatedIdx(i), repeatedIdx(i));
        
        interpolatedTimestamps = linspace(times(startIdx),times(endIdx),endIdx - startIdx + 1);
        
        % Replace values between startIdx and endIdx excluding them
        newTimes(startIdx + 1:endIdx - 1)= interpolatedTimestamps(2:end - 1);

        % Skip the indices that were already replaced in the last
        % interpolation
        delta = endIdx - startIdx;
        i = i + delta - 1;
    end
    
    % Replace the first repeated values with an interpolation before the
    % recording started assuming same separation between timestamps as the
    % rest of the recording
    if times(1)==times(2)
        % Compute a pseudo sample rate to interpolate the first timestamps
        pseudoFs = mean(diff(newTimes(endFirstRepeatedIdx:end)));
        firstInterpolatedSegment = linspace(newTimes(endFirstRepeatedIdx)-pseudoFs.*(endFirstRepeatedIdx-1),newTimes(endFirstRepeatedIdx),endFirstRepeatedIdx);
        newTimes(1:endFirstRepeatedIdx-1) = firstInterpolatedSegment(1:end-1);
    end
end

function cost = objective_function(C,tau)
    % Computes the cost based on the squared differences between a shifted matrix C and a set of tau values.
    %
    % Parameters:
    % C - [Array] A matrix or vector where each element represents a shifted timestamp.
    % tau - [Array] A matrix or vector of adjustment factors (delays or advances in time).
    %
    % Returns:
    % cost - [Numeric] The total squared difference cost, representing the alignment error.
    
    % Create a matrix of tau values where each column is filled with the corresponding tau value
    tau0 = tau(1);  % First element of tau

    % Calculate the total cost 
    cost = sum((C-tau+tau0).^2); % tau and tau0 get broadcasted

end

function [tau_opt, fval] = optimizeAlignment(T_ms,T_ns)
    % Optimizes the alignment between two sets of timestamps (in ms and ns) using the fmincon optimization tool.
    %
    % Parameters:
    % T_ms - [Array] Millisecond timestamps.
    % T_ns - [Array] Nanosecond timestamps, presumably needing alignment to T_ms.
    %
    % Returns:
    % tau_opt - [Array] The optimal tau values that minimize the alignment error.
    % fval - [Numeric] The function value (cost) at the solution, indicating the alignment error.
    
    options = optimoptions('fmincon', 'Algorithm',   'sqp', 'Display', 'off');
    % options = optimoptions('fmincon', 'Algorithm', 'sqp', 'UseParallel', true);
    % options.Display = 'iter-detailed';% For debugging
    
    tau_initial = -0.05 + 0.1 * rand(height(T_ms), 1);  % Generates n random numbers between -0.05 and 0.05
    lower_bounds = [];
    upper_bounds = [];
    
    tms0 = T_ms(1);
    C = T_ns - T_ms + tms0;  % Precompute shift
    [tau_opt,fval] = fmincon(@(tau) objective_function(C, tau), tau_initial, [], [], [], [], lower_bounds, upper_bounds, [], options);

end

function ECG = processECGSessions(subsession_list, optimized_alignment, verbose)
    % Helper function to process the ECG sessions. It processes multiple
    % sessions and merges them in the end.
    % 
    % Parameters:
    % subsession_list - [Cell Array] 1xN Contains the individual data that
    %                   will be processed.
    % optimized_alignment - [Logical] Determines whether optimized Timestamp 
    %                   alignment is used (default: false).
    % verbose - [Logical] Controls the display of the waitbar (default: false).
    % 
    % Returns:
    % ECG - [Struct] Contains the ecg_data and the rri_data tables with
    %               preprocessed data.

    if verbose
        h = waitbar(0, 'Processing ECG Sessions...');
    end
    
    ECG_sessions = cell(1, length(subsession_list));  % Preallocate cell array for efficiency
    total_subsessions = length(subsession_list);

    for i = 1:total_subsessions
        % This try-catch block attempts to ignore failed sessions where the ECG
        % was too short to be filtered
        try
            % Process an individual segment of the session
            ECG_sessions{i} = getECG(subsession_list{i}, optimized_alignment);
        catch ex
            % Catch if any errors occur
            disp(ex.message);
        end

        % Update the waitbar if needed
        if verbose
            waitbar(i / total_subsessions, h);
        end
    end
    
    % Merge the data for all subsessions
    ECG.ecg_data = extractAndConcatenateDataTable(ECG_sessions, true);
    ECG.rri_data = extractAndConcatenateDataTable(ECG_sessions, false);

    if verbose
        close(h);
    end
end

function RRI = processRRISessions(subsession_list, verbose)
    % Helper function to process RRI (R-R Interval) streams from a list of sessions,
    % it concatenates them into a single data structure once finished.
    %
    % Parameters:
    % subsession_list - [Cell Array] 1xN array containing data sources for each session to be processed.
    % verbose - [Logical] If true, displays a waitbar indicating the progress of processing the RRI sessions.
    %
    % Returns:
    % RRI - [Struct] Contains a single table ('rri_data') with RRI data from all processed sessions.
    
    if verbose
        h = waitbar(0, 'Processing RRI Sessions...');
    end
    RRI_sessions = cell(1, length(subsession_list));  % Preallocate cell array for efficiency
    for i = 1:length(subsession_list)
        try 
            % Load RRI data directly into the preallocated structure
            RRI_sessions{i} = getRRI(subsession_list{i});
        catch ex
            % Log warning message
            disp(ex.message);
        end

        % Update waitbar
        if verbose
            waitbar(i / length(subsession_list), h);
        end
    end
    
    % Concatenate all data tables from the sessions
    RRI.rri_data = extractAndConcatenateDataTable(RRI_sessions,false);

    if verbose
        close(h);
    end
end

function [ECG,RRI] = readSession(filepath,optimized_alignment,save_processed,verbose)
    % Reads the raw data from a session. Returns the RRI either by computing it 
    % from the raw ECG, or extracted from the streamed RRI. 
    %
    % Parameters:
    % filepath - [String] Path to the session or merged physio file.
    % optimized_alignment - [Logical] Determines whether optimized Timestamp alignment is used (default: false).
    % save_processed - [Logical] Determines whether to save processed data (default: false).
    % verbose - [Logical] Controls the display of the waitbar (default: false).
    %
    % Returns:
    % ECG - [Table] Nx2 table containing the Timestamp as datetime and the
    %       ECG values
    % RRI - [Table] Nx2 table containing the Timestamp as datetime and the
    %       RRI values

    if nargin < 2
        optimized_alignment = false;
    end
    if nargin < 3
        save_processed = false;
    end
    if nargin < 4
        verbose = false;
    end

    [file_dir, file_name, ~] = fileparts(filepath);
    

    %% Process streamed ECG
    subsession_list = getSeparatedSessions(filepath, true);
    ECG = processECGSessions(subsession_list, optimized_alignment, verbose);
    RRI = ECG.rri_data; % Use the processed ECG
    ECG = ECG.ecg_data;
   
    %% Process streamed RRI
    % subsession_list = getSeparatedSessions(filepath,false);
    % RRI = processRRISessions(subsession_list, verbose);

    %% Save the tables
    if save_processed
        save_name = fullfile(file_dir,'ECG.mat');
        save(save_name,'ECG','-v7.3'); % For large tables
        
        save_name = fullfile(file_dir,'RRI.mat');
        save(save_name,'RRI','-v7.3'); % For large tables
    end
end

function data = removeDuplicates(data)
    % Removes rows with duplicated values in the 'Timestamp' column
    %
    % Parameters:
    % data - [Table] The input table containing a 'Timestamp' column and potentially
    %        other columns. Rows with duplicate timestamps are to be removed.
    %
    % Returns:
    % data - [Table] A new version of the input table where all rows have unique
    %        timestamps, retaining the first occurrence of each timestamp and the
    %        order of the original data.
    
    [~, ia, ~] = unique(data.Timestamp, 'stable');  % Get indices of unique timestamps
    data = data(ia, :);  % Filter the table with only unique timestamps
end 

function saveAlignedRRI(savePath, participantId, alignedRRI)
    % saveAlignedRRI Saves data to a specified path under participant-specific folders
    %
    % Inputs:
    %   savePath - String, base directory where data should be saved
    %   participantId - String, identifier for the participant, used to create subdirectory
    %   alignedRRI - Data to be saved, typically a structure or array
    %
    % This function checks if the provided savePath is valid and not empty,
    % creates a participant-specific subdirectory if it does not exist, and saves
    % the alignedRRI data into a MAT-file named after the participantId.

    % Check if savePath is provided and not empty
    if ~isempty(savePath)
        % Build the full directory path including participantId
        participantDirectory = fullfile(savePath, participantId);

        % Check if the directory exists, if not, create it
        if ~isfolder(participantDirectory)
            mkdir(participantDirectory);  % Create the directory if it does not exist
        end

        % Create the full file path for saving the data
        fileSavePath = fullfile(participantDirectory, sprintf('%s_alignedRRI.mat', participantId));

        % Save the data using the MATLAB save function
        save(fileSavePath, 'alignedRRI');    
    end
end
