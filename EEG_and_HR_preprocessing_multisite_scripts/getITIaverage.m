function getITIaverage(participantDir,processedDir,preprocessedEEGDir)
    % Preprocess Muse validation data. If it receives a preprocessedEEGDir it uses that mat data.
    % Otherwise merges the physio files into one which is deleted at the
    % end. Extracts the stimulus and feedback times from the schedule file. 
    % Computes the ITI as the period after the last trial's figure faded and the stimulation time. 
    % The last trial's figure fade time is 1s after the feedback time. 
    % The ITIs longer than 3s are clipped for cases when there was a missed trial. 
    % The minimum duration of an ITI is 2s (for example for the first trial). Matches the times of ITI to the
    % EEG and computes the TF analysis and saves the data for all trials,
    % all channels and all frequencies by taking the mean over the time domain. 
    % 
    % Parameters:
    % participantDir - [string] Directory where the raw data is for this
    %                           participant
    % processedDir - [string] Path to save the intermediate files and the
    %                           final results
    % preprocessedEEGDir - [string] Path to preprocessed EEG (merged physio
    %                      files). 

    if nargin<3
        preprocessedEEGDir = [];
    end

    %% Load libraries
    eeglab;
    close;
    ft_defaults;    

    %% Read and preprocess EEG
    EEG = prepareEEG(participantDir,processedDir,preprocessedEEGDir);

    %% Read and preprocess schedule file
    upperLimit = seconds(3);
    trialsTable = prepareScheduleFile(participantDir,upperLimit);
    
    %% Epoch the data
    EEG = epochData(EEG,trialsTable,upperLimit);

    %% TF analysis
    [TF, freqs] = performTFAnalysis(EEG);
    
    %% Save the data
    [~, participantID] = fileparts(participantDir);
    saveMeanITITF(TF,participantID, processedDir);
end

function EEG = addEventsFromTrial(EEG, Trial, eventTypeLabel)
    % Create EEG.event and .urevent entries from a Trial table.
    % Iterates through each row of Trial and adds in an event that closest
    % matches the time in the EEG data.
    %
    % Parameters:
    % EEG - [EEGLab object] with fields:
    %                           EEG.etc.originalDateTimes: Nx1 datetime array, one entry per sample
    %                           EEG.srate
    % Trial - [table] with columns: eventTime (datetime)
    %                               preWindow (duration)
    % eventTypeLabel - [string] Label for this type of event
    %
    % Returns:
    % EEG - [EEGLab object] Same EEG object with filled in events

    % Check incoming data
    if ~isfield(EEG.etc, 'originalDateTimes')
        error('EEG.etc.originalDateTimes not found. Cannot map datetimes to sample indices.');
    end
    if isempty(eventTypeLabel)
        eventTypeLabel = 'trialEvent';
    end
    
    % Initialize the event struct array
    nTrials = height(Trial);
    newEvents = repmat(struct('type', [], 'latency', [], 'duration', [], 'urevent', [], 'preWindowSec', []), nTrials, 1);
    
    % Iterate through each trial
    fprintf('Adding events: 0/%d\n', nTrials);

    for i = 1:nTrials
        % 1) Find the nearest sample index for this eventTime
        [~, idx] = min(abs(EEG.etc.originalDateTimes - Trial.eventTime(i)));

        % 2) Populate the event struct
        newEvents(i).type        = eventTypeLabel;
        newEvents(i).latency     = idx;     % 1-based sample index
        newEvents(i).duration    = 0;       % default 0 for point event
        newEvents(i).urevent     = i;       % link event <-> urevent

        % Store the preWindow in a new field
        if ismember('preWindow', Trial.Properties.VariableNames)
            newEvents(i).preWindowSec = seconds(Trial.preWindow(i));
        else
            newEvents(i).preWindowSec = [];
        end

        % --- Update waitbar every 250 trials ---
        if mod(i,250)==0 || i==nTrials
            fprintf('Adding events: %d/%d\n', i, nTrials);
        end

    end

    % If EEG.event is non-empty, append. Otherwise, just overwrite.
    if ~isfield(EEG, 'event') || isempty(EEG.event)
        EEG.event = newEvents;
    else
        % Append to the existing events
        nOld = numel(EEG.event);
        for i = 1:nTrials
            EEG.event(nOld + i) = newEvents(i);
            EEG.event(nOld + i).urevent = nOld + i; 
        end
    end

    % Update EEG.urevent
    EEG.urevent = EEG.event;
end

function EEGout = buildEEGLabStruct(EEGin,participantId)
    % Create an EEGLAB-compatible EEG struct from custom EEG struct.
    %
    % Parameters:
    % EEGin - [struct] with fields: EEGin.data  -- N x M numeric, timePoints x channels
    %                               EEGin.times -- N x 1 datetime vector corresponding to EEGIN.data rows
    %                               EEGin.sampling_rate  -- numeric, sampling frequency in Hz
    %
    % Returns:
    % EEGout - [EEGLab object] The output EEGout is arranged per EEGLAB’s standard:
    %                          fields: EEGout.data = [M x N]
    %                                  EEGout.nbchan, EEGout.pnts, EEGout.trials, EEGout.srate, etc.
    %                                  EEGout.xmin, EEGout.xmax computed from first/last sample
    %

    % Sanity checks
    if ~isfield(EEGin, 'data') || ~isfield(EEGin, 'times') || ~isfield(EEGin, 'sampling_rate')
        error('Input EEG struct must contain data, times, and sampling_rate.');
    end
    [nSamples, nChannels] = size(EEGin.data);
    
    % Fill out required EEGLAB fields
    EEGout = struct();
    EEGout.data    = EEGin.data';                   % transpose: [channels x timePoints]
    EEGout.nbchan  = nChannels;                     % number of channels
    EEGout.pnts    = nSamples;                      % number of time points
    EEGout.trials  = 1;                             % continuous data has 1 "trial" for now
    EEGout.srate   = EEGin.sampling_rate;           % sampling frequency in Hz
    
    % Store the times
    timeVec = EEGin.times;                          % Nx1 datetime
    tStart  = timeVec(1);
    tEnd    = timeVec(end);
    totalDurationSec = seconds(tEnd - tStart);
    
    EEGout.xmin = 0;                                % EEGLAB's "start time" in seconds
    EEGout.xmax = totalDurationSec;                 % EEGLAB's "end time" in seconds

    % Store the entire datetime vector in EEGout.etc
    EEGout.etc.originalDateTimes = timeVec;
    
    % Fill out standard bookkeeping fields for EEGLab
    EEGout.setname   = char(participantId);
    EEGout.filename  = '';
    EEGout.filepath  = '';
    EEGout.subject   = participantId;
    EEGout.group     = '';
    EEGout.condition = '';
    EEGout.session   = [];
    EEGout.comments  = 'Events with stim_time and ITI';
    EEGout.etc.remove = EEGin.remove;

    % Add channel labels
    EEGout.chanlocs = struct('labels', cell(1, nChannels));
    museChannels = {'left_temp','left_front','right_front','right_temp'};
    for c = 1:nChannels
        EEGout.chanlocs(c).labels = museChannels{c};
    end

    % Initialize empty events/urevents
    EEGout.event   = [];
    EEGout.urevent = [];
    
    % Initialize ICA fields
    EEGout.icaweights = [];
    EEGout.icasphere  = [];
    EEGout.icaact     = [];
    EEGout.icawinv    = [];
    
end

function dateTimeTable = convertScheduleTableToDateTime(tempTable)
    % Convert all times in the table from posixtime to datetime objects
    % 
    % Parameters:
    % tempTable - [table] Contains the columns that need to be converted to
    %                      datetime.
    %
    % Returns:
    % dateTimeTable - [table] Same table but with datetime objects

    feedbackTime = convert_to_datetime(tempTable.feedback_time);
    choiceTime = convert_to_datetime(tempTable.choice_time);
    stimTime = convert_to_datetime(tempTable.stim_time);
    % schedTime = convert_to_datetime(tempTable.scheduled_time);
    dateTimeTable = table(feedbackTime,choiceTime,stimTime,'VariableNames',{'feedback_time','choice_time','stim_time'});

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
    % Create the 'EEG_muse' table with desired columns
    createTableQuery = ['CREATE TABLE EEG_muse (' ...
                        'recording_time DOUBLE, ' ...
                        'EEG1 TEXT, ' ...
                        'EEG2 TEXT, ' ...
                        'EEG3 TEXT, ' ...
                        'EEG4 TEXT, ' ...
                        'ISGOOD1 TEXT, ' ...
                        'ISGOOD2 TEXT, ' ...
                        'ISGOOD3 TEXT, ' ...
                        'ISGOOD4 TEXT);'];
    exec(db, createTableQuery);
    
    % Loop through each file in physioFiles to read and merge the data
    for i = 1:length(physioFiles)

        % Open each database file individually
        newdb = sqlite(physioFiles{i}, 'connect');
        
         try
            % Fetch available tables
            tables = fetch(newdb, 'SELECT name FROM sqlite_master WHERE type="table" AND name IN ("EEG_muse")');

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
                        if strcmp(table_name, 'EEG_muse')
                            colnames = {'recording_time', 'EEG1', 'EEG2', 'EEG3','EEG4','ISGOOD1','ISGOOD2','ISGOOD3','ISGOOD4'};
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

function EEG = epochData(EEG,Trial,upperLimit)
    % Epoch the EEG into the ITI events. Uses the stimulation time as time
    % zero and the ITI as window before the event. 
    % Note: EEGLab doesn't support independent event windows. The largest
    % window (upper limit) is used then the individual trials are
    % shortened.
    %
    % Parameters:
    % EEG - [EEGLab object] Data struct with the EEG data 
    %
    % Trial - [table] Contains the stim_times and event durations.
    %
    % upperLimit - [duration] Maximum duration of the ITI

    %% Add events to struct
    EEG = addEventsFromTrial(EEG, Trial, "stimTime"); % Add the stim events 
    
    %% Epoch the data to largest window
    largestPreWindowSec = max([EEG.event.preWindowSec]);   
    if largestPreWindowSec>seconds(upperLimit)
        disp("Error: largestPreWindow found larger than upperLimit")
    end
    
    EEG = pop_epoch(EEG, {"stimTime"}, [-largestPreWindowSec 0]); % Epoch to the maximum window
    
    %% Crop the excess data for each trial and fill with NaNs
    EEG = fillShortPrewindowWithNaN(EEG, seconds(upperLimit)); % Fill with NaN where outside the ITI window
    
end

function EEG = fillShortPrewindowWithNaN(EEG, targetPreWindowSec)
    % Fill pre-epoch data with NaNs if preWindow was shorter. Assumes the
    % original window times exist in EEG.urevent and that the data is
    % epoched (EEG.data is [chan x time x trials]
    %
    % Parameters:
    % EEG - [EEGLab object] Contains the epoched struct (after pop_epoch)
    %
    % targetPreWindowSec - [numeric] The max preWindow used during epoching
    %
    % Note: This function assumes that:
    %     - EEG.urevent(trial).preWindowSec exists (per-trial)
    %     - EEG.data is [chan x time x trials]

    nTrials = EEG.trials;
    fs = EEG.srate;
    preSamples = size(EEG.data, 2);  % number of timepoints per epoch

    for t = 1:nTrials
        if ~isfield(EEG.urevent(t), 'preWindowSec')
            warning('urevent(%d) missing preWindowSec. Skipping.', t);
            continue;
        end

        thisPreWindow = EEG.urevent(t).preWindowSec;

        % Only process if it's shorter than the full preWindow
        if thisPreWindow < targetPreWindowSec
            validSamples = round(thisPreWindow * fs);
            nToFill = preSamples - validSamples;

            if nToFill > 0
                EEG.data(:, 1:nToFill, t) = NaN;
            end
        end
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

function timeArray = getTrialITI(trialData, sampling_rate)
    % getTrialITI calculates the time vector for a given trial.
    %
    % Parameters:
    %   trialData    - [Array] Matrix of trial data [channels x time]
    %   sampling_rate - [double] Sampling rate in Hz (defaults to 256.03 if not provided)
    %
    % Returns:
    %   timeArray    - [Array] Time vector starting from -((Nsamples-1)/sampling_rate) to 0 seconds

    if nargin < 2
        sampling_rate = 256.03;
    end

    Nsamples = size(trialData, 2);
    timeArray = (-(Nsamples-1):0) / sampling_rate;
end

function [TF, freqs] = performTFAnalysis(EEG)
    % Performs time-frequency analysis on epoched EEG data.
    %
    % Parameters:
    % EEG    -   [struct] Structure containing the EEG data with fields:
    %            srate - [double] Sampling rate of the EEG data.
    %            data  - [Array] EEG data with dimensions [channels x time x events]
    %
    % Returns:
    % TF     - [Array] Time-frequency power spectrum [events x frequencies x time]
    % freqs  - [Array] Vector of frequencies of interest used in the analysis

    % Retrieve the sampling rate from the EEG structure
    sampling_rate = EEG.srate;
    
    % Permute EEG.data to have dimensions [events x channels x time]
    dataToAnalyze = permute(EEG.data, [3, 1, 2]);
    
    % Convert permuted data into a cell array; each cell corresponds to one event/trial
    dataToAnalyze = arrayfun(@(i) squeeze(dataToAnalyze(i, :, :)), ...
                             1:size(dataToAnalyze, 1), 'UniformOutput', false);
    
    % Remove time points with NaN values from each trial
    dataToAnalyze = cellfun(@removeNaN, dataToAnalyze, 'UniformOutput', false);
    
    % Apply a 60 Hz notch filter to each trial
    ftdata.trial = cellfun(@(x) vectorizedFilter(x, sampling_rate), ...
                           dataToAnalyze, 'UniformOutput', false);
    
    % Define the frequencies of interest for the analysis (in Hz)
    freqs = [1:0.5:10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 40];
    
    % Construct time arrays for each trial
    ftdata.time = cellfun(@(x) getTrialITI(x, sampling_rate), ...
                          ftdata.trial, 'UniformOutput', false);
    
    % Additional FieldTrip data configuration
    ftdata.fsample    = sampling_rate;
    ftdata.sampleinfo = [];

    % Define channel labels
    ftdata.label      = {EEG.chanlocs.labels};

    % Configure settings for time-frequency analysis using FieldTrip functions
    cfg              = [];
    cfg.output       = 'pow';
    cfg.method       = 'mtmconvol';
    cfg.taper        = 'hanning';
    cfg.foi          = freqs;
    cfg.t_ftimwin    = 3 ./ cfg.foi;  % Time window length per frequency (in seconds)
    cfg.toi          = 'all';         % Use all available time points for analysis
    cfg.keeptrials   = 'yes';         % Retain individual trial data
    cfg.pad          = 'nextpow2';    % Zero-pad to next power of 2 for efficient FFT
    
    TF = runFrequencyAnalysisWithNaNs(cfg,ftdata);
end

function EEG = prepareEEG(participantDir,processedDir,preprocessedEEGDir)
    % Merges all the physio files into one, preprocesses the final file, 
    % and builds an EEGLab object.
    %
    % Parameters:
    % participantDir - [string] Directory where the raw data is for this
    %                           participant
    % processedDir - [string] Path to save the intermediate files and the
    %                           final results
    %
    % preprocessedEEGDir - [string] Path to preprocessed EEG (merged physio
    %                      files). 
    %
    % Returns:
    % EEG - [EEGLab object] Struct containing all the EEG data for this
    %                       participant
    site = 'Pitt';
    [~, participantID] = fileparts(participantDir);
        
    if isempty(preprocessedEEGDir)
        % If the directory is not passed then merge the databases and
        % preprocess the EEG with readEEG.m

        disp("Merging databases");
    
        %% Merge the validation data and move it to processed directory temporarily
        pathToPhysioFiles = fullfile(participantDir,'physio'); % Directory where to look for the raw data/physio files.
        tempDbFile = createTemporalMergedDatabase(pathToPhysioFiles);
        
        % Move it to the relevant directory
        dirForReadEEG = fullfile(processedDir, "ITI_average","Data_Processed","subject_"+participantID); % ReadEEG needs this path and has to find just one file
        newDbFileName = participantID +"_merged_physio.db";
        fullDbFilePath = fullfile(dirForReadEEG,newDbFileName);
        
        % Check if the directory exists, and create it if it does not
        if ~exist(dirForReadEEG, 'dir')
            mkdir(dirForReadEEG);
        end
        movefile(tempDbFile,fullDbFilePath);
        
        %% Preprocess the data
        disp("Preprocessing EEG");
        [EEG, ~] = readEEG(fullfile(processedDir,"ITI_average"),char(participantID),site,0);
    else
        % Otherwise just load the preprocessed EEGDir
        dirForReadEEG =[];
        %% Preprocess the data
        disp("Preprocessing EEG");
        [EEG, ~] = readEEG(preprocessedEEGDir,char(participantID),site,0);
    end

    EEG.times = convert_to_datetime(EEG.times);
    
    % Move into eeglab
    fprintf("Building EEGLab struct for %s \n",participantID);
    EEG = buildEEGLabStruct(EEG,participantID);

    if ~isempty(dirForReadEEG)
        %% Clean merged physio 
        rmdir(dirForReadEEG, 's');
    end
end

function Trial = prepareScheduleFile(participantDir,upperLimit)
    % Read and preprocess the schedule file for this participant.
    % Converts the feedback, stim times to datetime and computes the ITI.
    % Receives an upper limit of how long the ITI can be. Lower limit is
    % set to 2 seconds.
    %
    % Parameters:
    % participantDir - [string] Directory where the raw data is for this
    %                           participant
    % upperLimit - [duration] The maximum duration the ITI can have.
    %                          ITIs longer than this will be truncated.
    %
    % Returns:
    % Trial -   [Table] Contains all the times for the trials as datetime,
    %                   but particularly has the columns preWindow and
    %                   eventTime with the ITI and the start time
    %                   respectively.

    %% Import the database
    disp("Preparing schedule file");
    [~, participantID] = fileparts(participantDir);
    Trial = readScheduleFile(participantID, participantDir);
    
    %% Convert to datetime
    Trial = convertScheduleTableToDateTime(Trial);
    
    %% Prepare ITI 
    % ITI: (black screen): 2-3 sec (sampled from a uniform distribution)
    difference = Trial.stim_time(2:end) - Trial.feedback_time(1:end-1) - seconds(1);% outcome appears for 1000 ms
    
    % Add dummy value for first trial
    difference = [seconds(4);difference];

    % If difference is greater than 3000ms (which is the upper limit) then use minimum which is 2 seconds 
    difference(difference>upperLimit) = seconds(2);
    Trial.preWindow = difference;
    Trial.eventTime = Trial.stim_time;

end 

function data = readScheduleFile(name,rootDir)
    % Read the schedule file and return the data as a table
    % 
    % Parameters:
    % name - [Char] The participant's Id
    % rootDir - [String] Directory where the Data_Raw subdirectory is located
    % 
    % Returns:
    % data - [Table] Table containing the schedule file feedback events
    
    filename = dir(fullfile(rootDir,"schedule","*schedule.db"));

    if length(filename) > 1
        error(sprintf('multiple schedule files found for subject',name,'%s'));
    elseif length(filename)<1
        error(sprintf('No schedule files found in ',rootDir,'%s'));
    end
    
    db = sqlite(strcat(filename(1).folder,'/',filename(1).name));
    data = fetch(db, 'SELECT feedback_time, stim_time, choice_time, scheduled_time FROM trials WHERE choice_time IS NOT NULL AND stim1>=0 AND stim2>=-1000 ORDER BY choice_time ASC');

    db.close;

    % For compatibility 
    if strcmp(class(data),'cell')
        data = cell2table(data,"VariableNames",["feedback_time" "stim_time" "choice_time","scheduled_time"]);
    end
    
end

function arrayOut = removeNaN(arrayIn)
    % Eliminates time points (columns) that contain any NaN values.
    %
    % Parameters:
    %   arrayIn - [Array] Input matrix [channels x time]
    %
    % Returns:
    %   arrayOut - [Array] Matrix with columns containing NaNs removed

    nanIndices = any(isnan(arrayIn), 1);
    arrayOut = arrayIn(:, ~nanIndices);
end

function TF = runFrequencyAnalysisWithNaNs(cfg, ftdata)
    % Run the frequency analysis, taking care of empty trials
    %
    % Parameters:
    % cfg - [struct] Configuration for the time frequency analysis
    % ftdata - [struct] Contains the data, times and channel labels for the
    %                   time frequency analysis
    %
    % Returns
    % TF     - [Array] Time-frequency power spectrum [events x frequencies x time]

    % Identify non-empty trials
    num_trials = length(ftdata.trial);
    non_empty_idx = cellfun(@(x) size(x,2)>0, ftdata.trial);

    % Save original trial count and prepare cleaned data
    original_trial_count = num_trials;
    ftdata_clean = ftdata;
    ftdata_clean.trial = ftdata.trial(non_empty_idx);
    ftdata_clean.time = ftdata.time(non_empty_idx);

    % Run frequency analysis
    pow = ft_freqanalysis(cfg, ftdata_clean);

    % Get TF data and trial dimensions
    TFclean = pow.powspctrm; % dimensions: trials x channels x frequencies x times
    [n_clean_trials, nchan, nfreq, ntime] = size(TFclean);

    % Create full TF array with NaNs for empty trials
    TF = NaN(original_trial_count, nchan, nfreq, ntime);
    TF(non_empty_idx, :, :, :) = TFclean;
end

function saveMeanITITF(TF,participantID,processedDir)
    % Save the mean power for the whole ITI, keeping the separation by
    % channel and trial.
    %
    % TF - [4D array] Dimensions [trial x channel x frequency x time]
    %
    % participantID - [string] Participant's id 
    %
    % processedDir - [string] Path to where the results will be saved
    
    % Take the mean across time
    meanTF = nanmean(TF,4);

    saveDir = fullfile(processedDir, "ITI_average");
    if ~exist(saveDir, 'dir')
        mkdir(saveDir);
    end
    save(fullfile(saveDir,participantID+"_averageITIPower"),"meanTF","-v7.3");

    % Save the full TF
    %saveDir = fullfile(processedDir, "ITI_average","FullTF");
    %if ~exist(saveDir, 'dir')
    %    mkdir(saveDir);
    %end
    %save(fullfile(saveDir, participantID+"_ITI_TF"), "TF", "-v7.3");
end

function dataOut = vectorizedFilter(dataIn, sampling_rate)
    % Applies a notch filter at 60 Hz to remove line noise.
    %
    % Parameters:
    % dataIn       - [Array] Trial data matrix [channels x time]
    % sampling_rate - [double] Sampling rate in Hz (defaults to 256.03 if not provided)
    %
    % Returns:
    % dataOut      - [Array] Filtered trial data with the 60 Hz component removed

    if nargin < 2
        sampling_rate = 256.03;
    end
    
    % Use FieldTrip's DFT filter for notch filtering at 60 Hz
    dataOut = ft_preproc_dftfilter(dataIn, sampling_rate, 60);
end
