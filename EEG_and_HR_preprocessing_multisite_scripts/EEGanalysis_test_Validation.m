function [TFdata] = EEGanalysis_test_Validation(name,rootDir,itemsToSave)
    % Full pipeline for preprocessing Muse and Biosemi data.
    % For Muse: Imports and merges the data.
    %           Corrects the EEG timestamps.
    %           Aligns the data to the feedback times and segments it.
    %           Runs the time-frequency analysis.
    % For Biosemi:  Imports the data.
    %               Aligns the phone feedback times to the high-speed camera.
    %               Uses the TTL pulses between the camera and the Biosemi,
    %               to align the feedback events.
    %               Runs the time-frequency analysis.
    %
    % Parameters:
    % name -        [char] Participant Id
    % rootDir -     [string] Directory where the raw data will be taken from.
    % itemsToSave - [string array] Contains the strings of the steps to run
    %               and save. Can contain any of: {"EEG","alignment","TF"}
    % 
    % Returns -     [struct] Contains the time-frequency analysis results
    %               for the muse and 4 biosemi sessions.
    %
    % Additional notes: The structure of the directories should be as follows: 
    % Muse raw data files:
    % rootDir->mat_files_csv_and_MUSE_db->{name}->{name}_physio_{YYYYMMDDHHmmSS}.db
    % Muse and Biosemi schedule file:          |->{name}_schedule.db
    % Biosemi camera timestamps:               |->{name}-{sessionNb}.mat
    % Biosemi scored frames:                   |->{name}-{sessionNb}.csv
    %       |
    % Biosemi raw data:    
    %       |->BIOSEMI_bdf->{name}-{sessionNb}.bdf             
    
    % Set up optional parameters
    if nargin<3
        itemsToSave = ['TF'];
    end
    
    % Open eeglab to load plugins, Note: running eeglab nogui; won't load them
    eeglab; 
    close;
    ft_defaults; % Load defaults

    % epochWindow = [-1.500, 3];  % Time window around the event, in s
    epochWindow = [-1.600, 3];  % Time window around the event, in s
    TFwindow = [-1000,2500];
    scenario = 'feedback';

    %% MUSE: 
    runMuseAnalysis(name,rootDir,scenario,epochWindow,TFwindow,itemsToSave);
 
    %% BIOSEMI :
    % Preprocess and align data
    eeglab nogui;
    [TFdata.biosemi,alignedEEG] = runAllBiosemiSessions(name,rootDir,scenario, epochWindow,TFwindow, itemsToSave);

end

function saveEEGDir = makeEEGSaveDir(name,rootDir)
    % Directory to save EEG Data only 
    saveEEGDir = fullfile(rootDir,'EEGData',name);
    if ~exist(saveEEGDir,'dir')
        mkdir(saveEEGDir);
    end
end

function [TFdata,alignedEEG] = runAllBiosemiSessions(name,rootDir,scenario, epochWindow,TFwindow,itemsToSave)
    TFdata = [];

    %% Directories to save the data
    % Directory to save alignment variables for analysis
    if any(strcmp('alignment', itemsToSave))
        saveDir = fullfile(rootDir,'AlignmentVariables',name);
        if ~exist(saveDir,'dir')
            mkdir(saveDir);
        end
    else
        saveDir = '';
    end

    %% Preprocessing
    numBiosemiSessions = 4;
    EEGBiosemi = cell(numBiosemiSessions,1); % Only used if collecting the EEG
    scheduleData = readScheduleFile(name,rootDir);
    
    for sessionNb = 1:numBiosemiSessions 

        % Camera timestamps and frames of interest
        blockNumber = getBlockNumber(str2double(name), sessionNb);
        sessionScheduleData = filterByBlock(scheduleData,blockNumber);
        
        % Prepare to save the alignment variables
        feedbackEvents = getPhoneCameraScoredTimestamps(name,rootDir,sessionNb,sessionScheduleData);
        if any(strcmp('alignment', itemsToSave))
            saveName = fullfile(saveDir,sprintf('%s_BSsession_%i_PhoneToCameraCorrection',name,sessionNb));
        else
            saveName = '';
        end

        % Align phone timestamps to camera timestamps
        feedbackEvents = alignPhoneToCameraTimestamps(feedbackEvents,saveName);
        
        % Preprocess EEG data and align camera timestamps to biosemi 
        [alignedEEG, feedbackEvents] = preprocessBiosemiSession(name, rootDir, sessionNb, feedbackEvents, epochWindow,saveDir);
        
        if any(strcmp('TF', itemsToSave))
            % Run TF analysis
            TFdata{sessionNb} = prepareTFAnalysis(alignedEEG, 'biosemi', scenario, TFwindow);
    
            % Add the missing trials as NaNs
            TFdata{sessionNb}.data = addMissingTrials(TFdata{sessionNb}.data, feedbackEvents.validIndices);
        end

        if any(strcmp('EEG', itemsToSave))
            EEGBiosemi{sessionNb} = addMissingTrials(alignedEEG.data, feedbackEvents.validIndices);
        end
    end

    if any(strcmp('TF', itemsToSave))
        % Concatenate sessions together
        TFdata = concatenateBiosemiSessions(TFdata);
    
        % Define the channel names
        biosemiChannelNames = arrayfun(@(x) x.labels, alignedEEG.chanlocs, 'UniformOutput', false); % Recover the channel names from any of the sessions
        
        % Save the processed data
        saveTFData(TFdata, rootDir, name, 'biosemi', biosemiChannelNames, scenario, alignedEEG.srate);
    end

    if any(strcmp('EEG', itemsToSave))
        
        saveEEGDir = makeEEGSaveDir(name,rootDir);

        % Concatenate the EEG in biosemi and save
        EEGBiosemi = cat(3,EEGBiosemi{:}); % Concatenate along the trial dimension before saving
        saveName = fullfile(saveEEGDir,'biosemiEEG.mat');
        save(saveName,'EEGBiosemi','-v7.3');
    end
end

function updatedData = addMissingTrials(data, validIndices)
    % Function to fill missing, unprocessed or ignored trials with NaNs
    % for both TF and EEG data arrays.
    %
    % Parameters:
    % data - [array] Processed data with dimensions:
    %        TF Data: Trials x Frequency Bins x Time Points x Channels
    %        EEG Data: Channels x Time Points x Trials
    % validIndices - [array] Contains 1's where the trials were valid and
    %                0's where they were invalid.
    %
    % Returns:
    % updatedData - [array] An array with NaN values where the trials
    %               were missing based on the validIndices array.

    % Determine the number of dimensions of the input data
    dataDims = ndims(data);

    % Check if there are any zeros in validIndices
    if all(validIndices == 1)
        updatedData = data;
        return;
    end

    % Get the size of the data and prepare updatedData with NaNs
    dataSize = size(data);
    totalTrials = length(validIndices);
    
    % Adjust the size of updatedData to include all trials
    dataSize(end) = totalTrials;  % Update number of trials dimension
    updatedData = NaN(dataSize);  % Initialize with NaNs
    
    % Find indices of the valid trials
    validTrialIndices = find(validIndices);

    if dataDims == 4  % TF Data
        % Fill valid trials with existing data
        updatedData(validTrialIndices, :, :, :) = data(1:length(validTrialIndices), :, :, :);
    elseif dataDims == 3  % EEG Data
        % Fill valid trials with existing data
        updatedData(:, :, validTrialIndices) = data(:, :, 1:length(validTrialIndices));
        updatedData = single(updatedData);
    end
end

function feedbackEvents = alignCameraWithTTL(feedbackEvents,biosemiSamplingRate,biosemiTimes,saveName)
    % Map the feedback events from the camera time to the biosemi scale
    % 
    % Parameters:
    % feedbackEvents - [struct] Contains the feedback times in the camera scale
    % biosemiSamplingRate - [numeric] The biosemi sampling rate
    % biosemiTimes - [Array of double] The array of biosemi timestamps 
    % saveName - [string] Optional: if not empty, saves the time drift slope and bias
    %                       under that directory
    %
    % Returns:
    % feedbackEvents - [struct] Contains the feedback times in the camera scale
    %                           and now also the datapoints in biosemi where the
    %                            feedback events happened
    
    if nargin<4
        saveName = '';
    end

    % Recover the camera TTL pulse 
    cameraTTLPulse = feedbackEvents.cameraTimestamps(80:80:end);
    
    % Convert the biosemi latencies to s
    biosemiLatencies_s = double(feedbackEvents.latencies)/biosemiSamplingRate;
    
    % Verify that sizes match and correct if there are repeated points
    if size(cameraTTLPulse, 1) ~= size(biosemiLatencies_s, 1)
        % Check and correct pulse continuity
        cameraTTLPulse = correctPulseContinuity(cameraTTLPulse);
        biosemiLatencies_s = correctPulseContinuity(biosemiLatencies_s);
        

        % Retry with a different slicing if sizes still don't match
        if size(cameraTTLPulse, 1) ~= size(biosemiLatencies_s, 1)
            
            [bestSlicingInd,bestDelay] = findBestSlicingAndDelay(feedbackEvents.cameraTimestamps,biosemiLatencies_s);
            
            cameraTTLPulse = feedbackEvents.cameraTimestamps(bestSlicingInd:bestSlicingInd:end);
            cameraTTLLength = length(cameraTTLPulse);
            biosemiTTLLength = length(biosemiLatencies_s);

            % Determine which array is smaller and which is larger and
            % correct it
            if cameraTTLLength > biosemiTTLLength
                cameraTTLPulse = cameraTTLPulse(bestDelay:biosemiTTLLength+bestDelay-1);
            else
                biosemiLatencies_s = biosemiLatencies_s(bestDelay:cameraTTLLength+bestDelay-1);
            end
        end
    end

    % Check for best TTL pulses, remove outliers
    P = polyfit(1:length(cameraTTLPulse), cameraTTLPulse-biosemiLatencies_s,1);
    yfit = polyval(P,1:length(cameraTTLPulse));
    ts_corrected = cameraTTLPulse-biosemiLatencies_s-yfit';
    
    %% Method 1: Mapping with linear function
    [~,outlierLogic] = removeOutliers(ts_corrected);

    % Fit a linear curve
    p = polyfit(cameraTTLPulse(~outlierLogic),biosemiLatencies_s(~outlierLogic),1);
    
    % Save if needed
    if ~isempty(saveName)
        save(saveName,'p');
    end

    % Get the feedback times in biosemi time scale
    alignedBiosemiEventTimes = polyval(p,feedbackEvents.alignedCameraTimestamps);

    % Get the biosemi indices of the events
    biosemiFeedbackEvents = round(alignedBiosemiEventTimes * biosemiSamplingRate);

    % Collect the computed event times which don't fit in the timestamps
    % and mark them as NaNs
    biosemiFeedbackEvents(biosemiFeedbackEvents < 0) = NaN;  % Set NaN where indices are negative
    biosemiFeedbackEvents(biosemiFeedbackEvents > length(biosemiTimes)) = NaN;  % Set NaN where indices are outside the maximum
    feedbackEvents.validIndices  = updateValidIndices(feedbackEvents.validIndices, find(isnan(biosemiFeedbackEvents)));

    %% Method 2: Finding closest TTL pulse and using its difference
    % Get the differences matrix between pulse and event times
    % differencesTTLEvent = feedbackEvents.alignedCameraTimestamps - cameraTTLPulse';
    % 
    % % Get the indices of the values closest to zero
    % [~,indClosest] = min(abs(differencesTTLEvent),[],2);
    % 
    % % Get the difference of the closest TTL camera pulse to the biosemi
    % tsDifference = cameraTTLPulse-biosemiLatencies_s;
    % correction = tsDifference(indClosest);
    % 
    % % Add the correction to the camera timestamps
    % alignedBiosemiEventTimes = feedbackEvents.alignedCameraTimestamps - correction;
    % 
    % % Get the indices of the events
    % bsSampleIndices = round(alignedBiosemiEventTimes * biosemiSamplingRate);
    % 
    % % Ensure indices are within valid range
    % biosemiFeedbackEvents = max(1,min(length(biosemiTimes),bsSampleIndices));
    
    %% Method 3: Adding yfit's smallest residual
    % % subtract off time between TTL pulse in BIOSEMI (any one will do) (corrected version) and
    % % feedback events
    % [~,TTL_ix_to_align] = min(abs(ts_corrected));
    % TTL_to_align = biosemiLatencies_s(TTL_ix_to_align);
    % 
    % deltat = ts_corrected(TTL_ix_to_align) - cameraTTLPulse(TTL_ix_to_align);
    % 
    % for i = 1:length(feedbackEvents.alignedCameraTimestamps)
    %     if feedbackEvents.alignedCameraTimestamps(i) > deltat
    %         biosemiFeedbackEvents(i) = feedbackEvents.alignedCameraTimestamps(i) - deltat;
    %     elseif feedbackEvents.alignedCameraTimestamps(i) < deltat
    %         biosemiFeedbackEvents(i) = feedbackEvents.alignedCameraTimestamps(i) + deltat;
    %     end
    % end
    % % Get the biosemi indices of the events
    % bsSampleIndices = round(biosemiFeedbackEvents' * biosemiSamplingRate);
    % 
    % % Ensure indices are within valid range
    % biosemiFeedbackEvents = max(1, min(length(biosemiTimes), bsSampleIndices));
    %% Add the result to the main feedbackEvents struct
    feedbackEvents.alignedBiosemiEvents= biosemiFeedbackEvents;
end

function feedbackEvents = alignPhoneToCameraTimestamps(feedbackEvents,saveName)
    % Corrects the manually scored feedback times to times which are closer to the phone timestamps.
    %
    % Parameters:
    % feedbackEvents - [struct] Contains the phone feedback times used as
    %                           ground truth.
    %                           Contains the manually scored feedback times that
    %                           will be corrected.
    %                           Contains the full array with camera
    %                           timestamps.
    % saveName - [string] Optional: if not empty, saves the alignment correction 
    %
    % Returns:
    % feedbackEvents - [struct] Same object but now containing the
    %                           corrected timestamps
    if nargin<2
        saveName = '';
    end

    % Get the minimal shift 
    [tau_opt, ~] = optimizeAlignment(feedbackEvents.scoredFeedbackTimes,feedbackEvents.phoneFeedbackTimes);
    
    % Correct the timestamps
    alignedCameraTimestamps = feedbackEvents.scoredFeedbackTimes+ tau_opt;
     
    %% If just getting the camera timestamps then obtain them here otherwise keep original values to avoid losing precision when aligning to a higher sampling frequency 
    % alignedCorrected = [];
    % for i=1:length(alignedCameraTimestamps)
    % 
    %     % Look for closest camera timestamp that matches the computed value
    %     % alignedCorrected(end+1)= findClosestValue(alignedCameraTimestamps(i), feedbackEvents.cameraTimestamps);
    % end
    % alignedCameraTimestamps = feedbackEvents.cameraTimestamps(alignedCorrected');

    %%
    % Feedback events relative to camera timestamps 
    feedbackEvents.alignedCameraTimestamps = alignedCameraTimestamps;
    
    if ~isempty(saveName)
        save(saveName,'tau_opt');
    end

end

% function EEG = applyChannelOps(EEG, type,mastoidRereference)
%     % Generalized function to apply channel operations based on type
%     %
%     % Parameters:
%     % EEG - [struct] EEGLAB data struct
%     % type - [string] 'reReference' or 'labelOnly'
%     % mastoidRereference - [logical] set to false to reference to FPz
%     % 
%     % Returns:
%     % EEG - [struct] updated EEGLAB data struct
% 
%     if nargin<3
%         mastoidRereference = true;
%     end
% 
%     % List of new channel labels
%     labels = {'Fp1', 'AF7', 'AF3', 'F1', 'F3', 'F5', 'F7', 'FT7', 'FC5', 'FC3', 'FC1', ...
%               'C1', 'C3', 'C5', 'T7', 'TP7', 'CP5', 'CP3', 'CP1', 'P1', 'P3', 'P5', 'P7', ...
%               'P9', 'PO7', 'PO3', 'O1', 'Iz', 'Oz', 'POz', 'Pz', 'CPz', 'Fpz', 'Fp2', 'AF8', ...
%               'AF4', 'AFz', 'Fz', 'F2', 'F4', 'F6', 'F8', 'FT8', 'FC6', 'FC4', 'FC2', ...
%               'FCz', 'Cz', 'C2', 'C4', 'C6', 'T8', 'TP8', 'CP6', 'CP4', 'CP2', 'P2', ...
%               'P4', 'P6', 'P8', 'P10', 'PO8', 'PO4', 'O2', 'VEO+', 'VEO-', 'HEOL', 'HEOR', 'M1', 'M2'};
% 
% 
%     if strcmp(type, 'reReference')
%         cmd = cell(1, numel(labels));
% 
%         for i = 1:numel(labels)
%             if ~ismember(labels{i}, {'VEO+', 'VEO-', 'HEOL', 'HEOR', 'M1', 'M2'})
%                 if mastoidRereference
%                     cmd{i} = sprintf('nch%d = ch%d - ((ch69 + ch70)/2) Label %s', i, i, labels{i});
%                 else
%                     cmd{i} = sprintf('nch%d = ch%d - ch33 Label %s', i, i, labels{i});
%                 end
%             else
%                 cmd{i} = sprintf('nch%d = ch%d Label %s', i, i, labels{i});
%             end
%         end
%     elseif strcmp(type, 'labelOnly')
%         cmd = cell(1, numel(labels)+2);
% 
%         for i = 1:numel(labels)
%             cmd{i} = sprintf('nch%d = ch%d Label %s', i, i, labels{i});
%         end
%         cmd{i+1} = 'nch71 = ch65 - ch66 Label biVEOG';
%         cmd{i+2} = 'nch72 = ch67 - ch68 Label biHEOG';
%     end
% 
%     % Apply channel operation in EEGLAB
%     EEG = pop_eegchanoperator(EEG, cmd,'Saveas','off'); % ERP Lab function
%     EEG = eeg_checkset(EEG); % Make sure there are no errors 
% end

function EEG = applyChannelOps(EEG, type, mastoidReference)
    % APPLYCHANNELOPS Applies channel operations (labeling, re-referencing) 
    % 
    % Usage:
    %   EEG = applyChannelOps(EEG, 'reReference', 'mastoids');
    %   EEG = applyChannelOps(EEG, 'reReference', 'fpz');
    %   EEG = applyChannelOps(EEG, 'reReference', 'ipsilateral');
    %   EEG = applyChannelOps(EEG, 'labelOnly');
    %
    % Parameters:
    %   EEG - [struct] EEGLAB data structure
    %   type - [string] 'reReference' or 'labelOnly'
    %   mastoidReference - [string] 'mastoids', 'fpz', or 'ipsilateral'
    %
    % Returns:
    %   EEG - [struct] updated EEGLAB data structure

    % Default to 'mastoids' if not specified:
    if nargin < 3 || isempty(mastoidReference)
        mastoidReference = 'mastoids';
    end

    % Validate mastoidReference input:
    validRefs = {'mastoids','fpz','ipsilateral'};
    if ~ismember(lower(mastoidReference), validRefs)
        error('mastoidReference must be one of: %s', strjoin(validRefs, ', '));
    end

    % List of new channel labels in the dataset (1-based indexing):
    labels = { ...
        'Fp1','AF7','AF3','F1','F3','F5','F7','FT7','FC5','FC3','FC1', ...
        'C1','C3','C5','T7','TP7','CP5','CP3','CP1','P1','P3','P5','P7', ...
        'P9','PO7','PO3','O1','Iz','Oz','POz','Pz','CPz','Fpz','Fp2','AF8', ...
        'AF4','AFz','Fz','F2','F4','F6','F8','FT8','FC6','FC4','FC2', ...
        'FCz','Cz','C2','C4','C6','T8','TP8','CP6','CP4','CP2','P2', ...
        'P4','P6','P8','P10','PO8','PO4','O2','VEO+','VEO-','HEOL','HEOR','M1','M2'};
    

    % Define sets of left, right, and midline scalp labels for ipsilateral reference
    [leftChLabels, rightChLabels, midlineChLabels] = categorizeLabels({labels{1:64}});

    % Build the "cmd" cell array that will be passed to pop_eegchanoperator
    if strcmpi(type, 'reReference')

        cmd = cell(1, numel(labels));

        for i = 1:numel(labels)
            currentLabel = labels{i};

            % Skip re-referencing for special channels
            if ismember(currentLabel, {'VEO+','VEO-','HEOL','HEOR','M1','M2'})
                % Just relabel these channels without any referencing
                cmd{i} = sprintf('nch%d = ch%d Label %s', i, i, currentLabel);
                continue;
            end

            switch lower(mastoidReference)
                case 'mastoids'
                    % Reference to the average of M1 (ch69) and M2 (ch70)
                    cmd{i} = sprintf('nch%d = ch%d - ((ch69 + ch70)/2) Label %s', ...
                                     i, i, currentLabel);

                case 'fpz'
                    % Reference to Fpz (ch33)
                    cmd{i} = sprintf('nch%d = ch%d - ch33 Label %s', ...
                                     i, i, currentLabel);

                case 'ipsilateral'
                    % Reference to M1 if left, M2 if right, (M1+M2)/2 if midline
                    if ismember(currentLabel, leftChLabels)
                        % Left channels -> reference to M1 (ch69)
                        cmd{i} = sprintf('nch%d = ch%d - ch69 Label %s', ...
                                         i, i, currentLabel);
                    elseif ismember(currentLabel, rightChLabels)
                        % Right channels -> reference to M2 (ch70)
                        cmd{i} = sprintf('nch%d = ch%d - ch70 Label %s', ...
                                         i, i, currentLabel);
                    elseif ismember(currentLabel, midlineChLabels)
                        % Midline channels -> reference to average(M1, M2)
                        cmd{i} = sprintf('nch%d = ch%d - ((ch69 + ch70)/2) Label %s', ...
                                         i, i, currentLabel);
                    else
                        % If a label is not found in the sets 
                        error('Unknown location of label');
                    end
            end
        end

    elseif strcmpi(type, 'labelOnly')
        % Simply rename channels without re-referencing
        % + add bipolars for VEOG and HEOG
        cmd = cell(1, numel(labels) + 2);

        % Label each channel according to the list
        for i = 1:numel(labels)
            cmd{i} = sprintf('nch%d = ch%d Label %s', i, i, labels{i});
        end

        % Add extra channels for VEOG and HEOG
        cmd{numel(labels)+1} = 'nch71 = ch65 - ch66 Label biVEOG';
        cmd{numel(labels)+2} = 'nch72 = ch67 - ch68 Label biHEOG';
    else
        error('Unknown type: %s. Must be ''reReference'' or ''labelOnly''.', type);
    end

    % Apply channel operation in EEGLAB (from ERPLAB plugin)
    EEG = pop_eegchanoperator(EEG, cmd, 'Saveas','off');
    EEG = eeg_checkset(EEG); % Double-check for consistency
    
end

function [leftChLabels, rightChLabels, midlineChLabels] = categorizeLabels(labels)
    % CATEGORIZELABELS Vectorized categorization of EEG channel labels.
    % 
    % Inputs:
    %   labels - Cell array of EEG channel labels
    %
    % Outputs:
    %   leftChLabels - Cell array of left hemisphere channels (odd numbered)
    %   rightChLabels - Cell array of right hemisphere channels (even numbered)
    %   midlineChLabels - Cell array of midline channels (ending with 'z')
    
    % Find midline channels (labels ending with 'z')
    midlineChLabels = labels(endsWith(labels, 'z'));

    % Extract numeric endings from labels and convert them to numbers
    numericEndings = regexp(labels, '\d+$', 'match');
    numericEndings = cellfun(@(x) str2double(x), numericEndings, 'UniformOutput', false);
    emptyIdx = cellfun(@isempty, numericEndings);
    numericEndings(emptyIdx)={NaN};

    % Identify labels with valid numeric endings
    % hasNumeric = ~isnan(numericEndings);
    
    % Determine even and odd channel numbers
    evenIdx = mod([numericEndings{:}], 2) == 0;
    oddIdx = mod([numericEndings{:}], 2) == 1;

    % Assign even and odd labels to right and left channels respectively
    rightChLabels = labels(evenIdx);
    leftChLabels = labels(oddIdx);
end

function x_filtered = bandpassFilterEEG(x, fs, lowFreq,highFreq)
    % Apply bandpass filter on all channels of x 
    %
    % Parameters:
    % x - [double] EEG signal dimensions: (samples,channels)
    % fs - [double] sampling frequency
    %
    % Returns:
    % EEG - [struct] updated EEGLAB data struct

    % Normalize the frequencies by the Nyquist frequency
    nyquist = fs / 2;
    lowCutoff = lowFreq / nyquist;
    highCutoff = highFreq / nyquist;

    % Define the filter order
    filterOrder = 4; 

    % Design the Butterworth bandpass filter
    [b, a] = butter(filterOrder, [lowCutoff, highCutoff], 'bandpass');

    % Initialize the filtered data matrix
    x_filtered = zeros(size(x));

    % Apply the filter to each channel
    for i = 1:size(x, 2) 
        x_filtered(:, i) = filtfilt(b, a, x(:, i));
    end

    % Return the filtered data
    return
end

function eegEvent = buildEEGevent(feedbackEvents)
    % Build the EEG event struct for the EEG object
    %
    % Parameters:
    % feedbackEvents - [struct] Contains the feedback events that will be
    %                   used as events
    %
    % Returns:
    % eegEvent - [struct] Contains the feedback and no-feedback events 

    % Add the type of feedback event
    % eventTypes = repmat({'No_Feedback'}, length(feedbackEvents.feedbackType), 1); 
    % eventTypes(feedbackEvents.feedbackType==1) = {'Feedback'}; 
    eventTypes = repmat({0}, length(feedbackEvents.feedbackType), 1); 
    eventTypes(feedbackEvents.feedbackType==1) = {1}; 

    % Keep track of the missing trials (marked with NaNs)
    nanTrials = isnan(feedbackEvents.alignedBiosemiEvents);
    eventTypes(nanTrials) = {-1}; % Will get deleted but are still flagged 
    
    % Build the struct for the biosemi using the aligned events
    eegEvent = struct('type', eventTypes, 'latency', num2cell(feedbackEvents.alignedBiosemiEvents), 'duration', num2cell(0*ones(size(feedbackEvents.alignedBiosemiEvents))));

end

function TFdataConcatenated = concatenateBiosemiSessions(TFdata)
    % Concatenates the biosemi sessions into a single array across the time
    % dimension
    %
    % Parameters:
    % TFdata - [cell array] Contains the biosemi sessions in separate cells
    %
    % Returns:
    % TFdataConcatenated - [4D array] Contains all the concatenated
    %                       sessions
    
    TFdataConcatenated = TFdata{1}; % All sessions share the same fields except the data
    TFdataConcatenated = rmfield(TFdataConcatenated, 'data');
    TFdataConcatenated.data = cat(1,cell2mat(TFdata).data);

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

function correctedPulse = correctPulseContinuity(originalPulse)
    % Remove the pulses that are too near each other.
    % 
    % Parameters:
    % originalPulse - [Array] Contains the timestamps of the pulses
    %                           arrival times.
    %
    % Returns:
    % correctedPulse - [Array] The corrected pulse

    % Calculate time differences
    timeDiffs = diff(originalPulse);
    
    % Find indices of pulses too close to the previous pulse
    consecutiveIndices = find(timeDiffs < 0.5*mean(timeDiffs)); % less than 50percent of the mean difference
    
    % Mark the indices of pulses to keep
    validIndices = true(size(originalPulse));
    validIndices(consecutiveIndices + 1) = false; % Assuming removing the second pulse
    
    % Extract the corrected pulse
    correctedPulse = originalPulse(validIndices);

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

function filteredTable = filterByBlock(inputTable,targetBlock)
    % Filter the schedule file by block number
    % 
    % Parameters:
    % inputTable - [Table] Contains the data with the column block 
    % targetBlock - [Array of double] The block(s) of interest
    % 
    % Returns:
    % filteredTable - [Table] The filtered table

    % Get the row indices of interest
    rowIndices = ismember(inputTable.block, targetBlock);

    % Filter the table using the logical index
    filteredTable = inputTable(rowIndices, :);
end

function [bestSlicingInd,bestMinInd] = findBestSlicingAndDelay(cameraTimestamps,biosemiLatencies_s)
    % Finds the best way to slice the camera TTL pulse array to match it to
    % the received biosemi array. It tries different slicing indices and
    % with the extracted signal finds the shift that best matches both
    % pulses.
    % 
    % Parameters:
    % cameraTimestamps - [Array] The raw camera timestamps.
    % biosemiLatencies_s - [Array] The recorded biosemi events in seconds
    %
    % Returns: 
    % bestSlicingInd - [scalar] The value of the camera slicing index that has the
    %                       best match to the biosemi.
    % bestMinInd - [scalar] The position or delay to shift the small array
    %           forward to best match the larger array in the slicing index.
    %           Note: a value of 1 indicates no shift. 

    % Initialize arrays to store the results
    slicingInds = 75:85;
    numSlices = length(slicingInds);
    minVals = zeros(1, numSlices);
    minInds = zeros(1, numSlices);
    
    for idx = 1:numSlices
        slicingInd = slicingInds(idx);
        cameraTTLPulse = cameraTimestamps(slicingInd:slicingInd:end);
        [minVal, minInd] = findMinimalDelay(cameraTTLPulse, biosemiLatencies_s);
        
        % Accumulate the minVal and minInd
        minVals(idx) = minVal;
        minInds(idx) = minInd;
    end
    
    % Find the slicingInd with the smallest minVal
    [overallMinVal, minIdx] = min(minVals);
    bestSlicingInd = slicingInds(minIdx);
    bestMinInd = minInds(minIdx);
end

function closestIndex = findClosestValue(targetValue, searchArray)
    % Find the corresponding index in the array nearest to targetvalue
    % 
    % Parameters:
    % targetValue - [double] Target value to look in the array
    % searchArray - [Array of double] Array where the targetValue will
    %                                    be searched.
    % 
    % Returns:
    % closestIndex - [Integer] The index where the closest match was found.
    
    % Calculate the absolute differences
    differences = abs(searchArray - targetValue);
    
    % Find the index of the minimum difference
    [val_, closestIndex] = min(differences);
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

function fullPathToFile = findFileByParticipantId(participantId, rootDir, extension, sessionNb)
    % Function to locate a file based on participantId, extension, and optional modifier.
    %
    % Parameters:
    %   participantId - [Char] Participant Id
    %   extension      - [String] File extension to look for (e.g., 'mat','csv')
    %   sessionNb       - [String] session number to look for
    %
    % Returns:
    % fullPathToFile - [String] The full path to the file if found, empty if not found

    % Define the directory path based on the integer
    % directoryPath = fullfile(rootDir,'Data_Raw',participantId);
    directoryPath = fullfile(rootDir,'mat_files_csv_and_MUSE_db',participantId);
    
    % Construct the file name based on the presence of an optional modifier
    fileName = strjoin([num2str(participantId), '-', sessionNb, '.', extension],'');
    
    % Create the full search path
    searchPath = fullfile(directoryPath, fileName);
    
    % Use the dir function to find files matching the pattern
    fileInfo = dir(searchPath);
    
    % Check if any file was found
    if ~isempty(fileInfo)
        % File exists, construct the full file path
        fullPathToFile = fullfile(fileInfo.folder, fileInfo.name);
    else
        % File does not exist
        disp('No file found matching the criteria.');
        fullPathToFile = [];
    end
end

function [minVal, minInd] = findMinimalDelay(array1, array2)
    % Takes 2 arrays' differences and finds the shift the smaller array needs to fit 
    %  inside the longer array to minimize the euclidean distance. It's
    %  similar to a correlation where it finds the best delay one of the
    %  array needs to make the best match, except is not in absolute values
    %  but in the distance between consecutive datapoints.
    %
    % Parameters: 
    % array1 and array2 - [array] Arrays to be matched
    %
    % Returns:
    % minVal - [scalar] The value of the lowest norm found
    % minInd - [scalar] The position or delay to shift the small array
    %           forward to best match the larger array. Note: a value of 1 indicates
    %           no shift.
    
    % Ensure input arrays are column vectors
    array1 = array1(:);
    array2 = array2(:);

    % Determine which array is smaller and which is larger
    if length(array1) < length(array2)
        smallArray = array1;
        bigArray = array2;
    else
        smallArray = array2;
        bigArray = array1;
    end

    n_s = length(smallArray);
    n_b = length(bigArray);
    possibleDelays = n_b - n_s + 1;

    % Precompute differences
    diffSmall = diff(smallArray); % (n_s - 1) x 1
    diffBig = diff(bigArray);     % (n_b - 1) x 1

    % Generate indices for vectorized extraction
    indices = bsxfun(@plus, (1:n_s - 1)', 0:possibleDelays - 1);

    % Extract windows and compute differences
    D = diffBig(indices);         % (n_s - 1) x possibleDelays
    E = D - diffSmall;            % Broadcasting diffSmall across columns

    % Compute norms for each possible delay
    norms = sqrt(sum(E.^2, 1));   % 1 x possibleDelays

    % Find the minimum norm and its index
    [minVal, minInd] = min(norms);
end

function  blockNumber = getBlockNumber(participantId, sessionNb)
    % Get the block number depending on the participant Id and the session
    % number 
    %
    % Parameters:
    % participantId - [Integer] Id of participant
    % sessionNb - [Integer] Session number (1 to 4)
    %
    % Returns:
    % blockNumber - [Integer] Block where the session belongs

    % Hardcoded values from MUSE_Counterbalancing.xlsx Accessed: 5Sep2024
    % if file says BioSemi use blocks: 500-503, if Muse then use blocks 504-507
    lookupTable = [[440484,500];
                    [440488,500];
                    [440885,504];
                    [440868,500];
                    [440883,504];
                    [440929,500];
                    [440931,504];
                    [221465,500];
                    [221595,504];
                    [440933,500];
                    [220521,504];
                    [440191,500];
                    [440923,504];
                    [440924,500];
                    [440847,504];
                    [440869,500];
                    [440865,500];
                    [440942,504];
                    [440812,500];
                    [440807,504];
                    [440906,504];
                    [440825,500];
                    [440833,504];
                    [440790,500];
                    [440813,504];
                    [440804,500];
                    [440133,504];
                    [440774,500];
                    [440932,504];
                    [440747,500];
                    [440871,504];
                    [440752,500];
                    [440731,504];
                    [440701,500];
                    [440754,504]];
    
    % Read the lookupTable
    mainBlock = lookupTable(find(participantId == lookupTable(:,1)),2);
    
    % Return either the correct Biosemi block depending on sessionNb or
    % return all muse blocks
    if sessionNb==0
        % Return all muse blocks 
        if mainBlock == 500
            blockNumber = 504:507;
        else
            blockNumber = 500:503;
        end
    else    
        blockNumber = mainBlock + sessionNb - 1;
    end
end

function feedbackEvents = getPhoneCameraScoredTimestamps(name,rootDir,sessionNb,sessionScheduleData)
        % Collect the phone and camera timestamps, the manually scored
        % frames and the estimated camera speed
        %
        % Parameters:
        % name - [Char] The participant's Id
        % rootDir - [String] Directory where the Data_Raw subdirectory is located
        % sessionNb - [Double] Session number to process {1,2,3,4}
        % sessionScheduleData - [Table] Table containing the schedule file feedback events
        % 
        % Returns: 
        % feedbackEvents - [Struct] Contains the timestamps of the manually
        %                           scored frames, the phone and camera timestamps. 
        
        % Get the phone timestamps
        feedbackEvents.phoneFeedbackTimes = double(sessionScheduleData.feedback_time)/1000; % Convert to seconds
        feedbackEvents.feedbackType = sessionScheduleData.feedback;

        % Read camera timestamps
        camera_timestamps_path = findFileByParticipantId(name,rootDir, 'mat',string(sessionNb));
        feedbackEvents.cameraTimestamps = processCameraTimestamps(camera_timestamps_path,convert_to_datetime(sessionScheduleData.feedback_time(1)));
        
        % Read manually scored csv file
        feedback_frames_path = findFileByParticipantId(name,rootDir, 'csv',string(sessionNb));
        feedbackFrames = readtable(feedback_frames_path,'VariableNamingRule','preserve').Feedback;

        % Correct for missing values
        validIndices = ~isnan(feedbackFrames);  % Find indices of valid (non-NaN) entries
        feedbackFrames = feedbackFrames(validIndices);
        feedbackEvents.feedbackType = feedbackEvents.feedbackType(validIndices);
        feedbackEvents.phoneFeedbackTimes = feedbackEvents.phoneFeedbackTimes(validIndices);
        feedbackEvents.phoneFeedbackTimes = feedbackEvents.phoneFeedbackTimes-feedbackEvents.phoneFeedbackTimes(1); % Once corrected, make relative timestamps
        feedbackEvents.validIndices = validIndices; % Keep which were valid indices 

        % Correct for the no-feedback trials
        scoredEventsAbs =  feedbackEvents.cameraTimestamps(feedbackFrames); 
        scoredEventsRelative = scoredEventsAbs - scoredEventsAbs(1); % Relative timestamps
        camera_timestamps_with_no_feedback = scoredEventsRelative(feedbackEvents.feedbackType==0); % relative timestamps of scored frames where there was no feedback 
        phone_no_feedback_event_times = feedbackEvents.phoneFeedbackTimes(feedbackEvents.feedbackType==0); % Phone timestamps where there was no feedback
        diff_no_feedback = median(phone_no_feedback_event_times  - camera_timestamps_with_no_feedback); % Error of scoring no feedback trials
        feedbackEvents.scoredFeedbackTimes = scoredEventsAbs; % Copy the timestamps

        % Add the correction if there was no shift between beginning of
        % camera timestamps and relative phone timestamps
        feedbackEvents.scoredFeedbackTimes(feedbackEvents.feedbackType==0) = feedbackEvents.scoredFeedbackTimes(feedbackEvents.feedbackType==0)+diff_no_feedback; 
        feedbackEvents.cameraFps = median(1./diff(feedbackEvents.cameraTimestamps)); % Estimate the fps
        
end

function rejica = getRejectedICA(EEG)
    % Get ICA to be rejected 
    % 
    % Parameters:
    % EEG - [EEG Lab object] EEG containing the ICA components
    %
    % Returns:
    % rejica - [array] Contains the indices which should be rejected
    %                   accorindg to the criteria

    EEG = iclabel(EEG);
    ica_classifications = EEG.etc.ic_classification.ICLabel.classifications;
    ica_classes  = EEG.etc.ic_classification.ICLabel.classes;
    classToRemove = 'Eye';
    classInd = find(strcmpi(classToRemove,ica_classes));
    threshold = 0.8; % How high should the eye class be to be rejected
    rejica = find(ica_classifications(:,classInd)>threshold);
    
end

function cost = objective_function(T_camera, T_phone, tau)
    % Computes a cost depending on how different T_camera from T_phone is
    % once the tau is added to T_camera and then setting them back to start at zero.
    % 
    % Parameters:
    % T_camera - [Array of double] Contains the camera timestamps that will be optimized 
    % T_phone - [Array of double] Contains the phone timestamps used as
    %                               ground truth.
    % tau - [Array] The optimal tau values that minimize the alignment error.
    %
    % Returns:
    % cost - [Numeric] The total squared difference cost, representing the alignment error.

    % Calculate the total cost 
    cost = sum((T_phone - (T_camera + tau - (T_camera(1) +  tau(1)))).^2);

end

function [tau_opt, fval] = optimizeAlignment(T_camera,T_phone)
    % Optimizes the alignment between two sets of timestamps using the fmincon optimization tool.
    % 
    % Parameters:
    % T_camera - [array of double] Contains the camera timestamps that will be optimized 
    % T_phone - [array of double] Contains the phone timestamps used as
    %                               ground truth.
    % Returns:
    % tau_opt - [Array] The optimal tau values that minimize the alignment error.
    % fval - [Numeric] The function value (cost) at the solution, indicating the alignment error.
    
    options = optimoptions('fmincon', 'Algorithm',   'sqp', 'Display', 'off');
    % options = optimoptions('fmincon', 'Algorithm', 'sqp', 'UseParallel', true);
    % options.Display = 'iter-detailed';% For debugging
    
    tau_initial = zeros(height(T_camera), 1);  % Start at zero
    lower_bounds = [];
    upper_bounds = [];
    
    [tau_opt,fval] = fmincon(@(tau) objective_function(T_camera,T_phone, tau), tau_initial, [], [], [], [], lower_bounds, upper_bounds, [], options);
end

function [TFdata] = prepareTFAnalysis(EEG, eegSource, scenario,TFwindow)
    % Prepare the parameters for the time-frequency analysis and collect
    % the data for individual channels.
    %
    % Parameters:
    % EEG - [struct] Contains either the Muse data struct or the Biosemi
    %               EEGLab struct with the segmented data to be processed.
    % eegSource - [char] The source of the data to be processed e.g.:{'muse','biosemi'}
    % scenario - [char] Contains the type of event that is being analyzed e.g.:{'feedback','choice','stim'}
    % TFwindow - [Array 1x2] The time window of interest for the
    %                        time-frequency analysis
    %
    % Returns:
    % TFdataCollection - [array] The TF analysis results for each
    %                               channel of the EEG Dimensions: Trials x frequency bins x Timepoints x Channels
    
    if strcmp(eegSource,'muse')
        % Define channels and sampling rate
        nbChannels = 4;
        samplingRate = EEG.sampling_rate;

        % Define time window
        time_window = EEG.time_window;
        
        % Select channel data
        fieldName = ['epoch_data_' scenario];  % Construct the field name
        data = EEG.(fieldName);  % Access the data    
        
    elseif strcmp(eegSource,'biosemi')
        % Define channels and sampling rate
        nbChannels = EEG.nbchan;
        samplingRate = EEG.srate;

        % Define time window
        time_window = EEG.times;
        
        % Select channel data
        data = permute(EEG.data, [3, 2, 1]); % Arrange as events x time x channels
        
    end
     % Time window manipulation
    scenarioTime = find(time_window == 0);
    start = find(time_window < TFwindow(1), 1, 'last');
    stop = find(time_window > TFwindow(2), 1, 'first');
    times = time_window(start:stop);

    % Iterate for the channels
    for channel = 1:nbChannels
        
        % % Get channel name
        % channelName = channelNames{channel};
        
        % Time-frequency transformation
        [TFChannelData, freqs] = EEGtimefreq_US(data(:,:,channel), samplingRate);
        TFChannelData = squeeze(TFChannelData);
        TFdataCollection{channel} = TFChannelData(:, :, start:stop);
        
    end
    
    % Collect the data into a single object
    TFdata.data = cell2mat(reshape(TFdataCollection, [1, 1, 1, numel(TFdataCollection)])); % Dimensions: Trials x frequency bins x Timepoints x Channels
    % TFdata.data = TFdataCollection;
    TFdata.freqs = freqs;
    TFdata.scenarioTime = scenarioTime;
    TFdata.times = times;

end

function [EEG,feedbackEvents] = preprocessBiosemiSession(name,rootDir,sessionNb,feedbackEvents,epochWindow,saveDir)
    % Preprocess the biosemi data
    % 
    % Parameters:
    % name - [char] Participant Id
    % rootDir - [string] Path to the Data_Raw subdirectory
    % sessionNb - [double] Session number 
    % feedbackEvents - [struct] Contains the feedback events 
    % epochWindow - [array size 1x2] time to segment before and after
    %                               feedback event in seconds
    % saveDir - [string] Optional: if not empty, saves the alignment correction 
    %
    % Returns:
    % EEG - [struct] EEGLAB data structure
    
    if nargin<6
        saveName = '';
    end

    %% Prepare path
    rawDir = fullfile(rootDir, 'BIOSEMI_bdf', name);
    rawFilePath = [name '-' num2str(sessionNb) '.bdf'];
    filename = fullfile(rawDir, rawFilePath);
    
    %% Stage 1 - Filtering and Rereferencing
    EEG = preprocessBiosemiStage1(filename,name);
    
    % Save the events recorded in biosemi
    feedbackEvents.latencies = vertcat(EEG.event.latency);
    
    % Get the feedback events in biosemi scale
    % Send the biosemi times as seconds
    if ~isempty(saveDir)
        saveName = fullfile(saveDir,sprintf('%s_BSsession_%i_LinearRegressionParameters',name,sessionNb));
    else
        saveName = '';
    end
    feedbackEvents = alignCameraWithTTL(feedbackEvents,EEG.srate,EEG.times'/1000,saveName);
    EEG.event = buildEEGevent(feedbackEvents);
    
    % For troubleshooting
    % biosemiTimes_rel = double(feedbackEvents.alignedBiosemiEvents)/EEG.srate;
    % biosemiTimes_rel = biosemiTimes_rel- biosemiTimes_rel(1);
    % biosemiTimes_diff = diff(biosemiTimes_rel);
    % phoneTimes_diff = diff(feedbackEvents.phoneFeedbackTimes);
    % feedbackEvents.biosemiTimes_diff = biosemiTimes_diff;
    % feedbackEvents.phoneTimes_diff = phoneTimes_diff;
    
    %% Stage 2 - ICA
    EEG = preprocessBiosemiStage2(EEG);
   
    %% Stage 3 - EOGcalcs, Epoching, ArtDet, NoBCorr
    [EEG,omittedTrials] = preprocessBiosemiStage3(EEG,epochWindow);
    feedbackEvents.validIndices = updateValidIndices(feedbackEvents.validIndices, omittedTrials); % If the resampling step rejects trials due to nan values, then pop_epoch will return the wrong index of the omitted trials. This function takes care of that.

    %% Stage 4 - ICA removal, Final ArtifactReview
    EEG = preprocessBiosemiStage4(EEG);

    %% Stage 5 - Artifact removal
    % EEG = preprocessBiosemiArtifactRemoval(EEG);

end

function EEG = preprocessBiosemiStage1(fullFilePath,subjectId)

    % Read the BDF file
    [folderPath, fileName, extension] = fileparts(fullFilePath);
    currentDir = pwd;  % Save current directory
    cd(folderPath);
    fullFileName = convertStringsToChars(string(fileName)+string(extension));
    EEG = pop_biosig(fullFileName);
    % EEG = pop_loadset(fullFileName); % Used when loading an EEG  object

    cd(currentDir);
    EEG = eeg_checkset( EEG ); % General check of imported data (already runs inside pop_biosig) 

    % Rename the dataset
    EEG = pop_editset(EEG, 'setname',subjectId);
        
    % Re reference
    EEG = applyChannelOps(EEG, 'reReference','ipsilateral');

    % Filtering 
    % EEG  = pop_basicfilter( EEG,  1:70 , 'Boundary', 'boundary', 'Cutoff',  30, 'Design', 'butter', 'Filter', 'lowpass', 'Order',  2 );
    EEG = pop_basicfilter( EEG,  1:70 , 'Boundary', 'boundary', 'Cutoff', [0.1 50], 'Design', 'butter', 'Filter', 'bandpass', 'Order',  2 );

    % EEG = eeg_checkset( EEG );
            
    % Trimming skipped
    
    % Adds channel locations 
    EEG = pop_chanedit(EEG, 'lookup','standard-10-5-cap385.elp');
    EEG = eeg_checkset(EEG);
    
    % Record processing done
    EEG.EVENTLIST.INFO.README='These data added by Max to track channels removed, ICA components removed and proportion of artifacts';
    EEG.EVENTLIST.INFO.OriginalChannels=EEG.chanlocs;
    EEG.EVENTLIST.INFO.ChannelsRemoved= {'N/A'} ;
    
end

function EEG = preprocessBiosemiStage2(EEG)
    %% Stage 2
    % Run ica if no weights are found
    if isempty(EEG.icaweights)
        EEG = pop_runica(EEG,  'icatype', 'runica', 'dataset',1, 'options',{ 'extended',1}); % Runs ICA
        EEG = eeg_checkset( EEG ); % 
    end

    % Save ica results
    % EEG = pop_saveset( EEG,  'filename', [subjectId 'ICA.set']); % save ICA file
end

function [EEG,omittedTrials] = preprocessBiosemiStage3(EEG,epochWindow)
    % Resamples to 512Hz. 
    % Filter EOG channels
    % Epoching to the desired window 
    % 
    % Parameters: 
    % EEG - [EEGLab object] Preprocessed EEG Lab object
    % epochWindow - [array size 1x2] time to segment before and after
    %                               feedback event in seconds
    % 
    % Returns: 
    % EEG - [EEGLab object] Object after the steps mentioned above
    % omittedTrials - [array] Contains the trial indices ignored during the
    %                           epoching. Note that other trials ignored
    %                           during resampling are not accounted for and
    %                           indices may change.
    
    %% Stage 3
   
    %Hold or create information about original channel locations and interpolated channels (gets wiped by ERPLAB) 
    if isfield(EEG,'EVENTLIST')
    
        INFOHOLD=EEG.EVENTLIST.INFO ;
    
    else
        
        %creates EVENTLIST with no channels removed log. 
        EEG.EVENTLIST.INFO.README='These data added by Max to track channels removed, ICA components removed and proportion of artifacts';
        EEG.EVENTLIST.INFO.OriginalChannels=EEG.chanlocs;
        EEG.EVENTLIST.INFO.ChannelsRemoved= {'N/A'} ;  
        
        INFOHOLD=EEG.EVENTLIST.INFO ;
    
    end

    if EEG.srate ~= 512 % makes sure sample rate is 512hz.
        EEG = pop_resample( EEG, 512);
        EEG = eeg_checkset( EEG );
    end
    
    %Re-add removed channels if necessary
    if length({EEG.EVENTLIST.INFO.OriginalChannels.labels})~=length({EEG.chanlocs.labels})
        EEG = pop_interp(EEG, EEG.EVENTLIST.INFO.OriginalChannels,'spherical'); %default spherical interpolation is used.
    end  

    % Filtering 
    EEG  = pop_basicfilter( EEG, [33 34 35 36] , 'Cutoff',  30, 'Design', 'butter', 'Filter', 'lowpass', 'Order',  2 ); % filter EOG channels  
    EEG = eeg_checkset( EEG );
   
    % Adding new channels
    EEG = applyChannelOps(EEG, 'labelOnly','ipsilateral'); % third argument is unimportant 
    
    % Epoching
    EEG = pop_creabasiceventlist( EEG ,'AlphanumericCleaning', 'on', 'BoundaryNumeric', { -99 }, 'BoundaryString', { 'boundary' }, 'Warning', 'off' );
    EEG.EVENTLIST.INFO=INFOHOLD ;% Re-add EEG.EVENTLIST.INFO (is wiped by ERPLAB)
    nbOriginalEvents = size(EEG.event,2);
    
    %% Using EEGLab's function
    [EEG,acceptedEventIndices] = pop_epoch( EEG, {},epochWindow);
    omittedTrials = setdiff((1:nbOriginalEvents),acceptedEventIndices); % Collect which trials were ignored during epoching

    %% Using epoch3
    % tol = 4; % 4ms tolerance of alignment
    
    % Get the time intervals around the event in ms
    % pre_window = -1000 * epochWindow(1);
    % post_window = 1000 * epochWindow(2);
    % Outputs events x time x channels:  
    % [EEG,gap_feedback] = epoch3(EEG.times', EEG.data', [EEG.event.latency]', pre_window, post_window, EEG.srate,tol);


end

function EEG = preprocessBiosemiStage4(EEG)
    %% Stage 4
    EEG = pop_chanedit(EEG, 'lookup','standard-10-5-cap385.elp');
    EEG = eeg_checkset( EEG );
    
    ARTCHAN = EEG.reject.rejmanualE; % Copies artifact detected trials (these will be reapplied below at averaging)
    ARTTRIALS = EEG.reject.rejmanual;
    
    % Automatically select and reject ica components 
    rejica = getRejectedICA(EEG);
    EEG = pop_subcomp( EEG, rejica);
    
    % Insert artifact detected trials back into dataset and review artifact detection rates and trials.
    % Note on rejection rates: If rejection rate seems excessive (> 30%) review selected trials. Detection
    % can be influenced by noise. Selected trials can removed by left click.
    
    EEG = eeg_checkset( EEG );
    EEG.reject.rejmanualE = ARTCHAN;% adds artifact detected trials/channels into ICA corrected dataset
    EEG.reject.rejmanual = ARTTRIALS;

    % Runs final pass artifact detection to find large amplitude shifts
    % (i.e. movement artifacts)
    % The script looks through only scalp channels, excluding EOG and
    % reference electrodes.
    
    EEG  = pop_artmwppth( EEG , 'Channel', [1:64], 'Flag', [ 1 8], 'Review', 'off', 'Threshold',  200, 'Windowsize',  200, 'Windowstep',  100 );
    EEG = eeg_checkset( EEG );

    % Syncs any changes to detected trials between EEGLAB and ERPLAB
    EEG = pop_syncroartifacts(EEG,'Direction','eeglab2erplab'); % 
end

% function EEG = preprocessBiosemiArtifactRemoval(EEG)
%     EEG  = pop_artmwppth( EEG , 'Channel', [1:32 39 40], 'Flag', [ 1 8], 'Review', 'off', 'Threshold',  200, 'Windowsize',  200, 'Windowstep',  100 );
%     EEG = eeg_checkset( EEG );
%     artifact_proportion = getardetection(EEG); % Tells how many trials are selected for rejection
%     EEG = pop_syncroartifacts(EEG,'Direction','eeglab2erplab'); %
% end

function EEG = preprocessBiosemiArtifactRemoval(EEG)
    % 1) Mark artifacts in ERPLAB
    EEG = pop_artmwppth( EEG, ...
                         'Channel', [1:32 39 40], ...  % channels to check
                         'Flag', [1 8], ...           % which artifact flags to use
                         'Review', 'off', ...
                         'Threshold', 200, ...
                         'Windowsize', 200, ...
                         'Windowstep', 100 );
    
    % 2) Check EEG consistency
    EEG = eeg_checkset(EEG);

    % 3) Get proportion of trials flagged for artifact
    artifact_proportion = getardetection(EEG);
    fprintf('Proportion of flagged trials: %.2f%%\n', artifact_proportion*100);

    % 4) Sync artifact flags from ERPLAB -> EEGLAB
    EEG = pop_syncroartifacts(EEG, 'Direction', 'erplab2eeglab');
    
    % 5) Actually remove the epochs flagged by the detection.
    %    For instance, pop_artmwppth with 'Flag',[1 8] often sets "rejmanual" 
    %    or "rejmanualE" in EEG.reject. Double-check which field is set
    %    in your data, but typically it's `EEG.reject.rejmanual`.
    flagged_epochs = find(EEG.reject.rejmanual == 1);
    fprintf('Number of epochs flagged for removal: %d\n', length(flagged_epochs));
    
    if ~isempty(flagged_epochs)
        EEG = pop_rejepoch(EEG, flagged_epochs, 0);
    end

    % 6) Final consistency check
    EEG = eeg_checkset(EEG);
end

function results = preprocessMuseEEG(Trial, EEG, sampling_rate,epochWindow,rootDir,name)
    % Segments the muse EEG into the trials
    % 
    % Parameters:
    % Trial -         [struct] Contains the data from the schedule file
    % EEG -           [struct] Contains the physio file information with corrected timestamps
    % sampling_rate - [double] Sampling rate of the EEG
    % epochWindow -   [array 1x2] The pre and post time intervals to segment
    %                             around the event of interest in s
    % rootDir -       [string] Directory to the path where the Data_Processed
    %                          subdirectory is located
    % name -          [char] Participant Id
    % 
    % Returns:
    % results - [struct] Contains the segmented EEG data 
    
    tol = 4; % 4ms tolerance of alignment
    
    % Get the time intervals around the event in ms
    pre_window = -1000 * epochWindow(1);
    post_window = 1000 * epochWindow(2);
    
    % raw data AndyP 2023-08-16
    [epoch_data_feedback,gap_feedback] = epoch3(EEG.times, EEG.data, Trial.feedbackTimes, pre_window, post_window, sampling_rate,tol);
    [epoch_data_stim,gap_stim] = epoch3(EEG.times, EEG.data, Trial.stimTimes, pre_window, post_window, sampling_rate,tol);
    [epoch_data_choice,gap_choice] = epoch3(EEG.times, EEG.data, Trial.choiceTimes, pre_window, post_window, sampling_rate,tol);
    
    EEG.cleandata = EEG.data; EEG.cleandata(EEG.remove~=0)=NaN;
    [epoch_data_feedback_filtered,gap_feedback_filtered] = epoch3(EEG.times, EEG.cleandata, Trial.feedbackTimes, pre_window, post_window, sampling_rate,tol);
    [epoch_data_stim_filtered,gap_stim_filtered] = epoch3(EEG.times, EEG.cleandata, Trial.stimTimes, pre_window, post_window, sampling_rate,tol);
    [epoch_data_choice_filtered,gap_choice_filtered] = epoch3(EEG.times, EEG.cleandata, Trial.choiceTimes, pre_window, post_window, sampling_rate,tol);
    
    if pre_window==post_window && pre_window==1500
        ind_na_all_feedback = any(any(isnan(epoch_data_feedback_filtered(:,333:693,:)),2),3); %feedback appears at sample 385 and ends at sample 641, so trials with nan in the range of [-0.2, +0.2] from feedback will be considered as noisy
        ind_na_all_stim = any(any(isnan(epoch_data_stim_filtered(:,333:693,:)),2),3); %stim appears at sample 385 and ends at sample 641, so trials with nan in the range of [-0.2, +0.2] from feedback will be considered as noisy
        ind_na_all_choice = any(any(isnan(epoch_data_choice_filtered(:,333:693,:)),2),3); %choice occurs at sample 385 and ends at sample 641, so trials with nan in the range of [-0.2, +0.2] from feedback will be considered as noisy
    else
        ind_na_all_feedback = [];
        ind_na_all_stim = [];
        ind_na_all_choice = [];
    end
    Ntotal = length(Trial.feedbackTimes);
    Ngood_all= size(epoch_data_feedback_filtered,1);
    EEG_percen_all=(Ngood_all/Ntotal)*100;
    
    time_window = linspace(-pre_window,post_window,size(epoch_data_feedback,2));
    
    %find the optimal combination of three electrodes:
    [best_single, best_two_config, best_three_config, EEG_percen_single, EEG_percen_best_two, EEG_percen_best_three, ind_na_best_single, ind_na_best_two, ind_na_best_three]=find_optimal_comb(epoch_data_feedback, Ntotal);
    % Define the struct and assign each variable as a field
    results = struct(...
        'time_window', time_window, ...
        'ind_na_all_feedback', ind_na_all_feedback, ...
        'ind_na_all_stim', ind_na_all_stim, ...
        'ind_na_all_choice', ind_na_all_choice, ...
        'epoch_data_feedback', epoch_data_feedback, ...
        'epoch_data_feedback_filtered', epoch_data_feedback_filtered, ...
        'epoch_data_stim', epoch_data_stim, ...
        'epoch_data_stim_filtered', epoch_data_stim_filtered, ...
        'epoch_data_choice', epoch_data_choice, ...
        'epoch_data_choice_filtered', epoch_data_choice_filtered, ...
        'EEG_percen_all', EEG_percen_all, ...
        'best_single', best_single, ...
        'best_two_config', best_two_config, ...
        'best_three_config', best_three_config, ...
        'EEG_percen_single', EEG_percen_single, ...
        'EEG_percen_best_two', EEG_percen_best_two, ...
        'EEG_percen_best_three', EEG_percen_best_three, ...
        'ind_na_best_single', ind_na_best_single, ...
        'ind_na_best_two', ind_na_best_two, ...
        'ind_na_best_three', ind_na_best_three, ...
        'sampling_rate', sampling_rate, ...
        'gap_feedback', gap_feedback, ...
        'gap_stim', gap_stim, ...
        'gap_choice', gap_choice, ...
        'gap_feedback_filtered', gap_feedback_filtered, ...
        'gap_stim_filtered', gap_stim_filtered, ...
        'gap_choice_filtered', gap_choice_filtered, ...
        'tol', tol ...
        );
    % save(fullfile(rootDir, 'Data_Processed',['subject_' name] ,[name '_EEG1.mat']), 'results');
end

function museData = preprocessMuseData(name,epochWindow,rootDir)
    % Preprocess Muse validation data. Merges the physio files into a
    % temporary database, corrects the timestamps, aligns the data to the
    % feedback events and segments it with a window around the event.
    % 
    % Parameters:
    % name - [char] Participant Id
    % epochWindow - [array 1x2] The pre and post time intervals to segment
    %                       around the event of interest in s
    % rootDir - [string] Path to the participants subdirectory to look for the files
    % 
    % Returns:
    % museData - [struct] Contains the segmented EEG data 

    %% Merge the validation data and move it to processed directory temporarily
    pathToPhysioFiles = fullfile(rootDir,'mat_files_csv_and_MUSE_db',name); % Directory where to look for the raw data/physio files.
    tempDbFile = createTemporalMergedDatabase(pathToPhysioFiles);
    
    % Move it to the relevant directory
    dirForReadEEG = fullfile(rootDir, 'Data_Processed', ['subject_' name]);
    newDbFileName = [name '_merged_physio.db'];
    fullDbFilePath = fullfile(dirForReadEEG, newDbFileName);
    % Check if the directory exists, and create it if it does not
    if ~exist(dirForReadEEG, 'dir')
        mkdir(dirForReadEEG);
    end
    movefile(tempDbFile,fullDbFilePath);

    %% Read the data
    site = 'Pitt';
    [EEG, sampling_rate] = readEEG(rootDir,name,site,0);

    %% Read schedule file
    temp = readScheduleFile(name, rootDir);
    blockNumber = getBlockNumber(str2double(name), 0);
    temp = filterByBlock(temp,blockNumber);

    % Save data into new variable
    temp = table2array(temp);
    Trial.feedbackTimes = temp(:,1);
    Trial.stimTimes = temp(:,2);
    Trial.choiceTimes = temp(:,3);
    Trial.feedback = temp(:,4);
    
    %% Bandpass Filter between 0.1-50Hz
    EEG.data = bandpassFilterEEG(EEG.data, EEG.sampling_rate, 0.1,50);

    %% read EEG and remove trials with NaN
	museData = preprocessMuseEEG(Trial,EEG,sampling_rate,epochWindow,rootDir,name);

    %% Clean merged physio 
    delete(fullDbFilePath);
end

function cameraTimestamps = processCameraTimestamps(camera_timestamps_path,startDate)
    % Read the camera timestamps and return a datetime object with a day
    % specified by startDate
    % 
    % Parameters:
    % camera_timestamps_path - [String] Path to the mat file with the camera timestamps
    % startDate - [datetime] Contains the day that will be copied over to
    %                       all the timestamps read in the file.
    % 
    % Returns:
    % cameraTimestamps - [datetime] Camera timestamps as datetime object.

    % The camera timestamps only have time of the day, so the startDate is
    % used that comes from the schedule file
    cameraTimestamps = load(camera_timestamps_path);
    cameraTimestamps = datetime(cellstr(cameraTimestamps.ts),'InputFormat','HH:mm:ss:SSS');
    % cameraTimestamps = dateshift(startDate, 'start', 'day') + timeofday(cameraTimestamps);]
    cameraTimestamps = seconds(cameraTimestamps-cameraTimestamps(1)); % Using relative time 

end

function data = readScheduleFile(name,rootDir)
    % Read the schedule file and return the data as a table
    % 
    % Parameters:
    % name - [Char] The participant's Id
    % rootDir - [String] Directory where the Data_Raw subdirectory is located
    % 
    % Parameters:
    % data - [Table] Table containing the schedule file feedback events

    filename = dir(strcat(fullfile(rootDir,'mat_files_csv_and_MUSE_db',name),'/*schedule.db'));

    
    if length(filename) > 1
        error(sprintf('multiple schedule files found for subject',name,'%s'));
    end
    
    db = sqlite(strcat(filename(1).folder,'/',filename(1).name));
    data = fetch(db, 'SELECT feedback_time, stim_time, choice_time, feedback, block FROM trials WHERE choice_time IS NOT NULL AND stim1>=0 AND stim2>=-1000 ORDER BY choice_time ASC');
    db.close;

    % For compatibility 
    if strcmp(class(data),'cell')
        data = cell2table(data,"VariableNames",["feedback_time" "stim_time" "choice_time" "feedback","block"]);
    end
    
end

function [cleanData, outlierLogic] = removeOutliers(data)
    % Removes outliers of the input data array
    %
    % Parameters:
    % data - [Array] Input data array
    % 
    % Returns:
    % cleanData - [Array] Contains the data without the outliers 
    % outlierLogic - [Array] Contains the locations where the outliers were
    %                   found
    
    % Remove NaN values
    nanIndices = isnan(data);
    dataNoNaN = data(~nanIndices);

    % Calculate the first and third quartiles
    Q1 = quantile(dataNoNaN, 0.25);
    Q3 = quantile(dataNoNaN, 0.75);

    % Compute the interquartile range (IQR)
    IQR = Q3 - Q1;

    % Define the outlier thresholds
    lowerThreshold = Q1 - 1.5 * IQR;
    upperThreshold = Q3 + 1.5 * IQR;

    % Find indices of outliers
    outlierLogic = (data < lowerThreshold) | (data > upperThreshold);

    % Remove outliers from data
    cleanData = data(~outlierLogic);
end

function  runMuseAnalysis(name,rootDir,scenario,epochWindow,TFwindow,itemsToSave,saveEEGDir)
    % Run the muse Pipeline
    % 
    % Parameters:
    % name -        [char] Participant Id
    % rootDir -     [string] Directory where the raw data will be taken from.    TFdata = struct();
    % scenario -    [string] The event type one of {'feedback','choice','stimuli'}
    % epochWindow - [array] Contains the epoching window in seconds e.g. [-1.600, 3]
    % TFwindow    - [array]  Contains the window for the TF analysis in ms e.g. [-1000,2500]
    % itemsToSave - [string array] Contains the strings of the steps to run
    %               and save. Can contain any of: {"EEG","alignment","TF"}    
    % 
    
    % Preprocess, align and segment data
    EEGData.muse = preprocessMuseData(name,epochWindow,rootDir);

    if any(strcmp('EEG', itemsToSave))
        % Define the saving directory
        saveEEGDir = makeEEGSaveDir(name,rootDir);
        
        % Save the EEGdata
        EEGMuse = EEGData.muse;
        saveName = fullfile(saveEEGDir,'museEEG.mat');
        save(saveName,'EEGMuse','-v7.3');
        clear 'EEGMuse'
    end

    if any(strcmp('TF', itemsToSave))
        % TF analysis
        TFdata.muse = prepareTFAnalysis(EEGData.muse, 'muse', scenario,TFwindow);
        museChannelNames = {'left_temp', 'left_front', 'right_front', 'right_temp'}; % Confirmed order on physio files

        % Save the processed data
        saveTFData(TFdata.muse, rootDir, name, 'muse', museChannelNames, scenario, EEGData.muse.sampling_rate)
    end

end

function saveTFData(TFdata, rootDir, participantId, eegSource, channelNames, scenario, samplingRate)
    % Saves the processed TF data in a directory dependent of the source.
    %
    % Parameters:
    % TFdata - [4D array] The data to be saved.
    % participantId - [string] The id of the participant being processed
    % eegSource - [string] The device source of the data {muse,biosemi}
    % channelNames - [array] The name of the channels 
    % scenario - [string] The epoch being processed {feedback, choice, stim}
    % sampligRate - [scalar] The sampling rate of the signal
   

    % Prepare additional information about the data
    freqs = TFdata.freqs;
    times = TFdata.times;
    scenarioTime = TFdata.scenarioTime;
    nbChannels = size(TFdata.data,4);
    
    for channel=1:nbChannels
        
        % Extract the channel data
        data = TFdata.data(:,:,:,channel);
        channelName = channelNames{channel};

        % Define the directory path
        filePath = fullfile(rootDir, 'Data_Processed_TF_Analysis', participantId, eegSource);
        
        % Check if the directory exists, if not, create it
        if ~exist(filePath, 'dir')
            mkdir(filePath);
        end
        
        % Define the file name and create full file path
        fileName = [participantId '_' scenario '_TF_' channelName '.mat'];  % Ensure the file has a .mat extension
        fullFilePath = fullfile(filePath, fileName);
        
        % Save the data
        save(fullFilePath, 'data', 'samplingRate', 'scenarioTime', 'freqs', 'times', '-v7.3');
    end
end

function newValidIndices = updateValidIndices(validIndices, omittedTrials)
    % Updates the validIndices array by setting to zero
    % the elements corresponding to omittedTrials after accounting for
    % the zeros (invalid indices) that were removed before processing.
    %
    % Parameters:
    %   validIndices - [array] Original array of 0's and 1's 
    %   omittedTrials - [array] Indices of trials omitted by the function after zeros were removed
    %
    % Returns:
    %   newValidIndices - [array] Updated array of valid indices 

    % Find the indices of valid (non-zero) entries
    indicesOfOnes = find(validIndices == 1);
    
    % Map omittedTrials back to indices in the original validIndices array
    indicesToSetZero = indicesOfOnes(omittedTrials);
    
    % Create a copy of validIndices to update
    newValidIndices = validIndices;
    
    % Set the corresponding indices to zero
    newValidIndices(indicesToSetZero) = 0;
end
