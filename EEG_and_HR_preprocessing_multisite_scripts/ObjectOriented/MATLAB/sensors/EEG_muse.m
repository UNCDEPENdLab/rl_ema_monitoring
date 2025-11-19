classdef EEG_muse < EEGSensor
    properties (Constant)
        dataType = "EEG_muse"
        fs=256.03
        museChannels = {'left_temp','left_front','right_front','right_temp'};
        restingEpoch = [0,10] %seconds
    end

    properties (Access=private)
        databasePath
        isPreprocessed
        asEEGLabObject
        isDatabaseFile
    end

    properties
        optimizedAlignment = false
    end
    
    methods
        function obj = EEG_muse(databasePath)
            % To know if receiving a database, a preprocessed file or a
            % directory
            obj.checkTypeOfData(databasePath);
        end
        
        function epochToRestingEpochs(obj,restingStateTimes,channelsToRemove)
            % Splits the resting state into epochs given in obj.restingEpoch
            obj.dropChannels(channelsToRemove);

            obj.eventName = 'RestingEpochs';
            newEvent = obj.generateEventsBetweenStartEndTimes(obj.restingEpoch,restingStateTimes);
            obj.updateEvents(newEvent);
            obj.epochToEvent(obj.restingEpoch);
        end
        
        function epochToRestingState(obj,eventBlockTable,channelsToRemove)
            obj.dropChannels(channelsToRemove);

            obj.eventName = 'RestingState';
            newEvent = obj.generateEvents(eventBlockTable);
            
            obj.updateEvents(newEvent);
            vals = num2cell(seconds(eventBlockTable.preWindow));  
            [obj.EEGLabObject.event.preWindowSec] = vals{:};
            maxPreWindow = max([obj.EEGLabObject.event.preWindowSec]);

            obj.EEGLabObject.urevent = obj.EEGLabObject.event;
            obj.epochToEvent([-maxPreWindow,0]);
            obj.EEGLabObject.event = obj.EEGLabObject.urevent; % To later extract session and trial labels

        end

        function epochToITI(obj,eventName,eventBlockTable,channelsToRemove)
            obj.dropChannels(channelsToRemove);

            obj.eventName = eventName;
            newEvent = obj.generateEvents(eventBlockTable);
            obj.updateEvents(newEvent);

            % Epoching to the end of ITI=time 0 and using the maximum
            % preWindow (start of the trial's ITI) to trim the data after
            % epoching
            vals = num2cell(seconds(eventBlockTable.preWindow));  
            [obj.EEGLabObject.event.preWindowSec] = vals{:};
            maxPreWindow = max([obj.EEGLabObject.event.preWindowSec]);
            obj.EEGLabObject.urevent = obj.EEGLabObject.event;

            obj.epochToEvent([-maxPreWindow,0]); 
            obj.fillShortPrewindowWithNaN(); % Fill with NaN where outside the ITI window

        end
           
        function epochToTable(obj,eventTable,eventName,windowToEpoch,windowToKeep)
            if nargin<5 
                windowToKeep = []; % or windowToEpoch
            end

            obj.eventName = eventName;
            newEvent = obj.generateEvents(eventTable);
            obj.updateEvents(newEvent);

            obj.windowToKeep = windowToKeep;
            
            % Add padding of one sample windowToEpoch in seconds
            windowToEpoch(1)=windowToEpoch(1)-1/obj.fs;
            windowToEpoch(2)=windowToEpoch(2)+2/obj.fs;
            
            obj.epochToEvent(windowToEpoch);
        end

        function preprocessData(obj,verbose)
            if nargin<2; verbose = false; end
            if obj.isPreprocessed
                obj.readPreprocessedFile(verbose);
            else

                disp("Reading EEG from temporal database...");
                % Move the temporal database and rename it to
                % allow readEEG to read it
                [temporalDirForMergedDatabase,tempId] = fileparts(obj.databasePath);
                dirForReadEEG = fullfile(temporalDirForMergedDatabase, 'Data_Processed', "subject_"+tempId);
                fullDbFilePath = fullfile(dirForReadEEG,  tempId+ "_merged_physio.db");
                
                % Check if the directory exists, and create it if it does not
                if ~exist(dirForReadEEG, 'dir')
                    mkdir(dirForReadEEG);
                end

                movefile(obj.databasePath,fullDbFilePath);
                [EEG] = obj.readEEG(temporalDirForMergedDatabase,tempId);
                % Dropping the data marked as removed
                % EEG.data(EEG.remove~=0) = NaN;

                EEG.times = Utils.convertPhoneTimestampsToDatetime(EEG.times);
                delete(fullDbFilePath);
                delete(dirForReadEEG);
                obj.convertToEEGLabStruct(EEG);
            end
        end
        
        function filter(obj)
            % obj.EEGLabObject = pop_eegfiltnew(obj.EEGLabObject, obj.firFilterPassLowFreq, obj.firFilterPassHighFreq);
            obj.EEGLabObject  = pop_basicfilter( obj.EEGLabObject,  1:numel(obj.EEGLabObject.chanlocs) , 'Boundary', 'boundary','Cutoff', [ 0.1 50], 'Design', 'butter', 'Filter', 'bandpass', 'Order',  2, 'RemoveDC', 'on');
            obj.EEGLabObject = eeg_checkset( obj.EEGLabObject );

        end
        
        function setParticipantId(obj,participantId)
            if ~isempty(char(obj.participantId))
                fprintf("Changing the participantId from %s to %s \n",obj.participantId,participantId);
            end
            
            obj.participantId = participantId;
            obj.EEGLabObject.setname = char(obj.participantId);
            obj.EEGLabObject.subject = obj.participantId;
        end

    end
    
    methods (Access=private)
        
        function EEG = readEEG(obj,databaseDir,tempId)
            % Copied from original function with same name
            filename = dir(strcat(fullfile(databaseDir,'Data_Processed',['subject_' tempId]),'/*physio.db'));
            db = sqlite(strcat(filename(1).folder,'/',filename(1).name));
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
                EEG.data(:,1:4) = str2double(DATA{:,2:5});  

                % Process columns 6 to 9 for EEG.isgood
                EEG.isgood(:,1:4) = str2double(DATA{:,6:9});  

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
            EEG.isgood = logical(EEG.isgood==1 | EEG.isgood==2);
    
            %% infer real times of samples
            times = double(EEG.times);

            % first pass with pre-specified sampling rate
            real_recording_time = Utilities.createRealTime(times, obj.fs);
            [corrected_times, break_indices] = Utilities.correctRealTime(real_recording_time, times, obj.fs);
            EEG.times = corrected_times;

            EEG.sampling_rate = obj.fs;
            %% remove zero values, those that are more than 10 SDs from median, and those that are within 5% of the limits of the sensor's range (0 to 1650)
            EEG.remove = zeros(size(EEG.data));
            for channel = 1:4
                indGood = EEG.isgood(:,channel);
                mn = nanmedian(EEG.data(indGood,channel)); %#ok<NANMEDIAN>
                sd = nanstd(EEG.data(indGood,channel)); %#ok<NANSTD>
                %nogood = EEG.data(:,channel) > mn + 10*sd | EEG.data(:,channel) < mn - 10*sd | EEG.data(:,channel)<=0 |  EEG.data(:,channel)<1650/20  |  EEG.data(:,channel)>19*1650/20;
                nogood_10sd = EEG.data(:,channel)>mn+10*sd | EEG.data(:,channel) < mn-10*sd;
                nogood_neg = EEG.data(:,channel)<=0;
                nogood_range = EEG.data(:,channel)<1650/20 | EEG.data(:,channel)>19*1650/20;
                EEG.remove(nogood_10sd,channel) = 1;
                EEG.remove(nogood_neg,channel) = 2;
                EEG.remove(nogood_range,channel) = 3;
                EEG.remove(~EEG.isgood(:,channel),channel) = 4; % 2022-10-20 AndyP
            end
        end

        function readPreprocessedFile(obj,verbose)
            % filePath = fullfile(obj.databasePath,sprintf("%s_EEG.mat",obj.participantId));
            % obj.rawData = load(filePath);
            % Assuming we are given the participant's path 
            [parentDir,~,~] = fileparts(obj.databasePath);
            [parentDir,~,~] = fileparts(parentDir);
            [EEG, ~] = readEEG(parentDir,char(obj.participantId),'Pitt',0);
            EEG.times = Utils.convertPhoneTimestampsToDatetime(EEG.times);
            obj.convertToEEGLabStruct(EEG);
        end

        function fillShortPrewindowWithNaN(obj)
            % Fill pre-epoch data with NaNs if preWindow was shorter. Assumes the
            % original window times exist in EEG.urevent and that the data is
            % epoched (EEG.data is [chan x time x trials]
 
            preSamples = size(obj.EEGLabObject.data, 2);  % number of timepoints per epoch
        
            for t = 1:obj.EEGLabObject.trials
                if ~isfield(obj.EEGLabObject.urevent(t), 'preWindowSec')
                    warning('urevent(%d) missing preWindowSec. Skipping.', t);
                    continue;
                end
        
                thisPreWindow = obj.EEGLabObject.urevent(t).preWindowSec;
        
                % Only process if it's shorter than the full preWindow
                if thisPreWindow < seconds(ScheduleDatabase.ITIupperLimit)
                    validSamples = round(thisPreWindow * obj.fs);
                    nToFill = preSamples - validSamples;
        
                    if nToFill > 0
                        obj.EEGLabObject.data(:, 1:nToFill, t) = NaN;
                    end
                end
            end
        end

        function checkTypeOfData(obj,databasePath)
            [~,fileName,ext] = fileparts(databasePath);
            obj.participantId = MomentumParticipant.validateParticipantId(fileName);

            if strcmpi(ext, '.db') && isfile(databasePath)
                obj.databasePath = databasePath;
                obj.isPreprocessed = false;
                obj.isDatabaseFile = true;
                return
            end
            obj.isDatabaseFile = false;

            eegFile   = fullfile(databasePath, obj.participantId + "_EEG.mat");
            physioDir = fullfile(databasePath, "physio");
            
            if isfile(eegFile)
                obj.isPreprocessed= true;
            elseif isfolder(physioDir)
                obj.isPreprocessed = false;
            else
                ex = MException('EEGMuse:DataMissing', ...
                                'No physio directory nor EEG.mat found in "%s" .', databasePath);                
                throw(ex);
            end
            obj.databasePath = databasePath;

        end
        
        function dropChannels(obj,channelNames)
            obj.EEGLabObject= pop_select(obj.EEGLabObject, 'nochannel', channelNames);
            obj.EEGLabObject= eeg_checkset(obj.EEGLabObject);
        end
        
        function newEvent = generateEvents(obj,eventTimes)

            % Validate inputs
            if ~(ismember(obj.eventName, eventTimes.Properties.VariableNames) && ismember("block", eventTimes.Properties.VariableNames))
                error("Block or eventName not found in eventTimes");
            end

            % Removing events before the first timestamp or after the last
            eventsToKeep= ~(eventTimes.(obj.eventName)<obj.EEGLabObject.etc.originalDateTimes(1) | ...
                            eventTimes.(obj.eventName)>obj.EEGLabObject.etc.originalDateTimes(end));
            eventTimes = eventTimes(eventsToKeep,:);

            % Find closest match 
            latencies = knnsearch(datenum(obj.EEGLabObject.etc.originalDateTimes), ...
                                  datenum(eventTimes.(obj.eventName)));
            

            nEpochs = numel(latencies); 
            
            obj.EEGLabObject.etc.blockLabels = eventTimes.block;
            obj.EEGLabObject.etc.trialLabels = eventTimes.trial;
            obj.EEGLabObject.etc.outcomeLabels = eventTimes.outcome;

            newEvent = struct('type',    repmat({obj.eventName},1,nEpochs)', ...
                          'latency', num2cell(latencies), ...
                          'trial',num2cell(eventTimes.trial), ...
                          'block',num2cell(eventTimes.block));
        end

        function [idxStart,idxEnd] = getStartEndLatencies(obj,startEndBlockTimesTable)
            % number of intervals
            nIntervals = height(startEndBlockTimesTable);
        
            % preallocate arrays for the first/last sample in each interval
            idxStart = nan(nIntervals,1);
            idxEnd   = nan(nIntervals,1);
            for ii = 1:nIntervals
                t0 = startEndBlockTimesTable.StartTime(ii);
                t1 = startEndBlockTimesTable.EndTime(ii);
        
                % map to samples (gives NaN if empty)
                f0 = find(obj.EEGLabObject.etc.originalDateTimes >= t0, 1, 'first');
                f1 = find(obj.EEGLabObject.etc.originalDateTimes <= t1, 1, 'last');
                if ~isempty(f0), idxStart(ii) = f0; end
                if ~isempty(f1), idxEnd(ii)   = f1; end
            end

        end

        function newEvent = generateEventsBetweenStartEndTimes(obj,epochLen,startEndBlockTimesTable)
            
            [idxStart,idxEnd] = obj.getStartEndLatencies(startEndBlockTimesTable);
            
            [onsetLatencies,epochLabels] = obj.epochBetweenStartEndLatencies(epochLen, idxStart,idxEnd,startEndBlockTimesTable.block);

            % obj.EEGLabObject.etc.epochLabels = epochLabels;
            nEpochs   = numel(onsetLatencies);

            % Create events
            newEvent = struct('type',    repmat({obj.eventName},1,nEpochs), ...
                          'latency', num2cell(onsetLatencies), ...
                          'block',num2cell(epochLabels));

        end
        
        function [onsetLatencies,epochLabels] = epochBetweenStartEndLatencies(obj,epochLen, idxStart,idxEnd,blockLabels)
            % Datapoints per epoch
            epochLen = epochLen(2)-epochLen(1);
            epochDatapoints = round(epochLen * obj.EEGLabObject.srate);

            % vector‐wise build each block of onsets: colon yields [] when start>end
            onsetLatenciesPerBlock = arrayfun( ...
                              @(s,e) s:epochDatapoints:(e-epochDatapoints+1), ...
                              idxStart, idxEnd, ...
                              'UniformOutput', false );
            
            % flatten into one list of latencies
            onsetLatencies = [onsetLatenciesPerBlock{:}];
            
            epochLabels = EEG_muse.getLatenciesLabelsFromBlocks(onsetLatenciesPerBlock,blockLabels);
            
        end

        function updateEvents(obj,newEvent)
            obj.EEGLabObject.event = [obj.EEGLabObject.event(:); newEvent(:)];
            obj.EEGLabObject = eeg_checkset(obj.EEGLabObject, 'eventconsistency');
        end

        function epochToEvent(obj,windowToEpoch)
            obj.EEGLabObject = pop_epoch( obj.EEGLabObject, {obj.eventName}, windowToEpoch, 'epochinfo','yes' );
            obj.EEGLabObject = eeg_checkset(obj.EEGLabObject);
            % obj.EEGLabObject.data with dimensions [channels x time x epochs]
            
        end

        function convertToEEGLabStruct(obj,EEGin)
            
            % Sanity checks
            if obj.asEEGLabObject; return; end
            if ~isfield(EEGin, 'data') || ~isfield(EEGin, 'times') || ~isfield(EEGin, 'sampling_rate')
                error('Input EEG struct must contain data, times, and sampling_rate.');
            end
            [nSamples, nChannels] = size(EEGin.data);
            
            % Fill out required EEGLAB fields
            obj.EEGLabObject = struct();
            obj.EEGLabObject.data    = EEGin.data';                   % transpose: [channels x timePoints]
            obj.EEGLabObject.nbchan  = nChannels;                     % number of channels
            obj.EEGLabObject.pnts    = nSamples;                      % number of time points
            obj.EEGLabObject.trials  = 1;                             % continuous data has 1 "trial" for now
            obj.EEGLabObject.srate   = EEGin.sampling_rate;           % sampling frequency in Hz
            obj.EEGLabObject.dipfit = [];                             % Necessary for removing channels later
            obj.EEGLabObject.chaninfo = struct('removedchans', []);   % Necessary for removing channels later
            obj.EEGLabObject.epoch = [];
            obj.EEGLabObject.specdata    = [];
            obj.EEGLabObject.icachansind = [];
            obj.EEGLabObject.specicaact =[];
            obj.EEGLabObject.reject = struct();
            % Store the times
            timeVec = EEGin.times;                          % Nx1 datetime
            tStart  = timeVec(1);
            tEnd    = timeVec(end);
            totalDurationSec = seconds(tEnd - tStart);
            
            obj.EEGLabObject.xmin = 0;                                % EEGLAB's "start time" in seconds
            obj.EEGLabObject.xmax = totalDurationSec;                 % EEGLAB's "end time" in seconds
        
            % Store the entire datetime vector
            obj.EEGLabObject.etc.originalDateTimes = timeVec;
            
            % Fill out standard bookkeeping fields for EEGLab
            obj.EEGLabObject.setname   = char(obj.participantId);
            obj.EEGLabObject.filename  = '';
            obj.EEGLabObject.filepath  = '';
            obj.EEGLabObject.subject   = obj.participantId;
            obj.EEGLabObject.group     = '';
            obj.EEGLabObject.condition = '';
            obj.EEGLabObject.block   = [];
            obj.EEGLabObject.history = [];
            obj.EEGLabObject.comments  = 'Events with resting state';
            obj.EEGLabObject.etc.remove = EEGin.remove;
        
            % Add channel labels
            obj.EEGLabObject.chanlocs = struct('labels', cell(1, nChannels));
            for c = 1:nChannels
                obj.EEGLabObject.chanlocs(c).labels = obj.museChannels{c};
            end
        
            % Initialize empty events/urevents
            obj.EEGLabObject.event   = [];
            obj.EEGLabObject.urevent = [];
            
            % Initialize ICA fields
            obj.EEGLabObject.icaweights = [];
            obj.EEGLabObject.icasphere  = [];
            obj.EEGLabObject.icaact     = [];
            obj.EEGLabObject.icawinv    = [];

            obj.asEEGLabObject = true;
        end

    end
    
    methods (Static)
        
        function epochLabels = getLatenciesLabelsFromBlocks(onsetLatenciesPerBlock,blockLabels)
            % Each cell has the same block label 
            onsetLatenciesPerBlock = cellfun(@(x) size(x,2),onsetLatenciesPerBlock,'UniformOutput',false);
            epochLabels = repelem(blockLabels,cell2mat(onsetLatenciesPerBlock));
        end
    end

end