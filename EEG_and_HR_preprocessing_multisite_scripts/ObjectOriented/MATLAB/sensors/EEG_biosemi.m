classdef EEG_biosemi < EEGSensor
    properties (Constant)
        dataType = "EEG_biosemi"
        fs = 2048
        preprocessingSamplingRate = 512 %Hz
        firFilterPassLowFreq = 0.1 %Hz
        firFilterPassHighFreq = 50 %Hz
        
        eventMarker = {'1','0'} % {'30'} Feedback onset
        epochWindow = [-0.5,2]%[-1.6,3];
        icaRejectionThreshold = 0.9;
        TFwindow = [-1500,1500]; %ms

        EEGchannelCapType = 'standard-10-5-cap385.elp'
        biosemiChannels = { ...
                'Fp1','AF7','AF3','F1','F3','F5','F7','FT7','FC5','FC3','FC1', ...
                'C1','C3','C5','T7','TP7','CP5','CP3','CP1','P1','P3','P5','P7', ...
                'P9','PO7','PO3','O1','Iz','Oz','POz','Pz','CPz','Fpz','Fp2','AF8', ...
                'AF4','AFz','Fz','F2','F4','F6','F8','FT8','FC6','FC4','FC2', ...
                'FCz','Cz','C2','C4','C6','T8','TP8','CP6','CP4','CP2','P2', ...
                'P4','P6','P8','P10','PO8','PO4','O2','VEO+','VEO-','HEOL','HEOR','M1','M2'};    
        
    end

    properties
        alignedEvents
        isPreprocessedCorrectly = false

    end

    properties (Access=private)
        mainDataDir % Parent dir of where the data is 
        databasePath % where the eeg file is
        fileName
        icaIsPreprocessed = false
        isEpoched = false
        ICAFileName
        ICAFilePath
        intermediateICADirPath
        isTrimmed= false
        intermediateICADirName
        processedEEGDirName
        processedTFDirName

        rawDataExtension
        timestampAligner
    end
    
    methods (Access=public)
        function obj = EEG_biosemi(databasePath)

            obj.databasePath = databasePath;
            [obj.mainDataDir,obj.fileName,obj.rawDataExtension] = fileparts(obj.databasePath);
            [~,obj.participantId,~] = fileparts(obj.mainDataDir);
            obj.readData();
        end
        
        function preprocessData(obj)
            %% Stage 1
            obj.EEGLabObject = pop_editset(obj.EEGLabObject, 'setname',obj.participantId);
            % obj.reReferenceToMastoids();
            obj.reReferenceToCAR();
            obj.filter();
            obj.addChannelLocations();
            obj.EEGLabObject.event = obj.alignedEvents;
            
            % obj.runICA();

            %% Stage 3
            obj.resample();
            obj.epoch();
            
            % obj.runArtifactRejection();

            %% Verification
            obj.verifyPreprocessing();
        end
        
        function filterEOGChannels(obj)
            obj.EEGLabObject  = pop_basicfilter( obj.EEGLabObject, [33 34 35 36] , 'Cutoff',  30, 'Design', 'butter', 'Filter', 'lowpass', 'Order',  2 ); % filter EOG channels  
            obj.EEGLabObject = eeg_checkset( obj.EEGLabObject );
        end
        
        function readData(obj,forceReprocessing)
            if nargin<2; forceReprocessing = false; end
            obj.checkICAavailability(forceReprocessing);
            
            if ~obj.icaIsPreprocessed
                obj.importRawData();
            else
                obj.importICA();
            end
        end

        function trim(obj)
            if obj.isTrimmed
                return 
            end

            % TF needs to run with a larger window than -1.5 to 1.5 and
            % here it is trimmed back 

            samplingRate = obj.EEGLabObject.srate;
            epochStart = double(obj.TFwindow(1))/1000; % seconds
            epochEnd = double(obj.TFwindow(2))/1000; % seconds
            
            % Convert times to points
            pointStart = (epochStart - obj.EEGLabObject.xmin) * samplingRate + 1;
            pointEnd = (epochEnd - obj.EEGLabObject.xmin) * samplingRate;
            
            % Select new time range
            obj.EEGLabObject = pop_select(obj.EEGLabObject, 'point', [pointStart pointEnd]);
            
            % Check the dataset and update history
            obj.EEGLabObject = eeg_checkset(obj.EEGLabObject);
            obj.isTrimmed = true;
        end

        function align(obj,opts)
            arguments
                opts.mainDataDir = ""          % Path to data
                opts.sessionNb = 0             % Current Biosemi session [1,4]
                opts.schedule = []             % Timings of schedule file
            end

            obj.timestampAligner = TimestampAligner(id              = obj.participantId, ...
                                                mainDataDir         = obj.pathToData,...
                                                sessionNb           = opts.sessionNb, ...
                                                schedule            = opts.schedule, ...
                                                biosemi = obj.EEGLabObject);
            obj.timestampAligner.align();
            obj.alignedEvents = obj.timestampAligner.EEGEvents;
        end
    end
    
    methods (Access= private)
        function verifyPreprocessing(obj)
            if all([obj.isEpoched])
                obj.isPreprocessedCorrectly=true;
            end
        end

        function addChannelLocations(obj)
            % Adds channel locations 
            obj.EEGLabObject = pop_chanedit(obj.EEGLabObject, 'lookup',EEG_biosemi.EEGchannelCapType);
            obj.EEGLabObject = eeg_checkset(obj.EEGLabObject);
            
            % Record processing done
            obj.EEGLabObject.EVENTLIST.INFO.README='';
            obj.EEGLabObject.EVENTLIST.INFO.OriginalChannels=obj.EEGLabObject.chanlocs;
            obj.EEGLabObject.EVENTLIST.INFO.ChannelsRemoved= {'N/A'} ;
        end
        
        function applyChannelOperation(obj,formulas)
            % Apply channel operation in EEGLab 
            obj.EEGLabObject = pop_eegchanoperator(obj.EEGLabObject, formulas, 'Saveas','off');
            obj.EEGLabObject = eeg_checkset(obj.EEGLabObject); % Double-check for consistency
            
        end
        
        function checkICAavailability(obj,forceReprocessing)
            obj.buildICAPath();  
            obj.icaIsPreprocessed = exist(obj.ICAFilePath, 'file') == 2;
            if forceReprocessing
                obj.icaIsPreprocessed = false;
            end
        end

        function buildICAPath(obj)
            %   if                  "parentDir\obj.mainDataDir\{id}.bdf"
            %   then
            %   intermediateDir = 'parentDir\obj.intermediateICApath\{id}.set'
            
            parentDir = fileparts(obj.mainDataDir);
            obj.intermediateICADirPath = fullfile(parentDir,obj.intermediateICADirName);
            obj.ICAFileName = obj.participantId + ".set";
            obj.ICAFilePath = fullfile(obj.intermediateICADirPath, obj.ICAFileName);
        end
        
        function epoch(obj)

            %Hold or create information about original channel locations and interpolated channels (gets wiped by ERPLAB) 
            if isfield(obj.EEGLabObject,'EVENTLIST')
                INFOHOLD=obj.EEGLabObject.EVENTLIST.INFO ;
            else
                
                %creates EVENTLIST with no channels removed log. 
                obj.EEGLabObject.EVENTLIST.INFO.README='';
                obj.EEGLabObject.EVENTLIST.INFO.OriginalChannels=obj.EEGLabObject.chanlocs;
                obj.EEGLabObject.EVENTLIST.INFO.ChannelsRemoved= {'N/A'} ;  
                INFOHOLD=obj.EEGLabObject.EVENTLIST.INFO ;
            end

            obj.EEGLabObject = pop_creabasiceventlist( obj.EEGLabObject ,'AlphanumericCleaning', 'on', 'BoundaryNumeric', { -99 }, 'BoundaryString', { 'boundary' }, 'Warning', 'off' );
            
            obj.EEGLabObject.EVENTLIST.INFO=INFOHOLD ;% Re-add EEG.EVENTLIST.INFO (is wiped by ERPLAB)

            % Epoching 
            [obj.EEGLabObject,acceptedEventIndices]  = pop_epoch( obj.EEGLabObject, obj.eventMarker, obj.epochWindow );
            
            if isempty(obj.EEGLabObject.event)
                disp("No events found for this block");
                return;
            end
            allTypesAsStrings = cellfun(@num2str, {obj.EEGLabObject.event.type}, 'UniformOutput', false);
            nbOriginalEvents = sum( ismember(allTypesAsStrings, obj.eventMarker) );

            obj.EEGLabObject.etc.omittedTrials = setdiff((1:nbOriginalEvents),acceptedEventIndices); % Collect which trials were ignored during epoching
            obj.EEGLabObject.etc.trialLabels =[obj.EEGLabObject.event.trial]';
            obj.isEpoched=true;
        end

        function fillTFtimeWindow(obj)
            % This will be passed to the TF to trim it after analysis is
            % complete

            timeWindow = obj.EEGLabObject.times;
        
            % Time window manipulation
            obj.EEGLabObject.etc.TF.scenarioIdx = find(timeWindow == 0);
            start = find(timeWindow <= obj.TFwindow(1), 1, 'last');
            stop = find(timeWindow >= obj.TFwindow(2), 1, 'first');
            
            obj.EEGLabObject.etc.TF.eventMarker = obj.eventMarker;
            obj.EEGLabObject.etc.TF.times = timeWindow(start:stop);
            obj.EEGLabObject.etc.TF.startIdx = start;
            obj.EEGLabObject.etc.TF.stopIdx = stop;

        end

        function filter(obj)
            % obj.EEGLabObject = pop_eegfiltnew(obj.EEGLabObject, obj.firFilterPassLowFreq, obj.firFilterPassHighFreq);
            obj.EEGLabObject = pop_basicfilter( obj.EEGLabObject,  1:70 , 'Boundary', 'boundary', 'Cutoff', [obj.firFilterPassLowFreq, obj.firFilterPassHighFreq], 'Design', 'butter', 'Filter', 'bandpass', 'Order',  2 );
        end

        function getRejectedICA(obj)        
            obj.EEGLabObject = iclabel(obj.EEGLabObject);
            ica_classifications = obj.EEGLabObject.etc.ic_classification.ICLabel.classifications;
            ica_classes  = obj.EEGLabObject.etc.ic_classification.ICLabel.classes;
            classToRemove = 'Eye';
            classInd = strcmpi(classToRemove,ica_classes);

            obj.EEGLabObject.EVENTLIST.INFO.ICAremoved=find(ica_classifications(:,classInd)>obj.icaRejectionThreshold);

        end

        function importRawData(obj)
        
            if ~exist(obj.databasePath,'file')
                fprintf("Can't read the data because it doesn't exist in %s.\n",obj.databasePath);
                return
            end

            currentDir = pwd; 
            cd(obj.mainDataDir);
            % fullFileNameChar = convertStringsToChars(string(obj.participantId)+string(obj.rawDataExtension));
            fullFileNameChar = convertStringsToChars(string(obj.fileName)+string(obj.rawDataExtension));
            obj.EEGLabObject = pop_biosig(fullFileNameChar);
            cd(currentDir); 
            
            obj.EEGLabObject.subject = obj.participantId;
            obj.EEGLabObject = eeg_checkset( obj.EEGLabObject ); % General check of imported data (already runs inside pop_biosig) 
            
        end

        function labelChannels(obj)
            formulas = cell(1, numel(obj.biosemiChannels) + 2);
    
            % Label each channel according to the list
            for i = 1:numel(obj.biosemiChannels)
                formulas{i} = sprintf('nch%d = ch%d Label %s', i, i, obj.biosemiChannels{i});
            end
    
            % Add extra channels for VEOG and HEOG
            formulas{numel(obj.biosemiChannels)+1} = 'nch71 = ch65 - ch66 Label biVEOG';
            formulas{numel(obj.biosemiChannels)+2} = 'nch72 = ch67 - ch68 Label biHEOG';
            obj.applyChannelOperation(formulas);

        end

        function importICA(obj)
            currentDir = pwd;  % Save current directory
            cd(obj.intermediateICADirPath);
            obj.EEGLabObject = pop_loadset( 'filename', char(obj.ICAFileName));
            cd(currentDir); % Return to the directory
        end

        function reReferenceToFpz(obj)
            formulas = cell(1, numel(obj.biosemiChannels));
    
            for i = 1:numel(obj.biosemiChannels)
                currentLabel = obj.biosemiChannels{i};

                % Skip re-referencing for special channels
                if ismember(currentLabel, {'VEO+','VEO-','HEOL','HEOR','M1','M2'})
                    % Just relabel these channels without any referencing
                    formulas{i} = sprintf('nch%d = ch%d Label %s', i, i, currentLabel);
                    continue;
                end

                % Reference to Fpz (ch33)
                formulas{i} = sprintf('nch%d = ch%d - ch33 Label %s', ...
                                         i, i, currentLabel);  
            end
            
            obj.applyChannelOperation(formulas);
        end

        function reReferenceToIpsilateral(obj)

            [leftChLabels, rightChLabels, midlineChLabels] = EEGSensor.categorizeLabels({obj.biosemiChannels{1:64}});
            formulas = cell(1, numel(obj.biosemiChannels));
    
            for i = 1:numel(obj.biosemiChannels)
                currentLabel = obj.biosemiChannels{i};

                % Skip re-referencing for special channels
                if ismember(currentLabel, {'VEO+','VEO-','HEOL','HEOR','M1','M2'})
                    % Just relabel these channels without any referencing
                    formulas{i} = sprintf('nch%d = ch%d Label %s', i, i, currentLabel);
                    continue;
                end

                % Reference to M1 if left, M2 if right, (M1+M2)/2 if midline
                if ismember(currentLabel, leftChLabels)
                    % Left channels -> reference to M1 (ch69)
                    formulas{i} = sprintf('nch%d = ch%d - ch69 Label %s', ...
                                     i, i, currentLabel);
                elseif ismember(currentLabel, rightChLabels)
                    % Right channels -> reference to M2 (ch70)
                    formulas{i} = sprintf('nch%d = ch%d - ch70 Label %s', ...
                                     i, i, currentLabel);
                elseif ismember(currentLabel, midlineChLabels)
                    % Midline channels -> reference to average(M1, M2)
                    formulas{i} = sprintf('nch%d = ch%d - ((ch69 + ch70)/2) Label %s', ...
                                     i, i, currentLabel);
                else
                    % If a label is not found in the sets 
                    error('Unknown location of label');
                end
            end

            obj.applyChannelOperation(formulas);

        end
        
        function reReferenceToCAR(obj)

            numberChannels = numel(obj.biosemiChannels);
            formulas = cell(1, numberChannels);
        
            % Labels to exclude from the average (non-EEG / references)
            excludeLabels = {'VEO+','VEO-','HEOL','HEOR','M1','M2','CMS','DRL'};
        
            labels = obj.biosemiChannels(:);
            isExcluded = ismember(labels, excludeLabels);
        
        
            % Indices to include in the CAR pool
            includeIdx = find(~isExcluded);
            if numel(includeIdx) < 2
                error('Not enough EEG channels to compute CAR (need >= 2).');
            end
        
            % (ch1 + ch2 + ... + chN) / N
            avgExprAll = sprintf('( %s ) / %d', ...
                strjoin(arrayfun(@(k) sprintf('ch%d', k), includeIdx, 'UniformOutput', false), ' + '), ...
                numel(includeIdx));
        
            for i = 1:numberChannels
                currentLabel = labels{i};
        
                if isExcluded(i)
                    % Pass-through unchanged for excluded channels
                    formulas{i} = sprintf('nch%d = ch%d Label %s', i, i, currentLabel);
                    continue;
                end
        
                % Standard CAR: subtract the same global average for everyone
                avgExpr = avgExprAll;
                
        
                formulas{i} = sprintf('nch%d = ch%d - %s Label %s', i, i, avgExpr, currentLabel);
            end
        
            obj.applyChannelOperation(formulas);
        end

        function reReferenceToMastoids(obj)
            formulas = cell(1, numel(obj.biosemiChannels));
    
            for i = 1:numel(obj.biosemiChannels)
                currentLabel = obj.biosemiChannels{i};

                % Skip re-referencing for special channels
                if ismember(currentLabel, {'VEO+','VEO-','HEOL','HEOR','M1','M2'})
                    % Just relabel these channels without any referencing
                    formulas{i} = sprintf('nch%d = ch%d Label %s', i, i, currentLabel);
                    continue;
                end

                 % Reference to the average of M1 (ch69) and M2 (ch70)
                formulas{i} = sprintf('nch%d = ch%d - ((ch69 + ch70)/2) Label %s', ...
                                         i, i, currentLabel);
            end

            obj.applyChannelOperation(formulas);
        end
        
        function resample(obj)
           
            if obj.EEGLabObject.srate ~= obj.preprocessingSamplingRate % makes sure sample rate is 512hz.
                obj.EEGLabObject = pop_resample( obj.EEGLabObject, obj.preprocessingSamplingRate);
                obj.EEGLabObject = eeg_checkset( obj.EEGLabObject);
            end

            %Re-add removed channels if necessary
            if length({obj.EEGLabObject.EVENTLIST.INFO.OriginalChannels.labels})~=length({obj.EEGLabObject.chanlocs.labels})
                obj.EEGLabObject = pop_interp(obj.EEGLabObject, obj.EEGLabObject.EVENTLIST.INFO.OriginalChannels,'spherical'); %default spherical interpolation is used.
            end  
            
        end

        function runArtifactRejection(obj)
            % If epoching failed then skip this preprocessing step
            if ~obj.isEpoched;return; end

            obj.EEGLabObject = pop_chanedit(obj.EEGLabObject, 'lookup',EEG_biosemi.EEGchannelCapType);
            obj.EEGLabObject = eeg_checkset(obj.EEGLabObject);
            
            ARTCHAN = obj.EEGLabObject.reject.rejmanualE; % Copies artifact detected trials (these will be reapplied below at averaging)
            ARTTRIALS = obj.EEGLabObject.reject.rejmanual;
            
            % Automatically select and reject ica components 
            obj.getRejectedICA();
            componentsToRemove = obj.EEGLabObject.EVENTLIST.INFO.ICAremoved;
            obj.EEGLabObject = pop_subcomp( obj.EEGLabObject, componentsToRemove);
            
            % Insert artifact detected trials back into dataset and review artifact detection rates and trials.
            % Note on rejection rates: If rejection rate seems excessive (> 30%) review selected trials. Detection
            % can be influenced by noise. Selected trials can removed by left click.
            
            obj.EEGLabObject = eeg_checkset( obj.EEGLabObject );
            obj.EEGLabObject.reject.rejmanualE = ARTCHAN;% adds artifact detected trials/channels into ICA corrected dataset
            obj.EEGLabObject.reject.rejmanual = ARTTRIALS;
        
            % Runs final pass artifact detection to find large amplitude shifts
            % (i.e. movement artifacts)
            % The script looks through only scalp channels, excluding EOG and
            % reference electrodes.
            
            obj.EEGLabObject  = pop_artmwppth( obj.EEGLabObject , 'Channel', 1:64, 'Flag', [ 1 8], 'Review', 'off', 'Threshold',  200, 'Windowsize',  200, 'Windowstep',  100 );
            obj.EEGLabObject = eeg_checkset( obj.EEGLabObject );
            
            artifact_proportion = getardetection(obj.EEGLabObject);
            fprintf('%s: Percentage of rejected trials was %1.2f\n', obj.EEGLabObject.subject, artifact_proportion);
            
            % Syncs any changes to detected trials between EEGLAB and ERPLAB
            obj.EEGLabObject = pop_syncroartifacts(obj.EEGLabObject,'Direction','eeglab2erplab'); % 
                        
            obj.EEGLabObject.etc.eyeIcaThreshold = obj.icaRejectionThreshold;

        
        end
        
        function runICA(obj)
            if isempty(obj.EEGLabObject.icaweights)
                obj.EEGLabObject = pop_runica(obj.EEGLabObject,  'icatype', 'runica', 'dataset',1, 'options',{ 'extended',1});
                obj.EEGLabObject = eeg_checkset(obj.EEGLabObject );
            end
        end

        function saveICA(obj)
            if ~exist(obj.intermediateICADirPath,'dir')
                mkdir(obj.intermediateICADirPath);
            end
        
            % save(intermediatePathDir,'EEG','-v7.3');
            obj.EEGLabObject.filepath = obj.intermediateICADirPath;
            currentDir = pwd;
            cd(obj.intermediateICADirPath);
            obj.EEGLabObject = pop_saveset( obj.EEGLabObject, 'filename', char(obj.ICAFileName));
            cd(currentDir); % Return to the directory
        end

    end

end