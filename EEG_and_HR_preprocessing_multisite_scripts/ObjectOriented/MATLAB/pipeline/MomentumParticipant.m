classdef MomentumParticipant < handle
    properties (Constant)
        availableEventNames = {'stim_time','choice_time','feedback_time','ITI','RestingState','RestingEpoch'}
        virtualSources = {'Polar_RRIfromECG'}
        % Sibling to the main raw data dir containing the participants data
        generalProcessedSaveDirectoryName = "DataProcessed"
        regularBlockLabels = 0:118 % Used when saving the data binned by block
    end
    
    properties (Access=public)
        id
        sources = struct()
        dataTypes = {}
        schedule =[]
        TFanalyzers = struct()
        
        allBiosemi =[]
    end

    properties (Access = private)
        pathToData
        pathToPhysioDir
        pathToScheduleFile
        parentDirToData
        preprocessedDirs =struct()
        biosemiDirectoryPath = []

        mainProcessedDirectoryPath
        EEGsaveDirectoryPath
        TFsaveDirectoryPath
        TFInducedSaveDirectoryPath
        RRIsaveDirectoryPath
        scheduleSaveDirectoryPath

        % emptyPreprocessedDirs
        acceptedDataTypes = {'Polar_ECG', 'Polar_heartrate','Polar_RRIfromECG', 'Polar_accelerometer', 'EEG_muse','EEG_biosemi'};
        metrics
        epochs
        eventName 
        validation

        EEGSourceToRunTF

    end

    methods
        function obj = MomentumParticipant(opts)
            arguments
                opts.id = ""
                opts.pathToData = ""
                opts.preprocessedDirs   = struct()
                opts.validation = false
            end

            obj.id=opts.id; % Gets overwritten by dir name if validation=false
            obj.pathToData=opts.pathToData;
            obj.preprocessedDirs = opts.preprocessedDirs;
            obj.validation = opts.validation;
            
            obj.generateSavingDirectoryPaths();
            obj.prepareSourceDirectories();
        end
        
        function getTrialsWithSystem(obj)
            obj.getScheduleFile(); 
            obj.schedule.getTrialsWithSystem(obj.id);
            MomentumParticipant.prepareSaveDirectory(obj.scheduleSaveDirectoryPath);
            saveName = fullfile(obj.scheduleSaveDirectoryPath,sprintf('%s_trials.csv',obj.id));
            writetable(obj.schedule.trials,saveName);
        end

        function getRRFromACC(obj)
            if ~isfield(obj.sources,"Polar_accelerometer")
                obj.getData("Polar_accelerometer");
            end
            %% Split non-continuous data
            threshold = 1; %seconds
            blockData = SessionSplitter(obj.sources.Polar_accelerometer.rawData);
            blockData.split(threshold); 

            delete(obj.sources.Polar_accelerometer);
            
        end

        function updatePreprocessedDirs(obj,newPreprocessedDirs)
            obj.preprocessedDirs = Utils.mergeStructs(obj.preprocessedDirs,newPreprocessedDirs);
            % obj.emptyPreprocessedDirs = isempty(fieldnames(obj.preprocessedDirs));
        end
        
        function runTFAnalysis(obj,opts)
            arguments
              obj
              opts.source             = "EEG_muse" %EEG_{muse,biosemi}
              opts.subtractERP        = false % {false,[],"block","block_condition"} % 26.09.25 Subtract session-wise average from every trial data
              opts.getInducedTF       = false % false, "block", "block_condition"
              opts.TFWindow           = [] % The window to keep around the TF, if empty keeps whatever epoch is in the EEG
            end

            obj.validateTFSource(opts.source);
            obj.TFanalyzers.(obj.EEGSourceToRunTF).referenceEEG(opts.subtractERP);
            obj.TFanalyzers.(obj.EEGSourceToRunTF).getTF();
            obj.TFanalyzers.(obj.EEGSourceToRunTF).trimTF(opts.TFWindow);
            obj.TFanalyzers.(obj.EEGSourceToRunTF).getInducedTF(opts.getInducedTF);

        end
        
        function saveRRI(obj, opts)

            arguments
                obj
                opts.source             = "Polar_RRIfromECG" 
                opts.saveMode           = "asParquet" %{"asMat","asCSV","asParquet"} Note: "asCSV" will save it as .csv then further compress it to .csv.gz and delete the .csv
                opts.timeBinningMode    = "byTimepoints" %{"byTimepoints","byTime"}
                opts.blocksPerBin       = 0 % saves the files by binning the block 
            end
            
            saveMode = MomentumParticipant.validateSavingMode(opts.saveMode);
            timeBinningMode = MomentumParticipant.validateBinningMode(opts.timeBinningMode);
            
            % sourceDirName = extractAfter(opts.source, "_");

            saveDir = fullfile(obj.RRIsaveDirectoryPath, obj.eventName,"ByParticipant",obj.id);
            % if obj.validation; saveDir = fullfile(saveDir,sourceDirName);end

            if ~isfolder(saveDir); mkdir(saveDir); end
            
            obj.sources.(opts.source).save( id = obj.id,...
                                              saveDir = saveDir, ...
                                              saveMode = saveMode, ...
                                              timeBinningMode = timeBinningMode, ...
                                              blocksPerBin = opts.blocksPerBin);
        end

        function saveEEG(obj, opts)

            arguments
                obj
                opts.source             = "EEG_muse" % EEG_+{muse,biosemi}
                opts.saveMode           = "asParquet" %{"asMat","asCSV","asParquet"} Note: "asCSV" will save it as .csv then further compress it to .csv.gz and delete the .csv
                opts.timeBinningMode    = "byTimepoints" %{"byTimepoints","byTime"}
                opts.blocksPerBin       = 0 % saves the files by binning the block 
                opts.binByChannel       = false % Splits the files by channel
                opts.tPerBin            = 0 % Time or timepoints per bin when saving. In ms if opts.timeBinningMode== "byTime", else in datapoints
            end
            
            saveMode = MomentumParticipant.validateSavingMode(opts.saveMode);
            timeBinningMode = MomentumParticipant.validateBinningMode(opts.timeBinningMode);
            
            sourceDirName = extractAfter(opts.source, "_");

            saveDir = fullfile(obj.EEGsaveDirectoryPath, obj.eventName,"ByParticipant",obj.id);
            if obj.validation; saveDir = fullfile(saveDir,sourceDirName);end

            if ~isfolder(saveDir); mkdir(saveDir); end
            
            obj.sources.(opts.source).save(saveDir = saveDir, ...
                                              saveMode          = saveMode, ...
                                              timeBinningMode   = timeBinningMode, ...
                                              blocksPerBin      = opts.blocksPerBin,...
                                              binByChannel      = opts.binByChannel,...
                                              tPerBin           = opts.tPerBin);
        end

        function saveTFAnalysis(obj,opts)
            arguments
              obj
              opts.source           = "EEG_muse" % EEG_{muse,biosemi}
              opts.saveMode         = "asParquet" %{"asMat","asCSV","asParquet"}
              opts.timeBinningMode  = "byTimepoints" %{"byTimepoints","byTime"}
              opts.blocksPerBin     = 0 % saves the files by binning the block 
              opts.binByChannel     = false % Splits the TF files by channel
              opts.tPerBin          = 0 % Time or timepoints per bin when saving. In ms if opts.timeBinningMode== "byTime", else in datapoints
            end

            saveMode = MomentumParticipant.validateSavingMode(opts.saveMode);
            timeBinningMode = MomentumParticipant.validateBinningMode(opts.timeBinningMode);
            
            sourceDirName = extractAfter(opts.source, "_");

            saveDir = fullfile(obj.TFsaveDirectoryPath, obj.eventName,"ByParticipant",obj.id);
            if obj.validation; saveDir = fullfile(saveDir,sourceDirName);end

            if ~isfolder(saveDir);mkdir(saveDir);end

            obj.TFanalyzers.(opts.source).saveTF(saveDir            = saveDir, ...
                                                    saveMode        = saveMode, ...
                                                    timeBinningMode = timeBinningMode, ...
                                                    blocksPerBin    = opts.blocksPerBin,...
                                                    binByChannel    = opts.binByChannel,...
                                                    tPerBin         = opts.tPerBin);
            
            if obj.TFanalyzers.(opts.source).hasInducedTF
                saveDir = fullfile(obj.TFInducedSaveDirectoryPath, obj.eventName,"ByParticipant",obj.id);
                if ~isfolder(saveDir);mkdir(saveDir);end

                obj.TFanalyzers.(opts.source).saveInduced(saveDir   = saveDir, ...
                                                    saveMode        = saveMode, ...
                                                    timeBinningMode = timeBinningMode, ...
                                                    blocksPerBin    = opts.blocksPerBin,...
                                                    binByChannel    = opts.binByChannel,...
                                                    tPerBin         = opts.tPerBin);
            end
            
            
        end
        
        function runValidation(obj)
            
            obj.eventName = "feedback_time";
            saveEEG = true; 
            obj.getScheduleFile();
            obj.schedule.getEventTimes(obj.eventName);
            saveMode = "byTimepoints"; % Normally DataWriter set to 0 timepoints to save all together

            %% EEG_muse
            obj.runMuseValidation(saveEEG,saveMode);

            %% EEG_biosemi
            obj.runBiosemiValidation(saveEEG,saveMode);
        end

        function getEEGEpochedEvent(obj,opts)
            arguments
              obj
              opts.eventName                = 'RestingState'% {RestingEpoch,ITI,{feedback,stim,choice}+_time}
              opts.windowToEpoch            = [-1.5,2]      % Necessary for {x}_time events  
              opts.channelsToRemove         = {} 
              opts.padForTF                 = false % If true, computes extra padding, epochs at new padding but saves original window in time domain. If false, it uses same window for TF. 
              opts.verbose                  = true
            end

            obj.validateEventName(opts.eventName);

            obj.getScheduleFile(opts.verbose);
            obj.schedule.getEventTimes(obj.eventName);
            
            if opts.verbose; fprintf("Extracted %s times. \n",obj.eventName); end
            
            if ~isfield(obj.sources,"EEG_muse")
                obj.getData("EEG_muse");
            end
            
            if opts.padForTF 
                windowToKeep = opts.windowToEpoch;
                margin = TimeFrequencyAnalyzer.getMarginFromRunParameters();
                windowToEpoch =  [opts.windowToEpoch(1)-margin, opts.windowToEpoch(2)+margin];
                fprintf("WindowToEpoch: %d \n",windowToEpoch);
            else
                windowToEpoch = opts.windowToEpoch;
                windowToKeep = [];
            end

            switch obj.eventName
                case "RestingEpoch"
                    obj.sources.EEG_muse.epochToRestingEpochs(obj.schedule.restingTimes,opts.channelsToRemove);
                case "RestingState"
                    obj.sources.EEG_muse.epochToRestingState(obj.schedule.restingTimes,opts.channelsToRemove);
                case "ITI"
                    obj.sources.EEG_muse.epochToITI(obj.eventName,obj.schedule.itiTimes,opts.channelsToRemove);
                otherwise
                    % n = height(obj.schedule.trials);    
                    % idx = randperm(n, 300);
                    % obj.schedule.trials = obj.schedule.trials(idx, :);   
                    
                    obj.sources.EEG_muse.epochToTable(obj.schedule.trials,obj.eventName,windowToEpoch,windowToKeep);
                    % nan_mask = isnan(obj.sources.EEG_muse.EEGLabObject.data);                 % chan × time × epoch
                    % epochs_with_nan = squeeze(any(nan_mask,[1 2]));   % 1 × epoch logical
                    % epochsWithNaNs = sum(epochs_with_nan);
                    % fprintf("%i - %.4f%%  of epochs \n",epochsWithNaNs,100*epochsWithNaNs/size(obj.sources.EEG_muse.EEGLabObject.data,3));
            end

        end

        function getRRIEpochedEvent(obj,opts)
            arguments
              obj
              opts.eventName                = 'RestingState'% {RestingEpoch,ITI,{feedback,stim,choice}+_time}
              opts.windowToEpoch            = [-1.5,2]      % Necessary for {x}_time events  
              opts.verbose                  = true
            end

            obj.validateEventName(opts.eventName);

            obj.getScheduleFile(opts.verbose);
            obj.schedule.getEventTimes(obj.eventName);

            if opts.verbose; fprintf("Extracted %s times. \n",obj.eventName); end
            
            if ~isfield(obj.sources,"Polar_RRIfromECG")
                obj.getData("Polar_RRIfromECG");
            end
            
            switch obj.eventName
                % case "ITI"
                    % obj.sources.Polar_heartrate.epochToITI(obj.eventName,obj.schedule.itiTimes,opts.channelsToRemove);
                otherwise
                    obj.sources.Polar_RRIfromECG.epochToTable(obj.schedule.trials,obj.eventName,opts.windowToEpoch);
            end

        end

        function getData(obj, givenDataTypes, verbose)
            if nargin<3; verbose = true; end
        
            givenDataTypes = obj.validateSensorInput(givenDataTypes);
        
            isInPreprocessedDir = ismember(givenDataTypes, fieldnames(obj.preprocessedDirs));
        
            preprocessedList = givenDataTypes(isInPreprocessedDir); 
            rawList = givenDataTypes(~isInPreprocessedDir);     
        
            for preprocessedIdx = 1:numel(preprocessedList)
                sourceName = preprocessedList{preprocessedIdx};

                obj.sources.(sourceName) = feval(sourceName, obj.preprocessedDirs.(sourceName));
                obj.sources.(sourceName).preprocessData();
            end
        
            if ~isempty(rawList)
                obj.readRawData(rawList);
            end
        end
        
        function clearData(obj)
            if isa(obj.sources.EEG_muse,'handle') && isvalid(obj.sources.EEG_muse)
                delete(obj.sources.EEG_muse);
            end
            obj.sources.EEG_muse = [];
    
            if isa(obj.TFanalyzers,'handle') && isvalid(obj.TFanalyzers)
                delete(obj.TFanalyzers);
            end
            obj.TFanalyzers = [];

            if isa(obj.schedule,'handle') && isvalid(obj.schedule)
                delete(obj.schedule);
            end
            obj.schedule = [];

        end
        
        function getScheduleFile(obj,verbose)
            if nargin<2; verbose = false; end
            if isempty(obj.schedule)
                if verbose; disp("Finding participant schedule file"); end
                obj.schedule = ScheduleDatabase(obj.pathToScheduleFile);
            end
        end
    end

    methods (Access=private)
        
        function runBiosemiValidation(obj,saveEEG,saveMode)
            if nargin<2; saveEEG = false; end
            binByChannel  = true; 

            dataSource = "biosemi";
            dataSourceName = "EEG_"+dataSource;
            
            nbSessions = 4;

            for sessionInd = 1:nbSessions 
                obj.runSingleBiosemiSessionAlignment(sessionInd);
            end
            
            [obj.sources.(dataSourceName), ~] = pop_mergeset(obj.allBiosemi, 1:numel(obj.allBiosemi), 0);
            obj.allBiosemi = [];
            obj.sources.(dataSourceName) = EEGSensor(obj.sources.(dataSourceName), ...
                                                obj.id, ...
                                                "feedback_time");
            obj.sources.(dataSourceName).EEGLabObject.etc.trialLabels =[obj.sources.(dataSourceName).EEGLabObject.event.trial]';

            if saveEEG
                obj.saveEEG(source = dataSourceName,...
                        saveMode ="asParquet", ...
                        timeBinningMode =saveMode, ...
                        blocksPerBin=0, ...
                        binByChannel = binByChannel);

            end
            obj.runTFAnalysis(byFrequencyBand=false, ...
                                source=dataSourceName);
            obj.saveTFAnalysis(source=dataSourceName,...
                                saveMode="asParquet", ...
                                blocksPerBin=0, ...
                                binByChannel = binByChannel,...
                                timeBinningMode=saveMode); % By timepoint binning

        end
        
        function runSingleBiosemiSessionAlignment(obj,sessionInd)
            dataSource = "biosemi";
            dataSourceName = "EEG_"+dataSource;

            obj.updatePreprocessedDirs(struct(dataSourceName, ...
                            fullfile(obj.biosemiDirectoryPath,obj.id+"-"+string(sessionInd)+".bdf")));
            obj.getData(dataSourceName);
                
            obj.sources.EEG_biosemi.align();
            obj.sources.EEG_biosemi.preprocessData();

            if obj.sources.EEG_biosemi.isPreprocessedCorrectly
                [obj.allBiosemi, ~, ~] = eeg_store(obj.allBiosemi, ...
                                                    obj.sources.EEG_biosemi.EEGLabObject, ...
                                                    numel(obj.allBiosemi)+1); %sessionInd
            end
            obj.sources = rmfield(obj.sources, dataSourceName);
        end

        function runMuseValidation(obj,saveEEG,saveMode)
            dataSource = "muse";
            dataSourceName = "EEG_"+dataSource;

            obj.getData(dataSourceName);
            obj.sources.EEG_muse.setParticipantId(obj.id);
            obj.getEEGEpochedEvent(eventName='feedback_time',...
                            windowToEpoch = [-0.5, 2]);
            if saveEEG
                % If intermediate save is needed:
                obj.saveEEG(source = dataSourceName,...
                            saveMode ="asParquet", ...
                            timeBinningMode = saveMode, ...
                            blocksPerBin=0);
            end
            obj.runTFAnalysis(byFrequencyBand=false, ...
                                source=dataSourceName);
            obj.saveTFAnalysis( source=dataSourceName,...
                                saveMode="asParquet", ...
                                blocksPerBin=0, ...
                                timeBinningMode=saveMode); 
        end

        function validateTFSource(obj,desiredEEGSource)
            obj.EEGSourceToRunTF = desiredEEGSource;
        
            if ~isfield(obj.sources, obj.EEGSourceToRunTF) || isempty(obj.sources.(obj.EEGSourceToRunTF))
                error("No EEG data found for source '%s'. Make sure obj.sources.%s is populated.", ...
                      sourceName, obj.EEGSourceToRunTF);
            end

            if ~isfield(obj.TFanalyzers, obj.EEGSourceToRunTF) || isempty(obj.TFanalyzers.(obj.EEGSourceToRunTF))
                obj.TFanalyzers.(obj.EEGSourceToRunTF) = TimeFrequencyAnalyzer(obj.sources.(obj.EEGSourceToRunTF));    
            end

        end

        function prepareSourceDirectories(obj)
            % Sometimes obj.pathToData contains physio and schedule
            % together because the data is arranged that way e.g. For
            % validation
            if obj.validation
                obj.pathToPhysioDir = fullfile(obj.pathToData,"mat_files_csv_and_MUSE_db",obj.id);
                obj.pathToScheduleFile =  obj.pathToPhysioDir;
                obj.biosemiDirectoryPath = fullfile(obj.pathToData,"BIOSEMI_bdf",obj.id);
            else
                obj.pathToPhysioDir = fullfile(obj.pathToData,"physio");
                obj.pathToScheduleFile = fullfile(obj.pathToData,"schedule");
            end
        end

        function readRawData(obj, givenDataTypes)
            toLoad = obj.validateSensorInput(givenDataTypes);
            if isempty(toLoad)
                return 
            end

            physical = obj.getPhysicalDataTypes(toLoad);

            tempName = Utils.createTemporalMergedDatabase(...
                obj.pathToPhysioDir, physical);

            obj.loadPhysicalData(physical, tempName);
            obj.processVirtualData(toLoad, tempName);
            % delete(tempName);
        end

        function toLoad = validateSensorInput(obj, inputList)
            if ischar(inputList)
                inputList = {inputList};
            elseif isstring(inputList)
                inputList = cellstr(inputList);
            end
            if ~iscellstr(inputList)
                error('Input must be char, string, or cell array of char.');
            end

            invalid = setdiff(inputList, obj.acceptedDataTypes);
            if ~isempty(invalid)
                error('Invalid input(s): %s\nAllowed: %s', ...
                    strjoin(invalid, ', '), ...
                    strjoin(obj.acceptedDataTypes, ', '));
            end

            obj.dataTypes = inputList;

            existing = fieldnames(obj.sources);
            toLoad = setdiff(inputList, existing);

            if isempty(toLoad)
                warning('All requested data types already loaded. Skipping load.');
            end
        end

        function validateEventName(obj,eventName)
            if ismember(eventName,obj.availableEventNames)
                obj.eventName = eventName;
            else
                error("Unrecognized event name %s",eventName);
            end
        end

        function physical = getPhysicalDataTypes(~, allTypes)
            physical = setdiff(allTypes, MomentumParticipant.virtualSources);
            if isempty(physical) 
                if any(allTypes=="Polar_RRIfromECG")
                    physical = "Polar_ECG";
                %elseif any(allTypes=="")
                end
            end
        end
    
        function generateSavingDirectoryPaths(obj)
            % Main processed directory path is sibling to raw data
            if obj.validation
                % Validation receives a pathToData already 2 levels above the
                % participant data
                analysisName = "Validation";
                
                obj.parentDirToData =obj.pathToData;
               
            else
                analysisName = "AlignedEvent";
                [parentDirToDataDir,obj.id,~] = fileparts(obj.pathToData);
                obj.parentDirToData = fileparts(parentDirToDataDir);
            end

            obj.EEGsaveDirectoryPath = fullfile(obj.parentDirToData,MomentumParticipant.generalProcessedSaveDirectoryName,analysisName,"EEG");
            obj.TFsaveDirectoryPath = fullfile(obj.parentDirToData,MomentumParticipant.generalProcessedSaveDirectoryName,analysisName,"TF");
            obj.TFInducedSaveDirectoryPath = fullfile(obj.parentDirToData,MomentumParticipant.generalProcessedSaveDirectoryName,analysisName,"TF_Induced");
            obj.RRIsaveDirectoryPath = fullfile(obj.parentDirToData,MomentumParticipant.generalProcessedSaveDirectoryName,analysisName,"RRI");
            obj.scheduleSaveDirectoryPath = fullfile(obj.parentDirToData,MomentumParticipant.generalProcessedSaveDirectoryName,analysisName,"Schedule");
        end

        function loadPhysicalData(obj, types, dbPath)
            for t = types
                name = t{:};
                if ~isfield(obj.sources, name)
                    obj.sources.(name) = feval(name, dbPath);
                    obj.sources.(name).preprocessData();
                end
            end
        end
    
        function processVirtualData(obj, allTypes, dbPath)
            if any(allTypes=="Polar_RRIfromECG")
                % ensure ECG is ready
                if ~isfield(obj.sources, "Polar_ECG")
                    temporalPolarECG = Polar_ECG(dbPath);
                    temporalPolarECG.preprocessData();
                else
                    temporalPolarECG = obj.sources.Polar_ECG;
                end
                % extract RRI without re-preprocessing ECG
                obj.sources.Polar_RRIfromECG = Polar_RRIfromECG(temporalPolarECG);
                obj.sources.Polar_RRIfromECG.preprocessData();
            end
        end
        
    end

    methods (Static)

        function participantId = validateParticipantId(fileName)
            digitsOnly = regexprep(fileName, '[^0-9]', '');
            
            if numel(char(digitsOnly)) >= 5 && numel(char(digitsOnly)) <= 6
                participantId = string(digitsOnly);
            else
                participantId = "";
            end
        end

        function saveMode = validateSavingMode(saveMode)

            % Convert string objects to char vector
            if isa(saveMode, 'string')
                saveMode = char(saveMode);
            end
            if ~ischar(saveMode)
                error('validateExportMode:InvalidType', ...
                    'exportMode must be a character vector or string scalar.');
            end
        
            % Trim leading/trailing whitespace
            saveMode = strtrim(saveMode);
        
            % Check for "mat" variant (with or without 'as')
            if ~isempty(regexp(saveMode, '^(?:as)?mat$', 'ignorecase'))
                saveMode = 'asMat';
        
            % Check for "csv" variant (with or without 'as')
            elseif ~isempty(regexp(saveMode, '^(?:as)?csv$', 'ignorecase'))
                saveMode = 'asCSV';
        
            % Check for "parquet" variant (with or without 'as')
            elseif ~isempty(regexp(saveMode, '^(?:as)?parquet$', 'ignorecase'))
                saveMode = 'asParquet';
        
            else
                error('validateExportMode:InvalidValue', ...
                    'Invalid export mode "%s". Valid options are "asMat", "asCSV", or "asParquet" (case‐insensitive).', ...
                    saveMode);
            end
        end

        function binningMode = validateBinningMode(binningMode)        
            % Convert string objects to char
            if isa(binningMode,'string')
                binningMode = char(binningMode);
            end
            if ~ischar(binningMode)
                error('validateSavingMode:InvalidType', ...
                    'saveMode must be a character vector or string scalar.');
            end
        
            % Trim whitespace
            binningMode = strtrim(binningMode);
        
            % Check for "timepoints" variant:
            if ~isempty(regexp(binningMode, '^(?:by)?timepoints$', 'ignorecase'))
                binningMode = 'byTimepoints';
        
            % Check for "time" variant:
            elseif ~isempty(regexp(binningMode, '^(?:by)?time$', 'ignorecase'))
                binningMode = 'byTime';
        
            else
                error('validateSavingMode:InvalidValue', ...
                    'Invalid save mode "%s". Valid options are "byTime" or "byTimepoints" (case‐insensitive).', ...
                    binningMode);
            end
        end

        function fileName = getFileName(opts)
            arguments
                opts.participantId   = "123456"
                opts.eventName       = "RestingState"
                opts.section         = ""               % now default is "", so it’s ignored unless specified
                opts.freqLabel       = []               % (band name or bin string)
                opts.binningMode     = "byTimepoints"   % {byTimepoints, byTime}
                opts.timeBinIdx      = 0                % Numeric time‐bin index
                opts.blockBinIdx   = 0                % If not 0, insert "blockBin_%03d" expects numeric
                opts.channelLabel    = ""
                opts.extension       = ".parquet"
                opts.asPattern       = false            % Drop participantId if true
                opts.dataType        = "TF"             % {TF, EEG,RRI}
            end
        
            %─── 1) Compute the time‐bin label once ────────────────────────────────────────
            timeBinLabel = MomentumParticipant.validateTimeBinningMode(opts.binningMode);
        
            %─── 2) Build the “prefix” (participantId vs. pattern), omitting section if empty ───
            % hasSection = ~(opts.section == "" || opts.section == 0);  % true if section is nonempty or nonzero
            hasSection = ( ...
                         ( isstring(opts.section) && strlength(opts.section)>0 )    ... % non‐empty string
                      || ( ischar(opts.section)   && ~isempty(opts.section)     )    ... % non‐empty char
                      || ( isnumeric(opts.section)&& opts.section~=0            ) );  % non‐zero number

            if opts.asPattern
                if hasSection
                    % "<event>_<section>"
                    fmt  = "%s_%s";
                    args = { opts.eventName, opts.section };
                else
                    % "<event>" (omit underscore and section)
                    fmt  = "%s";
                    args = { opts.eventName };
                end
            else
                if hasSection
                    % "<participant>_<event>_<section>"
                    fmt  = "%s_%s_%s";
                    args = { opts.participantId, opts.eventName, opts.section };
                else
                    % "<participant>_<event>"
                    fmt  = "%s_%s";
                    args = { opts.participantId, opts.eventName };
                end
            end
        
            %─── 3) Insert blockBin piece if provided ────────────────────────────────
            if opts.blockBinIdx > 0
                fmt = fmt + "_blockBin_%03d";
                args{end+1} = opts.blockBinIdx;
            end
            
            %─── 4) Insert freqBin if dataType == TF ─────────────────────────────────
            if ~strcmp(opts.channelLabel,'')
                fmt = fmt + "_%s";
                args{end+1} = opts.channelLabel;
            end

            %─── 5) Insert freqBin if dataType == TF ─────────────────────────────────
            if startsWith(opts.dataType, "TF")
                % Only for TF: validate and append frequency label
                frequencyString = MomentumParticipant.validateFrequencyLabel(opts.freqLabel);
                fmt = fmt + "_freqBin_%s";
                args{end+1} = frequencyString;
            end
        
            %─── 6) Append the tail: include timeBinLabel & index if idx > 0, otherwise skip ──
            if opts.timeBinIdx > 0
                % Include time‐bin label and zero‐padded index
                fmt = fmt + "_%s_%03d%s";
                args{end+1} = timeBinLabel;        % e.g. "byTime" or "byTimepoints"
                args{end+1} = opts.timeBinIdx;     % e.g. 5 → printed as “005”
                args{end+1} = opts.extension;      % e.g. ".parquet"
            else
                % Skip time‐bin label/index; just append extension
                fmt = fmt + "%s";
                args{end+1} = opts.extension;      % e.g. ".parquet"
            end
        
            %─── 7) Build the fileName ───────────────────────────────────────────────────
            fileName = sprintf(fmt, args{:});
        end

        function frequencyString = validateFrequencyLabel(frequencyLabel)
            if isnumeric(frequencyLabel)
                frequencyString = sprintf('%02d', frequencyLabel);
            else
                frequencyString = char(frequencyLabel);
            end
        end
        
        function timeBinLabel= validateTimeBinningMode(binningMode)

            if strcmp(binningMode,"byTimepoints")
                timeBinLabel= "tpBin";
            elseif strcmp(binningMode,"byTime")
                timeBinLabel = "tBin";
            end
        
        end
        
        function prepareSaveDirectory(saveDir)
            if ~isfolder(saveDir); mkdir(saveDir); end
        end

    end
end
