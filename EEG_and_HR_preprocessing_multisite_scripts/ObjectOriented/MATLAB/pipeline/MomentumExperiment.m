classdef MomentumExperiment < handle
    properties
        participantId
        rawDataDir
        preprocessedEEGDir
        mode
        participantDir
        preprocessedDirStruct
        participant
        modeHandlerMap
        validationEvent 
    end
    
    properties (Access = private)
        eegEnvInitialized (1,1) logical = false;
    end
    
    methods
        function obj = MomentumExperiment(participantId, rawDataDir, preprocessedEEGDir, mode, validationEvent)
            if nargin<5 
                validationEvent = "feedback_time";
            end
            obj.participantId = participantId;
            obj.rawDataDir = rawDataDir;
            obj.preprocessedEEGDir = preprocessedEEGDir;
            obj.mode = char(mode);
            obj.validationEvent = validationEvent;
            obj.participantDir = fullfile(rawDataDir, participantId);
            obj.preprocessedDirStruct = MomentumExperiment.buildPreprocessedDirStruct(obj.participantDir, preprocessedEEGDir);
            obj.modeHandlerMap = obj.buildModeHandlerMap();
        end

        function participant = run(obj)
            fprintf("Processing started %s \n", datetime());
            tic;
            obj.dispatchMode();
            fprintf("Processing finished %s \n", datetime());
            toc;
            participant = obj.participant;
        end

        function modeHandlerMap = buildModeHandlerMap(obj)
            modeHandlerMap = containers.Map('KeyType','char','ValueType','any');

            % Modes that need eeglab/fieldtrip:
            modeHandlerMap('trialDf')       = @() obj.withEEGEnv(@() obj.runTrialDfMode());
            modeHandlerMap('restingState')  = @() obj.withEEGEnv(@() obj.runRestingStateMode());
            modeHandlerMap('feedback')      = @() obj.withEEGEnv(@() obj.runFeedbackMode());
            modeHandlerMap('stim')          = @() obj.withEEGEnv(@() obj.runStimMode());
            modeHandlerMap('choice')        = @() obj.withEEGEnv(@() obj.runChoiceMode());
            modeHandlerMap('validation')    = @() obj.withEEGEnv(@() obj.runValidationMode());

            % Rest of modes
            modeHandlerMap('rri')           = @() obj.runRriMode();
            modeHandlerMap('ecg')           = @() obj.ECGMode();
        end

        function dispatchMode(obj)
            modeKey = char(obj.mode);
            if isKey(obj.modeHandlerMap, modeKey)
                modeHandler = obj.modeHandlerMap(modeKey);
                modeHandler();
            else
                error("MomentumExperiment:UnknownMode", "Unknown mode: %s", obj.mode);
            end
        end

        function ensureDefaultParticipantInitialized(obj)
            if isempty(obj.participant)
                obj.participant = MomentumParticipant( ...
                    pathToData = obj.participantDir, ...
                    preprocessedDirs = obj.preprocessedDirStruct);
            end
        end

        function runTrialDfMode(obj)
            obj.participant = MomentumParticipant(id = obj.participantId, ...
                                                pathToData = obj.preprocessedEEGDir, ...
                                                validation = true);
            obj.participant.getTrialsWithSystem();
        end

        function runRestingStateMode(obj)
            obj.ensureDefaultParticipantInitialized();
            channelsToRemove = {'left_front', 'right_front'};
            obj.participant.getEEGEpochedEvent( ...
                eventName = 'RestingState', ...
                windowToEpoch = [], ...
                channelsToRemove = channelsToRemove);
            obj.participant.runTFAnalysis();
            obj.participant.saveTFAnalysis( ...
                saveMode = "asParquet", ...
                timeBinningMode = "byTimepoints", ...
                blocksPerBin = 8);
        end

        function runFeedbackMode(obj)
            obj.ensureDefaultParticipantInitialized();
            obj.participant.getEEGEpochedEvent( eventName = 'feedback_time', ...
                                                padForTF = true, ...
                                                windowToEpoch = [-0.2, 3.00]);
            obj.participant.saveEEG(timeBinningMode = "byTimepoints", ...
                                                    tPerBin = 12);
            obj.participant.runTFAnalysis(subtractERP ="block",...
                                        getInducedTF = false,... %"block",...
                                        TFWindow = [-0.2,3.00])
            obj.participant.saveTFAnalysis(timeBinningMode = "byTimepoints",...
                                        tPerBin = 12); 
        end

        function runStimMode(obj)
            obj.ensureDefaultParticipantInitialized();
            obj.participant.getEEGEpochedEvent(eventName = 'stim_time', ...
                                            padForTF = false, ...
                                            windowToEpoch = [-0.2, 2.00]);
            obj.participant.saveEEG(timeBinningMode = "byTimepoints",...
                                       tPerBin = 12);
            obj.participant.runTFAnalysis(subtractERP = false, ...
                                            getInducedTF = false, ...
                                            TFWindow = [-0.2, 3.00]);
            obj.participant.saveTFAnalysis(timeBinningMode  = "byTimepoints",...
                                       tPerBin = 12);    
        end

        function runChoiceMode(obj)
            obj.ensureDefaultParticipantInitialized();
            obj.participant.getEEGEpochedEvent(eventName = 'choice_time', ...
                                        windowToEpoch = [-2.0, 0.5]);
            obj.participant.saveEEG(saveMode ="asParquet", ...
                                        timeBinningMode = "byTimepoints");
            obj.participant.runTFAnalysis();
            obj.participant.saveTFAnalysis(saveMode = "asParquet", ...
                                    timeBinningMode = "byTimepoints");
        end
        
        function ECGMode(obj)
            obj.ensureDefaultParticipantInitialized();
            obj.participant.getECGEpochedEvent(eventName = 'feedback_time', ...
                                                windowToEpoch = [-1.0, 10]);
            obj.participant.saveECG();
        end

        function runRriMode(obj)
            obj.ensureDefaultParticipantInitialized();
            obj.participant.getRRIEpochedEvent( ...
                eventName = 'feedback_time', ...
                windowToEpoch = [-1.0, 10]);
            obj.participant.saveRRI();
        end

        function runValidationMode(obj)
            switch obj.validationEvent
                case "stim_time"
                    windowToEpoch = [-0.2, 2.0];
                case "choice_time"
                    windowToEpoch = [-2.0, 0.5];
                case "feedback_time"
                    windowToEpoch = [-0.5, 2.0];
            end

            obj.participant = MomentumParticipant(id = obj.participantId, ...
                                                pathToData = obj.preprocessedEEGDir, ...
                                                validation = true);
            obj.participant.runValidation(obj.validationEvent, windowToEpoch);
        end
    end
    
    methods (Access = private)

        function withEEGEnv(obj, fn)
            obj.initEEGEnvironment();
            fn();
        end

         function initEEGEnvironment(obj)
            if obj.eegEnvInitialized
                return;
            end
            eeglab;
            close;         
            ft_defaults;

            obj.eegEnvInitialized = true;
        end
    end

    methods (Static)
        
        function preprocessedDirs = buildPreprocessedDirStruct(participantRawDir,preprocEEGDir)
            if ~isempty(preprocEEGDir)
                [~, participantId, ~] = fileparts(participantRawDir);
                preprocDir = fullfile(preprocEEGDir, 'Data_Processed', sprintf('subject_%s', participantId));
                preprocessedDirs = struct('EEG_muse', preprocDir);
            else
                preprocessedDirs = struct();
            end
        end

    end

end
