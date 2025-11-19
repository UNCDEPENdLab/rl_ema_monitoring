function participant = runMomentumParticipantAnalysis(participantId,rawDataDir,preprocessedEEGDir)
    eeglab;
    close;
    ft_defaults;  
    participantDir = fullfile(rawDataDir,participantId);
	fprintf("Processing started %s \n",datetime());
	tic;

    preprocStruct = MomentumExperiment.buildPreprocessedDirStruct(participantDir,preprocessedEEGDir);
    participant = MomentumParticipant(pathToData=participantDir, ...
                                       preprocessedDirs=preprocStruct);
    mode="stim";
    switch mode 
        case "trialDf"
            
            participant = MomentumParticipant(id=participantId, ...
                                            pathToData=preprocessedEEGDir, ...
                                                validation=true);
            participant.getTrialsWithSystem();

        case "restingState"
            %-------- Resting State------------
            channelsToRemove = {'left_front','right_front'};
            participant.getEEGEpochedEvent(eventName='RestingState', ...
                                       windowToEpoch = [], ...
                                       channelsToRemove=channelsToRemove);
            participant.runTFAnalysis();
            participant.saveTFAnalysis(saveMode="asParquet", ...
                                       timeBinningMode="byTimepoints",...
                                         blocksPerBin=8);
        case "feedback"
            %-------- Feedback ------------
            participant.getEEGEpochedEvent(eventName    = 'feedback_time', ...
                                        padForTF        = true,... % Add padding to have TF with the same range
                                        windowToEpoch   = [-0.2,3.00]); % new window of interest % Previously [-1.5,3.00]
            % participant.saveEEG(timeBinningMode       = "byTimepoints",...
            %                           tPerBin         = 12);
            participant.runTFAnalysis(subtractERP="block",...
                                        getInducedTF      = false,... %"block",...
                                        TFWindow        = [-0.2,3.00])
            participant.saveTFAnalysis(timeBinningMode  = "byTimepoints",...
                                        tPerBin         = 12); 
        case "stim"
            %-------- Stim ------------
            participant.getEEGEpochedEvent(eventName    = 'stim_time', ...
                                        padForTF        = false,... % Add padding to have TF with the same range
                                        windowToEpoch   = [-0.2,2.00]); 
            % participant.saveEEG(timeBinningMode       = "byTimepoints",...
            %                            tPerBin         = 12);
            participant.runTFAnalysis(subtractERP=false,...
                                       getInducedTF      = false,... %"block",...
                                       TFWindow        = [-0.2,3.00])
            %participant.saveTFAnalysis(timeBinningMode  = "byTimepoints",...
            %                            tPerBin         = 12);                                 
        case "choice"
            %-------- Choice ------------
            participant.getEEGEpochedEvent(eventName ='choice_time', ...
                                       windowToEpoch = [-2.0,0.5]);
            %participant.saveEEG(saveMode ="asParquet", ...
            %               timeBinningMode = "byTimepoints");
            participant.runTFAnalysis();
            participant.saveTFAnalysis(saveMode="asParquet", ...
                                   timeBinningMode="byTimepoints");
        case "rri"
            %-------- RRI------------
            addpath(genpath('/ix1/adombrovski/lab_resources/PhysioNet-Cardiovascular-Signal-Toolbox-master'));
            participant.getRRIEpochedEvent(eventName ='feedback_time', ...
                                       windowToEpoch = [-1.0,10]);
            participant.saveRRI();
        case "validation"
            %-------- Validation ------------
            % Sending the preprocessed as the raw here:
            participant = MomentumParticipant(id=participantId, ...
                                            pathToData=preprocessedEEGDir, ...
                                                validation=true);
            participant.runValidation();
    end
	% ----- End-----
	fprintf("Processing finished %s \n",datetime());
	toc;
end
