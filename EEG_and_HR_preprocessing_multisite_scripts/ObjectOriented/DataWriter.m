classdef DataWriter < handle
    properties (Constant)
        timepointsPerBin = 0 % with 0 it means all timepoints in one bin
    end

    properties
        saveDir
        dataType
        timeBinnedData
    end

    properties (Access=protected)
        % Main input data
        data        
        participantId
        eventName

        % Saving parameters
        saveMode
        saveExtension
        
        % Time binning
        timeBinningMode
        timePerBin = 25 %ms
        timeToBinMapping
        numberOfTimeBins
        timeLabels
        
        % Section binning
        sectionLabels
        numberOfSectionBins
        sectionBinnedData
        currentSection
        
        % Session binning
        sessionLabels
        sessionsPerBin
        uniqueSessions
        sessionToBinMapping
        numberOfSessionBins        
        expectedSessions
        sessionBinnedData

        % Frequency binning
        frequencyBinLabel =[]

        % Channel binning
        binByChannel
        channelLabels
        numberOfChannelBins
        channelBinnedData
        currentChannelLabel
    end

    methods (Access=public)
        function obj = DataWriter(opts)
            arguments 
                opts.data = []
                opts.dataType = "RRI"
                opts.id= ""
                opts.eventName= ""
                opts.trialLabels   = [] 
            end
            
            obj.data = opts.data;
            obj.dataType = opts.dataType;
            obj.participantId = opts.id;
            obj.eventName = opts.eventName;
            obj.getTimeLabels();

        end
        
        function save(obj,opts)
            arguments 
                obj
                opts.saveDir = saveDir
                opts.saveMode = "asParquet"
                opts.timeBinningMode = "byTime"
                opts.sessionsPerBin = 0
                opts.binByChannel = false
            end

            obj.saveDir =opts.saveDir;
            obj.timeBinningMode = opts.timeBinningMode;
            obj.sessionsPerBin = opts.sessionsPerBin;
            obj.binByChannel = opts.binByChannel;

            if ~exist(obj.saveDir, 'dir'); mkdir(obj.saveDir);end
            
            obj.getOutputExtension(opts.saveMode);
            switch obj.saveExtension
                case ".mat"
                    % Save as nD data 
                    data = obj.data;
                    save(obj.saveDir, data, "-v7.3");
                    fprintf("Data has been saved to: %s \n",obj.saveDir);
                case {".csv",".parquet"}
                    obj.calculateTimeBinning();
                    obj.calculateSessionBinning();
                    obj.iterateTimeBins();
            end

            fprintf("%s saving complete.\n",obj.dataType);

        end

    end
    
    methods (Access=protected)
        function getTimeLabels(obj)
            if size(obj.data,2)>=1
                obj.timeLabels = unique(obj.data.timeBin);
            end
        end

        function getOutputExtension(obj,saveMode)
            if strcmpi(saveMode,"asCSV")
                obj.saveExtension = ".csv";
            elseif strcmpi(saveMode,"asParquet")
                obj.saveExtension = ".parquet";
            elseif strcmpi(saveMode,"asMat")
                obj.saveExtension = ".mat";
            else
                error("Unrecognized save mode %s",saveMode);
            end
        end

        function iterateTimeBins(obj)
            for timeBinIdx = 1:obj.numberOfTimeBins  
                obj.getThisTimeBinData(timeBinIdx);
                
                if isscalar(1:obj.numberOfTimeBins); timeBinIdx = 0; end
                obj.iterateSectionBin(timeBinIdx);

                Utils.updateProgress(10,timeBinIdx,obj.numberOfTimeBins,"Saved", "files");
            end
        end

        function calculateSectionBinning(obj)
            if ismember('section', obj.timeBinnedData.Properties.VariableNames)
                
                obj.sectionLabels = unique(obj.timeBinnedData.section);
                % if isscalar(obj.sectionLabels)
                %     obj.sectionLabels = 0;
                % end
            else
                obj.sectionLabels = 0;
            end

            obj.numberOfSectionBins = numel(obj.sectionLabels);
        end
        
        function iterateSectionBin(obj,timeBinIdx)
            obj.calculateSectionBinning();

            for sectionIdx = 1:obj.numberOfSectionBins
                obj.getThisSectionBinData(sectionIdx);
                
                if obj.numberOfSectionBins==1; sectionIdx = 0; end

                obj.iterateSessionBin(timeBinIdx,sectionIdx)
            end 
        end

        function getThisSectionBinData(obj,sectionIdx)
            if ismember('section', obj.timeBinnedData.Properties.VariableNames)
                obj.currentSection = obj.sectionLabels{sectionIdx};
                obj.sectionBinnedData = obj.timeBinnedData(obj.timeBinnedData.section ==obj.currentSection ,:);
                obj.sectionBinnedData.section = [];
            else
                obj.sectionBinnedData = obj.timeBinnedData;
            end
        end

        function iterateSessionBin(obj, timeBinIdx,section)

            for sessionBinIdx = 1:obj.numberOfSessionBins
                
                obj.getThisSessionBinData(sessionBinIdx);
                
                if all(isnan(obj.sessionBinnedData.signal))
                    continue
                end

                % Lets the function below know there is just one session bin
                if obj.numberOfSessionBins==1; sessionBinIdx = 0; end
                
                obj.iterateChannels(timeBinIdx,section,sessionBinIdx);
                % obj.writeBinnedData(timeBinIdx,section,sessionBinIdx);
            end

        end

        function calculateChannelBinning(obj)
            if obj.binByChannel
                obj.numberOfChannelBins = numel(obj.channelLabels);
            else
                obj.numberOfChannelBins = 1;

            end

        end
        
        function iterateChannels(obj,timeBinIdx,section,sessionBinIdx)
            obj.calculateChannelBinning();
            for channelIdx = 1:obj.numberOfChannelBins
                obj.getThisChannelBinData(channelIdx);
                obj.writeBinnedData(timeBinIdx,section,sessionBinIdx)
            end
        
        end
        
        function getThisChannelBinData(obj,channelIdx)
            if obj.binByChannel
                obj.currentChannelLabel = obj.channelLabels{channelIdx};
                obj.channelBinnedData = obj.sessionBinnedData(ismember(obj.sessionBinnedData.channel,obj.currentChannelLabel),:);
                obj.channelBinnedData.channel = [];
            else
                obj.currentChannelLabel = "";
                obj.channelBinnedData = obj.sessionBinnedData;
            end
        end

        function writeBinnedData(obj,timeBinIdx,section,sessionBin)
                                    
            fileSaveName = MomentumParticipant.getFileName(participantId=obj.participantId, ...
                                            eventName = obj.eventName, ...
                                            section = section, ...
                                            freqLabel = obj.frequencyBinLabel, ...
                                            binningMode = obj.timeBinningMode, ...
                                            timeBinIdx = timeBinIdx, ...
                                            sessionBinIdx = sessionBin,...
                                            channelLabel = obj.currentChannelLabel,...
                                            extension = obj.saveExtension, ...
                                            dataType=obj.dataType);

            fileSavePath = fullfile(obj.saveDir,fileSaveName);

            if strcmp(obj.saveExtension,".csv")
                writetable(obj.channelBinnedData,fileSavePath);
                gzip(fileSavePath);            % compress it 
                delete(fileSavePath); 
            elseif strcmp(obj.saveExtension,".parquet")
                parquetwrite(fileSavePath,obj.channelBinnedData);
            end
        end

        function getThisSessionBinData(obj,sessionBinIdx)
            sessionsInThisBin = obj.expectedSessions(obj.sessionToBinMapping==sessionBinIdx);
            obj.sessionBinnedData = obj.sectionBinnedData(ismember(obj.sectionBinnedData.session,categorical(sessionsInThisBin)),:);
        end

        function getThisTimeBinData(obj,timeBin)
            mask = ismember(obj.data.timeBin,obj.timeLabels(obj.timeToBinMapping==timeBin));
            obj.timeBinnedData = obj.data(mask,:);
        end

        function calculateTimeBinning(obj)
            switch obj.timeBinningMode
                case "byTimepoints"
                    
                    timeDatapoints = obj.getTimeDatapoints();
                    if obj.timepointsPerBin ==0
                        obj.timeToBinMapping = ones(timeDatapoints,1);
                    else
                        obj.timeToBinMapping = ceil((1:timeDatapoints)'/obj.timepointsPerBin);
                    end

                case "byTime"
                    % tMin = min(obj.timeLabels);
                    % obj.timeToBinMapping = floor((obj.timeLabels - tMin)/obj.timePerBin)+1;
                    tMin = min(obj.timeLabels);
                    tMax = max(obj.timeLabels);
                    edges = tMin : obj.timePerBin : (tMax + obj.timePerBin);
                    obj.timeToBinMapping = discretize(obj.timeLabels, edges);

                    if max(obj.timeToBinMapping)>numel(obj.timeLabels)
                        error("Time binning resulted in more bins than given time labels," + ...
                            " the timePerBin %s ms is probably shorter than the spacing between" + ...
                            "the data time labels",obj.timePerBin);
                    end
            end
            obj.numberOfTimeBins = max(obj.timeToBinMapping);
        end

        function timeDatapoints = getTimeDatapoints(obj)
            timeDatapoints = numel(obj.timeLabels);
        end
        
        function getExpectedSessions(obj)
            obj.expectedSessions = MomentumParticipant.regularSessionLabels;
        end

        function calculateSessionBinning(obj)
            obj.getExpectedSessions();
            
            nbSessions = numel(obj.expectedSessions);
            if isnumeric(obj.sessionsPerBin) && isscalar(obj.sessionsPerBin) && obj.sessionsPerBin >= 1
                obj.sessionToBinMapping = ceil((1:nbSessions)/obj.sessionsPerBin);
            else
                % When set to zero, we want all mappings to be in the same
                % group
                obj.sessionToBinMapping = ones(1,nbSessions);
            end

            obj.numberOfSessionBins = max(obj.sessionToBinMapping);
        end
    end

end
