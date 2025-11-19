classdef DataWriter < handle
    properties (Constant)
    end

    properties
        saveDir
        dataType
        timeBinnedData
        timepointsPerBin = 0 % with 0 it means all timepoints in one bin
        timePerBin = 25 %ms
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
        timeToBinMapping
        numberOfTimeBins
        timeLabels
        
        % Section binning
        sectionLabels
        numberOfSectionBins
        sectionBinnedData
        currentSection
        
        % Block binning
        blockLabels
        blocksPerBin
        uniqueBlocks
        blockToBinMapping
        numberOfBlockBins        
        expectedBlocks
        blockBinnedData

        % Frequency binning
        frequencyBinLabel =[]

        % Channel binning
        binByChannel
        channelLabels
        numberOfChannelBins
        channelBinnedData
        currentChannelLabel

        % outcome labels
        outcomeLabels
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
                opts.saveDir            = saveDir
                opts.saveMode           = "asParquet"
                opts.timeBinningMode    = "byTime"
                opts.blocksPerBin       = 0
                opts.binByChannel       = false
                opts.tPerBin            = 0 % Updates either timepointsPerBin or timePerBin properties depending on opts.timeBinningMode 
            end

            obj.saveDir =opts.saveDir;
            obj.timeBinningMode = opts.timeBinningMode;
            obj.blocksPerBin = opts.blocksPerBin;
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
                    obj.calculateTimeBinning(opts.tPerBin);
                    obj.calculateBlockBinning();
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

                Utils.updateProgress(round(obj.numberOfTimeBins/10),timeBinIdx,obj.numberOfTimeBins,"Saved", "files");
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

                obj.iterateBlocksBin(timeBinIdx,sectionIdx)
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

        function iterateBlocksBin(obj, timeBinIdx,section)

            for blockBinIdx = 1:obj.numberOfBlockBins
                
                obj.getThisBlockBinData(blockBinIdx);
                
                if all(isnan(obj.blockBinnedData.signal))
                    continue
                end

                % Lets the function below know there is just one block bin
                if obj.numberOfBlockBins==1; blockBinIdx = 0; end
                
                obj.iterateChannels(timeBinIdx,section,blockBinIdx);
                % obj.writeBinnedData(timeBinIdx,section,blockBinIdx);
            end

        end

        function calculateChannelBinning(obj)
            if obj.binByChannel
                obj.numberOfChannelBins = numel(obj.channelLabels);
            else
                obj.numberOfChannelBins = 1;

            end

        end
        
        function iterateChannels(obj,timeBinIdx,section,blockBinIdx)
            obj.calculateChannelBinning();
            for channelIdx = 1:obj.numberOfChannelBins
                obj.getThisChannelBinData(channelIdx);
                obj.writeBinnedData(timeBinIdx,section,blockBinIdx)
            end
        
        end
        
        function getThisChannelBinData(obj,channelIdx)
            if obj.binByChannel
                obj.currentChannelLabel = obj.channelLabels{channelIdx};
                obj.channelBinnedData = obj.blockBinnedData(ismember(obj.blockBinnedData.channel,obj.currentChannelLabel),:);
                obj.channelBinnedData.channel = [];
            else
                obj.currentChannelLabel = "";
                obj.channelBinnedData = obj.blockBinnedData;
            end
        end

        function writeBinnedData(obj,timeBinIdx,section,blockBin)
                                    
            fileSaveName = MomentumParticipant.getFileName(participantId=obj.participantId, ...
                                            eventName = obj.eventName, ...
                                            section = section, ...
                                            freqLabel = obj.frequencyBinLabel, ...
                                            binningMode = obj.timeBinningMode, ...
                                            timeBinIdx = timeBinIdx, ...
                                            blockBinIdx = blockBin,...
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

        function getThisBlockBinData(obj,blockBinIdx)
            blocksInThisBin = obj.expectedBlocks(obj.blockToBinMapping==blockBinIdx);
            obj.blockBinnedData = obj.sectionBinnedData(ismember(obj.sectionBinnedData.block,categorical(blocksInThisBin)),:);
        end

        function getThisTimeBinData(obj,timeBin)
            mask = ismember(obj.data.timeBin,obj.timeLabels(obj.timeToBinMapping==timeBin));
            obj.timeBinnedData = obj.data(mask,:);
        end

        function calculateTimeBinning(obj,tPerBin)
            switch obj.timeBinningMode
                case "byTimepoints"
                    obj.timepointsPerBin = tPerBin;
                    timeDatapoints = obj.getTimeDatapoints();
                    if obj.timepointsPerBin ==0
                        obj.timeToBinMapping = ones(timeDatapoints,1);
                    else
                        obj.timeToBinMapping = ceil((1:timeDatapoints)'/obj.timepointsPerBin);
                    end

                case "byTime"
                    obj.timePerBin = tPerBin;
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
        
        function getExpectedBlocks(obj)
            obj.expectedBlocks = MomentumParticipant.regularBlockLabels;
        end

        function calculateBlockBinning(obj)
            obj.getExpectedBlocks();
            
            nbBlocks = numel(obj.expectedBlocks);
            if isnumeric(obj.blocksPerBin) && isscalar(obj.blocksPerBin) && obj.blocksPerBin >= 1
                obj.blockToBinMapping = ceil((1:nbBlocks)/obj.blocksPerBin);
            else
                % When set to zero, we want all mappings to be in the same
                % group
                obj.blockToBinMapping = ones(1,nbBlocks);
            end

            obj.numberOfBlockBins = max(obj.blockToBinMapping);
        end
    end
    
    
    methods (Static)
    
        function tOut = sortTableByCardinality(tIn)

            % If empty or no rows, just return as-is
            if isempty(tIn) || height(tIn) == 0
                tOut = tIn;
                return;
            end

            varNames = tIn.Properties.VariableNames;
            nRows    = height(tIn);

            % Preallocate cardinality ratio array
            cardRatio = zeros(1, numel(varNames));

            % Compute #unique / #rows for each variable
            for k = 1:numel(varNames)
                v = tIn.(varNames{k});
                cardRatio(k) = numel(unique(v)) / nRows;
            end

            % Sort variables by ascending cardinality (low → high)
            [~, idx] = sort(cardRatio, 'ascend');
            sortVars = varNames(idx);

            % Sort the table rows using that variable order
            tOut = sortrows(tIn, sortVars);
        end
    end

end
