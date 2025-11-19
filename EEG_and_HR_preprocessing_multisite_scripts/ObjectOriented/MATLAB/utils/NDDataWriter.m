classdef NDDataWriter < DataWriter
    properties (Constant)
        winsorizeThreshold = 7.5
    end

    properties (Access=private)

        % Channel binning
        singleChannelBinData
        
        % Frequency binning
        byFrequencyBand
        frequenciesOfInterest
        singleFrequencyBinData

        trialLabels
        isValidation

    end

    methods
        function obj = NDDataWriter(opts)
            arguments 
                opts.ndData = []
                opts.dataType = "EEG" % {EEG,TF}
                opts.id = ""
                opts.eventName = ""
                opts.timeLabels = [] 
                opts.trialLabels = []
                opts.blockLabels = []
                opts.channelLabels = {}
                opts.outcomeLabels = []
                opts.byFrequencyBand = false % defines the frequency labels
            end

            obj.data =opts.ndData;
            obj.dataType = opts.dataType;
            obj.participantId = opts.id;
            obj.eventName = opts.eventName;
            obj.timeLabels = opts.timeLabels;
            obj.trialLabels= opts.trialLabels;
            obj.blockLabels = opts.blockLabels;
            obj.channelLabels = opts.channelLabels;
            obj.outcomeLabels = opts.outcomeLabels;
            obj.byFrequencyBand = opts.byFrequencyBand;

            obj.checkIfValidationMode();
        end

        function save(obj,opts)
            arguments
                obj
                opts.saveDir            = ""
                opts.saveMode           = "asParquet" %{asMat,asParquet,asCSV}
                opts.timeBinningMode    = "byTime" % {byTime,byTimepoints}
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
                    
                    switch obj.dataType
                        case "TF"
                            % obj.createTrialLabels(); 
                            obj.iterateFrequencyBins();
                        case "EEG"
                            obj.iterateTimeBins()
                    end
            end
            fprintf("%s saving complete.\n",obj.dataType);

        end
    end

    methods(Access=protected)
        
        function checkIfValidationMode(obj)
            % We want to have the same block numbers in each bin file for
            % all participants. So we need to consider that not all
            % blocks may be present in the data (through the
            % blockLabels).
            %  When doing validation it doesn't matter because the blocks
            %  are different between participants anyways, even if it's the same system.
            % To distinguish, validation has block numbers >=500
            obj.uniqueBlocks = unique(obj.blockLabels);
            obj.isValidation = obj.uniqueBlocks(1)>=500; % validation starts 

        end

        function createTrialLabels(obj)
            if ~isempty(obj.trialLabels); return; end
            [~,~,uniqueBlockId] = unique(obj.blockLabels(:),'stable');
           
            nTrials = numel(uniqueBlockId);

            [sortedBlock, sortIdx] = sort(uniqueBlockId);
            trialsPerBlock = accumarray(sortedBlock, 1);
            blockStartIdx = cumsum([0; trialsPerBlock]);    % length = #blocks+1
            
            % Within the sorted list, trial k belongs to block sortedBlocks(k),
            %    whose block starts at blockStartIdx(sortedBlock(k)).  So its 1…N index is:
            trialsSorted = (1:nTrials).' - blockStartIdx(sortedBlock);

            % Undo the sort to get back to original trial order
            obj.trialLabels = zeros(nTrials,1);
            obj.trialLabels(sortIdx) = trialsSorted;
        end

        function timeDatapoints = getTimeDatapoints(obj)
                    
            switch obj.dataType
                case "EEG"
                    dimension = 2;
                case "TF"
                    dimension= 4;
            end
            timeDatapoints = size(obj.data,dimension);

        end
        
        function getExpectedBlocks(obj)
            if obj.isValidation
                obj.expectedBlocks = obj.uniqueBlocks; 
            else
                % See obj.checkIfValidationMode()
                obj.expectedBlocks = MomentumParticipant.regularBlockLabels;
            end
        end
        
        function iterateFrequencyBins(obj)
            obj.frequenciesOfInterest = TimeFrequencyAnalyzer.getFrequenciesOfInterest(obj.byFrequencyBand);
            frequencyLabels = obj.getFrequencyLabels();
            numFreqs   = numel(frequencyLabels);

            for frequencyBinIdx = 1:numFreqs
                obj.frequencyBinLabel = frequencyLabels{frequencyBinIdx};
                obj.winsorizeAndZScale(frequencyBinIdx);
                obj.iterateTimeBins();
                fprintf("Saved %02d of %d frequency bins \n", ...
                                    frequencyBinIdx,numFreqs);
            end
        end
                
        function writeBinnedData(obj,timeBinIdx,section,blockBin)
    
            
            fileSaveName = MomentumParticipant.getFileName(participantId=obj.participantId, ...
                                            eventName = obj.eventName, ...
                                            section = obj.currentSection, ...
                                            freqLabel = obj.frequencyBinLabel, ...
                                            binningMode = obj.timeBinningMode, ...
                                            timeBinIdx = timeBinIdx, ...
                                            blockBinIdx = blockBin,...
                                            channelLabel=obj.currentChannelLabel,...
                                            extension = obj.saveExtension, ...
                                            dataType=obj.dataType);

            fileSavePath = fullfile(obj.saveDir,fileSaveName);

            if strcmp(obj.saveExtension,".csv")
                writetable(obj.channelBinnedData,fileSavePath);
                gzip(fileSavePath);            % compress it 
                delete(fileSavePath); 
            elseif strcmp(obj.saveExtension,".parquet")
                % obj.channelBinnedData = DataWriter.sortTableByCardinality(obj.channelBinnedData);
                parquetwrite(fileSavePath,obj.channelBinnedData);
            end
         end
        
        function getThisTimeBinData(obj,timeBinIdx)
            switch obj.dataType
                case "TF"
                    obj.slice4DtoTable(timeBinIdx);
                case "EEG"
                    obj.slice3DtoTable(timeBinIdx);
            end

            obj.fillBinnedTableColumns(timeBinIdx);
        end
                
        function labels = getFrequencyLabels(obj)
            % Return labels based on band setting
            if obj.byFrequencyBand
                labels = TimeFrequencyAnalyzer.frequencyBands(:,1); 
            else
                labels = num2cell(1:numel(obj.frequenciesOfInterest));
            end
        end

        function fillBinnedTableColumns(obj,timeBinIdx)
            %% Signal 
            obj.timeBinnedData = renamevars(obj.timeBinnedData, 'Value', 'signal');
            
            %% Time bins
            timepointsIdx = find(obj.timeToBinMapping == timeBinIdx);
            % obj.timeBinnedData.timeBin = timepointsIdx(obj.timeBinnedData.timeBin); % Continuous datapoint numbering across bins/files
            obj.timeBinnedData.timeBin = obj.timeLabels(timepointsIdx(obj.timeBinnedData.timeBin)).'; % Continuous datapoint value across bins/files

            %% Trial and block numbers
            % trial column is used to map block number and its trial number 
            % from the schedule file 
            obj.timeBinnedData.block    = categorical( obj.blockLabels( obj.timeBinnedData.trial ) );
            if ~isempty(obj.outcomeLabels)
                obj.timeBinnedData.outcome      = obj.outcomeLabels(obj.timeBinnedData.trial);
            end

            if ~isempty(obj.trialLabels)
                obj.timeBinnedData.trial      = obj.trialLabels   ( obj.timeBinnedData.trial );
            else
                obj.timeBinnedData.trial = [];
            end

            obj.timeBinnedData.id         = repmat( obj.participantId, height(obj.timeBinnedData), 1 );
            
            %% Channel and side + section
            obj. handleChannelColumn();

        end

        function handleChannelColumn(obj)
            % Split channel by side and section 
            splits = split(string(obj.timeBinnedData.channel), "_");

            obj.timeBinnedData.side       = splits(:,1); 
            if size(splits,2)>1
                obj.timeBinnedData.side       = splits(:,1); 
                obj.timeBinnedData.section    = splits(:,2); 
                obj.timeBinnedData.channel = [];

                % Reordering
                % obj.timeBinnedData = obj.timeBinnedData(:, {'block','trial','timeBin','signal','side','id','section','outcome'});
                desiredOrder = {'block','trial','timeBin','signal','side','id','section','outcome'};                
                existingColumnNames = intersect(desiredOrder, obj.timeBinnedData.Properties.VariableNames, 'stable');
                obj.timeBinnedData = obj.timeBinnedData(:, existingColumnNames);
            else
                % Biosemi has no side or section, so the channel column is kept
                obj.timeBinnedData.channel       = splits(:,1); 
                obj.timeBinnedData.section = repmat("", height(obj.timeBinnedData), 1);
                
                % Reordering
                obj.timeBinnedData = obj.timeBinnedData(:, {'block','trial','timeBin','signal','channel','id','section','outcome'});

            end
        end

        function slice3DtoTable(obj,binIdx)
            % Slicing the 3D data into 2D [channel x trials] 
            timepointsIdx = find(obj.timeToBinMapping == binIdx);
            obj.timeBinnedData = squeeze(obj.data(:,timepointsIdx,:));
            
            % Convert the 3D data [channel x timeBin x trials] to table
            dimnames = {"channel","timeBin","trial"}; % Column order of table sorted from right to left
            dimvalues = cell(numel(dimnames), 1);
            dimvalues{1} =  obj.channelLabels;
            obj.timeBinnedData = ndarray2table(obj.timeBinnedData, dimnames,dimvalues);

        end
        
        function slice4DtoTable(obj,timeBinIdx)
            timepointsIdx = find(obj.timeToBinMapping == timeBinIdx);
            obj.timeBinnedData = squeeze(obj.singleFrequencyBinData(:,:,timepointsIdx)); % Slice the time 

            % Convert the 3D data [trials x channels x timepoints] to table
            dimnames = {"trial","channel","timeBin"};
            dimvalues = cell(numel(dimnames), 1);
            dimvalues{2} = obj.channelLabels;
            obj.timeBinnedData = ndarray2table(obj.timeBinnedData, dimnames,dimvalues);
        end

        function winsorizeAndZScale(obj,frequencyBinIdx)
            % [trials x channels x timepoints]        
            obj.singleFrequencyBinData = squeeze(obj.data(:,:,frequencyBinIdx,:));

            [trials, channels, timepoints] = size(obj.singleFrequencyBinData);
        
            % 
            for channelIdx = 1:channels
                % Extract slice
                obj.singleChannelBinData = squeeze(obj.singleFrequencyBinData(:, channelIdx, :)); % [trials x timepoints]
                
                % Winsorize robustly
                obj.robustWinsorizeChannelBin();
                obj.zscoreChannelData();
                
                % Store back
                obj.singleFrequencyBinData(:, channelIdx, :) = reshape(obj.singleChannelBinData, [trials, 1, 1, timepoints]);

            end
            % Clear memory
            obj.singleChannelBinData= [];
        end
        
        function robustWinsorizeChannelBin(obj)
            % Robust winsorizing using median and MAD            
            medX = median(obj.singleChannelBinData,'all','omitnan');
            madX = mad(obj.singleChannelBinData,1,'all'); 
            
            if madX == 0
                madX = eps; % Avoid division by zero
            end
            
            % Robust z-score
            obj.singleChannelBinData = (obj.singleChannelBinData - medX) / madX;
            
            % Winsorize z-scores
            obj.singleChannelBinData(obj.singleChannelBinData > NDDataWriter.winsorizeThreshold) = NDDataWriter.winsorizeThreshold;
            obj.singleChannelBinData(obj.singleChannelBinData < -NDDataWriter.winsorizeThreshold) = -NDDataWriter.winsorizeThreshold;            
        end
        
        function zscoreChannelData(obj)
             
            % Compute global mean & std, omitting NaNs
            mu    = mean(obj.singleChannelBinData(:),    'omitnan');
            sigma = std( obj.singleChannelBinData(:),0,'omitnan');
            % Apply normalization
            obj.singleChannelBinData = (obj.singleChannelBinData - mu) ./ sigma;
                
        end

    end

end
