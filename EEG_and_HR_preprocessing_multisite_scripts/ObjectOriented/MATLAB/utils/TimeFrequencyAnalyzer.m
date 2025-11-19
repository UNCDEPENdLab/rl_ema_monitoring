classdef TimeFrequencyAnalyzer < handle
    properties (Constant)
        discreteFrequenciesOfInterest = [1:0.5:10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 40];
        continuousFrequenciesOfInterest = [0.5:0.5:30]
        notchFrequency = 60 %Hz
        frequencyBands = {'delta', [0.5, 3];
                  'theta', [3 7];
                  'alpha', [8 12];
                  'beta',  [13 30]};
        TFmethod = 'mtmconvol';
        byFrequencyBand = false; % When set to true it delivers results by band ignoring the discrete FOI 
    end

    properties (Access = public)
        TF
        EEG
        EEG_3D
        timeLabels
        participantId
        channelLabels
        epochLabels
        trialLabels
        outcomeLabels
        runConfiguration
        TFAnalysisComplete = false
        totalTrialsToAnalyze
        indicesTrialsWithData
        windowOfInterest
        hasInducedTF = false
        
    end

    properties (Access = private)
        meanOfRestingEpochs = false;
        samplingRate
        frequenciesOfInterest
        eventName
        ERP_Block 
        ERP_BlockLabels = []
        TotalPower_Block
        InducedPower_Block = []
        originalTimeLabels
    end
    
    methods
        function obj = TimeFrequencyAnalyzer(EEG)
            obj.validateData(EEG);
            obj.frequenciesOfInterest = TimeFrequencyAnalyzer.getFrequenciesOfInterest(TimeFrequencyAnalyzer.byFrequencyBand);
        end
        
        function getTF(obj)
            if ~obj.TFAnalysisComplete
                obj.prepareData();
                obj.prepareRunConfiguration();
                obj.runFrequencyAnalysis();
                obj.fillNaNTrials();
                obj.TFAnalysisComplete = true;
            end
        end

        function saveTF(obj,opts)
            arguments
                obj
                opts.saveDir            = ""
                opts.saveMode           = "asParquet" %{asMat,asParquet,asCSV}
                opts.timeBinningMode    = "byTime" % {byTime,byTimepoints}
                opts.blocksPerBin       = 0
                opts.binByChannel       = false
                opts.tPerBin            = 0 % Defines the timeBinning. In ms if opts.timeBinningMode== "byTime", else in datapoints
            end

            dataWriter = NDDataWriter(dataType = "TF",...
                                        ndData = obj.TF, ...
                                        id=obj.participantId,...
                                        eventName = obj.eventName,...
                                        timeLabels=obj.timeLabels,...
                                        blockLabels=obj.epochLabels,...
                                        channelLabels=obj.channelLabels,...
                                        byFrequencyBand=TimeFrequencyAnalyzer.byFrequencyBand,...
                                        trialLabels=obj.trialLabels, ...
                                        outcomeLabels=obj.outcomeLabels);
            nv = Utils.packStructAsNameValuePairs(opts);
            dataWriter.save(nv{:});
        end

        function saveInduced(obj,opts)
            arguments
                obj
                opts.saveDir            = ""
                opts.saveMode           = "asParquet" %{asMat,asParquet,asCSV}
                opts.timeBinningMode    = "byTime" % {byTime,byTimepoints}
                opts.blocksPerBin       = 0
                opts.binByChannel       = false
                opts.tPerBin            = 0 % Defines the timeBinning. In ms if opts.timeBinningMode== "byTime", else in datapoints
            end

            if ~obj.hasInducedTF 
                return
            end

            dataWriter = NDDataWriter(dataType = "TF",...
                                        ndData = obj.InducedPower_Block, ...
                                        id=obj.participantId,...
                                        eventName = obj.eventName,...
                                        timeLabels=obj.timeLabels,...
                                        blockLabels=obj.ERP_BlockLabels,...
                                        channelLabels=obj.channelLabels,...
                                        byFrequencyBand=TimeFrequencyAnalyzer.byFrequencyBand,...
                                        trialLabels=[],...              %zeros(size(obj.ERP_BlockLabels)), ...       % Because they're the averages    
                                        outcomeLabels=[]);                              % -2*ones(size(obj.ERP_BlockLabels)));      % Filling only
            nv = Utils.packStructAsNameValuePairs(opts);
            dataWriter.save(nv{:});
        end

        function getInducedTF(obj,mode)
            if islogical(mode) && ~mode 
                return
            end
                
            obj.computeBlockERP(mode);
            obj.EEG_3D = []; % Clear memory

            % Using obj.ERP_Block send to TF( overwrite on obj.ERP_Block)
            obj.computeEvokedPowerBlock();
            
            obj.trimTF([],"ERP_Block");

            % Get obj.TotalPower_Block
            obj.getTotalPowerBlock();
            % obj.TF = []; % Clear memory

            % Subtract obj.ERP_Block from TotalPower_Block to get InducedPower_Block
            obj.InducedPower_Block = obj.TotalPower_Block - obj.ERP_Block;
            obj.TotalPower_Block = []; % Clear memory
            obj.ERP_Block = [];

            if ~isempty(obj.InducedPower_Block)
                obj.hasInducedTF = true;
            end

            % Save InducedPower_Block
            % obj.saveInduced();

        end

        function referenceEEG(obj, mode)
        
            if islogical(mode) && ~mode
                return
            end
                
            %% Carry out the DTW
            aligner = ErpDtwAligner(obj.EEG, obj.samplingRate, obj.timeLabels, obj.epochLabels, 1);

            aligner.regionOfInterestWindowMs =[obj.timeLabels(1),obj.timeLabels(end)];% [400 700];  
            aligner.maximumWarpMs            = 40;         % (default ±40 ms warping band)
            aligner.doSecondPassIteration    = true;      

            [warpedEegByBlock, erpEnhancedByBlock, blockIds] = aligner.process();
            EEG = warpedEegByBlock;
            %%
            % EEG = obj.EEG;
            obj.validateEEGDimensions();
        
            [G,~,groupLabelFcn] = obj.getBlockGrouping(mode);

            nGroups = max(G);
            if nGroups == 0
                warning('No groups found — nothing to do.');
                return
            end
        
            MIN_TRIALS = 2;
        
            for gi = 1:nGroups
                idx = (G == gi);
                if ~any(idx)
                    continue
                end
                nGi = nnz(idx);
                if nGi < MIN_TRIALS
                    warning('Group %s has only %d trial(s); subtracting its own mean will nearly zero them. Consider merging or skipping.', groupLabelFcn(gi), nGi);
                end
        
                % Compute ERP template: 1 x nCh x nTime (mean over trials in group)
                tpl = mean(obj.EEG(idx, :, :), 1, 'omitnan'); 

                %% Plot to check DTW result
                %indTrial=1;indChannel=1; 
                %trialsInBlock = find(idx);
                %figure;
                %plot(obj.timeLabels,squeeze(obj.EEG(trialsInBlock(indTrial),indChannel,:)),'Color', [0 0 0 0.3],'DisplayName','OriginalEEG');
                %hold on;
                %title("MUSE - Left temporal Channel",'FontSize',15);
                %subtitle(sprintf("Id:%s - Block %i, Trial %i",obj.participantId,gi,indTrial));
                %plot(obj.timeLabels,squeeze(warpedEegByBlock(trialsInBlock(indTrial),indChannel,:)),'Color', [0 0 0 1],'DisplayName','Warped Signal');
                %plot(obj.timeLabels,squeeze(tpl(1,indChannel,:)),'Color', [0.298 0.447 0.69 1],"LineWidth",3,'DisplayName','Original Block ERP');
                %plot(obj.timeLabels,squeeze(erpEnhancedByBlock(gi,indChannel,:)),'DisplayName','NewMeanERP');
                %xlabel("Time [s]",'FontSize',14);
                %ylabel("EEG",'FontSize',14);
                %leg = legend('show');   
                %set(leg, 'Box', 'off');   
                %set(leg, 'FontSize', 14);    
                %box off;
                %disp("DTW done")
                %%
                % Subtract template from every trial in the group
                % EEG(idx, :, :) = EEG(idx, :, :) - tpl;
                obj.EEG(idx, :, :) = obj.EEG(idx, :, :) - erpEnhancedByBlock(gi,:,:);
                % obj.EEG(idx, :, :) = EEG(idx, :, :) - erpEnhancedByBlock(gi,:,:);
            end
        
            % obj.EEG = EEG;
        
        end

        function trimTF(obj, TFWindow, targetField)

            % -------- default targetField --------
            if nargin < 3 || isempty(targetField)
                targetField = 'TF';
            end
            if ~ischar(targetField) && ~isstring(targetField)
                error('targetField must be a string like ''TF'' or ''ERP_Block''.');
            end
            targetField = char(targetField);
        
            % -------- resolve / validate TFWindow --------
            % If TFWindow is empty but we have a stored windowOfInterest, reuse it.
            % If neither exists, do nothing.
            if (isempty(TFWindow) || all(isnan(TFWindow))) && isempty(obj.windowOfInterest)
                % nothing to do
                return
            elseif isempty(TFWindow) || all(isnan(TFWindow))
                TFWindow = obj.windowOfInterest;
            end
        
            if ~isnumeric(TFWindow) || numel(TFWindow) ~= 2 || any(~isfinite(TFWindow))
                error('TFWindow must be a 1x2 finite numeric vector in seconds.');
            end
        
            % sort/record window in seconds
            TFWindow = sort(TFWindow(:).');   % enforce ascending [tStart tEnd]
            obj.windowOfInterest = TFWindow;
        
            % convert to ms for comparison with time axes (which are ms)
            msWindow = TFWindow * 1000;
        
            % -------- basic time label checks --------
            if ~isprop(obj,'timeLabels') || ~isnumeric(obj.timeLabels) || isempty(obj.timeLabels)
                error('obj.timeLabels must exist and be a non-empty numeric vector (ms).');
            end
        
            % Make sure we have a backup of the original full timeline.
            % (If this is the first call, store it now.)
            if (~isprop(obj,'originalTimeLabels')) || isempty(obj.originalTimeLabels)
                obj.originalTimeLabels = obj.timeLabels;
            end
        
            % Sanity check on originalTimeLabels
            if ~isnumeric(obj.originalTimeLabels) || isempty(obj.originalTimeLabels)
                error('obj.originalTimeLabels must be a non-empty numeric vector (ms).');
            end
        
            % -------- get the requested data field --------
            if ~isprop(obj, targetField)
                error('Object does not have property "%s".', targetField);
            end
        
            data = obj.(targetField);
            if isempty(data)
                warning('%s is empty; nothing to trim.', targetField);
                return
            end
        
            % assume LAST dim is time
            nd      = ndims(data);
            timeDim = nd;
            nTimeInData = size(data, timeDim);
        
            % -------- choose which time axis to trust for trimming --------
            % start with current obj.timeLabels
            timeAxis = obj.timeLabels(:);  % column
        
            if nTimeInData ~= numel(timeAxis)
                % mismatch -> fall back to originalTimeLabels
                timeAxis = obj.originalTimeLabels(:);
        
                if nTimeInData ~= numel(timeAxis)
                    error(['Size mismatch: size(obj.%s,%d) = %d but neither obj.timeLabels (%d) ' ...
                           'nor obj.originalTimeLabels (%d) match.'], ...
                           targetField, timeDim, nTimeInData, ...
                           numel(obj.timeLabels), numel(obj.originalTimeLabels));
                end
            end
        
            % we'll work with a row vector for logical masking
            timeAxisRow = timeAxis(:).';
        
            % -------- build keep mask using selected timeAxis --------
            keepByWindow = (timeAxisRow >= msWindow(1)) & (timeAxisRow <= msWindow(2));
        
            if ~any(keepByWindow)
                warning('trimTF:EmptyWindow', ...
                    ['No samples within [%.3f, %.3f] s (ms: [%.1f, %.1f]). ', ...
                     'Nothing trimmed.'], ...
                     TFWindow(1), TFWindow(2), msWindow(1), msWindow(2));
                return
            end
        
            % -------- index into last dim with keepByWindow --------
            idx = repmat({':'}, 1, nd);
            idx{timeDim} = keepByWindow;
        
            dataTrimmed = data(idx{:});
        
            % overwrite the field
            obj.(targetField) = dataTrimmed;
        
            % update the public timeLabels to the trimmed axis
            obj.timeLabels = timeAxis(keepByWindow);
        
            % -------- status message --------
            fprintf('trimTF -> %s: kept %d/%d samples in [%.3f %.3f] s. Final nTime = %d.\n', ...
                targetField, nnz(keepByWindow), numel(keepByWindow), ...
                TFWindow(1), TFWindow(2), numel(obj.timeLabels));
        
        end

    end
    
    methods (Access=private)
                
        function runFrequencyAnalysis(obj)
            
            % Run frequency analysis
            pow = ft_freqanalysis(obj.runConfiguration, obj.EEG);
            
            obj.extractPowerSpectrum(pow);

            % Clear memory
            % obj.EEG = [];

        end

        function arrangeDataByTrial(obj)
            
            % Convert permuted data into a cell array; each cell corresponds to one event/trial
            obj.EEG = arrayfun(@(i) squeeze(obj.EEG(i, :, :)), ...
                                     1:size(obj.EEG, 1), 'UniformOutput', false);
            
            % Remove time points with NaN values from each trial
            trials = cellfun(@obj.cleanChannelData, obj.EEG, 'UniformOutput', false);
            obj.EEG = struct();
            obj.EEG.trial = trials;
        end
        
        function applyFiltering(obj)
            
            % Apply a 60 Hz notch filter to each trial
            filteredData.trial = cellfun(@(x) obj.vectorizedFilter(x), ...
                                   obj.EEG, 'UniformOutput', false);
            obj.EEG = filteredData;
        end

        function prepareData(obj)

            obj.arrangeDataByTrial();
            
            % Construct time arrays for each epoch
            obj.EEG.time = cellfun(@(x) obj.getEpochTime(x), ...
                                  obj.EEG.trial, 'UniformOutput', false);
            
            % Additional FieldTrip data configuration
            obj.EEG.fsample    = obj.samplingRate;
            obj.EEG.sampleinfo = [];
        
            % Define channel labels
            obj.EEG.label = obj.channelLabels;
            
            % Identify non-empty trials
            obj.totalTrialsToAnalyze = length(obj.EEG.trial);
            obj.indicesTrialsWithData = cellfun(@(x) size(x,2)>0, obj.EEG.trial);
        
            % Save original trial count and prepare cleaned data
            obj.EEG.trial = obj.EEG.trial(obj.indicesTrialsWithData);
            obj.EEG.time = obj.EEG.time(obj.indicesTrialsWithData);
        end

        function prepareRunConfiguration(obj)

            % Configure settings for time-frequency analysis using FieldTrip functions
            obj.runConfiguration              = [];
            obj.runConfiguration.output       = 'pow';
            obj.runConfiguration.method       = TimeFrequencyAnalyzer.TFmethod;
            obj.runConfiguration.taper        = 'hanning';
            obj.runConfiguration.foi          = obj.frequenciesOfInterest;
            obj.runConfiguration.t_ftimwin    = TimeFrequencyAnalyzer.getTFLengthOfTimeWindow(obj.runConfiguration.foi);
            obj.runConfiguration.keeptrials   = 'yes';         % Retain individual trial data
            
            obj.runConfiguration.toi          = 'all';
            obj.runConfiguration.pad          =  'nextpow2';
            
        end

        function fillNaNTrials(obj)
            [~, nchan, nfreq, ntime] = size(obj.TF);
        
            % Create full TF array with NaNs for empty trials
            nanFilledTF = NaN(obj.totalTrialsToAnalyze, nchan, nfreq, ntime);
            nanFilledTF(obj.indicesTrialsWithData,:,:,:) = obj.TF;
            obj.TF = nanFilledTF;
        end

        function validateData(obj,EEG)
            % Placeholder for more types of data
            obj.EEG = permute(EEG.EEGLabObject.data, [3, 1, 2]);% Permute EEG.data to have dimensions [events x channels x time]
            obj.EEG_3D = obj.EEG;
            obj.timeLabels = EEG.EEGLabObject.times; 
            obj.originalTimeLabels = obj.timeLabels;
            obj.participantId = EEG.EEGLabObject.subject;
            obj.samplingRate = EEG.EEGLabObject.srate;
            obj.channelLabels = {EEG.EEGLabObject.chanlocs.labels};
            obj.eventName = EEG.eventName;
            obj.epochLabels = EEG.EEGLabObject.etc.blockLabels; % Formerly in: vertcat(EEG.EEGLabObject.event.block); obj.EEGLabObject.etc.epochLabels
            obj.trialLabels = EEG.EEGLabObject.etc.trialLabels;
            obj.outcomeLabels = EEG.EEGLabObject.etc.outcomeLabels;
            % if isfield(EEG.EEGLabObject.etc,"epochLabels")
            %     obj.epochLabels = EEG.EEGLabObject.etc.epochLabels;
            % end

        end
                        
        function extractPowerSpectrum(obj,pow)
            % normalize by frequency
            fmat    = reshape(pow.freq, [1,1,numel(pow.freq),1]);
            pow.powspctrm = pow.powspctrm .* fmat; 

            if TimeFrequencyAnalyzer.byFrequencyBand
                nBands   = size(obj.frequencyBands,1);
                [nTrials,nChans,~,nTimes] = size(pow.powspctrm);
                obj.TF = zeros(nTrials, nChans, nBands, nTimes);

                for bandIndex = 1:size(obj.frequencyBands,1)
                    bandLimits = obj.frequencyBands{bandIndex,2};
                    frequencyBandMask = pow.freq >= bandLimits(1) & pow.freq <= bandLimits(2);
                    frequencyBandData = pow.powspctrm(:,:, frequencyBandMask, :);
                  obj.TF(:,:,bandIndex,:) = squeeze(nanmean(frequencyBandData, 3) );
                end
                pow= [];

            else
                % Get TF data and trial dimensions
                obj.TF = pow.powspctrm; % dimensions: trials x channels x frequencies x times
            end
        end
        
        function timeArray = getEpochTime(obj,trialData)
        
            Nsamples = size(trialData, 2);
            timeArray = (0:(Nsamples-1)) / obj.samplingRate;
        end

        function filteredData = vectorizedFilter(obj,trialData)
            
            % Use FieldTrip's DFT filter for notch filtering at 60 Hz
            filteredData = ft_preproc_dftfilter(trialData, obj.samplingRate, obj.notchFrequency);
        end
        
        function getTimesOfInterest(obj)
            dt                   = 1/obj.samplingRate;
            obj.timesOfInterest  = obj.roiWindow(1) : dt : obj.roiWindow(2);
        end

        function validateEEGDimensions(obj)
            if ndims(obj.EEG) ~= 3
                error('EEG must be a 3-D array [trials x channels x time].');
            end
            [nTrials, ~, ~] = size(obj.EEG);
        
            mustHave = {'trialLabels','epochLabels','outcomeLabels'};
            for k = 1:numel(mustHave)
                if ~isprop(obj, mustHave{k})
                    error('Object is missing required property "%s".', mustHave{k});
                end
                if numel(obj.(mustHave{k})) ~= nTrials
                    error('Property "%s" must have one entry per trial (%d).', mustHave{k}, nTrials);
                end
            end
        
        end

        function [G,blockLabels, groupLabelFcn] = getBlockGrouping(obj,mode)
            blocks = obj.epochLabels(:);     
        
            switch mode
                case 'block'
                    [G,blockLabels] = findgroups(double(blocks));    % group id per trial
                    groupLabelFcn = @(g) sprintf('block=%d', g);
                case 'block_condition'
                    conds    = obj.outcomeLabels(:); 
                    [G,blockLabels] = findgroups(double(blocks), double(conds));
                    groupLabelFcn = @(g) sprintf('block×cond group #%d', g);
                otherwise
                    error('Unknown mode "%s".', mode);
            end
        
        end

        function computeBlockERP(obj,mode)
            % EEG: [nTrials x nCh x nTime]
            % obj.ERP_Block: [nGroups x nCh x nTime]
            % obj.validateEEGDimensions();
        
            [groupingByBlock,obj.ERP_BlockLabels,~] = obj.getBlockGrouping(mode);

            [nTrials, nCh, nTime] = size(obj.EEG_3D);
        
            % 1. Collapse channels x time into one long feature dimension
            obj.ERP_Block = reshape(obj.EEG_3D, nTrials, nCh * nTime);  % [nTrials x (nCh*nTime)]
        
            % 2. Grouped mean across trials for each block
            %    splitapply will give one row per unique block in G
            obj.ERP_Block= splitapply(@(x) mean(x, 1, 'omitnan'), obj.ERP_Block, groupingByBlock);
            % ERP_Block is [nGroups x (nCh*nTime)]
        
            % 3. Reshape back to [blocks x nCh x nTime]
            obj.ERP_Block = reshape(obj.ERP_Block, [], nCh, nTime);

        end

        function getTotalPowerBlock(obj)
        
            % ----- sanity checks -----
            if ~isprop(obj, 'TF')
                error('Object is missing required property "TF".');
            end
            if isempty(obj.TF)
                error('obj.TF is empty; compute TF per trial before calling getTotalPowerBlock.');
            end
            if ndims(obj.TF) ~= 4
                error('obj.TF must be a 4-D array [trials x channels x freqs x timepoints].');
            end
        
            if ~isprop(obj, 'epochLabels')
                error('Object is missing required property "epochLabels".');
            end
            if isempty(obj.epochLabels)
                error('obj.epochLabels is empty; cannot group trials into blocks.');
            end
        
            [nTrials, nCh, nFreq, nTime] = size(obj.TF);
        
            % Make sure epochLabels matches nTrials
            if numel(obj.epochLabels) ~= nTrials
                error('epochLabels must have one entry per trial (%d).', nTrials);
            end
        
            % ----- make grouping per trial -----
            % G is a group index 1..nBlocks for each trial.
            % blockIDs are the corresponding unique block labels.
            [G, blockIDs] = findgroups(double(obj.epochLabels(:)));
        
            nBlocks = max(G);
            if nBlocks == 0
                warning('No groups found in epochLabels. Returning empty.');
                obj.TotalPower_Block = [];
                return
            end
        
            % ----- reshape trials -> 2D for splitapply -----
            % Collapse [nCh x nFreq x nTime] into one long feature dimension
            obj.TotalPower_Block = reshape(obj.TF, nTrials, nCh * nFreq * nTime);
            % TF2D is now [nTrials x (nCh*nFreq*nTime)]
        
            % ----- grouped mean across trials in each block -----
            % splitapply applies the function separately to each group in G
            obj.TotalPower_Block = splitapply(@(x) mean(x, 1, 'omitnan'), obj.TotalPower_Block, G);
            % TotalPower_Block is [nBlocks x (nCh*nFreq*nTime)]
        
            % ----- reshape back to [nBlocks x nCh x nFreq x nTime] -----
            obj.TotalPower_Block = reshape(obj.TotalPower_Block, nBlocks, nCh, nFreq, nTime);
        
        end

        function computeEvokedPowerBlock(obj)
       
            % -----------------
            % Sanity checks
            % -----------------
            if ~isprop(obj, 'ERP_Block') || isempty(obj.ERP_Block)
                error('ERP_Block is missing or empty. You must compute ERP_Block first.');
            end
        
            if ndims(obj.ERP_Block) ~= 3
                error('ERP_Block must be [nBlocks x nCh x nTime].');
            end
        
            [nBlocks, nCh, ~] = size(obj.ERP_Block);
        
            if ~isprop(obj, 'samplingRate') || isempty(obj.samplingRate)
                error('samplingRate must be defined on obj before computing EvokedPower_Block.');
            end
        
            if ~isprop(obj, 'channelLabels') || isempty(obj.channelLabels)
                error('channelLabels must be defined on obj before computing EvokedPower_Block.');
            end
        
            obj.prepareRunConfiguration();
        
            ERPdata = struct();
            ERPdata.trial = cell(nBlocks,1);
            ERPdata.time  = cell(nBlocks,1);
        
            for b = 1:nBlocks
                thisWave = squeeze(obj.ERP_Block(b,:,:));   % [nCh x nTime]
                % remove timepoints where ANY channel is NaN
                thisWaveNoNaN = obj.cleanChannelData(thisWave);
        
                ERPdata.trial{b} = thisWaveNoNaN;                           % [nCh x nTimeClean]
                ERPdata.time{b}  = obj.getEpochTime(thisWaveNoNaN);         % [1 x nTimeClean]
            end
        
            ERPdata.fsample    = obj.samplingRate;
            ERPdata.sampleinfo = [];
            ERPdata.label      = obj.channelLabels;   % {nCh x 1} cell
        
            pow = ft_freqanalysis(obj.runConfiguration, ERPdata);
  
            % Frequency normalization step:
            fmat = reshape(pow.freq, [1,1,numel(pow.freq),1]);
            pow.powspctrm = pow.powspctrm .* fmat;
        
            if TimeFrequencyAnalyzer.byFrequencyBand
                % Collapse frequencies into predefined bands
                nBands = size(obj.frequencyBands,1);
                [nBlocksCheck,nChCheck,~,nTimesTF] = size(pow.powspctrm);
        
                if nBlocksCheck ~= nBlocks || nChCheck ~= nCh
                    error('Unexpected dimension mismatch after ft_freqanalysis.');
                end
        
                EvokedPower_Block = zeros(nBlocks, nCh, nBands, nTimesTF);
        
                for bandIndex = 1:nBands
                    bandLimits = obj.frequencyBands{bandIndex,2};
                    freqMask   = pow.freq >= bandLimits(1) & pow.freq <= bandLimits(2);
        
                    bandData = pow.powspctrm(:,:, freqMask, :); % [nBlocks x nCh x nFreqInBand x nTimesTF]
                    EvokedPower_Block(:,:,bandIndex,:) = squeeze(nanmean(bandData, 3));
                end
        
            else
                % Keep all frequencies
                EvokedPower_Block = pow.powspctrm;  % [nBlocks x nCh x nFreq x nTimeTF]
            end
    
            % obj.EvokedPower_Block = EvokedPower_Block;
            obj.ERP_Block = EvokedPower_Block;

            % Also nice to keep track of freq/time axes for downstream alignment
            % obj.EvokedPower_freq = pow.freq;
            % obj.EvokedPower_time = pow.time;
        end

    end 

    methods (Static)

        function frequenciesOfInterest = getFrequenciesOfInterest(byFrequencyBand)
            if byFrequencyBand
                frequenciesOfInterest = TimeFrequencyAnalyzer.continuousFrequenciesOfInterest;
            else
                frequenciesOfInterest = TimeFrequencyAnalyzer.discreteFrequenciesOfInterest;
            end
        end
        
        function t_ftimwin = getTFLengthOfTimeWindow(frequenciesOfInterest)
            % Time window length per frequency (in seconds)
            t_ftimwin = 3 ./ frequenciesOfInterest;     
        end

        function trialDataWithoutNaNs = cleanChannelData(trialData)
            % Expects dimensions: channels x time 
            % Removes the whole timepoint if any of the channels has
            % invalid/NaN data because the analysis fails without this step
            nanIndices = any(isnan(trialData), 1);
            trialDataWithoutNaNs = trialData(:, ~nanIndices);
        end
        
        function margin = computeTFMargin(cfg)
            % Returns the required side margin (seconds) for TF analysis
            switch lower(cfg.method)
                case 'mtmconvol'
                    % Largest temporal window governs edge safety
                    margin = 0.5 * max(cfg.t_ftimwin);
                case 'wavelet'
                    % Requires cfg.width (cycles)
                    assert(isfield(cfg, 'width') && ~isempty(cfg.width), ...
                        'For wavelet, cfg.width must be set.');
                    margin = (cfg.width/2) / min(cfg.foi);
                otherwise
                    margin = 0;
            end
        end

        function margin = getMarginFromRunParameters()
                cfg = [];
                cfg.method = TimeFrequencyAnalyzer.TFmethod;
                byFrequencyBand = TimeFrequencyAnalyzer.byFrequencyBand;
                frequenciesOfInterest = TimeFrequencyAnalyzer.getFrequenciesOfInterest(byFrequencyBand);
                cfg.t_ftimwin = TimeFrequencyAnalyzer.getTFLengthOfTimeWindow(frequenciesOfInterest);
                margin = TimeFrequencyAnalyzer.computeTFMargin(cfg);
        end

    end
end