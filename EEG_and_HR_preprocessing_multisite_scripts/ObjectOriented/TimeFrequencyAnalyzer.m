classdef TimeFrequencyAnalyzer < handle
    properties (Constant)
        discreteFrequenciesOfInterest = [1:0.5:10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 40];
        continuousFrequenciesOfInterest = [0.5:0.5:30]
        notchFrequency = 60 %Hz
        frequencyBands = {'delta', [0.5, 3];
                  'theta', [3 7];
                  'alpha', [8 12];
                  'beta',  [13 30]};
    end

    properties (Access = public)
        TF
        EEG
        timeLabels
        participantId
        channelLabels
        epochLabels
        trialLabels
        runConfiguration
        TFAnalysisComplete = false
        totalTrialsToAnalyze
        indicesTrialsWithData

    end

    properties (Access = private)
        meanOfRestingEpochs = false;
        samplingRate
        byFrequencyBand
        frequenciesOfInterest
        eventName

    end
    
    methods
        function obj = TimeFrequencyAnalyzer(EEG)
            obj.validateData(EEG);
        end
        
        function getTF(obj,byFrequencyBand)
            if nargin<2; byFrequencyBand=false; end
            if ~obj.TFAnalysisComplete
                obj.byFrequencyBand = byFrequencyBand;
                obj.prepareData();
                obj.prepareRunConfiguration();
                obj.runFrequencyAnalysis();
                obj.fillNaNTrials();
                obj.TFAnalysisComplete = true;
            end
        end
        
        function runFrequencyAnalysis(obj)
            
            % Run frequency analysis
            pow = ft_freqanalysis(obj.runConfiguration, obj.EEG);
            
            obj.extractPowerSpectrum(pow);

            % Clear memory
            obj.EEG = [];

        end

        function saveTF(obj,opts)
            arguments
                obj
                opts.saveDir = ""
                opts.saveMode = "asParquet" %{asMat,asParquet,asCSV}
                opts.timeBinningMode = "byTime" % {byTime,byTimepoints}
                opts.sessionsPerBin = 0
                opts.binByChannel = false
            end

            dataWriter = NDDataWriter(dataType = "TF",...
                                        ndData = obj.TF, ...
                                        id=obj.participantId,...
                                        eventName = obj.eventName,...
                                        timeLabels=obj.timeLabels,...
                                        sessionLabels=obj.epochLabels,...
                                        channelLabels=obj.channelLabels,...
                                        byFrequencyBand=obj.byFrequencyBand,...
                                        trialLabels=obj.trialLabels);
            nv = Utils.packStructAsNameValuePairs(opts);
            dataWriter.save(nv{:});
        end
           
    end
    
    methods (Access=private)
        
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

            obj.frequenciesOfInterest = TimeFrequencyAnalyzer.getFrequenciesOfInterest(obj.byFrequencyBand);
            
            % Configure settings for time-frequency analysis using FieldTrip functions
            obj.runConfiguration              = [];
            obj.runConfiguration.output       = 'pow';
            obj.runConfiguration.method       = 'mtmconvol';
            obj.runConfiguration.taper        = 'hanning';
            obj.runConfiguration.foi          = obj.frequenciesOfInterest;
            obj.runConfiguration.t_ftimwin    = 3 ./ obj.runConfiguration.foi;  % Time window length per frequency (in seconds)
            obj.runConfiguration.toi          = 'all';         % Use all available time points for analysis
            obj.runConfiguration.keeptrials   = 'yes';         % Retain individual trial data
            obj.runConfiguration.pad          = 'nextpow2';    % Zero-pad to next power of 2 for efficient FFT
            
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
            obj.timeLabels = EEG.EEGLabObject.times; 
            obj.participantId = EEG.EEGLabObject.subject;
            obj.samplingRate = EEG.EEGLabObject.srate;
            obj.channelLabels = {EEG.EEGLabObject.chanlocs.labels};
            obj.eventName = EEG.eventName;
            obj.epochLabels = vertcat(EEG.EEGLabObject.event.session);% Formerly in: obj.EEGLabObject.etc.epochLabels
            obj.trialLabels = EEG.EEGLabObject.etc.trialLabels;
            % if isfield(EEG.EEGLabObject.etc,"epochLabels")
            %     obj.epochLabels = EEG.EEGLabObject.etc.epochLabels;
            % end

        end
                        
        function extractPowerSpectrum(obj,pow)
            % normalize by frequency
            fmat    = reshape(pow.freq, [1,1,numel(pow.freq),1]);
            pow.powspctrm = pow.powspctrm .* fmat; 

            if obj.byFrequencyBand
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
        
    end 

    methods (Static)

        function frequenciesOfInterest = getFrequenciesOfInterest(byFrequencyBand)
            if byFrequencyBand
                frequenciesOfInterest = TimeFrequencyAnalyzer.continuousFrequenciesOfInterest;
            else
                frequenciesOfInterest = TimeFrequencyAnalyzer.discreteFrequenciesOfInterest;
            end
        end

        function trialDataWithoutNaNs = cleanChannelData(trialData)
            % Expects dimensions: channels x time 
            % Removes the whole timepoint if any of the channels has
            % invalid/NaN data because the analysis fails without this step
            nanIndices = any(isnan(trialData), 1);
            trialDataWithoutNaNs = trialData(:, ~nanIndices);
        end
    
    end
end