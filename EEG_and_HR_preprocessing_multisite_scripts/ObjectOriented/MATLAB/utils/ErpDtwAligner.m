classdef ErpDtwAligner
  
    properties
        eegData                            % [trials x channels x time]
        samplingRateHz                     % scalar
        timeVectorMs                       % [1 x time]
        blockLabels                        % [trials x 1] block or block×cond ID
        alignmentChannel                   % scalar channel index

        regionOfInterestWindowMs = [200 400];  % ms window to align 
        maximumWarpMs            = 40;         % ± allowed ms deviation in DTW
        doSecondPassIteration    = false;      % run iterative refinement
        lpB;
        lpA;

    end

    methods
        function obj = ErpDtwAligner(eegData, samplingRateHz, timeVectorMs, blockLabels, alignmentChannel)
            obj.eegData          = eegData;
            obj.samplingRateHz   = samplingRateHz;
            obj.timeVectorMs     = timeVectorMs;
            obj.blockLabels      = blockLabels(:); % ensure column
            obj.alignmentChannel = alignmentChannel;
            [ obj.lpB, obj.lpA ] = butter(4, 20/(samplingRateHz/2));

        end

        function [warpedEegByBlock, erpEnhancedByBlock, blockIds] = process(obj)
            
            uniqueBlocks = unique(obj.blockLabels, 'stable');
            numberOfBlocks = numel(uniqueBlocks);

            warpedEegByBlock    = cell(numberOfBlocks,1);
            erpEnhancedByBlock  = cell(numberOfBlocks,1);
            blockIds            = uniqueBlocks;

            for b = 1:numberOfBlocks
                currentBlockId = uniqueBlocks(b);
                trialsInBlock  = find(obj.blockLabels == currentBlockId);

                eegBlockCurrent = obj.eegData(trialsInBlock, :, :);

                [currentErpEnhanced, warpedTrialsAligned] = obj.buildEnhancedErpWithDtwBlock( ...
                    eegBlockCurrent, ...
                    obj.timeVectorMs, ...
                    obj.samplingRateHz, ...
                    obj.alignmentChannel, ...
                    obj.regionOfInterestWindowMs, ...
                    obj.maximumWarpMs, ...
                    obj.doSecondPassIteration);

                warpedEegByBlock{b}   = warpedTrialsAligned;
                erpEnhancedByBlock{b} = currentErpEnhanced;
                Utils.updateProgress(10,b,numberOfBlocks,"DTW blocks ", "completed")
            end

            warpedEegByBlock=vertcat(warpedEegByBlock{:});

            erpEnhancedByBlock = cat(3, erpEnhancedByBlock{:});
            erpEnhancedByBlock = permute(erpEnhancedByBlock, [3 1 2]);
        end
    end

    methods (Access = private)

        function [erpEnhanced, warpedTrialsAligned] = buildEnhancedErpWithDtwBlock( ...
                obj, eegBlockCurrent, timeVectorMs, samplingRateHz, alignmentChannel, ...
                regionOfInterestWindowMs, maximumWarpMs, doSecondPassIteration)
           

            numberOfTrials     = size(eegBlockCurrent, 1);
            numberOfChannels   = size(eegBlockCurrent, 2);
            numberOfTimepoints = size(eegBlockCurrent, 3);

            % ----- Pass 1 reference ERP from alignment channel -----
            referenceErpFirstPass = obj.computeInitialReferenceErp(eegBlockCurrent, alignmentChannel);

            warpedTrialsAligned = nan(numberOfTrials, numberOfChannels, numberOfTimepoints);

            for trialIndex = 1:numberOfTrials
                thisTrialAllChannels = squeeze(eegBlockCurrent(trialIndex,:,:)); % [channels x time]

                warpedTrialsAligned(trialIndex,:,:) = obj.warpTrialSegmentWithDtw( ...
                    thisTrialAllChannels, ...
                    referenceErpFirstPass, ...
                    timeVectorMs, ...
                    samplingRateHz, ...
                    regionOfInterestWindowMs, ...
                    maximumWarpMs, ...
                    alignmentChannel);
            end

            erpEnhanced = squeeze(mean(warpedTrialsAligned, 1)); % [channels x time]

            % ----- Optional second-pass refinement  -----
            if doSecondPassIteration
                refinedReferenceErp = erpEnhanced(alignmentChannel, :); % use first-pass enhanced ERP from align channel
                warpedTrialsAlignedSecondPass = nan(size(warpedTrialsAligned));

                for trialIndex = 1:numberOfTrials
                    thisTrialAllChannels = squeeze(eegBlockCurrent(trialIndex,:,:));

                    warpedTrialsAlignedSecondPass(trialIndex,:,:) = obj.warpTrialSegmentWithDtw( ...
                        thisTrialAllChannels, ...
                        refinedReferenceErp, ...
                        timeVectorMs, ...
                        samplingRateHz, ...
                        regionOfInterestWindowMs, ...
                        maximumWarpMs, ...
                        alignmentChannel);
                end

                warpedTrialsAligned = warpedTrialsAlignedSecondPass;
                erpEnhanced         = squeeze(mean(warpedTrialsAlignedSecondPass, 1));
            end
        end


        function referenceErp = computeInitialReferenceErp(~, eegBlockCurrent, alignmentChannel)
          
            referenceErp = squeeze(mean(eegBlockCurrent(:, alignmentChannel, :), 1)); % [time x 1]
            referenceErp = referenceErp(:)'; % row vector
        end

        function warpedTrialAllChannels = warpTrialSegmentWithDtw( ...
            obj, trialDataAllChannels, referenceErp, timeVectorMs, samplingRateHz, ...
            regionOfInterestWindowMs, maximumWarpMs, alignmentChannel)
        
            if nargin < 5 || isempty(regionOfInterestWindowMs)
                regionOfInterestWindowMs = [200 400];
            end
            if nargin < 6 || isempty(maximumWarpMs)
                maximumWarpMs = 40;
            end
        
            [numberOfChannels, numberOfTimepoints] = size(trialDataAllChannels);
        
            % Get ROI indices
            regionOfInterestIdx = find(timeVectorMs >= regionOfInterestWindowMs(1) & ...
                                       timeVectorMs <= regionOfInterestWindowMs(2));
        
            if numel(regionOfInterestIdx) < 5
                % ROI is too short to align, just return unwarped
                warpedTrialAllChannels = trialDataAllChannels;
                return
            end
        
            % Extract ROI segments
            referenceSegment           = referenceErp(regionOfInterestIdx); % [1 x roiLen]
            trialSegmentForAlignment   = trialDataAllChannels(alignmentChannel, regionOfInterestIdx); % [1 x roiLen]
        
            % Check NaN / Inf content
            badRefMask   = ~isfinite(referenceSegment);
            badTrialMask = ~isfinite(trialSegmentForAlignment);
        
            fracBadRef   = mean(badRefMask);
            fracBadTrial = mean(badTrialMask);
        
            % If too many bad samples in either, skip DTW for this trial
            maxAllowedBadFraction = 0.30; % 30% threshold – tunable
            if fracBadRef > maxAllowedBadFraction || fracBadTrial > maxAllowedBadFraction
                warpedTrialAllChannels = trialDataAllChannels;
                return
            end
        
            % Otherwise, interpolate small gaps to get rid of NaNs/Infs before dtw
            if any(badRefMask)
                goodPosRef   = find(~badRefMask);
                badPosRef    = find(badRefMask);
                referenceSegment(badPosRef) = interp1( ...
                    goodPosRef, referenceSegment(goodPosRef), badPosRef, ...
                    'linear','extrap');
            end
            if any(badTrialMask)
                goodPosTrial   = find(~badTrialMask);
                badPosTrial    = find(badTrialMask);
                trialSegmentForAlignment(badPosTrial) = interp1( ...
                    goodPosTrial, trialSegmentForAlignment(goodPosTrial), badPosTrial, ...
                    'linear','extrap');
            end
        
            % Safety check: if either segment is now constant/flat (zero variance),
            % DTW path is meaningless; just don't warp.
            if std(referenceSegment) == 0 || std(trialSegmentForAlignment) == 0
                warpedTrialAllChannels = trialDataAllChannels;
                return
            end
        
            % Convert ms -> samples constraint for Sakoe–Chiba-type band
            maximumWarpSamples = round((maximumWarpMs/1000) * samplingRateHz);
        
            % Constrained DTW to get warping path
            [~, referenceIdxPath, trialIdxPath] = dtw(referenceSegment, ...
                                                      trialSegmentForAlignment, ...
                                                      maximumWarpSamples);
        
            % Start output as copy of trial
            warpedTrialAllChannels = trialDataAllChannels;
        
            roiLength = numel(regionOfInterestIdx);
        
            % === Vectorized warp across channels ===
            % Grab all channels' data in ROI
            allChannelsSegment = trialDataAllChannels(:, regionOfInterestIdx);          % [channels x roiLen]
            warpedSegments     = nan(numberOfChannels, roiLength, 'like', allChannelsSegment);
        
            % Scatter map using the shared DTW path
            warpedSegments(:, referenceIdxPath) = allChannelsSegment(:, trialIdxPath);
        
            % Interpolate gaps along time for each channel row
            warpedSegments = fillmissing(warpedSegments.', 'linear', 'EndValues','extrap').';  % now no NaNs
        
            % Low-pass filter rows to remove staircase artifacts.
            % Using filtfilt with precomputed filter coeffs (obj.lpB/obj.lpA)
            % so it's zero-phase and doesn't shift latency.
            if isprop(obj,'lpB') && isprop(obj,'lpA') && ~isempty(obj.lpB)
                warpedSegments = filtfilt(obj.lpB, obj.lpA, double(warpedSegments.')).'; % filter each row
                % cast back to original numeric type
                warpedSegments = cast(warpedSegments, 'like', trialDataAllChannels);
            else
                % fallback: simple lowpass if filter not precomputed
                for channelIndex = 1:numberOfChannels
                    warpedSegments(channelIndex,:) = lowpass( ...
                        warpedSegments(channelIndex,:), 20, samplingRateHz);
                end
            end
        
            % Put warped ROI back
            warpedTrialAllChannels(:, regionOfInterestIdx) = warpedSegments;
        end
    end

    methods (Static)
        function smoothedSignal = lowpassSmoothSignalStatic(inputSignal, samplingRateHz, cutoffHz)
         
            if nargin < 3 || isempty(cutoffHz)
                cutoffHz = 20;
            end

            smoothedSignal = lowpass(inputSignal, cutoffHz, samplingRateHz);
        end
    end
end
