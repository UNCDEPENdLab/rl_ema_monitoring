classdef TimestampAligner <handle
    properties 
        id
        mainDataDir
        schedule
        sessionNb
        eventName
        
        feedbackEvents
        tau
        tauSaveName = []
        polyfitSaveName = []
        EEGEvents

    end
    
    properties (Access=private)
        biosemi
    end

    methods
        function obj = TimestampAligner(opts)
            arguments
                opts.id = ""
                opts.mainDataDir = ""
                opts.sessionNb = 0
                opts.schedule = []
                opts.eventName = "feedback_time"
                opts.biosemi = []
            end

            obj.id = opts.id;
            obj.mainDataDir = opts.mainDataDir;
            obj.sessionNb = opts.sessionNb;
            obj.schedule = opts.schedule;
            obj.eventName = opts.eventName;
            obj.biosemi = opts.biosemi;
        end
        
        function align(obj)
            obj.filterScheduleByBlock();
            obj.getScoredCameraFeedbackTimestamps();
            obj.alignPhoneToCameraTimestamps();
            obj.alignCameraWithTTL();
            obj.buildEEGevent();
            
        end

        function updateValidIndices(obj,omittedTrials)
            % Updates the validIndices array by setting to zero
            % the elements corresponding to omittedTrials after accounting for
            % the zeros (invalid indices) that were removed before processing.
            
            % Find the indices of valid (non-zero) entries
            indicesOfOnes = find(obj.feedbackEvents.validIndices == 1);
            
            % Map omittedTrials back to indices in the original validIndices array
            indicesToSetZero = indicesOfOnes(omittedTrials);
            
            obj.feedbackEvents.validIndices(indicesToSetZero) = 0;

        end
        
        function  blockNumber = getBlockNumber(obj)
            % Get the block number depending on the participant Id and the session
            % number 

            % Hardcoded values from MUSE_Counterbalancing.xlsx Accessed: 5Sep2024
            % if file says BioSemi use blocks: 500-503, if Muse then use blocks 504-507
            lookupTable = [[440484,500];
                            [440488,500];
                            [440885,504];
                            [440868,500];
                            [440883,504];
                            [440929,500];
                            [440931,504];
                            [221465,500];
                            [221595,504];
                            [440933,500];
                            [220521,504];
                            [440191,500];
                            [440923,504];
                            [440924,500];
                            [440847,504];
                            [440869,500];
                            [440865,500];
                            [440942,504];
                            [440812,500];
                            [440807,504];
                            [440906,504];
                            [440825,500];
                            [440833,504];
                            [440790,500];
                            [440813,504];
                            [440804,500];
                            [440133,504];
                            [440774,500];
                            [440932,504];
                            [440747,500];
                            [440871,504];
                            [440752,500];
                            [440731,504];
                            [440701,500];
                            [440754,504]];
            
            % Read the lookupTable
            mainBlock = lookupTable(find(str2double(obj.id) == lookupTable(:,1)),2);
            
            % Return either the correct Biosemi block depending on sessionNb or
            % return all muse blocks
            if obj.sessionNb==0
                % Return all muse blocks 
                if mainBlock == 500
                    blockNumber = 504:507;
                else
                    blockNumber = 500:503;
                end
            else    
                blockNumber = mainBlock + obj.sessionNb - 1;
            end
        end
    end

    methods (Access=private)

        function buildEEGevent(obj)
            % Build the EEG event struct for the EEG object
           

            % Add the type of feedback event
            % eventTypes = repmat({'No_Feedback'}, length(feedbackEvents.feedbackType), 1); 
            % eventTypes(feedbackEvents.feedbackType==1) = {'Feedback'}; 
            eventTypes = repmat({0}, length(obj.feedbackEvents.feedbackType), 1); 
            eventTypes(obj.feedbackEvents.feedbackType==1) = {1}; 
        
            % Keep track of the missing trials (marked with NaNs)
            nanTrials = isnan(obj.feedbackEvents.alignedBiosemiEvents);
            eventTypes(nanTrials) = {-1}; % Will get deleted but are still flagged 
            
            % Build the struct for the biosemi using the aligned events
            obj.EEGEvents = struct('type', eventTypes, ...
                              'latency', num2cell(obj.feedbackEvents.alignedBiosemiEvents), ...
                              'duration', num2cell(0*ones(size(obj.feedbackEvents.alignedBiosemiEvents))), ...
                              'trial',num2cell(obj.feedbackEvents.trialLabels), ...
                              'block',num2cell(obj.feedbackEvents.blockLabels));
        
        end

        function alignCameraWithTTL(obj)

            % Map the feedback events from the camera time to the biosemi scale
        
            % Recover the camera TTL pulse 
            cameraTTLPulse = obj.feedbackEvents.cameraTimestamps(80:80:end);
            
            % Convert the biosemi latencies to s
            biosemiLatencies_s = double( vertcat(obj.biosemi.event.latency))/obj.biosemi.srate;
            
            % Verify that sizes match and correct if there are repeated points
            if size(cameraTTLPulse, 1) ~= size(biosemiLatencies_s, 1)
               
                % Check and correct pulse continuity
                cameraTTLPulse = TimestampAligner.correctPulseContinuity(cameraTTLPulse);
                biosemiLatencies_s = TimestampAligner.correctPulseContinuity(biosemiLatencies_s);
                
        
                % Retry with a different slicing if sizes still don't match
                if size(cameraTTLPulse, 1) ~= size(biosemiLatencies_s, 1)
                    
                    [bestSlicingInd,bestDelay] = TimestampAligner.findBestSlicingAndDelay(obj.feedbackEvents.cameraTimestamps,biosemiLatencies_s);
                    
                    cameraTTLPulse = obj.feedbackEvents.cameraTimestamps(bestSlicingInd:bestSlicingInd:end);
                    cameraTTLLength = length(cameraTTLPulse);
                    biosemiTTLLength = length(biosemiLatencies_s);
        
                    % Determine which array is smaller and which is larger and
                    % correct it
                    if cameraTTLLength > biosemiTTLLength
                        cameraTTLPulse = cameraTTLPulse(bestDelay:biosemiTTLLength+bestDelay-1);
                    else
                        biosemiLatencies_s = biosemiLatencies_s(bestDelay:cameraTTLLength+bestDelay-1);
                    end
                end
            end
        
            % Check for best TTL pulses, remove outliers
            P = polyfit(1:length(cameraTTLPulse), cameraTTLPulse-biosemiLatencies_s,1);
            yfit = polyval(P,1:length(cameraTTLPulse));
            ts_corrected = cameraTTLPulse-biosemiLatencies_s-yfit';
            
            %% Method 1: Mapping with linear function
            obj.getBiosemiEventsByMappingLinearFunction(cameraTTLPulse,ts_corrected,biosemiLatencies_s);

            %% Method 2: 
            % obj.getBiosemiEventsByClosestTTLAndDifference(cameraTTLPulse,biosemiLatencies_s,biosemiSamplingRate, biosemiTimes)
           
            %% Method 3: Adding yfit's smallest residual
            % obj.getBiosemiEventsByAddingSmallestResidual(cameraTTLPulse,ts_corrected,biosemiLatencies_s,biosemiSamplingRate,biosemiTimes);

        end

        function filterScheduleByBlock(obj)
            targetBlock = obj.getBlockNumber();
            rowIndices = ismember(obj.schedule.block, targetBlock);        
            obj.schedule = obj.schedule(rowIndices, :);
        end
        
        function getBiosemiEventsByClosestTTLAndDifference(obj,cameraTTLPulse,biosemiLatencies_s,biosemiSamplingRate, biosemiTimes)
            %% Method 2: Finding closest TTL pulse and using its difference
            % Get the differences matrix between pulse and event times
            differencesTTLEvent = obj.feedbackEvents.alignedCameraTimestamps - cameraTTLPulse';

            % Get the indices of the values closest to zero
            [~,indClosest] = min(abs(differencesTTLEvent),[],2);

            % Get the difference of the closest TTL camera pulse to the biosemi
            tsDifference = cameraTTLPulse-biosemiLatencies_s;
            correction = tsDifference(indClosest);

            % Add the correction to the camera timestamps
            alignedBiosemiEventTimes = obj.feedbackEvents.alignedCameraTimestamps - correction;

            % Get the indices of the events
            bsSampleIndices = round(alignedBiosemiEventTimes * biosemiSamplingRate);

            % Ensure indices are within valid range
            biosemiFeedbackEvents = max(1,min(length(biosemiTimes),bsSampleIndices));
            obj.feedbackEvents.alignedBiosemiEvents=biosemiFeedbackEvents;
        end
        
        function getBiosemiEventsByMappingLinearFunction(obj,cameraTTLPulse,ts_corrected,biosemiLatencies_s)
            [~,outlierLogic] = TimestampAligner.removeOutliers(ts_corrected);
        
            % Fit a linear curve
            p = polyfit(cameraTTLPulse(~outlierLogic),biosemiLatencies_s(~outlierLogic),1);
            
            % Save if needed
            if ~isempty(obj.polyfitSaveName)
                save(obj.polyfitSaveName,'p');
            end
        
            % Get the feedback times in biosemi time scale
            alignedBiosemiEventTimes = polyval(p,obj.feedbackEvents.alignedCameraTimestamps);
        
            % Get the biosemi indices of the events
            biosemiFeedbackEvents = round(alignedBiosemiEventTimes * obj.biosemi.srate);
        
            % Collect the computed event times which don't fit in the timestamps
            % and mark them as NaNs
            biosemiFeedbackEvents(biosemiFeedbackEvents < 0) = NaN;  % Set NaN where indices are negative
            biosemiFeedbackEvents(biosemiFeedbackEvents > length( obj.biosemi.times'/1000)) = NaN;  % Set NaN where indices are outside the maximum
            obj.updateValidIndices(find(isnan(biosemiFeedbackEvents)));
            obj.feedbackEvents.alignedBiosemiEvents = biosemiFeedbackEvents;
        end
        
        function getBiosemiEventsByAddingSmallestResidual(obj,cameraTTLPulse,ts_corrected,biosemiLatencies_s,biosemiSamplingRate,biosemiTimes)
            % subtract off time between TTL pulse in BIOSEMI (any one will do) (corrected version) and
            % feedback events
            [val,TTL_ix_to_align] = min(abs(ts_corrected));
            TTL_to_align = biosemiLatencies_s(TTL_ix_to_align);
    
            deltat = ts_corrected(TTL_ix_to_align) - cameraTTLPulse(TTL_ix_to_align);
    
            for i = 1:length(obj.feedbackEvents.alignedCameraTimestamps)
                if obj.feedbackEvents.alignedCameraTimestamps(i) > deltat
                    biosemiFeedbackEvents(i) = obj.feedbackEvents.alignedCameraTimestamps(i) - deltat;
                elseif obj.feedbackEvents.alignedCameraTimestamps(i) < deltat
                    biosemiFeedbackEvents(i) = obj.feedbackEvents.alignedCameraTimestamps(i) + deltat;
                end
            end
            % Get the biosemi indices of the events
            bsSampleIndices = round(biosemiFeedbackEvents' * biosemiSamplingRate);
    
            % Ensure indices are within valid range
            obj.feedbackEvents.alignedBiosemiEvents = max(1, min(length(biosemiTimes), bsSampleIndices));
        end

        function feedbackFrames = getValidFeedbackFrames(obj)
            % Read manually scored csv file
            feedback_frames_path = obj.findFileByParticipantId('csv');
            feedbackFrames = readtable(feedback_frames_path,'VariableNamingRule','preserve').Feedback;
    
            % Correct for missing values
            validIndices = ~isnan(feedbackFrames);  % Find indices of valid (non-NaN) entries
            feedbackFrames = feedbackFrames(validIndices);
            obj.feedbackEvents.validIndices = validIndices; % Keep which were valid indices 

        end
        
        function updateFeedbackType(obj)
            obj.feedbackEvents.feedbackType = obj.schedule.feedback;
            obj.feedbackEvents.feedbackType = obj.feedbackEvents.feedbackType(obj.feedbackEvents.validIndices);
        end
        
        function getPhoneFeedbackTimes(obj)
         
            obj.feedbackEvents.phoneFeedbackTimes = obj.schedule.(obj.eventName); 
            obj.feedbackEvents.phoneFeedbackTimes = obj.feedbackEvents.phoneFeedbackTimes(obj.feedbackEvents.validIndices);
            obj.feedbackEvents.phoneFeedbackTimes = seconds(obj.feedbackEvents.phoneFeedbackTimes-obj.feedbackEvents.phoneFeedbackTimes(1));
    
        end
        
        function updateValidScheduleEvents(obj)
            obj.feedbackEvents.trialLabels = obj.schedule.trial(obj.feedbackEvents.validIndices);
            obj.feedbackEvents.blockLabels = obj.schedule.block(obj.feedbackEvents.validIndices);
        end

        function getScoredCameraFeedbackTimestamps(obj)

            feedbackFrames = obj.getValidFeedbackFrames();
            obj.updateValidScheduleEvents();
            obj.updateFeedbackType();
            obj.getPhoneFeedbackTimes();
            
            obj.processCameraTimestamps();
            obj.feedbackEvents.scoredFeedbackTimes =  obj.feedbackEvents.cameraTimestamps(feedbackFrames); 

            scoredEventsRelative = obj.feedbackEvents.scoredFeedbackTimes- obj.feedbackEvents.scoredFeedbackTimes(1); % Relative timestamps
   
            camera_timestamps_with_no_feedback = scoredEventsRelative(obj.feedbackEvents.feedbackType==0); % relative timestamps of scored frames where there was no feedback 
            phone_no_feedback_event_times = obj.feedbackEvents.phoneFeedbackTimes(obj.feedbackEvents.feedbackType==0); % Phone timestamps where there was no feedback
            diff_no_feedback = median(phone_no_feedback_event_times  - camera_timestamps_with_no_feedback); % Error of scoring no feedback trials
            obj.feedbackEvents.scoredFeedbackTimes(obj.feedbackEvents.feedbackType==0) = obj.feedbackEvents.scoredFeedbackTimes(obj.feedbackEvents.feedbackType==0)+diff_no_feedback; 
            obj.feedbackEvents.cameraFps = median(1./diff(obj.feedbackEvents.cameraTimestamps)); % Estimate the fps
            
        end

        function processCameraTimestamps(obj)
            % Read the camera timestamps and return a datetime object with a day
            % specified by startDate
            
            camera_timestamps_path = obj.findFileByParticipantId('mat');

            % The camera timestamps only have time of the day, so the startDate is
            % used that comes from the schedule file
            cameraTimestamps = load(camera_timestamps_path);
            cameraTimestamps = datetime(cellstr(cameraTimestamps.ts),'InputFormat','HH:mm:ss:SSS');
            % cameraTimestamps = dateshift(startDate, 'start', 'day') + timeofday(cameraTimestamps);]
            cameraTimestamps = seconds(cameraTimestamps-cameraTimestamps(1)); % Using relative time 
            
            obj.feedbackEvents.cameraTimestamps = cameraTimestamps;

        end

        function alignPhoneToCameraTimestamps(obj)
            % Corrects the manually scored feedback times to times which are closer to the phone timestamps.        
            % Get the minimal shift 
            relativeScoredFeedbackTimes = obj.feedbackEvents.scoredFeedbackTimes-obj.feedbackEvents.scoredFeedbackTimes(1);
            [tau_opt, ~] = MomentumSensor.optimizeAlignment(relativeScoredFeedbackTimes,obj.feedbackEvents.phoneFeedbackTimes);
            fprintf("maxTau: %.4f mean tau: %.4f \n",max(tau_opt),mean(tau_opt));
            % [tau_opt, ~] = MomentumSensor.optimizeAlignment(obj.feedbackEvents.scoredFeedbackTimes,obj.feedbackEvents.phoneFeedbackTimes);

            % Correct the timestamps
            alignedCameraTimestamps = obj.feedbackEvents.scoredFeedbackTimes+ tau_opt;

            %% If just getting the camera timestamps then obtain them here otherwise keep original values to avoid losing precision when aligning to a higher sampling frequency 
            % alignedCorrected = [];
            % for i=1:length(alignedCameraTimestamps)
            % 
            %     % Look for closest camera timestamp that matches the computed value
            %     % alignedCorrected(end+1)= findClosestValue(alignedCameraTimestamps(i), feedbackEvents.cameraTimestamps);
            % end
            % alignedCameraTimestamps = feedbackEvents.cameraTimestamps(alignedCorrected');
        
            %%
            % Feedback events relative to camera timestamps 
            obj.feedbackEvents.alignedCameraTimestamps = alignedCameraTimestamps;
            obj.tau = tau_opt;

            if ~isempty(obj.tauSaveName)
                save(obj.tauSaveName,'tau_opt');
            end
        
        end

        function fullPathToFile = findFileByParticipantId(obj, extension)
            % Function to locate a file based on participantId, extension, and optional modifier.

            directoryPath = fullfile(obj.mainDataDir,'mat_files_csv_and_MUSE_db',obj.id);
            fileName = obj.id+ "-"+ num2str(obj.sessionNb)+"."+extension;
            searchPath = fullfile(directoryPath, fileName);
            fileInfo = dir(searchPath);
            
            if ~isempty(fileInfo)
                fullPathToFile = fullfile(fileInfo.folder, fileInfo.name);
            else
                disp('No file found matching the criteria.');
                fullPathToFile = [];
            end
        end

        
        
    end
    
    methods(Static)

        function correctedPulse = correctPulseContinuity(originalPulse)
            % Remove the pulses that are too near to each other.
        
            % Calculate time differences
            timeDiffs = diff(originalPulse);
            
            % Find indices of pulses too close to the previous pulse
            consecutiveIndices = find(timeDiffs < 0.5*mean(timeDiffs)); % less than 50percent of the mean difference
            
            % Mark the indices of pulses to keep
            validIndices = true(size(originalPulse));
            validIndices(consecutiveIndices + 1) = false; % Removing the second pulse
            
            % Extract the corrected pulse
            correctedPulse = originalPulse(validIndices);
        
        end

        function [bestSlicingInd,bestMinInd] = findBestSlicingAndDelay(cameraTimestamps,biosemiLatencies_s)
            % Finds the best way to slice the camera TTL pulse array to match it to
            % the received biosemi array. It tries different slicing indices and
            % with the extracted signal finds the shift that best matches both
            % pulses.
         
            % Initialize arrays to store the results
            slicingInds = 75:85;
            numSlices = length(slicingInds);
            minVals = zeros(1, numSlices);
            minInds = zeros(1, numSlices);
            
            for idx = 1:numSlices
                slicingInd = slicingInds(idx);
                cameraTTLPulse = cameraTimestamps(slicingInd:slicingInd:end);
                [minVal, minInd] = TimestampAligner.findMinimalDelay(cameraTTLPulse, biosemiLatencies_s);
                
                % Accumulate the minVal and minInd
                minVals(idx) = minVal;
                minInds(idx) = minInd;
            end
            
            % Find the slicingInd with the smallest minVal
            [overallMinVal, minIdx] = min(minVals);
            bestSlicingInd = slicingInds(minIdx);
            bestMinInd = minInds(minIdx);
        end

        function [minVal, minInd] = findMinimalDelay(array1, array2)
            % Takes 2 arrays' differences and finds the shift the smaller array needs to fit 
            %  inside the longer array to minimize the euclidean distance. It's
            %  similar to a correlation where it finds the best delay one of the
            %  array needs to make the best match, except is not in absolute values
            %  but in the distance between consecutive datapoints.
            %
            % Parameters: 
            % array1 and array2 - [array] Arrays to be matched
            %
            % Returns:
            % minVal - [scalar] The value of the lowest norm found
            % minInd - [scalar] The position or delay to shift the small array
            %           forward to best match the larger array. Note: a value of 1 indicates
            %           no shift.
            
            % Ensure input arrays are column vectors
            array1 = array1(:);
            array2 = array2(:);
        
            % Determine which array is smaller and which is larger
            if length(array1) < length(array2)
                smallArray = array1;
                bigArray = array2;
            else
                smallArray = array2;
                bigArray = array1;
            end
        
            n_s = length(smallArray);
            n_b = length(bigArray);
            possibleDelays = n_b - n_s + 1;
        
            % Precompute differences
            diffSmall = diff(smallArray); % (n_s - 1) x 1
            diffBig = diff(bigArray);     % (n_b - 1) x 1
        
            % Generate indices for vectorized extraction
            indices = bsxfun(@plus, (1:n_s - 1)', 0:possibleDelays - 1);
        
            % Extract windows and compute differences
            D = diffBig(indices);         % (n_s - 1) x possibleDelays
            E = D - diffSmall;            % Broadcasting diffSmall across columns
        
            % Compute norms for each possible delay
            norms = sqrt(sum(E.^2, 1));   % 1 x possibleDelays
        
            % Find the minimum norm and its index
            [minVal, minInd] = min(norms);
        end

        function [cleanData, outlierLogic] = removeOutliers(data)
            % Removes outliers of the input data array
            %
            % Parameters:
            % data - [Array] Input data array
            % 
            % Returns:
            % cleanData - [Array] Contains the data without the outliers 
            % outlierLogic - [Array] Contains the locations where the outliers were
            %                   found
            
            % Remove NaN values
            nanIndices = isnan(data);
            dataNoNaN = data(~nanIndices);
        
            % Calculate the first and third quartiles
            Q1 = quantile(dataNoNaN, 0.25);
            Q3 = quantile(dataNoNaN, 0.75);
        
            % Compute the interquartile range (IQR)
            IQR = Q3 - Q1;
        
            % Define the outlier thresholds
            lowerThreshold = Q1 - 1.5 * IQR;
            upperThreshold = Q3 + 1.5 * IQR;
        
            % Find indices of outliers
            outlierLogic = (data < lowerThreshold) | (data > upperThreshold);
        
            % Remove outliers from data
            cleanData = data(~outlierLogic);
        end

    end

end