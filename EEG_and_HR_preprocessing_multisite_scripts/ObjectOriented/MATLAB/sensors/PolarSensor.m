classdef PolarSensor < MomentumSensor
    properties (Abstract, Constant)
        resamplingPeriod
        bufferTime
        dataColumn
    end

    properties (Access=protected)
        dataIsEpoched = false
        dataIsPreprocessed = false
        eventName
        data
        preEventWindow
        postEventWindow
        timeVector
    end

    methods (Access=public)

        function epochToTable(obj,eventTable,eventName,windowToEpoch)
            if obj.dataIsEpoched; return; end

            obj.eventName = eventName;

            [obj.preEventWindow, obj.postEventWindow] = PolarSensor.parseWindow(windowToEpoch);
            obj.timeVector = PolarSensor.makeTimeVectorFromWindow(obj.resamplingPeriod,obj.preEventWindow,obj.postEventWindow);
            obj.epochToAllEvents(eventTable.(obj.eventName));
            obj.buildEpochedTable(eventTable);
            obj.dataIsEpoched = true;
        end

        function save(obj,opts)
            arguments 
                obj
                opts.id = ""
                opts.saveDir = saveDir
                opts.saveMode = "asParquet"
                opts.timeBinningMode = "byTime"
                opts.blocksPerBin = 0
            end

            participantId = opts.id;
            opts = rmfield(opts, 'id');
            obj.data.id = repmat( participantId, height(obj.data), 1 );
            
            dataWriter = DataWriter(data        = obj.data, ...
                                    dataType    = obj.dataColumn, ...
                                    id          = participantId,...
                                    eventName   = obj.eventName);
            
            nv = Utils.packStructAsNameValuePairs(opts);
            dataWriter.save(nv{:});
        

        end

    end

    methods (Access=private)

        function epochToAllEvents(obj,eventTimestamps)

            nbTimepoints = numel(obj.timeVector);
            nbEvents = height(eventTimestamps);
            epochedData = NaN(nbEvents, nbTimepoints);
            disp("Epoching data...")
            for eventIdx = 1:nbEvents
                epochedData(eventIdx,:) = obj.epochSingleEvent(eventTimestamps(eventIdx));  
                Utils.updateProgress(500,eventIdx,nbEvents,"Epoched", "events");

            end

            % Flatten [M events x N timepoints] into a single column
            %       [E1t1,...,E1tn,...,Emtn]'
            obj.data = reshape(epochedData.',[],1);
        end

        function eventSegment= epochSingleEvent(obj,eventTimestamp)
            % Look for a window slightly bigger than what's needed to
            % better interpolate. 
            epochStartTimestamp = eventTimestamp - obj.preEventWindow;
            epochEndTimestamp = eventTimestamp + obj.postEventWindow;

            eventTimeIndices = obj.data.Timestamp >= epochStartTimestamp - obj.bufferTime & obj.data.Timestamp <= epochEndTimestamp + obj.bufferTime;
            eventSegment = obj.data(eventTimeIndices, :);
            newSamplingTimes = (epochStartTimestamp : obj.resamplingPeriod : epochEndTimestamp)';

            if isempty(eventSegment)
                eventSegment = NaN(1,numel(newSamplingTimes));
                return; 
            end
        
            eventSegment = table2timetable(eventSegment, 'RowTimes','Timestamp');
            eventSegment = retime(eventSegment, newSamplingTimes, 'spline');
            eventSegment = eventSegment.(obj.dataColumn); % Drop the timestamps
        end

        function buildEpochedTable(obj,eventTable)

            nbTimepoints = numel(obj.timeVector);
            nbEvents = height(eventTable.(obj.eventName));

            obj.data = table(categorical(repelem(eventTable.block,nbTimepoints)), ...
                                repelem(eventTable.trial,nbTimepoints), ...
                                repmat(1e3*seconds(obj.timeVector)',nbEvents,1), ...
                                obj.data, ...
                                'VariableNames', ...
                                {'block','trial','timeBin','signal'});
        end

    end
    
    methods (Static)
        
        function sessionData = readDatabase(filepath,tableName)
            % Read data from database
            db = sqlite(filepath);
            % Older versions of matlab import as cell array without column
            % names
            if isMATLABReleaseOlderThan("R2023a")
                dataCell = fetch(db,sprintf('SELECT * FROM %s',tableName));
                colNames   = fetch(db, sprintf('SELECT name FROM pragma_table_info(''%s'')',tableName ));
                varNames    = colNames(:)';                     
                sessionData = cell2table(dataCell, ...
                            'VariableNames', varNames);
            else
                sessionData = fetch(db,sprintf('SELECT * FROM %s',tableName));
            end
            
            db.close();
        end

        function Timestamps = processTimestamps(raw_data, optimized_alignment, fs, datapoints_per_timestamp)

            % Calculate interpolated intervals between data points
            interpolated_interval = PolarSensor.getInterpolatedIntervals(datapoints_per_timestamp, fs);
        
            % Process sensor timestamps (in nanoseconds)
            T_from_ns = 1e-9 * (double(raw_data.polar_timestamp) - double(raw_data.polar_timestamp(1)));
        
            if optimized_alignment
                % Align using optimized method
                start_timestamp = double(raw_data.time_ms(1));
                T_from_ms = (double(raw_data.time_ms) - start_timestamp) * 1e-3;
        
                % Optimize alignment
                [tau_opt, ~] = MomentumSensor.optimizeAlignment(T_from_ms, T_from_ns);
        
                % Adjust timestamps
                T_base = seconds(T_from_ms + tau_opt) + Utils.convertPhoneTimestampsToDatetime(start_timestamp);
            else
                % Use first phone timestamp as reference
                raw_data.time_ms_timestamp = Utils.convertPhoneTimestampsToDatetime(double(raw_data.time_ms));
                raw_data.polar_timedate = PolarSensor.convertPolarTimestampsToDatetime(raw_data.polar_timestamp);
                
                T_base = raw_data.time_ms_timestamp(1) + (raw_data.polar_timedate - raw_data.polar_timedate(1));
                % T_base = raw_data.time_ms_timestamp; % Using less resolution overlaps the timestamps 
            end
        
            % Expand timestamps to match each data point
            Timestamps = repmat(T_base, 1, datapoints_per_timestamp) + seconds(interpolated_interval);
            Timestamps = reshape(Timestamps', [], 1);
        end

        function [interpolated_interval] = getInterpolatedIntervals(datapointsPerTimestamp,fs)
            currentTs = 0;
            previousTs = -(datapointsPerTimestamp-1)./fs;% datapoints_per_timestamp-1 because we'll use the currentTs
            interpolated_interval = linspace(previousTs, currentTs, datapointsPerTimestamp); % Extrapolate from current timestamp back a number of datapoints 
        end

        function concatenated = processSessions(sessionData, gapThreshold, processorFunc, verbose, varargin)

            % 1) Pre-clean: remove duplicates and sort
            cleanData = PolarSensor.cleanAndSort(sessionData);
            
            % 2) Split into continuous subsessions
            subsessions = PolarSensor.splitSubsessions(cleanData, gapThreshold);

            % 3) Process each subsession with provided function
            processed = PolarSensor.processSubsessions(...
                subsessions, processorFunc, verbose, varargin{:});

            % 4) Merge all processed results
            concatenated = PolarSensor.mergeSubsessions(processed);
        end

        function cleanData = cleanAndSort(sessionData)
            % Remove duplicates and sort by timestamp(s)
            % Remove repeated rows (excluding the first timestamp column)
            varNames = sessionData.Properties.VariableNames;
            dataCols = varNames(2:end);
            [~, idx] = unique(sessionData(:, dataCols), 'rows', 'stable');
            if numel(idx) < height(sessionData)
                warning('Found repeated records; removing duplicates.');
                sessionData = sessionData(idx, :);
            end

            % Sort by primary timestamp and, if present, polar_timestamp
            if ismember('polar_timestamp', varNames)
                cleanData = sortrows(sessionData, {'time_ms','polar_timestamp'});
            else
                cleanData = sortrows(sessionData, varNames{1});
            end
        end
    
        function subsessions = splitSubsessions(cleanData, gapThreshold)
            % Split table at time gaps longer than threshold
            %   cleanData: sorted table with timestamp in first var
            %   gapThreshold: seconds to split on

            % Convert first column to datetime array
            timeCol = cleanData{:,1};
            if isnumeric(timeCol)
                timestamps = Utils.convertPhoneTimestampsToDatetime(timeCol);
            elseif isdatetime(timeCol)
                timestamps = timeCol;
            else
                error('Timestamp column must be numeric POSIX or datetime');
            end

            % Compute intervals between successive timestamps
            diffs = [seconds(diff(timestamps)); 0];

            % Identify split points
            splitIdx = find(diffs > gapThreshold);

            % Build subsessions cell array of tables
            subsessions = {};
            startIdx = 1;
            for i = 1:numel(splitIdx)
                endIdx = splitIdx(i);
                subsessions{end+1} = cleanData(startIdx:endIdx, :); 
                startIdx = endIdx + 1;
            end
            % Add final chunk
            if startIdx <= height(cleanData)
                subsessions{end+1} = cleanData(startIdx:end, :);
            end
        end

        function processed = processSubsessions(subsessions, processorFunc, verbose, varargin)
            %processSubsessions Apply processor to each subsession
            disp("Processing Subsessions...")
            nbSubsessions = numel(subsessions);
            processed = cell(1, nbSubsessions);

            for subsessionIdx = 1:nbSubsessions
                try
                    processed{subsessionIdx} = processorFunc(subsessions{subsessionIdx}, varargin{:});

                catch ex
                    warning('Subsession %d failed: %s', subsessionIdx, ex.message);
                    processed{subsessionIdx} = [];  % or handle differently
                end
                if verbose
                    Utils.updateProgress(10,subsessionIdx,nbSubsessions,"Processed", "subsessions");
                end

            end
            disp("All subsessions processed.")
        end
    
        function merged = mergeSubsessions(processed)
            % Vertically concatenate processed outputs
            % Remove empty cells (failed processing)
            valid = ~cellfun(@isempty, processed);
            merged = vertcat(processed{valid});
        end
    
        function polarDateTime = convertPolarTimestampsToDatetime(polarTimestamps)
            polarDateTime = datetime(double(946684800000000000+polarTimestamps)*1e-9,...
                                                    'ConvertFrom', 'posixtime',...
                                                    'Format', 'dd-MMM-yyyy HH:mm:ss.SSSSSSSSS',...
                                                    'TimeZone', 'America/New_York');
        end
        
        function [preEventWindow, postEventWindow] = parseWindow(windowToEpoch)
            preEventWindow  = seconds(abs(windowToEpoch(1)));
            postEventWindow = seconds(abs(windowToEpoch(2)));
        end

        function timeVector = makeTimeVectorFromWindow(resamplingPeriod,preWindow, postWindow)
            % From preWindow to +postWindow at the resampling period
            timeVector = (-preWindow : resamplingPeriod : postWindow);
        end

    end

end
