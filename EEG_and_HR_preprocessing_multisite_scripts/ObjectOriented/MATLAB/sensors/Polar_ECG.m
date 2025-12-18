classdef Polar_ECG < PolarSensor
    properties (Constant)
        dataType = "Polar_ECG"
        dataColumn = "ECG"
        fs = 130 %Hz
        optimizedAlignment = true
        resamplingPeriod = seconds(1/130) % For precisely centering the event 
        bufferTime = seconds(1) % To capture previous ECG datapoints before resampling
    end

    properties (Access=private)
        databasePath
    end
    
    methods
        function obj = Polar_ECG(databasePath)
            obj.databasePath = databasePath;
        end

        function preprocessData(obj)
            verbose = true;
            if ~obj.dataIsPreprocessed
    
                sessionData = PolarSensor.readDatabase(obj.databasePath,obj.dataType);

                obj.data = PolarSensor.processSessions(...
                                    sessionData, ...            % Data
                                    10, ...                      % split on gaps > X s
                                    @Polar_ECG.getECG, ...      % process the ECG
                                    verbose,...                 % show waitbar
                                    obj.optimizedAlignment...   % With optimizedAlignment or not
                                );
                
                obj.dataIsPreprocessed = true;
            end
        end
        
    end
    
    methods (Static)

        function dataTable = extractAndConcatenateDataTable(processed_session, fieldName)

            tableList = cell(size(processed_session));  % Preallocate a cell array for tables
           
            
            % Remove the empty values
            nonEmptyIndex = ~cellfun(@isempty, processed_session);
            processed_session = processed_session(nonEmptyIndex);    % Filter out empty cells
        
            % Merge into a new table
            for i = 1:length(processed_session)
                tableList{i} = processed_session{i}.(fieldName);
            end
        
            % Concatenating all extracted tables into one and assigning to the appropriate field
            dataTable = vertcat(tableList{:});
        end

        function ECG = getECG(rawData, optimized_alignment)
                    
            % Exit early if no data is present
            if isempty(rawData) || height(rawData) == 0
                ECG.ecg_data = table([], [], 'VariableNames', {'Timestamp', 'ECG'});
                return;
            end
        
            % Preprocess ECG data
            [ecg, datapoints_per_timestamp] = Polar_ECG.expandStringsToNumeric(rawData);
        
            % Process timestamps
            Timestamps = PolarSensor.processTimestamps(rawData, optimized_alignment, Polar_ECG.fs, datapoints_per_timestamp);
            
            if ~issorted(Timestamps)
                disp("Found non-sorted Timestamps in getECG")
                [Timestamps, sortIdx] = sort(Timestamps);  
                ecg = ecg(sortIdx);
            end

            % Filter ECG data
            ecg = Polar_ECG.filterECG(ecg,Polar_ECG.fs);
        
            % Organize ECG data into a table
            ECG = table(Timestamps, ecg, 'VariableNames', {'Timestamp', 'ECG'});
        end 

        function filteredECG = filterECG(ecgData, fs)

            f0 = 60;             % line frequency (Hz)
            Q  = 35;             % quality factor 
            
            % normalized notch frequency
            wo = f0/(fs/2);
            bw = wo/Q;
        
            % design a second-order notch filter
            [b, a] = iirnotch(wo, bw);
        
            % apply zero-phase filtering to avoid phase distortion
            filteredECG = filtfilt(b, a, ecgData);
        end

        function [ECG_data_raw, datapoints_per_timestamp] = expandStringsToNumeric(raw_data)
            % Converts ECG data from strings to numerical arrays and flattens it.
            %
            % Parameters:
            %   raw_data - [Table] Raw data containing ECG in string format.
            %
            % Returns:
            %   ECG_data_raw - [Vector] Flattened raw ECG data.
            %   datapoints_per_timestamp - [Integer] Number of data points per timestamp.
        
            % Convert ECG data from strings to numerical arrays
            raw_data.ECG = cellfun(@(x) str2num(strrep(strrep(x, '[', ''), ']', '')), ...
                                   raw_data.ECG, 'UniformOutput', false);
        
            % Determine data points per timestamp (assuming constant packet size)
            datapoints_per_timestamp = numel(raw_data.ECG{1});
        
            % Flatten ECG data into a single column vector
            ECG_data_raw = cell2mat(raw_data.ECG')';
        end

    end
end