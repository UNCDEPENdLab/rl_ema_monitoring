classdef Polar_ECG < MomentumSensor
    properties (Constant)
        dataType = "Polar_ECG"
        fs=130
    end

    properties (Access=private)
        databasePath
        dataIsPreprocessed = false
    end

    properties
        rawData
        optimizedAlignment = true
    end
    
    methods
        function obj = Polar_ECG(databasePath)
            obj.databasePath = databasePath;
        end

        function preprocessData(obj)
            verbose = true;
            if ~obj.dataIsPreprocessed
    
                sessionData = MomentumSensor.readDatabase(obj.databasePath,obj.dataType);

                obj.rawData = MomentumSensor.processSessions(...
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
            % Extracts the tables from a struct and concatenates them into a single
            % table.
            %
            % Parameters:
            % processed_session - [Cell Array] 1xN contains the processed sessions.
            % source - [String] Determines whether to extract the ECG, RRI or RR
            %                       table from the struct {'ecg_data','rri_data','rr_data'}
            %
            % Returns:
            % dataTable - [Table] Mx2 Contains all the merged data from the
            %                   sessions
        
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
            Timestamps = MomentumSensor.processTimestamps(rawData, optimized_alignment, Polar_ECG.fs, datapoints_per_timestamp);
            
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
        % filterECG  Remove mains interference with a notch filter
        %
        %   filteredECG = filterECG(ecgData, fs) applies a zero-phase IIR notch
        %   filter to ecgData sampled at fs (Hz), attenuating the power-line
        %   frequency. 
        %
        %   Parameters:
        %     ecgData : vector of raw ECG samples
        %     fs      : sampling rate in Hz
        %
        %   Returns:
        %     filteredECG : ECG after notch filtering at f0
        
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

        function filteredECG = filterECG_obsolete(ecgData, fs)
            % Applies a bandpass FIR filter to ECG data to reduce noise and enhance signal quality.
            %
            % The filter is designed using a FIR (Finite Impulse Response) approach with a 
            % specified filter order and cutoff frequencies calculated from the Nyquist frequency.
            % The bandpass filter passes frequencies between 9 Hz and 50 Hz, which are typical
            % cutoff frequencies for ECG data processing.
            %
            % Parameters:
            % ecgData - [Array] Raw ECG data to be filtered.
            % fs - [Double] Sampling frequency of the ECG data in Hz.
            %
            % Returns:
            % filteredECG - [Array] ECG data after being processed by the bandpass filter.
            %
            Nyquist = fs / 2;                       % Calculate the Nyquist frequency
            lowFreq = 9 / Nyquist;                  % Normalize the low cutoff frequency
            highFreq = 50 / Nyquist;                % Normalize the high cutoff frequency
            
            filterOrder = 100;                      % Define the order of the FIR filter
            b = fir1(filterOrder, [lowFreq highFreq], 'bandpass'); % Design the FIR bandpass filter
            filteredECG = filtfilt(b, 1, ecgData);  % Apply the filter using zero-phase filtering
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