classdef Polar_accelerometer <MomentumSensor
    properties (Constant)
        dataType = "Polar_accelerometer"
        fs = 200;
    end

    properties (Access=private)
        databasePath
        dataIsPreprocessed = false
    end

    properties
        rawData
    end

    methods
        function obj = Polar_accelerometer(databasePath)
            obj.databasePath = databasePath;
        end

        function preprocessData(obj)
            verbose = true;
            if ~obj.dataIsPreprocessed
    
                sessionData = MomentumSensor.readDatabase(obj.databasePath,obj.dataType);

                obj.rawData = MomentumSensor.processSessions(...
                                    sessionData, ...        % Data
                                    3, ...                  % split on gaps > 3 s
                                    @Polar_accelerometer.parseStringToTable, ...  % Processing function
                                    verbose...              % show waitbar
                                );
                
                obj.dataIsPreprocessed = true;
            end
        end
    end
    
    methods (Static)

        function triples = parseXYZRow(rowStr)
            % Parses a single 'a b c|d e f|...' string into M×3 numeric array
            % Replace '|' separators with spaces
            clean = strrep(rowStr, '|', ' ');
            % Scan all numbers into a vector
            nums = sscanf(clean, '%f');
            % Ensure count is a multiple of 3
            if mod(numel(nums), 3) ~= 0
                error('Invalid XYZ data: expected triples but got %d values.', numel(nums));
            end
            % Reshape into M×3
            triples = reshape(nums, 3, [])';
        end

        function accTable = parseStringToTable(rawData)
            
            % Parse the XYZ data
            rowData = cellfun(@Polar_accelerometer.parseXYZRow, rawData.XYZ, 'UniformOutput', false);
            numericXYZ = cat(1, rowData{:});
            datapointsPerTimestamp=size(rowData{1},1); % Assuming all datapoints have the same length

            % Process the timestamps
            Timestamps = MomentumSensor.processTimestamps(rawData, false, Polar_accelerometer.fs, datapointsPerTimestamp);
            
            % Build into table
            accTable = array2table(numericXYZ,'VariableNames',{'x','y','z'});
            accTable.Timestamps = Timestamps;
            accTable = movevars(accTable, 'Timestamps', 'Before', 'x');

        end
    end

end