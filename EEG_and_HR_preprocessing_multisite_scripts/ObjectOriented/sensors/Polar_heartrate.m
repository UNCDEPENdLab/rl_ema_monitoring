classdef Polar_heartrate < MomentumSensor
    properties (Constant)
        dataType = "Polar_heartrate"
    end
    properties
        databasePath
        rawData
        dataIsPreprocessed = false
        eventName
        windowToEpoch
        eventBlockTable % delete
    end

    methods
        function obj = Polar_heartrate(input)
            obj.databasePath = input;
        end

        function preprocessData(obj)
            if ~obj.dataIsPreprocessed
                obj.getDataFromDatabase();
                obj.dataIsPreprocessed = true; 

            end
        end

        function getDataFromDatabase(obj)
            verbose = true;
            sessionData = MomentumSensor.readDatabase(obj.databasePath,obj.dataType);
            obj.rawData = MomentumSensor.processSessions(...
                                sessionData, ...            % Data
                                15, ...                      % split on gaps > 3 s
                                @Polar_heartrate.cleanRawData, ...     
                                verbose...                 % show waitbar
                            );
        end
        

    end
    
    methods (Static)
        
        function rriTable = cleanRawData(rawData)
            % Keeping the heart rate
            rowsToKeep = rawData.heartrate>0;
            hrValues = rawData.heartrate(rowsToKeep);
            timestamps = Utils.convertPhoneTimestampsToDatetime(rawData.time_ms(rowsToKeep));
            rriTable= table(timestamps,hrValues,'VariableNames',{'Timestamp','heartrate'});
            % TODO: need to extract the RRI and interpolate the timestamps
            % depending on the number of values
        end

        
    end
end
