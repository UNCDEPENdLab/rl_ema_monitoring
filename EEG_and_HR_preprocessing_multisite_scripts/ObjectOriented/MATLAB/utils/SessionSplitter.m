classdef SessionSplitter < handle
    properties
        data
        threshold
        sessionData
    end
    
    properties (Access=private)
        isSplit = false
    end

    methods 
        function obj = SessionSplitter(data)
            obj.data=data;
            
        end
        function split(obj,threshold)
            if obj.isSplit; return; end

            obj.threshold = threshold; % Expects numeric in seconds
            obj.sortByTime();
            
            timeDiffs = [seconds(diff(obj.data.Timestamps)); 0]; % Append a zero for the last element to maintain array size
            
            % Identify splits when time difference is greater than x seconds
            splitPoints = find(timeDiffs > threshold);
            
            % Split data at these points
            sessionData = {};
            startIdx = 1;
            for i = 1:length(splitPoints)
                endIdx = splitPoints(i);
                sessionData{end+1} = obj.data(startIdx:endIdx, :);
                startIdx = endIdx + 1;
            end
        
            % Add the last segment if it wasn't included
            if startIdx <= size(sessionData, 1)
                sessionData{end+1} = sessionData(startIdx:end, :);
            end
            obj.sessionData = sessionData;
            obj.data = [];
            obj.isSplit = true;
        end

    end
    
    methods (Access=private)
        
        function sortByTime(obj)
            obj.data = sortrows(obj.data,{'Timestamps'});
        end

    end

end