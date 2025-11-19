classdef RespiratoryRateEstimator < handle
    properties (Constant)
        availableSources= {'ACC','ECG'}
    end

    properties
        data
        source
        fs
    end

    properties (Access=private)
        isCentered =false
    end

    methods 
        function obj = RespiratoryRateEstimator(data,source)
            obj.data = data;
            obj.validateSource(source);
            obj.estimateFs();
        end
        
        function calculateRR(obj,method)
            obj.centerData();
            obj.preFilter();
            obj.decompose();
            obj.postFilter();
            obj.getRR(method);
            obj.interpolate()
        end

    end

    methods(Access=private)

        function centerData(obj)
            if obj.isCentered; return; end
            columnNames = obj.data.Properties.VariableNames(2:end);
            for columnIdx = 1:numel(columnNames)
                columnName = columnNames{columnIdx};
                obj.data.(columnName) = obj.data.(columnName)- nanmean(obj.data.(columnName));
            end
            obj.isCentered = true;
                
        end

        function validateSource(obj,source)
            if ismember(source,obj.availableSources)
                obj.source = source;
            else
                error("Unrecognized event name %s",source);
            end
        end
        
        function estimateFs(obj)
            obj.fs = 1/seconds(mode(diff(obj.data.Timestamps)));
        end

    end
end