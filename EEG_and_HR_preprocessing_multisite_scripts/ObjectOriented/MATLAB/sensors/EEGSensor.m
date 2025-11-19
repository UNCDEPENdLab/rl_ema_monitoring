classdef EEGSensor < MomentumSensor
    properties
        EEGLabObject
        participantId
        eventName
        windowToKeep = [] % in seconds e.g. [-0.2,3.0]
    end
    
    methods

        function obj = EEGSensor(EEGLabObject, participantId, eventName)
            % To skip to subclass constructor if not enough arguments are
            % passed
            if nargin == 0; return; end

            if nargin == 3
                obj.EEGLabObject  = EEGLabObject;
                obj.participantId = participantId;
                obj.eventName     = eventName;
            else
                error('EEGSensor:WrongNumberOfInputs', ...
                      'EEGSensor expects 0 OR 3 inputs.');
            end
        end
                
        function preprocessData(~)
            % Each subclass implements its own
        end
        
        function save(obj,opts)
            arguments
                obj
                opts.saveDir            = ""
                opts.saveMode           = "asParquet" %{asMat,asParquet,asCSV}
                opts.timeBinningMode    = "byTime" % {byTime,byTimepoints}
                opts.blocksPerBin       = 0
                opts.binByChannel       = false % Splits the files by channel
                opts.tPerBin            = 0 % Defines the timeBinning. In ms if opts.timeBinningMode== "byTime", else in datapoints
            end
            
            obj.trimEEGToWindowToKeep();

            dataWriter = NDDataWriter(ndData = obj.EEGLabObject.data, ...
                                      dataType="EEG", ...
                                      id            = obj.participantId,...
                                      eventName     = obj.eventName,...
                                      timeLabels    = obj.EEGLabObject.times, ...
                                      trialLabels   = obj.EEGLabObject.etc.trialLabels,... % Formerly in: obj.EEGLabObject.event.trial
                                      blockLabels   = obj.EEGLabObject.etc.blockLabels,... % Formerly in: obj.EEGLabObject.event.session
                                      channelLabels ={obj.EEGLabObject.chanlocs.labels},...
                                      outcomeLabels = obj.EEGLabObject.etc.outcomeLabels);
            nv = Utils.packStructAsNameValuePairs(opts);
            dataWriter.save(nv{:});
        end

    end
    
    methods(Access=private)
        
        function trimEEGToWindowToKeep(obj)
            if ~isempty(obj.windowToKeep)
                obj.EEGLabObject = pop_select(obj.EEGLabObject,'time',obj.windowToKeep);
            end
        end

    end

    methods (Static)
        
        function [leftChLabels, rightChLabels, midlineChLabels] = categorizeLabels(labels)
            
            %   leftChLabels - Cell array of left hemisphere channels (odd numbered)
            %   rightChLabels - Cell array of right hemisphere channels (even numbered)
            %   midlineChLabels - Cell array of midline channels (ending with 'z')
            
            % Find midline channels (labels ending with 'z')
            midlineChLabels = labels(endsWith(labels, 'z'));
        
            % Extract numeric endings from labels and convert them to numbers
            numericEndings = regexp(labels, '\d+$', 'match');
            numericEndings = cellfun(@(x) str2double(x), numericEndings, 'UniformOutput', false);
            emptyIdx = cellfun(@isempty, numericEndings);
            numericEndings(emptyIdx)={NaN};
        
            % Identify labels with valid numeric endings
            % hasNumeric = ~isnan(numericEndings);
            
            % Determine even and odd channel numbers
            evenIdx = mod([numericEndings{:}], 2) == 0;
            oddIdx = mod([numericEndings{:}], 2) == 1;
        
            % Assign even and odd labels to right and left channels respectively
            rightChLabels = labels(evenIdx);
            leftChLabels = labels(oddIdx);
        end
        
    end
end
