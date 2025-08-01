classdef EEGSensor < MomentumSensor
    properties
        EEGLabObject
        participantId
        eventName
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
                
        function preprocessData(obj)
            % Each subclass implements its own
        end
        
        function save(obj,opts)
            arguments
                obj
                opts.saveDir = ""
                opts.saveMode = "asParquet" %{asMat,asParquet,asCSV}
                opts.timeBinningMode = "byTime" % {byTime,byTimepoints}
                opts.sessionsPerBin = 0
                opts.binByChannel = false % Splits the files by channel
            end

            dataWriter = NDDataWriter(ndData = obj.EEGLabObject.data, ...
                                      dataType="EEG", ...
                                      id = obj.participantId,...
                                      eventName = obj.eventName,...
                                      timeLabels = obj.EEGLabObject.times, ...
                                      trialLabels = vertcat(obj.EEGLabObject.event.trial),... % Formerly in: obj.EEGLabObject.etc.trialLabels
                                      sessionLabels = vertcat(obj.EEGLabObject.event.session),...% Formerly in: obj.EEGLabObject.etc.epochLabels
                                      channelLabels={obj.EEGLabObject.chanlocs.labels});
            nv = Utils.packStructAsNameValuePairs(opts);
            dataWriter.save(nv{:});
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
