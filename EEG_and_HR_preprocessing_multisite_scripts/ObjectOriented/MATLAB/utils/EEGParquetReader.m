classdef EEGParquetReader < ParquetReaderBase
    properties (SetAccess = private)
        timeBinIdxs double = []
    end

    methods
        function obj = EEGParquetReader(dirPath,device)
            if nargin<2
                device="muse";
            end
            
            obj@ParquetReaderBase(dirPath,device);
            obj = obj.scan();
        end

        function obj = scan(obj)
            scan@ParquetReaderBase(obj);
            obj.timeBinIdxs = sort([obj.metas.timeBinIdx]);
        end

        function EEG = getEEGLabObjectFromParquet(obj)
            if strcmp(obj.device,"muse")
                channelOrder = EEG_muse.museChannels;
            elseif strcmp(obj.device,"biosemi")
                channelOrder = EEG_biosemi.biosemiChannels;
            end
            EEG = EEGParquetReader.tableToEventChanTime3D(obj.loadAll(),channelOrder);
            EEG.etc.eventName = obj.eventName;
            EEG = EEGParquetReader.struct2erplab(EEG);
        end
    end
    
    methods (Access = protected)

        function [ok, report] = isGroupComplete(~, metasInGroup)
            bins = sort([metasInGroup.timeBinIdx]);
            if isempty(bins)
                ok = false; report = 'no time bins'; return;
            end
            missing = setdiff(1:max(bins), bins);
            ok = isempty(missing);
            if ok, report = ""; else, report = "missing time bins " + mat2str(missing); end
        end
    end

    methods (Static, Access = private)

        function EEG = struct2erplab(eegStruct)
            etcInfo = eegStruct.etc;

            % 1) Build EEG
            EEG = EEGParquetReader.struct2eeglab(eegStruct);
            clear eegStruct            
            
            EEG = EEGParquetReader.add_trialinfo_to_events(EEG, etcInfo);
            EEG.etc = etcInfo;
        
        end

        function EEG = add_trialinfo_to_events(EEG, etcInfo)
        
            % Number of trials and points
            nTrials = EEG.trials;
            pnts    = EEG.pnts;
        
            % Find time-zero index in the epoch
            [~, zeroIdx] = min(abs(EEG.times));  
        
            % Vectorized latency: each epoch is pnts samples apart in the "virtual" continuous stream
            latencies = zeroIdx + (0:nTrials-1) * pnts;
        
            eventName = etcInfo.eventName;
            % Epoch and urevent indices
            epochs  = 1:nTrials;
            urevent = 1:nTrials;
            
            % Build struct arrays in one shot (no for-loops over trials)
            EEG.event = struct( ...
                'type',    repmat({eventName}, nTrials, 1), ...
                'latency', num2cell(latencies(:)), ...
                'epoch',   num2cell(epochs(:)), ...
                'urevent', num2cell(urevent(:)) );
        
            % Here urevent is identical to event (simple case)
            EEG.urevent = EEG.event;
        
            % Sanity check / consistency
            EEG = eeg_checkset(EEG, 'eventconsistency');
        end

        function EEGLabObject = tableToEventChanTime3D(dataTable, channelOrder)   
            EEGLabObject = struct();

            % --- Basic checks -----------------------------------------------------
            requiredVars = ["block","trial","timeBin","signal","id","channel"];
            if ~all(ismember(requiredVars, string(dataTable.Properties.VariableNames)))
                missing = requiredVars(~ismember(requiredVars, string(dataTable.Properties.VariableNames)));
                error('tableToEventChanTime3D:MissingVars', ...
                    'Input table is missing required variables: %s', strjoin(missing, ', '));
            end
        
            timeCol    = dataTable.timeBin;
            chanCol    = dataTable.channel;
        
            % --- ID: check that it is constant -----------------------------------
            idUnique = unique(dataTable.id);
            if numel(idUnique) ~= 1
                error('tableToEventChanTime3D:NonConstantId', ...
                    'Column "id" must have a single unique value; found %d.', numel(idUnique));
            end
            EEGLabObject.subject = idUnique(1);
        
            % --- Event dimension: group by (block, trial) -----------------------
            %   G_events: group index per row
            %   blockLabels, trialLabels, outcomeLabels: unique combos, sorted lexicographically
            [G_events, blockLabels, trialLabels, outcomeLabels] = findgroups(dataTable.block, dataTable.trial,dataTable.outcome);
            nEvents = numel(blockLabels);
        
            % --- Channel dimension: map channels to indices in channelOrder -------
            chanOrderStr = string(channelOrder);
            [tfChan, chanIdx] = ismember(string(chanCol), chanOrderStr);
        
            if ~all(tfChan)
                bad = unique(chanCol(~tfChan));
                error('tableToEventChanTime3D:UnknownChannel', ...
                    'Some channels in table are not in channelOrder: %s', ...
                    strjoin(string(bad), ', '));
            end
        
            nChannels = numel(chanOrderStr);
        
            % --- Time dimension: sorted unique time bins --------------------------
            % Assumes timeBin is numeric / datetime / duration; unique sorts ascending.
            timeBins = unique(timeCol);
            nTime    = numel(timeBins);
        
            [tfTime, timeIdx] = ismember(timeCol, timeBins);
            if ~all(tfTime)
                error('tableToEventChanTime3D:TimeMappingError', ...
                    'Unexpected error mapping timeBin values to indices.');
            end
        
            % --- Build 3D array: events x channels x time -------------------------
            nRows = height(dataTable);
        
            % Linear indices into the 3D array
            linIdx = sub2ind([nChannels, nTime,nEvents], ...
                             chanIdx, timeIdx,G_events);
        
            if numel(unique(linIdx)) ~= nRows
                error('tableToEventChanTime3D:DuplicateSamples', ...
                    ['There are duplicate (session, trial, channel, timeBin) combinations ', ...
                     'in the table. Cannot uniquely fill 3D array.']);
            end
            EEGLabObject.data = nan(nChannels, nTime,nEvents);
        
            % Fill in one vectorized assignment
            EEGLabObject.data(linIdx) = dataTable.signal;

            EEGLabObject.etc = struct();
            EEGLabObject.etc.epochLabels = blockLabels;
            EEGLabObject.etc.trialLabels = trialLabels;
            EEGLabObject.etc.outcomeLabels = outcomeLabels;
            EEGLabObject.chanlocs= struct('labels', channelOrder);
            EEGLabObject.times = timeBins(:)'; 
            EEGLabObject.srate = 1000/median(diff(timeBins));
           
        end
    end

    methods(Static)
        function EEG = struct2eeglab(S)
            % Start from an empty EEGLAB dataset
            EEG = eeg_emptyset;
        
            % Core data and dimensions
            EEG.data = S.data;                        % [chan x pnts x trials]
            [EEG.nbchan, EEG.pnts, EEG.trials] = size(S.data);
        
            % Sampling info
            EEG.srate = S.srate;
            EEG.times = S.times;                      % usually ms
            EEG.xmin  = S.times(1) / 1000;           % convert ms → seconds
            EEG.xmax  = S.times(end) / 1000;
        
            % Channel info
            EEG.chanlocs = S.chanlocs;               % must be a struct array with .labels etc.
        
            % Dataset / subject metadata
            EEG.subject = S.subject;
            EEG.setname = [S.subject '_epoched'];
        
            % Make sure everything is consistent
            EEG = eeg_checkset(EEG);
        end
    end
end
