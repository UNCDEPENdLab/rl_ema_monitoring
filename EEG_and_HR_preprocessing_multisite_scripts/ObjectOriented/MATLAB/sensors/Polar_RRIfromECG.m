classdef Polar_RRIfromECG < handle
    properties (Constant)
        lowLimitRRI = 462 %130 bpm
        highLimitRRI = 1200 % 50bpm
    end
    
    properties
        
        data
        dataIsPreprocessed = false
        eventName
    end

    properties (Access=private)
        sourceECG
        dataIsEpoched =false
        upsamplingPeriod = seconds(2/3) % For precisely centering the event 
        maxGapToUpsample = seconds(5)
        bufferTime = seconds(5) % To capture previous RR datapoints before upsampling

    end

    methods
        function obj = Polar_RRIfromECG(input)
            % Build from ECG object
            obj.sourceECG = input;
        end

        function preprocessData(obj)
            if ~obj.dataIsPreprocessed
                obj.getDataFromPreprocessedECG();
                obj.dataIsPreprocessed = true; 

            end
        end
        
        function epochToTable(obj,eventTable,eventName,windowToEpoch)
            if obj.dataIsEpoched; return; end

            obj.eventName = eventName;

            [preEventWindow, postEventWindow] = MomentumSensor.parseWindow(windowToEpoch);
            obj.epochToAllEvents(eventTable.(obj.eventName), ...
                                    preEventWindow, ...
                                    postEventWindow);

            obj.buildEpochedTable(eventTable,preEventWindow,postEventWindow);
            % obj.resampleTable();
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
            
            dataWriter = DataWriter(data = obj.data, ...
                                    dataType = "RRI", ...
                                    id=participantId,...
                                    eventName = obj.eventName);
            
            nv = Utils.packStructAsNameValuePairs(opts);
            dataWriter.save(nv{:});
        

        end

        function Tres = resample_legacy(obj)
              % pull in the table
              T = obj.data;
            
              % 1) convert to timetable with row‐times in seconds
              TT = table2timetable(T, 'RowTimes', seconds(T.timeBin/1000));
            
              % drop the original timeBin so retime only sees numeric vars
              TT.timeBin = [];
            
              % 2) define new 1.5 Hz timestep
              dt = seconds(1/1.5);
            
              % 3) group by block, trial, id
              [G, sessList, trialList, idList] = findgroups(TT.session, TT.trial, TT.id);
            
              % 4) prepare an empty timetable for output
              outTT    = TT([], :);
              varNames = outTT.Properties.VariableNames;
            
              % 5) loop over each group: isolate → retime → reattach labels
              for g = 1:max(G)
                subTT = TT(G==g, :);
                subTT = sortrows(subTT, 'Time');
            
                % isolate just the numeric signal
                sigTT = subTT(:, 'signal');
                rSig  = retime(sigTT, 'regular', 'linear', 'TimeStep', dt);
            
                % add back the grouping columns
                n = height(rSig);
                rSig.session = repmat(sessList(g),  n, 1);
                rSig.trial   = repmat(trialList(g), n, 1);
                rSig.id      = repmat(idList(g),    n, 1);
            
                % reorder to match outTT
                rSig = rSig(:, varNames);
            
                outTT = [outTT; rSig];
              end
            
              % 6) back to table, restore timeBin in ms, drop Time
              Tres         = timetable2table(outTT);
              Tres.timeBin = milliseconds(Tres.Time);
              Tres.Time    = [];
            
              % 7) final column order
              Tres = Tres(:, {'session','trial','id','timeBin','signal'});
            end

    end
    
    methods (Access=private)
        

        function getDataFromPreprocessedECG(obj)
            % obj.data = Polar_RRIfromECG.computeRRI(obj.sourceECG.rawData.ECG,obj.sourceECG.rawData.Timestamp);
            % obj.data = Polar_RRIfromECG.computeRRIwithPCST(obj.sourceECG.rawData);
            obj.data = Polar_RRIfromECG.computeRRIwithPanTompkins(obj.sourceECG.rawData.ECG,obj.sourceECG.rawData.Timestamp);
            obj.sourceECG = [];
        end

        function buildEpochedTable(obj,eventTable,preEventWindow,postEventWindow)
            timeVector = MomentumSensor.makeTimeVectorFromWindow(obj.upsamplingPeriod,preEventWindow,postEventWindow);

            nbTimepoints = numel(timeVector);
            nbEvents = height(eventTable.(obj.eventName));

            obj.data = table(categorical(repelem(eventTable.block,nbTimepoints)), ...
                                repelem(eventTable.trial,nbTimepoints), ...
                                repmat(1e3*seconds(timeVector)',nbEvents,1), ...
                                obj.data, ...
                                'VariableNames', ...
                                {'block','trial','timeBin','signal'});
        end

        function epochToAllEvents(obj,eventTimestamps,preEventWindow,postEventWindow)

            timeVector = MomentumSensor.makeTimeVectorFromWindow(obj.upsamplingPeriod,preEventWindow,postEventWindow);
            nbTimepoints = numel(timeVector);
            nbEvents = height(eventTimestamps);
            epochedRRI = NaN(nbEvents, nbTimepoints);
            disp("Epoching data...")
            for eventIdx = 1:nbEvents
                epochedRRI(eventIdx,:) = obj.epochSingleEvent(eventTimestamps(eventIdx), ...
                                                    preEventWindow, ...
                                                    postEventWindow);  
                Utils.updateProgress(500,eventIdx,nbEvents,"Epoched", "events");

            end

            % Flatten [M events x N timepoints] into a single column
            %       [E1t1,...,E1tn,...,Emtn]'
            obj.data = reshape(epochedRRI.',[],1);

        end
        
        function RRIEventSegment= epochSingleEvent(obj,eventTimestamp,preEventWindow,postEventWindow)
            % Look for a window slightly bigger than what's needed to
            % better interpolate. 
            epochStartTimestamp = eventTimestamp - preEventWindow;
            epochEndTimestamp = eventTimestamp + postEventWindow;

            eventTimeIndices = obj.data.Timestamp >= epochStartTimestamp - obj.bufferTime & obj.data.Timestamp <= epochEndTimestamp + obj.bufferTime;
            RRIEventSegment = obj.data(eventTimeIndices, :);
            newSamplingTimes = (epochStartTimestamp : obj.upsamplingPeriod : epochEndTimestamp)';

            if isempty(RRIEventSegment)
                RRIEventSegment = NaN(1,numel(newSamplingTimes));
                return; 
            end
        
            RRIEventSegment = table2timetable(RRIEventSegment, 'RowTimes','Timestamp');
            RRIEventSegment  = retime(RRIEventSegment, newSamplingTimes, 'spline');
            RRIEventSegment=RRIEventSegment.RRI; % Drop the timestamps
        end
    end

    methods (Static)

        function rriData = computeRRIwithPCST(ecgTable)
            HRVparams = InitializeHRVparams("none");
            HRVparams.windowLength = 30;        
            % params.overlap      = 0;         % no overlap between windows
            % params.lowFreq      = 0.04;      % LF band for spectral analysis
            HRVparams.Fs = Polar_ECG.fs;
            % call the function that perform peak detection
            ind_locs = jqrs(ecgTable.ECG,HRVparams);
            if ~issorted(ind_locs)
                disp("NonSortedRRI locs");
            end
            
            rriData = Polar_RRIfromECG.getRRITableFromPeakLocations(ecgTable.Timestamp,ind_locs);
        end
        
        function rriData = computeRRIwithPanTompkins(ecg_data,timestamps)
            sensorLimit = 1500;
            mask=ecg_data<=sensorLimit & ecg_data>=-sensorLimit; % Drop the values outside the expected sensor range (heuristically obtained)
            ecg_data = ecg_data(mask);
            timestamps = timestamps(mask);
            clear mask;

            plottingNeeded = false;
            disp("Running Pan Tompkins algorithm to find QRS peaks")
            [~,ind_locs,~] = pan_tompkin(ecg_data,Polar_ECG.fs,plottingNeeded);
            disp("Peaks found");

            clear ecg_data

            rriData = Polar_RRIfromECG.getRRITableFromPeakLocations(timestamps,ind_locs);
        end
        
        function rriData = getRRITableFromPeakLocations(timestamps,ind_locs)
            locs = timestamps(ind_locs); % Using the indices to get the actual locations
            rri = seconds(diff(locs))*1e3; % in ms
        
            % Organizing into table
            rri_timestamp = locs(2:end);
            rriData = table(rri_timestamp,rri,'VariableNames',{'Timestamp','RRI'});
            
            % Cap values
            lowerLimitMask = rriData.RRI<Polar_RRIfromECG.lowLimitRRI;
            rriData.RRI(lowerLimitMask) = Polar_RRIfromECG.lowLimitRRI;

            highLimitMask = rriData.RRI>Polar_RRIfromECG.highLimitRRI;
            rriData.RRI(highLimitMask) = Polar_RRIfromECG.highLimitRRI;

            % Ignore values outside limits
            % maskToKeep = rriData.RRI>=Polar_RRIfromECG.lowLimitRRI & rriData.RRI<Polar_RRIfromECG.highLimitRRI;
            % rriData = rriData(maskToKeep,:);

        end

        function rriData = computeRRI(ecg_data,timestamps)
            % Detect peaks
            [~, ind_locs] = findpeaks(ecg_data,  ... % Only using the datapoints with no reference to return the peak indices
                                       'MinPeakDistance',50,...
                                       'MinPeakHeight', 100, ...
                                       'MinPeakProminence',10);
        
            rriData = Polar_RRIfromECG.getRRITableFromPeakLocations(timestamps,ind_locs);
        end


    end

end