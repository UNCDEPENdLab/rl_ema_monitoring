classdef Polar_RRIfromECG < PolarSensor
    properties (Constant)
        lowLimitRRI = 462 %130 bpm
        highLimitRRI = 1200 % 50bpm
        dataColumn = "RRI"
        resamplingPeriod = seconds(2/3) % For precisely centering the event 
        bufferTime = seconds(5) % To capture previous RR datapoints before resampling
        maxGapToUpsample = seconds(5)
    end
    
    properties (Access=private)
        sourceECG
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
        
    end
    
    methods (Access=private)
        
        function getDataFromPreprocessedECG(obj)
            obj.data = Polar_RRIfromECG.computeRRIwithPanTompkins(obj.sourceECG.data.ECG,obj.sourceECG.data.Timestamp);
            obj.sourceECG = [];
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