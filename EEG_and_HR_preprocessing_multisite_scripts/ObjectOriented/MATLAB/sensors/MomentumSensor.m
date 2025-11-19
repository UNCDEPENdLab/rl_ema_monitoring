classdef MomentumSensor <handle
    methods (Abstract)
        preprocessData(obj)
    end
    
    methods (Static)

        function cost = objective_function(C,tau)
            % Computes the cost based on the squared differences between a shifted matrix C and a set of tau values.
            %
            % Parameters:
            % C - [Array] A matrix or vector where each element represents a shifted timestamp.
            % tau - [Array] A matrix or vector of adjustment factors (delays or advances in time).
            %
            % Returns:
            % cost - [Numeric] The total squared difference cost, representing the alignment error.
            
            % Create a matrix of tau values where each column is filled with the corresponding tau value
            tau0 = tau(1);  % First element of tau
        
            % Calculate the total cost 
            cost = sum((C-tau+tau0).^2); % tau and tau0 get broadcasted
        
        end

        function [tau_opt, fval] = optimizeAlignment(T_ms,T_ns)
            % Optimizes the alignment between two sets of timestamps (in ms and ns) using the fmincon optimization tool.
            %
            % Parameters:
            % T_ms - [Array] Millisecond timestamps.
            % T_ns - [Array] Nanosecond timestamps, presumably needing alignment to T_ms.
            %
            % Returns:
            % tau_opt - [Array] The optimal tau values that minimize the alignment error.
            % fval - [Numeric] The function value (cost) at the solution, indicating the alignment error.
            

            function [cost, grad] = obj_and_grad(C,tau)
                tau0 = tau(1);
                R = (C - tau + tau0);        % residuals
                cost = R'*R;
                % ?J/??_i = -2*R_i  for i>1 , and for i=1 sum(2*R)
                g = -2*R;
                g(1) = 2*sum(R);
                grad = g;
            end

            % tau_initial = -0.05 + 0.1 * rand(height(T_ms), 1);  % Generates n random numbers between -0.05 and 0.05
            tau_initial = T_ns-T_ms+median(T_ns-T_ms); % For faster convergence
            lower_bounds = [];
            upper_bounds = [];
            
            tms0 = T_ms(1);
            C = T_ns - T_ms + tms0;  % Precompute shift
            %% Method 1
            options = optimoptions('fmincon', 'Algorithm', 'sqp' ,'TolFun',1e-4, ...        % stop when cost changes <
                      'TolX',1e-4, ...          % stop when ?-changes
                      'MaxIterations',20, ...  % cap total iterations % 'MaxFunctionEvaluations',9000,...% 
                      'SpecifyObjectiveGradient',true,...
                        'Display', 'off');
            % options = optimoptions('fmincon', 'Algorithm', 'sqp', 'UseParallel', true);
            % options.Display = 'iter-detailed';% For debugging

            
            % [tau_opt,fval] = fmincon(@(tau) MomentumSensor.objective_function(C, tau), tau_initial, [], [], [], [], lower_bounds, upper_bounds, [], options);
            [tau_opt,fval] = fmincon(@(tau) obj_and_grad(C, tau), tau_initial, [], [], [], [], lower_bounds, upper_bounds, [], options);

            % figure; plot(1e3*(T_ns-(T_ms+tau_opt)),'DisplayName','difference+ tau_{opt}');hold on; plot(1e3*(T_ns-T_ms),'DisplayName','original difference'); plot(1e3*tau_opt,'DisplayName','tau_{opt}');legend('show');ylabel('ms');title('By 1e-6 tolerance');fprintf('sumAbs differences %d sumAbs new diff %d \n',sum(abs(T_ns-T_ms)),sum(abs(T_ns-(T_ms+tau_opt))));

        end

        

        function [preEventWindow, postEventWindow] = parseWindow(windowToEpoch)
            preEventWindow  = seconds(abs(windowToEpoch(1)));
            postEventWindow = seconds(abs(windowToEpoch(2)));
        end

        function timeVector = makeTimeVectorFromWindow(upsamplingPeriod,preWindow, postWindow)
            % From ?preW to +postW at the upsampling period
            timeVector = (-preWindow : upsamplingPeriod : postWindow);
        end

        function [interpolated_interval] = getInterpolatedIntervals(datapoints_per_timestamp,fs)
            % Compute the timestamps going back in time by interpolating a number
            % of datapoints using a given sampling frequency.
            %
            % Parameters:
            % datapoints_per_timestamp - [Double] the length of the resulting
            %                           interval.
            % fs - [Double] the sampling frequency
            %
            % Returns:
            % interpolated_interval - [Double] The interval of interpolated values
        
            currentTs = 0;
            previousTs = -(datapoints_per_timestamp-1)./fs;% datapoints_per_timestamp-1 because we'll use the currentTs
            interpolated_interval = linspace(previousTs, currentTs, datapoints_per_timestamp); % Extrapolate from current timestamp back a number of datapoints 
        end
        
        function concatenated = processSessions(...
                sessionData, gapThreshold, processorFunc, verbose, varargin)
            %processSessions Full pipeline: split, process, and merge
            %   sessionData: table with timestamp and sensor variables
            %   gapThreshold: scalar (seconds) to split on
            %   processorFunc: function handle for preprocessing subsession
            %   verbose: logical to show waitbar
            %   varargin: optional args passed to processorFunc

            % 1) Pre-clean: remove duplicates and sort
            cleanData = MomentumSensor.cleanAndSort(sessionData);
            
            % 2) Split into continuous subsessions
            subsessions = MomentumSensor.splitSubsessions(cleanData, gapThreshold);

            % 3) Process each subsession with provided function
            processed = MomentumSensor.processSubsessions(...
                subsessions, processorFunc, verbose, varargin{:});

            % 4) Merge all processed results
            concatenated = MomentumSensor.mergeSubsessions(processed);
        end

        function concatenatedSessions = concatenateSessions(subsession_list, optimized_alignment, verbose)
            if verbose
                h = waitbar(0, 'Processing ECG Sessions...');
            end
            
            allSessions = cell(1, length(subsession_list));  % Preallocate cell array for efficiency
            total_subsessions = length(subsession_list);
        
            for i = 1:total_subsessions
                % This try-catch block attempts to ignore failed sessions where the
                % ACC/ECG was too short to be filtered
                try
                    % Process an individual segment of the session
                    % ACC_sessions{i} = getECG(subsession_list{i}, optimized_alignment);
                    allSessions{i} = getRRfromACC(subsession_list{i});
                    
                catch ex
                    % Catch if any errors occur
                    disp(ex.message);
                end
        
                % Update the waitbar if needed
                if verbose
                    waitbar(i / total_subsessions, h);
                end
            end
            
            % Merge the data for all subsessions
            concatenatedSessions = vertcat(allSessions{:});
        
            if verbose
                close(h);
            end
        end
        
        function processed = processSubsessions(subsessions, processorFunc, verbose, varargin)
            %processSubsessions Apply processor to each subsession
            disp("Processing Subsessions...")
            nbSubsessions = numel(subsessions);
            processed = cell(1, nbSubsessions);

            for subsessionIdx = 1:nbSubsessions
                try
                    processed{subsessionIdx} = processorFunc(subsessions{subsessionIdx}, varargin{:});

                catch ex
                    warning('Subsession %d failed: %s', subsessionIdx, ex.message);
                    processed{subsessionIdx} = [];  % or handle differently
                end
                if verbose
                    Utils.updateProgress(10,subsessionIdx,nbSubsessions,"Processed", "subsessions");
                end

            end
            disp("All subsessions processed.")
        end

        function merged = mergeSubsessions(processed)
            %mergeSubsessions Vertically concatenate processed outputs
            % Remove empty cells (failed processing)
            valid = ~cellfun(@isempty, processed);
            merged = vertcat(processed{valid});
        end

        function sessionData = readDatabase(filepath,tableName)
            %% Read data from database
            db = sqlite(filepath);
            % Older versions of matlab import as cell array without column
            % names
            if isMATLABReleaseOlderThan("R2023a")
                dataCell = fetch(db,sprintf('SELECT * FROM %s',tableName));
                colNames   = fetch(db, sprintf('SELECT name FROM pragma_table_info(''%s'')',tableName ));
                varNames    = colNames(:)';                     
                sessionData = cell2table(dataCell, ...
                            'VariableNames', varNames);
            else
                sessionData = fetch(db,sprintf('SELECT * FROM %s',tableName));
            end
            
            db.close();
        end
        
        function Timestamps = processTimestamps(raw_data, optimized_alignment, fs, datapoints_per_timestamp)

            % Calculate interpolated intervals between data points
            interpolated_interval = MomentumSensor.getInterpolatedIntervals(datapoints_per_timestamp, fs);
        
            % Process sensor timestamps (in nanoseconds)
            T_from_ns = 1e-9 * (double(raw_data.polar_timestamp) - double(raw_data.polar_timestamp(1)));
        
            if optimized_alignment
                % Align using optimized method
                start_timestamp = double(raw_data.time_ms(1));
                T_from_ms = (double(raw_data.time_ms) - start_timestamp) * 1e-3;
        
                % Optimize alignment
                [tau_opt, ~] = MomentumSensor.optimizeAlignment(T_from_ms, T_from_ns);
        
                % Adjust timestamps
                T_base = seconds(T_from_ms + tau_opt) + Utils.convertPhoneTimestampsToDatetime(start_timestamp);
            else
                % Use first phone timestamp as reference
                raw_data.time_ms_timestamp = Utils.convertPhoneTimestampsToDatetime(double(raw_data.time_ms));
                raw_data.polar_timedate = MomentumSensor.convertPolarTimestampsToDatetime(raw_data.polar_timestamp);
                
                T_base = raw_data.time_ms_timestamp(1) + (raw_data.polar_timedate - raw_data.polar_timedate(1));
                % T_base = raw_data.time_ms_timestamp; % Using less resolution overlaps the timestamps 
            end
        
            % Expand timestamps to match each data point
            Timestamps = repmat(T_base, 1, datapoints_per_timestamp) + seconds(interpolated_interval);
            Timestamps = reshape(Timestamps', [], 1);
        end

        function subsessions = splitSubsessions(cleanData, gapThreshold)
            %splitSubsessions Split table at time gaps longer than threshold
            %   cleanData: sorted table with timestamp in first var
            %   gapThreshold: seconds to split on

            % Convert first column to datetime array
            timeCol = cleanData{:,1};
            if isnumeric(timeCol)
                timestamps = Utils.convertPhoneTimestampsToDatetime(timeCol);
            elseif isdatetime(timeCol)
                timestamps = timeCol;
            else
                error('Timestamp column must be numeric POSIX or datetime');
            end

            % Compute intervals between successive timestamps
            diffs = [seconds(diff(timestamps)); 0];

            % Identify split points
            splitIdx = find(diffs > gapThreshold);

            % Build subsessions cell array of tables
            subsessions = {};
            startIdx = 1;
            for i = 1:numel(splitIdx)
                endIdx = splitIdx(i);
                subsessions{end+1} = cleanData(startIdx:endIdx, :); 
                startIdx = endIdx + 1;
            end
            % Add final chunk
            if startIdx <= height(cleanData)
                subsessions{end+1} = cleanData(startIdx:end, :);
            end
        end

        function cleanData = cleanAndSort(sessionData)
            %cleanAndSort Remove duplicates and sort by timestamp(s)
            % Remove repeated rows (excluding the first timestamp column)
            varNames = sessionData.Properties.VariableNames;
            dataCols = varNames(2:end);
            [~, idx] = unique(sessionData(:, dataCols), 'rows', 'stable');
            if numel(idx) < height(sessionData)
                warning('Found repeated records; removing duplicates.');
                sessionData = sessionData(idx, :);
            end

            % Sort by primary timestamp and, if present, polar_timestamp
            if ismember('polar_timestamp', varNames)
                cleanData = sortrows(sessionData, {'time_ms','polar_timestamp'});
            else
                cleanData = sortrows(sessionData, varNames{1});
            end
        end
        
        function polarDateTime = convertPolarTimestampsToDatetime(polarTimestamps)
            polarDateTime = datetime(double(946684800000000000+polarTimestamps)*1e-9,...
                                                    'ConvertFrom', 'posixtime',...
                                                    'Format', 'dd-MMM-yyyy HH:mm:ss.SSSSSSSSS',...
                                                    'TimeZone', 'America/New_York');
        end

    end

end