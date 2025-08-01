classdef ScheduleDatabase < handle
    properties (Constant)
        ITIupperLimit =seconds(3)

    end

    properties (Access=private)
        databasePath
        pathToScheduleDir
        scheduleFilePath
    end

    properties
        trials=[]
        sessions=[]
        questionnaires=[]
        restingTimes=[]
        itiTimes = []
    end

    methods
        function obj = ScheduleDatabase(pathToScheduleDir)
            % Expects pathToScheduleDir to contain a ./*schedule.db
            % file
            obj.pathToScheduleDir = pathToScheduleDir;
            obj.findParticipantsScheduleFile();
        end
        

        function getAllDatabases(obj)
            obj.readTrials(true);
            obj.readSessions(true);
            obj.readQuestionnaires(true);
        end
               
        function getEventTimes(obj,eventName)
            switch eventName
                case "RestingEpoch"
                    obj.getRestingTimes();
                case "ITI"
                    obj.getITITimes();
                case "RestingState"
                    obj.getRestingTimes();
                    obj.prepareRestingTimes();
                otherwise
                    obj.readTrials(true);
            end
        end

        function readTrials(obj, forceReading,fullTable)
            if nargin<3; fullTable=false;end

            obj.readTable("trials",forceReading,fullTable);
            obj.trials.feedback_time = Utils.convertPhoneTimestampsToDatetime(obj.trials.feedback_time);
            obj.trials.stim_time = Utils.convertPhoneTimestampsToDatetime(obj.trials.stim_time);
            obj.trials.choice_time = Utils.convertPhoneTimestampsToDatetime(obj.trials.choice_time);
            if ismember('scheduled_time', obj.trials.Properties.VariableNames)
                obj.trials.scheduled_time = Utils.convertPhoneTimestampsToDatetime(obj.trials.scheduled_time);
            end


        end
        
        function getTrialsWithSystem(obj,id)
            obj.readTrials(true,true);

            timestampAligner = TimestampAligner(id = id, ...
                                                schedule = obj.trials);
            museBlockRange = timestampAligner.getBlockNumber();
            if isequal(museBlockRange, 500:503)
                biosemiBlockRange = 504:507;
            elseif isequal(museBlockRange, 504:507)
                biosemiBlockRange = 500:503;
            end

            obj.trials.system= repmat("muse",height(obj.trials),1);
            biosemiBlockIndices = find(ismember(obj.trials.block, biosemiBlockRange));      

            obj.trials.system(biosemiBlockIndices)="biosemi";
        end

    end

    methods (Access =private)
        function prepareRestingTimes(obj)
            eventName = 'RestingState';
            % Rename columns and fill trials
            obj.restingTimes.preWindow =  obj.restingTimes.EndTime- obj.restingTimes.StartTime;
            obj.restingTimes = renamevars( obj.restingTimes, 'EndTime', eventName);
            obj.restingTimes.StartTime = [];
            obj.restingTimes.trial=ones(height(obj.restingTimes),1);
            obj.restingTimes = obj.restingTimes(:, {'block','trial','preWindow',eventName});

        end

        function getITITimes(obj)
            obj.readTrials(false);
            % ITI: (black screen): 2-3 sec (sampled from a uniform distribution)
            difference = obj.trials.stim_time(2:end) - obj.trials.feedback_time(1:end-1) - seconds(1);% outcome appears for 1000 ms
            
            % Add dummy value for first trial
            difference = [seconds(4);difference];
        
            % If difference is greater than 3000ms (which is the upper limit) then use minimum which is 2 seconds 
            difference(difference>obj.ITIupperLimit) = seconds(2);
            
            % Saving the difference as preWindow (startTime) and the end of
            % ITI as ITI 
            obj.itiTimes = table( ...
                            difference, ...
                            obj.trials.stim_time,   ...
                            obj.trials.trial,     ...
                            obj.trials.block,    ...
                            'VariableNames', {'preWindow','ITI','trial','block'} );
        end

        function getRestingTimes(obj,dropFailed)
            if nargin <2; dropFailed = true; end
            if isempty(obj.restingTimes)
                obj.readQuestionnaires(false);
                obj.restingTimes = obj.questionnaires(strcmp(obj.questionnaires.description,"5m Resting State"),:);
                noStartOrCompletedTimes = obj.restingTimes.start_time == 0 | obj.restingTimes.completed_time == 0;
                obj.restingTimes = obj.restingTimes(~noStartOrCompletedTimes,{'start_time','completed_time'});
                obj.restingTimes.Properties.VariableNames = ["StartTime","EndTime"];
                
                obj.restingTimes.StartTime = Utils.convertPhoneTimestampsToDatetime(obj.restingTimes.StartTime);
                obj.restingTimes.EndTime = Utils.convertPhoneTimestampsToDatetime(obj.restingTimes.EndTime);
                
                % The last 5 mins of resting are more accurate according to A.L.
                earlyStartTime = minutes(5);
                mask = (obj.restingTimes.EndTime - obj.restingTimes.StartTime) >= earlyStartTime;    
                obj.restingTimes.StartTime(mask) = obj.restingTimes.EndTime(mask) - earlyStartTime;
                
                if dropFailed
                    longPeriodBetweenConsecutiveStarts = [diff(obj.restingTimes.StartTime);seconds(Inf)]>minutes(5);
                    numberShortRestingPeriods = sum(longPeriodBetweenConsecutiveStarts==0);
                    if numberShortRestingPeriods; fprintf("Dropped %i resting periods \n",numberShortRestingPeriods);end
                    obj.restingTimes = obj.restingTimes(longPeriodBetweenConsecutiveStarts,:);
                end
                obj.addBlocksLabelToRestingTimes(true);
            end
        end

        function addBlocksLabelToRestingTimes(obj,firstBlockOnly)
            obj.readSessions();
            timeDifferences =repmat(obj.sessions.start_time_ms,1,height(obj.restingTimes))-obj.restingTimes.StartTime';
            
            valid = timeDifferences > duration(0,0,0); 
            restIdxPerSession = sum(valid,2);
            blocksPerRestingPeriod = accumarray( restIdxPerSession, ...
                                    obj.sessions.block, ...
                                    [height(obj.restingTimes),1], ...           % one cell for each restingTimes row
                                    @(x){x}, ...         % wrap each group into a cell
                                    {[]} );              % default for empty groups
            uniqueBlocksPerRestingPeriod = cellfun(@(x) unique(x),blocksPerRestingPeriod,'UniformOutput',false);
            obj.restingTimes.block = uniqueBlocksPerRestingPeriod;

            % Ignore resting periods that don't have blocks associated
            mask = cellfun(@(x) ~isempty(x),obj.restingTimes.block,'UniformOutput',false);
            mask=cell2mat(mask);
            obj.restingTimes = obj.restingTimes(mask,:);
            if firstBlockOnly
                firstBlock = cellfun(@(x) x(1), obj.restingTimes.block,'UniformOutput',false); 
                obj.restingTimes.block = cell2mat(firstBlock);
            end

        end
        
        function readSessions(obj, forceReading)
            if nargin<2; forceReading = false; end
            obj.readTable("sessions",forceReading);
            obj.sessions.start_time_ms = Utils.convertPhoneTimestampsToDatetime(obj.sessions.start_time_ms );
            obj.sessions.stop_time_ms = Utils.convertPhoneTimestampsToDatetime(obj.sessions.stop_time_ms );
            
        end 
        
        function readQuestionnaires(obj, forceReading)
            obj.readTable("questionnaires", forceReading);
        end

        function readTable(obj, tableName, forceReading,fullTable)
            if nargin<4; fullTable=false; end 
            if isempty( obj.(tableName) ) || forceReading
              obj.extractTableFromDatabase(tableName,fullTable);
            end
        end

        function extractTableFromDatabase(obj,tableName,fullTable)
            if nargin<2; fullTable = false; end

            switch tableName
                case "sessions"
                    expected = ["block", ...
                                "start_trial","start_timestamp","start_time_ms", ...
                                "last_trial","stop_timestamp","stop_time_ms"];
                    query = sprintf('SELECT * FROM %s',tableName);

                case "trials"
                    expected = ["trial", ...
                                "feedback_time","stim_time","choice_time","feedback","block"];
                    query = sprintf('SELECT trial, feedback_time, stim_time, choice_time, feedback, block FROM %s WHERE choice_time IS NOT NULL AND stim1>=0 AND stim2>=-1000 ORDER BY choice_time ASC',tableName);
                    
                    if fullTable
                        expected = ["block","trial","stim1","stim2","feedback","choice","outcome","stim_time","choice_time","feedback_time","scheduled_time"];
                        query = 'SELECT * FROM trials';
                    end

                case "questionnaires"
                    expected = ["type","number","description","scheduled_time",...
                                "start_time","completed_time"];
                    query = sprintf('SELECT * FROM %s',tableName);

                otherwise
                    error("Unknown table name: %s", tableName)
            end

            % Connect to SQLite database
            db = sqlite(obj.scheduleFilePath);
            
            % Fetch data from database
            obj.(tableName) = fetch(db, query);
            db.close();

            obj.verifyCompatibility(expected, tableName);

        end
        
        function verifyCompatibility(obj, expected,tableName)
            
            dataToVerify = obj.(tableName);
            if iscell(dataToVerify)
                obj.(tableName) = cell2table(dataToVerify, ...
                    "VariableNames", expected);
            end
        end
        
        function findParticipantsScheduleFile(obj)
            
            % List directory contents
            files = dir(obj.pathToScheduleDir);
            
            % Convert to table if needed
            if ~istable(files)
                files = struct2table(files);
            end
        
            % Exclude folders
            files = files(~files.isdir, :);
        
            % Filter names by suffix
            suffix = 'schedule.db';
            mask   = endsWith(files.name, suffix);
        
            nMatch = sum(mask);
            if nMatch == 0
                error('No file ending with ''%s'' found in %s.', suffix, obj.pathToScheduleDir);
            elseif nMatch > 1
                error('Multiple files ending with ''%s'' found in %s.', suffix, obj.pathToScheduleDir);
            end
        
            % Grab the matching row
            row = files(mask, :);
        
            % Build full path
            obj.scheduleFilePath = fullfile(obj.pathToScheduleDir, row.name{1});
        end
    end
end
