classdef Utils <handle
    properties
    end
    methods
        function obj = Utils()
        end
    end
    
    methods(Static)
        function updateProgress(binSize,currentBin,totalBins,prefixText, suffixText)
            if mod(currentBin,binSize)==0
                fprintf("%s %d / %d %s \n",prefixText,currentBin,totalBins,suffixText);
            end
        end
        
        function s = mergeStructs(originalStruct, secondStruct)

            s = originalStruct;
            f2 = fieldnames(secondStruct);
            for k = 1:numel(f2)
                s.(f2{k}) = secondStruct.(f2{k});
            end
        end

        function nv = packStructAsNameValuePairs(inputStruct)
            f = fieldnames(inputStruct);
            v = struct2cell(inputStruct);
            
            nv = cell(1,2*numel(f)); nv(1:2:end)=f; nv(2:2:end)=v;
        end

        function datetime_array = convertPhoneTimestampsToDatetime(datenumMilliseconds)
            % Converts an array of date numbers in milliseconds to a datetime array with
            % a specified format and timezone.
            %
            % Parameters:
            % datenum_array_ms - [Array of Doubles or table] Contains date numbers in milliseconds since the Unix epoch (01-Jan-1970).
            %
            % Returns:
            % datetime_array - [Datetime Array] Contains the converted date and time values
            %                  in the 'dd-MMM-yyyy HH:mm:ss.SSSSSSSSS' format, adjusted to the
            %                  'America/New_York' timezone.
            if strcmp(class(datenumMilliseconds),'table')
                datenumMilliseconds = table2array(datenumMilliseconds);
            end

            datetime_array = double(datenumMilliseconds)/1000; % Convert to seconds
            datetime_array = datetime(datetime_array, 'ConvertFrom', 'posixtime', 'Format', 'dd-MMM-yyyy HH:mm:ss.SSSSSSSSS', 'TimeZone', 'America/New_York');
        end


        function writeDataToNewDatabase(db,tableName,data)
            % Check if there is data
            if ~isempty(data)
                 % Determine the action based on MATLAB version
                if isMATLABReleaseOlderThan("R2023a")
                    % Define column names based on the table name
                    switch tableName
                        case 'Polar_heartrate'
                            colnames = {'time_ms', 'heartrate', 'rr_intervals', 'contact'};
                        case 'Polar_ECG'
                            colnames = {'time_ms', 'polar_timestamp', 'ECG'};
                        case 'Polar_acceleormeter'
                            colnames = {'time_ms','polar_timestamp','XYZ'};
                        case 'EEG_muse'
                            colnames = {'recording_time', 'EEG1', 'EEG2', 'EEG3','EEG4','ISGOOD1','ISGOOD2','ISGOOD3','ISGOOD4'};
                        otherwise
                            error('Unknown table. Cannot determine column names.');
                    end
        
                    % Perform the insert operation with the specified column names
                    insert(db, tableName, colnames, data);
                else
                    % Use sqlwrite for newer MATLAB versions
                    sqlwrite(db, tableName, data);
                end
            end
        end
    
        function tempDbFile = createTemporalMergedDatabase(pathToPhysioFile,givenDataTypes)
            % Creates a temporary SQLite database by merging specific
            % tables from multiple database files. Returns the path of the
            % newly created table
            
            % Generate a list of all database files
            physioFiles = Utils.findDatabaseFiles(pathToPhysioFile);
        
            % Generate a unique temporary file name for the SQLite database
            tempDbFile = [tempname, '.db'];
        
            % Open a connection to the temporary database
            db = sqlite(tempDbFile, 'create');
            % cleanupObj = onCleanup(@() delete(tempDbFile));

            for dataType = givenDataTypes
                databaseQueryForThisEvent = Utils.getQueryForDatabaseEvent(dataType{:});
                exec(db, databaseQueryForThisEvent);
            end


            % Loop through each file in physioFiles to read and merge the data
            for ind = 1:length(physioFiles)
        
                % Open each database file individually
                newdb = sqlite(physioFiles{ind}, 'connect');
                
                 try
                    if isMATLABReleaseOlderThan("R2023a")
                        foundTableNames = string(fetch(db,"SELECT tbl_name FROM sqlite_master"));
                    else    
                        foundTableNames = fetch(db,"SELECT * FROM sqlite_master").tbl_name;
                    end
                    
                    for tableInd = 1:numel(foundTableNames)
                        % Fetch available tables
                        tableName = foundTableNames(tableInd);
                        data = fetch(newdb,sprintf('SELECT * FROM %s',tableName));
                        Utils.writeDataToNewDatabase(db,tableName,data);
                    end
            
                catch ME 
                    fprintf('An error occurred while fetching tables in file %s: %s\n',physioFiles{ind}, ME.message);
                end
                newdb.close();
                % if ind>5
                %     break % TODO: only temporary
                % end
            end
            db.close();
        
        end
        
        function queryCommand = getQueryForDatabaseEvent(dataSource)
            switch dataSource
                case 'Polar_heartrate'
                    queryCommand = ['CREATE TABLE Polar_heartrate (' ...
                                'time_ms INTEGER, ' ...
                                'heartrate INTEGER, ' ...
                                'rr_intervals TEXT, ' ...
                                'contact INTEGER);'];

                case 'Polar_ECG'
                    queryCommand = ['CREATE TABLE Polar_ECG (' ...
                                'time_ms INTEGER, ' ...
                                'polar_timestamp INTEGER, ' ...
                                'ECG REAL);'];
                case 'Polar_accelerometer'
                    queryCommand = ['CREATE TABLE Polar_accelerometer (' ...
                                'time_ms INTEGER, ' ...
                                'polar_timestamp INTEGER, ' ...
                                'XYZ TEXT);'];
                case 'EEG_muse'
                    queryCommand = ['CREATE TABLE EEG_muse (' ...
                                'recording_time DOUBLE, ' ...
                                'EEG1 TEXT, ' ...
                                'EEG2 TEXT, ' ...
                                'EEG3 TEXT, ' ...
                                'EEG4 TEXT, ' ...
                                'ISGOOD1 TEXT, ' ...
                                'ISGOOD2 TEXT, ' ...
                                'ISGOOD3 TEXT, ' ...
                                'ISGOOD4 TEXT);'];
            end
        end

        function dbFiles = findDatabaseFiles(directory)
            % Returns a list of all database files in the specified directory.
            % Parameters:
            % directory - [String] The directory to search
            % 
            % Returns:
            % dbFiles - [Cell Array] Contains all paths found to database files
        
            % Validate if the input is a directory
            if ~isfolder(directory)
                error('The provided path is not a valid directory.');
            end
        
            % Get a list of all '.db' files in the directory
            filePattern = fullfile(directory, '*.db');
            dbFiles = dir(filePattern);            
            dbFiles = {dbFiles.name};

            % Drop any schedule files
            mask = ~endsWith(dbFiles, 'schedule.db');
            dbFiles = dbFiles(mask);

            dbFiles = cellfun(@(x) fullfile(directory, x), dbFiles, 'UniformOutput', false);
        
        end

    end
end