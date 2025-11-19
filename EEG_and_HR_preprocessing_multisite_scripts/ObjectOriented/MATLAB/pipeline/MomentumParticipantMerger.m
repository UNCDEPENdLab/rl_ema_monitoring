classdef MomentumParticipantMerger < handle    
    properties
        rawDataDir
        processedMergedDir
        dataType %{EEG,TF}

    end
    
    properties (Access=private)
        mergedDirName
        frequencyString
        mergedFileName
        accumulatedTable =[]
        saveFileName
        sqliteFilePath
        sqliteTableName
        blocksPerBin = 20 % Resting state data has blocks ~30% density (one resting period every ~3 blocks) so this number is accounting for that
        inputSplitFilePattern
        opts
        validationMode = false
        museValidationDir
        biosemiValidationDir
        allFiles = {}
    end

    methods
        function obj = MomentumParticipantMerger(rawDataDir)    
            obj.rawDataDir    = rawDataDir; %Contains all participants dirs
            obj.verifyValidationMode();
            disp("Validation check");
            obj.prepareSaveDir();
            disp("Prepared save directory");
            % obj.getAllSplitFiles();
            % disp("Got all split files");

        end
        
        function merge(obj,opts)
            arguments
                obj
                opts.eventName    = "RestingState" 
                opts.sectionsToMerge = {"temp","frontal"} % {''} if not considered
                opts.freqLabel    = [] % {alpha,beta,...,1,2,3,..}
                opts.binningMode  = "byTimepoints" % {byTimepoints,byTime}
                opts.timeBinIdx   = 0 % Numeric time bin idx
                opts.extension    = ".parquet" 
                opts.blockBinIdx = 0 % 0 = expects no block
            end
            obj.opts = opts;

            if obj.validationMode
                % merge muse 
                obj.mergeMuseFiles({"front","temp"},obj.museValidationDir);
                
                % merge biosemi
                obj.mergeBiosemiValidation();
            else
                obj.mergeMuseFiles(opts.sectionsToMerge,obj.processedMergedDir);
            end
            
        end
        
    end
    
    methods (Access=private)
        
        function mergeMuseFiles(obj,sections,mainSaveDir)
            for sectionIdx = 1:numel(sections)
                obj.opts.section = sections{sectionIdx};
                obj.inputSplitFilePattern = MomentumParticipant.getFileName(eventName=obj.opts.eventName, ...
                                                    section = obj.opts.section, ...
                                                    freqLabel = obj.opts.freqLabel, ...
                                                    binningMode = obj.opts.binningMode,...
                                                    timeBinIdx = obj.opts.timeBinIdx, ...
                                                    blockBinIdx = obj.opts.blockBinIdx,...
                                                    extension = obj.opts.extension, ...
                                                    asPattern = true, ...
                                                    dataType = obj.dataType);
                disp(obj.inputSplitFilePattern);
                obj.mergeBlockFiles(mainSaveDir);
            end

        end

        function filesWithPatternFound = getFilesWithPattern(obj)

            % List only immediate subdirectories of obj.rawDataDir
            d = dir(obj.rawDataDir);
            isSubdir = [d.isdir] & ~ismember({d.name}, {'.','..'});
            subdirNames = {d(isSubdir).name};
        
            % Split the input pattern into name + extension
            % Example: obj.inputSplitFilePattern = 'myPattern.parquet'
            [~, basePattern, extPattern] = fileparts(obj.inputSplitFilePattern);
        
            % If no extension was given, default to '.parquet' (optional)
            if isempty(extPattern)
                extPattern = '.parquet';
            end
        
            nSub = numel(subdirNames);
            candidateFiles = cell(nSub,1);
        
            % Build the full path:
            % {rawDataDir}/{subdirName}/{subdirName}_{basePattern}{extPattern}
            for k = 1:nSub
                subdir = subdirNames{k};
        
                filename = sprintf('%s_%s%s', subdir, basePattern, extPattern);
                candidateFiles{k} = fullfile(obj.rawDataDir, subdir, filename);
            end
        
            % Keep only the ones that actually exist
            existsMask = cellfun(@isfile, candidateFiles);
            filesWithPatternFound = candidateFiles(existsMask);
            
            % Print the ones that do NOT exist
            missingFiles = candidateFiles(~existsMask);
            if ~isempty(missingFiles)
                fprintf('The following expected files were NOT found:\n');
                for i = 1:numel(missingFiles)
                    fprintf('  %s\n', missingFiles{i});
                end
            else
                fprintf('All expected files were found.\n');
            end
        end

        function mergeBiosemiValidation(obj)
            % Assumes biosemi validation is split by channel so we iterate over them
            for channelIdx = 1:numel(EEG_biosemi.biosemiChannels)
                obj.opts.section = EEG_biosemi.biosemiChannels{channelIdx};
                obj.inputSplitFilePattern = MomentumParticipant.getFileName(eventName=obj.opts.eventName, ...
                                                    freqLabel = obj.opts.freqLabel, ...
                                                    binningMode = obj.opts.binningMode,...
                                                    timeBinIdx = obj.opts.timeBinIdx, ...
                                                    blockBinIdx = obj.opts.blockBinIdx,...
                                                    extension = obj.opts.extension, ...
                                                    channelLabel =  obj.opts.section,...
                                                    asPattern = true, ...
                                                    dataType = obj.dataType);
                obj.mergeBlockFiles(obj.biosemiValidationDir);
            end
        end

        function mergeBlockFiles(obj,mainSaveDir)
            
            obj.mergedFileName = obj.inputSplitFilePattern;
            obj.saveFileName = fullfile(mainSaveDir, obj.mergedFileName);
            obj.displayProgress();
            
            %% Checking if already exists
            if isfile(obj.saveFileName)
                disp("File already exists, skipping");
                return;
            end
            
            obj.getAccumulatedTable();
            % if isempty(obj.accumulatedTable); return; end
        end

        function verifyValidationMode(obj)
            % Validate input
            if ~isfolder(obj.rawDataDir)
                error('"%s" is not a valid folder.', obj.rawDataDir);
            end
        
            % List only the immediate subfolders of basePath
            participantDirs = dir(obj.rawDataDir);
            isSubdir = [participantDirs.isdir] & ~ismember({participantDirs.name}, {'.','..'});
            participants = participantDirs(isSubdir);
        
            % Initialize as false
            obj.validationMode = false;
        
            % Loop through some participant folders
            for participantDirectoryInd = 1:numel(participants)
                currentParticipantDir = fullfile(obj.rawDataDir, participants(participantDirectoryInd).name);
                % List its subfolders
                currentParticipantDirContents = dir(currentParticipantDir);
                isInnerDir = [currentParticipantDirContents.isdir] & ~ismember({currentParticipantDirContents.name}, {'.','..'});
                if any(isInnerDir)
                    obj.validationMode = true;
                    return;    % stop at the first match
                end

                if participantDirectoryInd>=2
                    return;
                end
            end
        end

        function prepareSaveDir(obj)
            obj.getDataType();
            obj.getMergedDirName();
            obj.processedMergedDir = fullfile(obj.rawDataDir, '..',obj.mergedDirName);
            if ~isfolder(obj.processedMergedDir)
                mkdir(obj.processedMergedDir);
            end

            if obj.validationMode
                obj.museValidationDir = fullfile(obj.processedMergedDir,'muse');
                if ~isfolder(obj.museValidationDir)
                    mkdir(obj.museValidationDir);
                end  
                obj.biosemiValidationDir = fullfile(obj.processedMergedDir,'biosemi');
                if ~isfolder(obj.biosemiValidationDir)
                    mkdir(obj.biosemiValidationDir);
                end 
            end
        end
        
        function getAllSplitFiles(obj)
            dsObject = fileDatastore(obj.rawDataDir, ...
                            'IncludeSubfolders', true, ...
                            'FileExtensions',    {'.parquet'}, ... 
                            'ReadFcn',           @(x)x);

            obj.allFiles = dsObject.Files;    % cell array of full paths
            % d = dir(fullfile(obj.rawDataDir, '**', '*.parquet'));
            % obj.allFiles = fullfile({d.folder}, {d.name})';

        end

        function filesWithPatternFound = getFilesWithPattern_legacy(obj)
            obj.allFiles = obj.getAllSplitFiles();
            [~,inputFilePattern,~] =fileparts(obj.inputSplitFilePattern); 
            filesFoundMask = contains(obj.allFiles, inputFilePattern);
            filesWithPatternFound = obj.allFiles(filesFoundMask);
        end
        
        function getAccumulatedTable(obj)
            filesWithPatternFound = obj.getFilesWithPattern();

            if isempty(filesWithPatternFound)
                fprintf('No files for timeBinIdx: %d. Skipping. \n', obj.opts.timeBinIdx);
                return;
            end

            switch obj.opts.extension
                case '.parquet'
                    % obj.loadAndCombineTables_obsolete(filesWithPatternFound);
                    obj.loadAndCombineTablesOptimized(filesWithPatternFound);
                    parquetwrite(obj.saveFileName, obj.accumulatedTable);

                case '.db'
                    obj.loadAndAppendToSQLite(filesWithPatternFound);
                case ".csv"
                    obj.loadAndCombineTables_obsolete(filesWithPatternFound);
    
                    writetable(obj.accumulatedTable, obj.saveFileName);
                    gzip(obj.saveFileName);
                    delete(obj.saveFileName);
            end

            disp("Saving complete");

        end

        function displayProgress(obj)
        
            if strcmp(obj.dataType,"TF")
                obj.frequencyString = MomentumParticipant.validateFrequencyLabel(obj.opts.freqLabel);
                fprintf("section: %s freqBin: %s timeBin: %03d blockBin %03d \n",obj.opts.section,obj.frequencyString,obj.opts.timeBinIdx,obj.opts.blockBinIdx);
            else
                fprintf("section: %s timeBin: %03d \n",obj.opts.section,obj.opts.timeBinIdx);
            end
        end

        function getDataType(obj)
            [parentDir,~] = fileparts(obj.rawDataDir);
            [grandParentDir,~]= fileparts(parentDir);
            [~,obj.dataType ]= fileparts(grandParentDir); 
        end
        
        function getMergedDirName(obj)
            %if strcmp(obj.dataType,"TF")
            if startsWith(obj.dataType, "TF")
                obj.mergedDirName = "ByFreqTimeBin";
            elseif strcmp(obj.dataType,"EEG")
                obj.mergedDirName = "ByTimepoints";
            elseif strcmp(obj.dataType,"RRI")
                obj.mergedDirName = "ByTimeBin";
            end 
        end

        function loadTables(obj,filesAtThisTimeBin)
            ds   = fileDatastore(filesAtThisTimeBin, 'ReadFcn', @parquetread);
            obj.accumulatedTable   = tall(ds);  
        end

        function loadAndCombineTables_obsolete(obj,filesAtThisTimeBin)
            obj.accumulatedTable = cellfun(@parquetread, filesAtThisTimeBin, 'UniformOutput', false);
            obj.accumulatedTable = vertcat(obj.accumulatedTable{:});
        end

        function loadAndCombineTablesOptimized(obj,filesAtThisTimeBin)
            obj.accumulatedTable = []; 
            disp("Beginning Merge");
            for i = 1:numel(filesAtThisTimeBin)
                T = parquetread(filesAtThisTimeBin{i}); 
                if isempty(obj.accumulatedTable)
                    obj.accumulatedTable = T; 
                else
                    obj.accumulatedTable = [obj.accumulatedTable; T];
                end
                fprintf("Participant: %d completed \n",i);
            end
        end

        function loadAndAppendToSQLite(obj, filesAtThisTimeBin)
            conn = sqlite(obj.saveFileName, "create");
            sqlTableName = "MomentumData";

            fprintf("Appending %d parquet files into SQLite table '%s'\n", ...
                numel(filesAtThisTimeBin), obj.saveFileName);
            
            for idx = 1:numel(filesAtThisTimeBin)
                parquetPath = filesAtThisTimeBin{idx};
                
                % (A) Read this chunk into a MATLAB table
                T = parquetread(parquetPath);
                
                % (B) On the very first file, create the table; otherwise append
                if idx == 1                    
                    MomentumParticipantMerger.createSQLiteTableFromMATLABTable(conn, sqlTableName, T);
                else
                    MomentumParticipantMerger.insertTableInChunks(conn, sqlTableName, T, 1000000);
                end
                fprintf("  • Finished file %d/%d: %s\n", idx, numel(filesAtThisTimeBin), parquetPath);
            end
            
            close(conn);
            fprintf("Done. All rows have been written to '%s' inside %s\n", ...
                obj.saveFileName, obj.mergedDirName);
        end
    end

    methods (Static)
        function createSQLiteTableFromMATLABTable(conn, tableName, T)
          
            % 1) Get all variable names from the MATLAB table
            varNames = T.Properties.VariableNames;
        
            % 2) Build an array of “name type” strings, one for each column
            colDefCell = cell(1, numel(varNames));  % preallocate
            for k = 1:numel(varNames)
                colName = varNames{k};
                colData = T.(colName);
        
                % 2a) Determine the appropriate SQLite type
                if isnumeric(colData)
                    if isa(colData, "double") || isa(colData, "single")
                        % Check if every non-NaN value is an integer:
                        if all(isnan(colData) | (floor(colData)==colData))
                            sqlType = "INTEGER";
                        else
                            sqlType = "REAL";
                        end
                    elseif isinteger(colData) 
                        sqlType = "INTEGER";
                    else
                        sqlType = "REAL"; 
                    end
        
                elseif islogical(colData)
                    % Store logicals as INTEGER 0/1
                    sqlType = "INTEGER";
        
                elseif isstring(colData) || ischar(colData) || iscellstr(colData) ...
                       || iscategorical(colData)
                    % Any text‐like column → TEXT
                    sqlType = "TEXT";
        
                elseif isdatetime(colData) || isduration(colData)
                    % Store datetimes/durations as ISO‐strings
                    sqlType = "TEXT";
        
                else
                    error("Unsupported MATLAB type for column '%s'.", colName);
                end
        
                safeName = sprintf('%s', colName);        
                colDefCell{k} = sprintf("%s %s", safeName, sqlType);
            end
        
            % 3) Join all column‐definitions with commas
            allColumnDefs = strjoin(string(colDefCell), ", ");
        
            % 4) Build the final CREATE TABLE command
            queryCommand = sprintf( ...
                'CREATE TABLE "%s" (%s);', ...
                tableName, allColumnDefs );
        
            % 5) Execute the CREATE TABLE
            exec(conn, queryCommand);
        
            fprintf("Table '%s' created with %d columns.\n", tableName, numel(varNames));
        end

        function insertTableInChunks(conn, tableName, T, chunkSize)
    
            if nargin < 4
                chunkSize = 1000;
            end
        
            numRows = height(T);
            if numRows == 0
                return
            end
        
            % Precompute column names once
            colNames = T.Properties.VariableNames;
        
            idxStart = 1;
            while idxStart <= numRows
                idxEnd = min(idxStart + chunkSize - 1, numRows);
        
                % Extract sub‐table for this batch
                subT = T(idxStart:idxEnd, :);
        
                for k = 1:numel(colNames)
                    varName = colNames{k};
                    colData = subT.(varName);
        
                    if isstring(colData) || iscategorical(colData)
                        % string or categorical → cell array of char vectors
                        subT.(varName) = cellstr(colData);
        
                    elseif isdatetime(colData)
                        % datetime → cell array of character dates in ISO format
                        subT.(varName) = cellstr(datestr(colData, "yyyy-mm-dd HH:MM:SS"));
        
                    end
                end
        
                subCells = table2cell(subT);
        
                % Perform the batch insert
                insert(conn, tableName, colNames, subCells);
        
                % Move to next chunk
                idxStart = idxEnd + 1;
            end
        end

    end

end
