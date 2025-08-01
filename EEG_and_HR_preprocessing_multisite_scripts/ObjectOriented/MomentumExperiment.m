classdef MomentumExperiment < handle
    % Orchestrates batch processing of MomentumParticipant data
    properties
        rawDataDir       % root directory containing all raw participant folders
        preprocEEGDir    % optional dir for preprocessed EEG
        participantDirs  % cell array of full paths to each participant folder
        processedIndividualRestingTFDir % Path to the individual resting state TF processed dir
        processedMergedRestingTFDir
        
        % struct to keep the split and merged directories
        processedEEGDirs = struct('split', struct(), ...
                                    'merged', struct()); 
    end
    
    properties
        eventName
    end

    methods (Access=public)
        function obj = MomentumExperiment(rawDataDir, preprocEEGDir)
            % Constructor: initialize directories and discover participants
            if nargin < 2, preprocEEGDir = ""; end

            obj.rawDataDir    = rawDataDir;
            obj.preprocEEGDir = preprocEEGDir;
            
            obj.participantDirs = MomentumExperiment.discoverParticipants(obj.rawDataDir);
            obj.generateRestingStatePaths();
            obj.generateEventSavePaths();
        end

        function generateRestingStatePaths(obj)
            parentDir = fileparts(obj.rawDataDir);

            obj.processedIndividualRestingTFDir = fullfile(parentDir, ...
                                                MomentumParticipant.generalProcessedSaveDirectoryName, ...
                                                "AlignedEvent", ...
                                                "TF", ...
                                                'RestingState','ByParticipant');
            obj.processedMergedRestingTFDir = fullfile(parentDir, ...
                                                MomentumParticipant.generalProcessedSaveDirectoryName, ...
                                                "AlignedEvent", ...
                                                "TF",...
                                                'RestingState','ByFreqTimeBin');
        end
        
        function generateEventSavePaths(obj)
            parentDir = fileparts(obj.rawDataDir);

            for event = MomentumParticipant.availableEventNames
                % eventName = eventNames{eventIdx};
                obj.processedEEGDirs.split.(event{1}) = fullfile(parentDir, ...
                                        MomentumParticipant.generalProcessedSaveDirectoryName, ...
                                        "AlignedEvent", ...
                                        "EEG",...
                                        event{1}, ...
                                        "ByParticipant"); % Points to subdirs by participant

                obj.processedEEGDirs.merged.(event{1}) = fullfile(parentDir, ...
                                        MomentumParticipant.generalProcessedSaveDirectoryName, ...
                                        "AlignedEvent", ...
                                        "EEG",...
                                        event{1}, ...
                                        "ByTimeBin"); % Points to all files merged
            end
        end

        function startParallelPool(obj)
            if isempty(gcp('nocreate'))
                % Try Slurm’s setting first:
                slurmC = getenv('SLURM_CPUS_PER_TASK');
                nC = str2double(slurmC);
                
                if isnan(nC) || nC < 1
                    % No Slurm or invalid: run locally
                    allCores = maxNumCompThreads; %feature('numcores'); 
                    nC = max(allCores - 1, 1);          % leave at least 1 for the OS
                end
                
                fprintf('Starting parpool with %d workers...\n', nC);
                parpool('local', nC);
            end
        end

        function runRestingAnalysis(obj, channelsToRemove)
            % Run resting-state TF analysis in parallel
            %if isempty(gcp('nocreate'))
            %    parpool;  % start default pool if none
            %end
            obj.startParallelPool();
            
            %% Objects dirs 
            participantDirs =obj.participantDirs;
            preprocEEGDir = obj.preprocEEGDir;
            processedIndividualRestingTFDir = obj.processedIndividualRestingTFDir;


            nSubj = numel(obj.participantDirs);
            for i = 1:nSubj
                participantRawDir = participantDirs{i};
                preprocStruct = MomentumExperiment.buildPreprocessedDirStruct(participantRawDir,preprocEEGDir);

                try
                    participant = MomentumParticipant(pathToData = participantRawDir, ...
                                                        preprocessedDirs=preprocStruct);
                    fprintf("Processing participant %s \n",participant.id);
                    participant.getEEGEpochedEvent(eventName='RestingState', ...
                                                    channelsToRemove=channelsToRemove);
                    participant.runTFAnalysis(byFrequencyBand=false);
                    participant.saveTFAnalysis(saveMode="asParquet", ...
                                                timeBinningMode="byTimepoints", ...
                                                sessionBinning=22);
                    participant.clearData();
                    delete(participant);
                catch ME
                   fprintf('Error processing %s: %s \n', participantRawDir, ME.message);
                   continue
                end
            end
        end

        function runMergeRestingState(obj,asCompressedCSV)
            if nargin<2; asCompressedCSV=true;end
            
            % Merge files by freq_bin & big_t_bin across all participants
            if ~isfolder(obj.processedMergedRestingTFDir)
                mkdir(obj.processedMergedRestingTFDir);
            end
            
            % obj.startParallelPool();

            frequenciesOfInterest = TimeFrequencyAnalyzer.frequenciesOfInterest;
            nFreq = numel(frequenciesOfInterest);
            timeDuration = EEG_muse.restingEpoch(2)-EEG_muse.restingEpoch(1);
            restingEpochTimepoints = round(timeDuration * EEG_muse.fs);
            nBigT = ceil(restingEpochTimepoints/TimeFrequencyAnalyzer.bigTimeBins);
            combinations = MomentumExperiment.allCombinations(1:nFreq, 1:nBigT);
            mergedDir = obj.processedMergedRestingTFDir;
            individualRestingDir =obj.processedIndividualRestingTFDir;
            

            fds = fileDatastore(individualRestingDir, ...
                            'IncludeSubfolders', true, ...
                            'FileExtensions',    {'.parquet'}, ...
                            'ReadFcn',           @(x)x);
            allFiles = fds.Files;    % cell array of full paths

            for idx = 1:size(combinations,1)
                freq_bin = combinations(idx,1);
                big_time_bin = combinations(idx,2);
                if asCompressedCSV
                    outName = fullfile(mergedDir, sprintf('resting_freq_bin_%02d_big_t_bin_%03d.csv', freq_bin, big_time_bin));
                    saveName = fullfile(mergedDir, sprintf('resting_freq_bin_%02d_big_t_bin_%03d.csv.gz', freq_bin, big_time_bin));
                     if isfile(saveName)
                        fprintf("Skipping %s because output file exist already. \n",saveName);
                        continue;
                    end
                else
                    outName = fullfile(mergedDir, sprintf('resting_freq_bin_%02d_big_t_bin_%03d.parquet', freq_bin, big_time_bin));
                    if isfile(outName)
                        fprintf("Skipping %s because output file exist already. \n",outName);
                        continue;
                    end
                end

                pattern = sprintf('_resting_freq_bin_%02d_big_t_bin_%03d.parquet', freq_bin, big_time_bin);
                
                tf = contains(allFiles, pattern);
                files2read = allFiles(tf);
                if isempty(files2read)
                    warning('No files for freq %d bigT %d', freq_bin, big_time_bin);
                    continue;
                end
                
                tbls = cellfun(@parquetread, files2read, 'UniformOutput', false);
                accumulatedTable = vertcat(tbls{:});
                if all(isnan(accumulatedTable.signal))
                    fprintf("Skipping fbin %d tbin %d because the whole table has NaNs \n",freq_bin,big_time_bin);
                    return
                end
                if asCompressedCSV
                    writetable(accumulatedTable,outName);
                    gzip(outName);            % compress it 
                    delete(outName);
                else
                    parquetwrite(outName,accumulatedTable);
                end
                % delete(files2read{:});
            end
        end
        
        function getEEGEpochedEvent(obj,eventName,windowToEpoch)
            obj.startParallelPool();
            
            participantDirs = obj.participantDirs;
            preprocEEGDir = obj.preprocEEGDir;

            nSubj = numel(obj.participantDirs);
            for i = 1:nSubj
                try
                    participantRawDir = participantDirs{i}; 
                    preprocessedDirs = MomentumExperiment.buildPreprocessedDirStruct(participantRawDir,preprocEEGDir);
                    participant = MomentumParticipant(participantRawDir,preprocessedDirs);
                    participant.getEEGEpochedEvent(eventName,windowToEpoch);
                    participant.saveEEG("asParquet","byTime");
                catch ME
                   fprintf('Error processing %s: %s \n', participantRawDir, ME.message);
                   continue
                end
            end
        end
    
        function mergeParticipantsEEG(obj,eventName, asCompressedCSV)
            if nargin<2; asCompressedCSV=true;end
            
            obj.eventName = eventName;
            mergedDir = obj.ensureMergedDirExists();
            allFiles = obj.getAllSplitEEGFiles();
            numberOfBins = obj.getNumberOfBins(allFiles);
            
            sections = {'temp','front'};
            for section =sections
                
                for binIdx = 1:numberOfBins
                    
                    if obj.shouldSkipBin(mergedDir, eventName,section{1}, binIdx, asCompressedCSV)
                        continue;
                    end
    
                    filesAtThisTimeBin = MomentumExperiment.findFilesAtBin(allFiles, eventName,section{1}, binIdx);
    
                    if isempty(filesAtThisTimeBin)
                        warning('No files for time bin %d', bin_idx);
                        continue;
                    end
    
                    accumulatedTable = MomentumExperiment.loadAndCombineTables(filesAtThisTimeBin);
                    if all(isnan(accumulatedTable.signal))
                        fprintf("Skipping tbin %d because the whole table has NaNs \n",binIdx);
                        return
                    end

                    obj.saveCombinedTable(accumulatedTable, mergedDir, eventName,section{1}, binIdx, asCompressedCSV);
    
                    % delete(filesAtThisTimeBin{:});
                    MomentumExperiment.updateSaveProgress(binIdx,numberOfBins);
                end
                fprintf("%s section saved successfully \n",section{1});
            end
        end
        
        function outputTFDirs = buildOutputTFDirs(obj)
            outputTFDirs = {};
            for i=1:numel(obj.participantDirs)
                [~,participantId,~] = fileparts(obj.participantDirs{i});
                outputTFDirs{end+1} = fullfile(obj.processedIndividualRestingTFDir,participantId);
            end
        end

        function dropFullSplitDirs(obj)
            outputTFDirs = obj.buildOutputTFDirs();
            nbExpectedFiles = numel(TimeFrequencyAnalyzer.frequenciesOfInterest)*ceil(EEG_muse.fs*EEG_muse.restingEpoch/TimeFrequencyAnalyzer.timepointsPerBin);
            keep = true(size(outputTFDirs));

            for k = 1:numel(outputTFDirs)
                thisDir = outputTFDirs{k};
                % Only act if it’s an existing folder
                if isfolder(thisDir)
                    % List contents of thisDir
                    D = dir(thisDir);
                    % Remove “.” and “..”
                    D(ismember({D.name}, {'.','..'})) = [];
                    % Keep only files (not subfolders)
                    D = D(~[D.isdir]);
                    
                    % If exactly 3090 files, mark for removal
                    if numel(D) == nbExpectedFiles
                        keep(k) = false;
                        fprintf("Skipping %s because output files exist already. \n",obj.participantDirs{k})
                    end
                end
                % if not a folder, keep(k) remains true
            end
            % filter
            obj.participantDirs = obj.participantDirs(keep);
        end

    end
    
    methods (Access=private)
        function mergedDir = ensureMergedDirExists(obj)

            mergedDir = obj.processedEEGDirs.merged.(obj.eventName);

            if ~isfolder(mergedDir)
                mkdir(mergedDir);
            end
            
        end
        
        function allFiles = getAllSplitEEGFiles(obj)
            dsObject = fileDatastore(obj.processedEEGDirs.split.(obj.eventName), ...
                            'IncludeSubfolders', true, ...
                            'FileExtensions',    {'.parquet'}, ... 
                            'ReadFcn',           @(x)x);

            allFiles = dsObject.Files;    % cell array of full paths
        end

        function saveCombinedTable(obj, accumulatedTable, mergedDir, eventName,section, binIdx, asCompressedCSV)
            if asCompressedCSV
                csvFileName = obj.getMergedEEGFileName(eventName,section, binIdx, ".csv");
                csvFilePath = fullfile(mergedDir, csvFileName);
                writetable(accumulatedTable, csvFilePath);
                gzip(csvFilePath);
                delete(csvFilePath);
            else
                parquetFileName = MomentumExperiment.getMergedEEGFileName(eventName,section, binIdx, ".parquet");
                parquetFilePath = fullfile(mergedDir, parquetFileName);
                parquetwrite(parquetFilePath, accumulatedTable);
            end
        end

        function skip = shouldSkipBin(obj, mergedDir, eventName,section, binIdx, asCompressedCSV)
            if asCompressedCSV
                finalSaveName = fullfile(mergedDir, ...
                    obj.getMergedEEGFileName(eventName,section, binIdx, ".csv.gz"));
            else
                finalSaveName = fullfile(mergedDir, ...
                    obj.getMergedEEGFileName(eventName,section, binIdx, ".parquet"));
            end
        
            skip = isfile(finalSaveName);
            if skip
                fprintf("Skipping %s because output file exists already.\n", finalSaveName);
            end
        end
    end

    methods (Static)
    
        function C = allCombinations(A,B)
          [aGrid, bGrid] = ndgrid(A,B);
          C = [aGrid(:), bGrid(:)];

        end
        
        function preprocessedDirs = buildPreprocessedDirStruct(participantRawDir,preprocEEGDir)
            if ~isempty(preprocEEGDir)
                [~, participantId, ~] = fileparts(participantRawDir);
                preprocDir = fullfile(preprocEEGDir, 'Data_Processed', sprintf('subject_%s', participantId));
                preprocessedDirs = struct('EEG_muse', preprocDir);
            else
                preprocessedDirs = struct();
            end
        end

        function participantDirsFound = discoverParticipants(directoryToSearch)
            % List subdirectories under directoryToSearch as participants
            contents = dir(directoryToSearch);
            isDir = [contents.isdir] & ~startsWith({contents.name}, '.');
            dirs = contents(isDir);
            participantDirsFound = fullfile(directoryToSearch, {dirs.name});
        end

        function filteredComb = dropAlreadyMergedCombinations(combinations, outputDir)

            % Preallocate logical mask
            nRows = size(combinations,1);
            keep   = false(nRows,1);
        
            for i = 1:nRows
                freqBin     = combinations(i,1);
                bigTimeBin  = combinations(i,2);
        
                % build the file name
                
                fullpath = MomentumExperiment.restingParquetName(freqBin, bigTimeBin, outputDir);
                
                % isfile works from R2017b onward; fallback to exist(...) if needed
                if ~isfile(fullpath)
                    keep(i) = true;
                end
            end
        
            % return only those rows for which no file was found
            filteredComb = combinations(keep,:);
        end
        
        function filesAtThisTimeBin = findFilesAtBin(allFiles, eventName,section, binIdx)
            namePattern = MomentumParticipant.getSplitEEGFileName("1", eventName,section, binIdx, ".parquet", true);
            filesFoundMask = contains(allFiles, namePattern);
            filesAtThisTimeBin = allFiles(filesFoundMask);
        end

        function fileName = getMergedEEGFileName(eventName,section, timeBinIdx,extension)
            fileName = sprintf('%s_%s_tBin_%03d%s',eventName, section,timeBinIdx,extension);
        end

        function fileName = getMergedTFFileName(eventName,section, frequencyBinIdx, timeBinIdx,extension)
            fileName = sprintf('%s_%s_freqBin_%02d_tBin_%03d%s',eventName, section,frequencyBinIdx,timeBinIdx,extension);
        end

        function maxNumberOfBins = getNumberOfBins(filesCellArray)
            [~, baseNames, ~] = cellfun(@fileparts, filesCellArray, 'UniformOutput', false);
            timeBins = cellfun(@(name) str2double(name(end-2:end)), baseNames);
            maxNumberOfBins = max(timeBins);

        end

        function accumulatedTable = loadAndCombineTables( filesAtThisTimeBin)
            accumulatedTable = cellfun(@parquetread, filesAtThisTimeBin, 'UniformOutput', false);
            accumulatedTable = vertcat(accumulatedTable{:});
        end

        function fname = restingParquetName(freqBin, bigTimeBin, baseDir)
        
            % build the base file name
            baseName = sprintf( ...
                'resting_freq_bin_%02d_big_t_bin_%03d.parquet', ...
                 freqBin, bigTimeBin);
            
            % prepend the folder if requested
            if nargin>=3 && ~isempty(baseDir)
                fname = fullfile(baseDir, baseName);
            else
                fname = baseName;
            end
        end

        function updateSaveProgress(binIdx,numberOfBins)
            if mod(binIdx,10)==0
                fprintf("Saved %d / %d \n",binIdx,numberOfBins);
            end
        end
    end

end
