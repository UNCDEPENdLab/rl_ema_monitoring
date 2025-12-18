classdef (Abstract) ParquetReaderBase < handle
    properties (Access= public)
        device    string
        participantId       string

    end

    properties (SetAccess = protected)
        directory           string
        files               % dir() struct subset for chosen pattern
        metas               % array of per-file meta structs for chosen pattern
        patternKey          string

        % Common inferred bits
        eventName           string
        section             string
        sessionBinIdx       double
        channelLabel        string
        timeBinLabel        string
        binningMode         string
    end

    methods
        function obj = ParquetReaderBase(dirPath,validationDevice)
            if nargin<2
                validationDevice="muse";
            end
            obj.device = validationDevice;
            obj.verifyDirectory(dirPath);

        end
        
        function obj = scan(obj)
            % Scan directory for parquet files and select a complete pattern.
            d = dir(fullfile(obj.directory, '*.parquet'));
            if isempty(d)
                error('ParquetReaderBase:NoFiles', ...
                    'No *.parquet files found in "%s".', obj.directory);
            end

            % Parse metadata for all files via subclass hook
            metas = arrayfun(@(f) NameSchema.parse(f.name), d);

            % Group by patternKey supplied by parse
            patternKeys = string({metas.patternKey});
            [uKeys, ~, idx] = unique(patternKeys, 'stable');
            nGroups = numel(uKeys);

            completeMask   = false(1, nGroups);
            missingReports = cell(1, nGroups);
            for g = 1:nGroups
                these = find(idx == g);
                [ok, miss] = obj.isGroupComplete(metas(these));
                completeMask(g)   = ok;
                missingReports{g} = miss;
            end

            if ~any(completeMask)
                % Build a readable report
                msg = "No complete patterns found. Missing per pattern:";
                for g = 1:nGroups
                    miss = missingReports{g};
                    if ~isempty(miss)
                        msg(end+1) = sprintf('  Pattern "%s": %s', uKeys(g), miss); %#ok<AGROW>
                    end
                end
                error('ParquetReaderBase:NoCompletePattern', '%s', strjoin(msg, newline));
            end

            firstCompleteIdx = find(completeMask, 1, 'first');
            % if sum(completeMask) > 1
            %     warning('ParquetReaderBase:MultipleCompletePatterns', ...
            %         'Found %d complete patterns. Using the first: "%s".', ...
            %         sum(completeMask), uKeys(firstCompleteIdx));
            % end

            chosenKey   = uKeys(firstCompleteIdx);
            % keepMask    = patternKeys == chosenKey;

            obj.files   = d;%(keepMask);
            obj.metas   = metas;%(keepMask);
            obj.patternKey = chosenKey;

            % Copy a few common fields if present
            m1 = obj.metas(1);
            copyIf = @(fld) (isfield(m1, fld) && ~isempty(m1.(fld)));
            if copyIf('participantId'); obj.participantId = m1.participantId; end
            if copyIf('eventName');     obj.eventName     = m1.eventName;     end
            if copyIf('section');       obj.section       = m1.section;       end
            if copyIf('sessionBinIdx'); obj.sessionBinIdx = m1.sessionBinIdx; end
            if size(obj.files,1)==1 && copyIf('channelLabel');  obj.channelLabel  = m1.channelLabel;  end
            if copyIf('timeBinLabel');  obj.timeBinLabel  = m1.timeBinLabel;  end
            if copyIf('binningMode');   obj.binningMode   = m1.binningMode;   end
        end

        function T = loadAll(obj)
            % Concatenate all files for the chosen pattern (memory permitting)
            if isempty(obj.files)
                T = table(); return;
            end
            parts = cell(1, numel(obj.files));
            for k = 1:numel(obj.files)
                fullPath = fullfile(obj.directory, obj.files(k).name);
                parts{k} = parquetread(fullPath);
                thisFileMeta = NameSchema.parse(fullPath);
                if strcmp(obj.device,"biosemi")
                    parts{k}.channel = repmat(thisFileMeta.channelLabel,height(parts{k}),1);
                elseif strcmp(obj.device,"muse")
                    % parts{k}.section = repmat(thisFileMeta.section,height(parts{k}),1);
                    parts{k}.channel = parts{k}.side +"_"+thisFileMeta.section;
                    parts{k}.side = [];
                end
            end
            T = obj.concatTablesCell(parts, obj.files);
        end
    end
    
    methods (Access = private)
        function verifyDirectory(obj,dirPath)
            dirPathObject=dir(dirPath);
            dirPathObject = dirPathObject(~ismember({dirPathObject.name}, {'.', '..'}));

            hasValidationSubdirs = all([dirPathObject.isdir] & ismember({dirPathObject.name}, {'muse', 'biosemi'}));
            
            if hasValidationSubdirs
                dirPath = fullfile(dirPath,obj.device);
            end
            
            obj.directory = string(dirPath);

        end
    end

    methods (Access = protected)
        function T = concatTablesCell(~, parts, fileStructs)
            % Safe concatenation with common-vars/typing checks.
            n = numel(parts);
            if n == 0, T = table(); return; end

            common = parts{1}.Properties.VariableNames;
            for i = 2:n
                common = intersect(common, parts{i}.Properties.VariableNames, 'stable');
            end
            if isempty(common)
                error('ParquetReaderBase:NoCommonColumns', ...
                    'No common columns across %d files.', n);
            end

            % Type check
            ref = parts{1};
            for v = 1:numel(common)
                nm = common{v};
                cls0 = class(ref.(nm));
                for i = 2:n
                    if ~strcmp(class(parts{i}.(nm)), cls0)
                        error('ParquetReaderBase:TypeMismatch', ...
                            'Variable "%s" type mismatch across files "%s" vs "%s".', ...
                            nm, fileStructs(1).name, fileStructs(i).name);
                    end
                end
            end

            % Align order + drop non-common
            for i = 1:n
                parts{i} = parts{i}(:, common);
            end
            T = vertcat(parts{:});
        end
    end

    methods (Abstract, Access = protected)
        [isComplete, missingReport] = isGroupComplete(obj, metasInGroup)
    end
end
