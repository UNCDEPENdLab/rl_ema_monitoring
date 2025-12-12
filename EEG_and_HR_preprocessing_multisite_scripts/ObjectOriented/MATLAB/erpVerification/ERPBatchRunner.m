classdef ERPBatchRunner < handle

    properties
        RootDir     char

        BiosemiChannels cell = {}
        MuseChannels    cell = {}
        Outcomes        cell = {}
        Sides           cell = {'left'}   % e.g. 'left', 'right'

        ParseFcn function_handle          % @(fname)->[event, channel]

        % key = 'event|biosemiChannel|museChannel'
        % value = struct('biosemi', path, 'muse', path)
        PairMap
    end

    methods
        function obj = ERPBatchRunner(rootDir, varargin)
            if nargin == 0
                return;
            end

            validateattributes(rootDir, {'char','string'}, {'nonempty'});
            obj.RootDir   = char(rootDir);

            obj.ParseFcn = @ERPBatchRunner.defaultParseFileName;

            argumentParser = inputParser;
            addParameter(argumentParser, 'BiosemiChannels', {},  @ERPBatchRunner.isStringLike);
            addParameter(argumentParser, 'MuseChannels',    {},  @ERPBatchRunner.isStringLike);
            addParameter(argumentParser, 'Outcomes',        {},  @ERPBatchRunner.isStringLike);
            addParameter(argumentParser, 'Events',          {},  @ERPBatchRunner.isStringLike); % alias for Outcomes
            addParameter(argumentParser, 'Sides',           {'left'}, @ERPBatchRunner.isStringLike);
            addParameter(argumentParser, 'ParseFcn',        obj.ParseFcn, @(x) isa(x,'function_handle'));

            parse(argumentParser, varargin{:});

            obj.BiosemiChannels = ERPBatchRunner.normalizeList(argumentParser.Results.BiosemiChannels);
            obj.MuseChannels    = ERPBatchRunner.normalizeList(argumentParser.Results.MuseChannels);
            obj.Outcomes        = ERPBatchRunner.resolveOutcomes(argumentParser.Results.Outcomes, argumentParser.Results.Events);
            obj.Sides           = ERPBatchRunner.normalizeList(argumentParser.Results.Sides);
            obj.ParseFcn        = argumentParser.Results.ParseFcn;

            obj.discoverFromRoot();
        end

        function run(obj)
            if isempty(obj.PairMap) || obj.PairMap.Count == 0
                warning('ERPBatchRunner:NoPairs', ...
                    'No matching Biosemi/Muse pairs found. Nothing to run.');
                return;
            end

            pairKeys = obj.PairMap.keys;
            outcomesToPlot = obj.Outcomes;
            if isempty(outcomesToPlot)
                outcomesToPlot = {[]}; % use ERPComparison defaults when no outcomes are specified
            end

            for pairIndex = 1:numel(pairKeys)
                key  = pairKeys{pairIndex};
                pair = obj.PairMap(key);

                comparison = ERPComparison(ERPTable({pair.biosemi}),...
                                            ERPTable({pair.muse}));
                for sideIndex = 1:numel(obj.Sides)
                    side = obj.Sides{sideIndex};   % e.g. 'left' or 'right'

                    % "side" remains the only runtime selector; outcomes are passed explicitly.
                    for outcomeIndex = 1:numel(outcomesToPlot)
                        traces = outcomesToPlot{outcomeIndex};
                        comparison.plotComparison(traces, side);

                        figHandle = gcf;
                        if isgraphics(figHandle)
                            close(figHandle);
                        end
                    end
                end
            end
        end

        function discoverFromRoot(obj)
            biosemiDir = fullfile(obj.RootDir, 'biosemi');
            museDir    = fullfile(obj.RootDir, 'muse');

            biosemiFileStruct = dir(fullfile(biosemiDir, '*.parquet'));
            museFileStruct    = dir(fullfile(museDir, '*.parquet'));

            biosemiPaths = fullfile({biosemiFileStruct.folder}, {biosemiFileStruct.name});
            musePaths    = fullfile({museFileStruct.folder},    {museFileStruct.name});

            obj.PairMap = ERPBatchRunner.buildPairMap( ...
                biosemiPaths, ...
                musePaths, ...
                obj.ParseFcn, ...
                obj.BiosemiChannels, ...
                obj.MuseChannels);
        end
    end

    methods (Static)
        function obj = fromPathLists(biosemiPaths, musePaths, varargin)
            obj = ERPBatchRunner();

            obj.RootDir   = '';
            obj.ParseFcn  = @ERPBatchRunner.defaultParseFileName;

            argumentParser = inputParser;
            addParameter(argumentParser, 'Outcomes', {},  @ERPBatchRunner.isStringLike);
            addParameter(argumentParser, 'Events',   {},  @ERPBatchRunner.isStringLike); % alias for Outcomes
            addParameter(argumentParser, 'Sides',     {'left'}, @ERPBatchRunner.isStringLike);
            addParameter(argumentParser, 'ParseFcn',  obj.ParseFcn, @(x) isa(x,'function_handle'));

            parse(argumentParser, varargin{:});

            obj.Outcomes  = ERPBatchRunner.resolveOutcomes(argumentParser.Results.Outcomes, argumentParser.Results.Events);
            obj.Sides     = ERPBatchRunner.normalizeList(argumentParser.Results.Sides);
            obj.ParseFcn  = argumentParser.Results.ParseFcn;

            biosemiPaths = cellstr(biosemiPaths);
            musePaths    = cellstr(musePaths);

            obj.PairMap = ERPBatchRunner.buildPairMap( ...
                biosemiPaths, ...
                musePaths, ...
                obj.ParseFcn, ...
                {}, ...
                {});
        end

        function key = makeKey(event, biosemiChannel, museChannel)
            key = sprintf('%s|%s|%s', char(event), char(biosemiChannel), char(museChannel));
        end

        function [event, channel] = defaultParseFileName(fileName)
            % Placeholder; you should replace this or pass your own ParseFcn.
            % Example expected pattern:
            %   feedback_time_punishment_C4.parquet
            %   -> event = 'punishment', channel = 'C4'

            [~, baseName, ~] = fileparts(fileName);

            nameParts = strsplit(baseName, '_');
            if numel(nameParts) >= 3
                event   = nameParts{end-1};
                channel = nameParts{end};
            else
                error('defaultParseFileName:badFormat', ...
                    'Filename "%s" does not match expected pattern.', fileName);
            end
        end

        function pairMap = buildPairMap( ...
                biosemiPaths, ...
                musePaths, ...
                parseFcn, ...
                biosemiChannels, ...
                museChannels)

            pairMap = containers.Map('KeyType', 'char', 'ValueType', 'any');

            biosemiChannels = cellstr(biosemiChannels);
            museChannels    = cellstr(museChannels);

            useBiosemiChannelFilter = ~isempty(biosemiChannels);
            useMuseChannelFilter    = ~isempty(museChannels);

            % Parse and filter Biosemi files
            biosemiEntries = struct('event', {}, 'channel', {}, 'path', {});
            for biosemiIndex = 1:numel(biosemiPaths)
                filePath = biosemiPaths{biosemiIndex};
                [~, fileNameWithoutExtension, fileExtension] = fileparts(filePath);
                [event, channel] = parseFcn([fileNameWithoutExtension fileExtension]);

                if useBiosemiChannelFilter && ~any(strcmpi(channel, biosemiChannels))
                    continue;
                end
                biosemiEntries(end+1) = struct( ... %#ok<AGROW>
                    'event',   event, ...
                    'channel', channel, ...
                    'path',    filePath);
            end

            % Parse and filter Muse files
            museEntries = struct('event', {}, 'channel', {}, 'path', {});
            for museIndex = 1:numel(musePaths)
                filePath = musePaths{museIndex};
                [~, fileNameWithoutExtension, fileExtension] = fileparts(filePath);
                [event, channel] = parseFcn([fileNameWithoutExtension fileExtension]);

                if useMuseChannelFilter && ~any(strcmpi(channel, museChannels))
                    continue;
                end
                museEntries(end+1) = struct( ... %#ok<AGROW>
                    'event',   event, ...
                    'channel', channel, ...
                    'path',    filePath);
            end

            % Build pairs by matching events while allowing different channel labels per system
            for b = 1:numel(biosemiEntries)
                for m = 1:numel(museEntries)
                    if ~strcmpi(biosemiEntries(b).event, museEntries(m).event)
                        continue;
                    end
                    key = ERPBatchRunner.makeKey( ...
                        biosemiEntries(b).event, ...
                        biosemiEntries(b).channel, ...
                        museEntries(m).channel);
                    pairMap(key) = struct( ...
                        'biosemi', biosemiEntries(b).path, ...
                        'muse',    museEntries(m).path);
                end
            end
        end

        function isValid = isStringLike(value)
            isValid = ischar(value) || isstring(value) || ...
                (iscell(value) && all(cellfun(@(element) ischar(element) || isstring(element), value)));
        end

        function list = normalizeList(value)
            if isempty(value)
                list = {};
                return;
            end
            list = cellstr(value);
            list = list(~cellfun(@isempty, list));
        end

        function outcomes = resolveOutcomes(outcomesParam, eventsParam)
            outcomes = ERPBatchRunner.normalizeList(outcomesParam);
            if isempty(outcomes)
                outcomes = ERPBatchRunner.normalizeList(eventsParam);
            end
        end
    end
end
