classdef NameSchema
    properties (Constant)
        TOK_BLOCK = "blockBin";
        TOK_FREQ    = "freqBin";
        DEFAULT_EXT = ".parquet";
    end

    methods (Static)
        function fileName = format(opts)
            arguments
                opts.participantId   = "123456"
                opts.eventName       = "feedback"
                opts.section         = ""          % string|char|numeric(0 to omit)
                opts.freqLabel       = []          % numeric|char|string
                opts.binningMode     = "byTimepoints"
                opts.timeBinIdx      = 0
                opts.blockBinIdx   = 0
                opts.channelLabel    = ""
                opts.extension       = NameSchema.DEFAULT_EXT
                opts.asPattern       = false
                opts.dataType        = "TF"   
            end

            timeBinLabel = NameSchema.validateTimeBinningMode(opts.binningMode);

            % (2) Prefix (participant vs pattern), optional section
            hasSection = NameSchema.hasNonEmpty(opts.section);

            if opts.asPattern
                if hasSection
                    fmt  = "%s_%s";
                    args = { opts.eventName, string(opts.section) };
                else
                    fmt  = "%s";
                    args = { opts.eventName };
                end
            else
                if hasSection
                    fmt  = "%s_%s_%s";
                    args = { opts.participantId, opts.eventName, string(opts.section) };
                else
                    fmt  = "%s_%s";
                    args = { opts.participantId, opts.eventName };
                end
            end

            % (3) blockBin if provided
            if opts.blockBinIdx > 0
                fmt = fmt + "_" + NameSchema.TOK_BLOCK + "_%03d";
                args{end+1} = opts.blockBinIdx;
            end

            % (4) channel label if provided
            if opts.channelLabel ~= ""
                fmt = fmt + "_%s";
                args{end+1} = string(opts.channelLabel);
            end

            % (5) TF-specific frequency label
            if strcmpi(opts.dataType, "TF")
                frequencyString = NameSchema.validateFrequencyLabel(opts.freqLabel);
                fmt = fmt + "_" + NameSchema.TOK_FREQ + "_%s";
                args{end+1} = frequencyString;
            end

            % (6) Tail with time label + index if idx>0, else just extension
            if opts.timeBinIdx > 0
                fmt = fmt + "_%s_%03d%s";
                args{end+1} = timeBinLabel;
                args{end+1} = opts.timeBinIdx;
                args{end+1} = opts.extension;
            else
                fmt = fmt + "%s";
                args{end+1} = opts.extension;
            end

            fileName = sprintf(fmt, args{:});
        end
        
        function meta = parse(fileName, varargin)
           
            p = inputParser;
            addParameter(p, 'AssumePattern', false, @(x)islogical(x)||ismember(x,[0 1]));
            parse(p, varargin{:});
            assumePattern = logical(p.Results.AssumePattern);
        
            [~, base, ext] = fileparts(fileName);
            if ~strcmpi(ext, NameSchema.DEFAULT_EXT)
                error('NameSchema:Extension', ...
                    'File "%s" does not have "%s" extension.', fileName, NameSchema.DEFAULT_EXT);
            end
        
            parts  = split(string(base), "_");
            nParts = numel(parts);
            if nParts < 2
                error('NameSchema:BadFormat', ...
                    'Filename "%s" has too few underscore-separated parts.', fileName);
            end
        
            % Identify whether TF or EEG by presence of "freqBin"
            iFreq = find(parts == NameSchema.TOK_FREQ, 1, 'first');
            isTF  = ~isempty(iFreq);
        
            % Identify time tail if present (we accept missing tail if timeBinIdx==0)
            % Here we assume typical tails exist; if not, we set idx=0 and label=""
            timeBinIdx = NaN; timeBinLabel = "";
            if nParts >= 2
                maybeIdx = str2double(parts(end));
                if ~isnan(maybeIdx)
                    timeBinIdx   = maybeIdx;
                    timeBinLabel = parts(end-1);
                    tailSpan     = 2;
                else
                    timeBinIdx   = 0;
                    timeBinLabel = "";
                    tailSpan     = 0;
                end
            else
                tailSpan = 0;
            end
        
            % Helper to attempt parse with a given starting index for "middle" tokens
            function [m, ok] = tryParse(startIdx, expectTF)
                m = struct();
                m.fileName  = fileName;
                m.extension = ext;
                if startIdx == 3         % participant present
                    m.participantId = parts(1);
                    m.eventName     = parts(2);
                else                     % pattern form (no participant)
                    m.participantId = "";
                    m.eventName     = parts(1);
                end
        
                % Determine the "last index reserved by tail"
                lastIdx = nParts - tailSpan;
                if expectTF
                    if isempty(iFreq) || iFreq >= lastIdx
                        ok = false; return;
                    end
                    lastMetaBeforeFreq = iFreq - 1;
                    afterFreqLabel     = iFreq + 1;
                else
                    lastMetaBeforeFreq = lastIdx;
                    afterFreqLabel     = NaN;
                end
        
                % Defaults
                m.section        = "";
                m.blockBinIdx  = 0;
                m.channelLabel   = "";
                if expectTF
                    m.dataType = "TF";
                else
                    m.dataType = "EEG";
                end
                m.timeBinLabel   = timeBinLabel;
                m.timeBinIdx     = timeBinIdx;
                m.freqBinLabel   = "";
                m.freqBinIdx     = NaN;
        
                i = startIdx;
        
                % (A) Special case: allow eventName "*_time"
                % e.g. "feedback_time", "stim_time"
                if i <= lastMetaBeforeFreq && strcmpi(parts(i), "time")
                    m.eventName = m.eventName + "_time";
                    i = i + 1;
                end
        
                % (B) Section: ONLY "front" or "temp" are treated as section
                % anything else (that isn't "blockBin") is left for channelLabel
                if i <= lastMetaBeforeFreq && parts(i) ~= NameSchema.TOK_BLOCK
                    candidateSec = parts(i);
                    if ismember(candidateSec, ["front","temp"])
                        m.section = candidateSec;
                        i = i + 1;
                    end
                    % if not front/temp, we do NOT advance i here => token will be
                    % consumed later as part of channelLabel or blockBin
                end
        
                % (C) blockBin_%03d
                if i <= lastMetaBeforeFreq - 1 && parts(i) == NameSchema.TOK_BLOCK
                    sb = str2double(parts(i+1));
                    if isnan(sb), ok = false; return; end
                    m.blockBinIdx = sb;
                    i = i + 2;
                end
        
                % (D) channelLabel: anything left before freq/tail
                if i <= lastMetaBeforeFreq
                    m.channelLabel = strjoin(parts(i:lastMetaBeforeFreq), "_");
                end
        
                % TF-specific frequency label + derived idx (if numeric)
                if expectTF
                    if afterFreqLabel > lastIdx
                        ok = false; return;
                    end
                    m.freqBinLabel = parts(afterFreqLabel);
                    fIdx = str2double(m.freqBinLabel);
                    if ~isnan(fIdx), m.freqBinIdx = fIdx; end
                end
        
                % Infer binning mode from label (robust to custom labels)
                m.binningMode = NameSchema.inferBinningMode(m.timeBinLabel);
        
                % patternKey deliberately excludes varying indices
                m.patternKey = char(strjoin([ ...
                    string(m.participantId), ...
                    string(m.eventName), ...
                    string(m.section), ...
                    string(m.blockBinIdx), ...
                    string(m.channelLabel), ...
                    string(m.timeBinLabel), ...
                    string(m.dataType)], "|"));
        
                ok = true;
            end
        
            % Try normal head (participant present), then pattern head
            if ~assumePattern
                [meta, ok] = tryParse(3, isTF);
                if ~ok
                    [meta, ok2] = tryParse(2, isTF);
                    if ~ok2
                        error('NameSchema:Parse', 'Could not parse "%s".', fileName);
                    end
                end
            else
                [meta, ok] = tryParse(2, isTF);
                if ~ok
                    error('NameSchema:Parse', 'Could not parse "%s" as pattern form.', fileName);
                end
            end
        end

        function rx = regex(kind)
            % A permissive regex (dev aid) that matches current schema.
            % kind: "TF"|"EEG"|"any"
            arguments
                kind string = "any"
            end
            timeAlt = "(tpBin|tBin|byTimepoints|byTime|time|timeBin|timepoints)";
            head    = "(?<participantId>[^_]+)_(?<eventName>[^_]+)";
            headAlt = "(?<eventName>[^_]+)"; % pattern (no participant)
            sec     = "(?:_(?<section>(?!blockBin|freqBin)[^_]+))?";
            ses     = "(?:_blockBin_(?<blockBinIdx>\d{1,}))?";
            chan    = "(?:_(?<channelLabel>(?:(?!_freqBin_|_" + timeAlt + "_).)+))?";
            tail    = "(?:_(?<timeBinLabel>" + timeAlt + ")_(?<timeBinIdx>\d{1,}))?";
            ext     = "\.parquet$";

            tfFreq  = "_freqBin_(?<freqBinLabel>[^_]+)";

            switch lower(kind)
                case "tf"
                    core = "(?:" + head + "|" + headAlt + ")" + sec + ses + chan + tfFreq + tail;
                case "eeg"
                    core = "(?:" + head + "|" + headAlt + ")" + sec + ses + chan + tail;
                otherwise
                    core = "(?:" + head + "|" + headAlt + ")" + sec + ses + chan + "(?:" + tfFreq + ")?" + tail;
            end
            rx = "^" + core + ext;
        end

        function roundTripAssert(fileName)
            % Dev helper: parse -> rebuild -> parse again; throw if drift.
            m1 = NameSchema.parse(fileName);
            opts = NameSchema.buildOptsFromMeta(m1);
            f2 = NameSchema.format(opts);
            m2 = NameSchema.parse(f2);
            same = isequaln( rmfield(m1, {'fileName'}), rmfield(m2, {'fileName'}) );
            if ~same
                error('NameSchema:RoundTrip', 'Round-trip mismatch:\n  in : %s\n  out: %s', fileName, f2);
            end
        end

        function opts = buildOptsFromMeta(meta)
            % Convert a parsed meta struct back into format() opts (best effort).
            opts = struct();
            opts.participantId = string(meta.participantId);
            opts.eventName     = string(meta.eventName);
            opts.section       = string(meta.section);
            opts.blockBinIdx = double(meta.blockBinIdx);
            opts.channelLabel  = string(meta.channelLabel);
            opts.binningMode   = string(meta.binningMode);
            opts.timeBinIdx    = double(meta.timeBinIdx);
            opts.extension     = NameSchema.DEFAULT_EXT;
            opts.asPattern     = (opts.participantId == "");
            opts.dataType      = string(meta.dataType);
            if strcmpi(opts.dataType, "TF")
                if isfield(meta,'freqBinLabel') && ~isempty(meta.freqBinLabel)
                    opts.freqLabel = meta.freqBinLabel; % preserve raw label
                elseif isfield(meta,'freqBinIdx') && ~isnan(meta.freqBinIdx)
                    opts.freqLabel = sprintf('%02d', meta.freqBinIdx);
                else
                    opts.freqLabel = "01";
                end
            else
                opts.freqLabel = [];
            end
        end
    
        function participantId = validateParticipantId(fileName)
            digitsOnly = regexprep(fileName, '[^0-9]', '');
            
            if numel(char(digitsOnly)) >= 5 && numel(char(digitsOnly)) <= 6
                participantId = string(digitsOnly);
            else
                participantId = "";
            end
        end

        function binningMode = validateBinningMode(binningMode)        
            % Convert string objects to char
            if isa(binningMode,'string')
                binningMode = char(binningMode);
            end
            if ~ischar(binningMode)
                error('validateSavingMode:InvalidType', ...
                    'saveMode must be a character vector or string scalar.');
            end
        
            % Trim whitespace
            binningMode = strtrim(binningMode);
        
            % Check for "timepoints" variant:
            if ~isempty(regexp(binningMode, '^(?:by)?timepoints$', 'ignorecase'))
                binningMode = 'byTimepoints';
        
            % Check for "time" variant:
            elseif ~isempty(regexp(binningMode, '^(?:by)?time$', 'ignorecase'))
                binningMode = 'byTime';
        
            else
                error('validateSavingMode:InvalidValue', ...
                    'Invalid save mode "%s". Valid options are "byTime" or "byTimepoints" (case‐insensitive).', ...
                    binningMode);
            end
        end
    
        function saveMode = validateSavingMode(saveMode)

            % Convert string objects to char vector
            if isa(saveMode, 'string')
                saveMode = char(saveMode);
            end
            if ~ischar(saveMode)
                error('validateExportMode:InvalidType', ...
                    'exportMode must be a character vector or string scalar.');
            end
        
            % Trim leading/trailing whitespace
            saveMode = strtrim(saveMode);
        
            % Check for "mat" variant (with or without 'as')
            if ~isempty(regexp(saveMode, '^(?:as)?mat$', 'ignorecase'))
                saveMode = 'asMat';
        
            % Check for "csv" variant (with or without 'as')
            elseif ~isempty(regexp(saveMode, '^(?:as)?csv$', 'ignorecase'))
                saveMode = 'asCSV';
        
            % Check for "parquet" variant (with or without 'as')
            elseif ~isempty(regexp(saveMode, '^(?:as)?parquet$', 'ignorecase'))
                saveMode = 'asParquet';
        
            else
                error('validateExportMode:InvalidValue', ...
                    'Invalid export mode "%s". Valid options are "asMat", "asCSV", or "asParquet" (case‐insensitive).', ...
                    saveMode);
            end
        end
        
        function frequencyString = validateFrequencyLabel(frequencyLabel)
            if isnumeric(frequencyLabel)
                frequencyString = sprintf('%02d', frequencyLabel);
            else
                frequencyString = char(frequencyLabel);
            end
        end
    end

    methods (Static, Access = private)
        function isNonEmpty = hasNonEmpty(x)
            isNonEmpty = ( ...
               (isstring(x) && strlength(x)>0) || ...
               (ischar(x)   && ~isempty(x))    || ...
               (isnumeric(x)&& any(x~=0)) );
        end

        function timeBinLabel= validateTimeBinningMode(timeBinningMode)
            
            timeBinningMode = lower(string(timeBinningMode));
            if strcmp(timeBinningMode,"bytimepoints")
                timeBinLabel= "tpBin";
            elseif strcmp(timeBinningMode,"bytime")
                timeBinLabel = "tBin";
            else
                timeBinLabel = string(mode);
            end
        
        end

        

        function mode = inferBinningMode(timeBinLabel)
            lbl = lower(char(string(timeBinLabel)));
            if contains(lbl, "tpbin") || contains(lbl, "timepoints")
                mode = "byTimepoints";
            elseif contains(lbl, "tbin") || contains(lbl, "timebin") || contains(lbl, "time")
                mode = "byTime";
            else
                mode = "";
            end
        end
    end
end
