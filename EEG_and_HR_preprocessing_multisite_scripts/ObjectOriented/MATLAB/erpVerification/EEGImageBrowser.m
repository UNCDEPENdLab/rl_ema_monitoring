classdef EEGImageBrowser < handle

    properties
        ImgRoot            % root directory with event subfolders
        CapCsvPath         % full path to BIOSEMI_xyz.csv
        CapTable           % table with electrode,x,y,z
        X2D                % projected X coords
        Y2D                % projected Y coords
        
        EventFolders       % available event subfolder names
        CurrentEvent       % currently selected event name
        FileMap            % containers.Map: "channel|condition" -> full jpg path
        Conditions         % list of condition strings
        SideOptions        % list of side/device options
        
        SelectedElectrode  = ''
        SelectedFileChannel = ''
        
        Fig
        AxCap
        AxImage
        EventGroup
        ConditionGroup
        SideGroup
        StatusText
    end
    
    methods
        function obj = EEGImageBrowser(imgRoot, capCsvPath)
            % Constructor: set up paths, load cap, and scan events.
            
            % Image root (main folder with feedback/ choice/ stim/)
            if nargin < 1 || isempty(imgRoot)
                imgRoot = uigetdir(pwd, ...
                    'Select MAIN folder containing feedback/ choice/ stim/ subfolders');
                if isequal(imgRoot,0)
                    error('EEGImageBrowser:UserCancelled', ...
                          'User cancelled image root selection.');
                end
            end
            obj.ImgRoot = imgRoot;
            
            % Cap CSV path (BIOSEMI_xyz.csv) - default relative to repo root
            if nargin < 2 || isempty(capCsvPath)
                capCsvPath = obj.getDefaultCapCsvPath();
                if ~exist(capCsvPath,'file')
                    warning('EEGImageBrowser:CapCsvNotFound', ...
                        ['Default cap CSV not found at:\n  %s\n' ...
                         'You will be prompted to select it.'], capCsvPath);
                    [csvName, csvPath] = uigetfile({'*.csv','CSV files (*.csv)'}, ...
                        'Select BIOSEMI cap CSV (Electrode,x,y,z)');
                    if isequal(csvName,0)
                        error('EEGImageBrowser:UserCancelled', ...
                              'User cancelled cap CSV selection.');
                    end
                    capCsvPath = fullfile(csvPath, csvName);
                end
            end
            obj.CapCsvPath = capCsvPath;
            
            % Load cap table and events
            obj.loadCapTable();
            obj.initEvents();
        end
        
        function launch(obj)
            %LAUNCH Build and show the GUI.
            obj.buildGui();
        end
    end
    
    methods (Access = private)
        function capCsvPath = getDefaultCapCsvPath(obj) %#ok<MANU>
            % Compute default path to BIOSEMI_xyz.csv relative to this file.
            %
            % Assumes repository layout:
            %   repoRoot/
            %       BIOSEMI_xyz.csv
            %       ObjectOriented/
            %           MATLAB/
            %               erp_verification/
            %                   EEGImageBrowser.m   <-- this file
            thisFileDir = fileparts(which('EEGImageBrowser'));
            currDir = thisFileDir;
            repoRoot = '';
            while true
                [parentDir, folderName] = fileparts(currDir);
                if isempty(parentDir) || strcmp(currDir, parentDir)
                    break;
                end
                if strcmp(folderName, 'ObjectOriented')
                    repoRoot = parentDir;
                    break;
                end
                currDir = parentDir;
            end
            if isempty(repoRoot)
                % Fallback: one level above current dir
                repoRoot = fileparts(thisFileDir);
            end
            capCsvPath = fullfile(repoRoot, 'BIOSEMI_xyz.csv');
        end
        
        function loadCapTable(obj)
            % Load and standardize the cap CSV, then compute 2D projection.
            try
                T = readtable(obj.CapCsvPath, 'FileType', 'text');
            catch ME
                error('EEGImageBrowser:CapCsvReadError', ...
                    'Could not read cap CSV (%s): %s', obj.CapCsvPath, ME.message);
            end
            
            T = obj.standardizeCapTable(T);
            obj.CapTable = T;
            
            % Simple top-view projection: center and normalize x/y into unit circle
            x = T.x;
            y = T.y;
            x = x - mean(x);
            y = y - mean(y);
            scale = max(sqrt(x.^2 + y.^2));
            if scale == 0
                scale = 1;
            end
            obj.X2D = x / scale;
            obj.Y2D = y / scale;
        end
        
        function T = standardizeCapTable(obj, T) %#ok<INUSD>
            % Ensure we have columns: electrode, x, y, z (case-insensitive).
            origNames  = T.Properties.VariableNames;
            lowerNames = lower(origNames);
            
            idxElectrode = find(strcmp(lowerNames,'electrode') | strcmp(lowerNames,'label'), 1);
            idxX = find(strcmp(lowerNames,'x'), 1);
            idxY = find(strcmp(lowerNames,'y'), 1);
            idxZ = find(strcmp(lowerNames,'z'), 1);
            
            if isempty(idxElectrode) || isempty(idxX) || isempty(idxY) || isempty(idxZ)
                error('EEGImageBrowser:BadCapCsv', ...
                    'Cap CSV must contain columns: Electrode (or label), x, y, z.');
            end
            
            lowerNames{idxElectrode} = 'electrode';
            lowerNames{idxX} = 'x';
            lowerNames{idxY} = 'y';
            lowerNames{idxZ} = 'z';
            
            T = renamevars(T, origNames, lowerNames);
        end
        
        function initEvents(obj)
            % Locate available event subfolders and load the first with data.
            expectedEvents = {'feedback','choice','stim'};
            eventFolders = expectedEvents( ...
                cellfun(@(e) isfolder(fullfile(obj.ImgRoot, e)), expectedEvents));
            if isempty(eventFolders)
                error('EEGImageBrowser:NoEvents', ...
                    ['No event subfolders (feedback/ choice/ stim/) found in the ' ...
                     'selected folder:\n  %s'], obj.ImgRoot);
            end
            obj.EventFolders = eventFolders;
            
            obj.FileMap = [];
            obj.Conditions = {};
            obj.SideOptions = {};
            obj.CurrentEvent = '';
            
            for evIdx = 1:numel(eventFolders)
                trialEvent = eventFolders{evIdx};
                [fm, conds, sides] = obj.collectEventData(trialEvent);
                if ~isempty(fm)
                    obj.FileMap = fm;
                    obj.Conditions = conds;
                    obj.SideOptions = sides;
                    obj.CurrentEvent = trialEvent;
                    break;
                end
            end
            
            if isempty(obj.FileMap)
                error('EEGImageBrowser:NoImages', ...
                    'No parsable JPG files found in the event subfolders.');
            end
        end
        
        function buildGui(obj)
            % Build the figure, axes, and controls.
            fig = figure('Name','EEG Image Browser', ...
                         'NumberTitle','off', ...
                         'Color',[0.95 0.95 0.95], ...
                         'Units','normalized', ...
                         'Position',[0.1 0.1 0.8 0.8]);
            obj.Fig = fig;
            
            % Axes for cap
            axCap = axes('Parent',fig,'Position',[0.05 0.15 0.4 0.8]);
            obj.AxCap = axCap;
            hold(axCap,'on');
            axis(axCap,'equal');
            axis(axCap,[-1.1 1.1 -1.1 1.1]);
            axis(axCap,'off');
            title(axCap,'64-channel top view');
            
            % Head outline
            th = linspace(0,2*pi,200);
            plot(axCap, cos(th), sin(th), 'k-');
            
            % Plot electrodes
            T = obj.CapTable;
            numCh = height(T);
            for i = 1:numCh
                label = T.electrode{i};
                plot(axCap, obj.X2D(i), obj.Y2D(i), 'o', ...
                    'MarkerFaceColor',[0.2 0.4 0.9], ...
                    'MarkerEdgeColor','k', ...
                    'MarkerSize',8, ...
                    'ButtonDownFcn',@(src,evt)obj.onElectrodeClick(src,evt), ...
                    'UserData', label, ...
                    'HitTest','on', ...
                    'PickableParts','all');
                text(axCap, obj.X2D(i), obj.Y2D(i), [' ' label], ...
                    'VerticalAlignment','middle', ...
                    'HorizontalAlignment','left', ...
                    'FontSize',8, ...
                    'HitTest','off', ...
                    'PickableParts','none');
            end
            
            % Axes for image
            axImage = axes('Parent',fig,'Position',[0.5 0.2 0.45 0.75]);
            obj.AxImage = axImage;
            axis(axImage,'off');
            title(axImage,'Select an electrode and condition');
            
            % Bottom compact radio groups
            obj.EventGroup = uibuttongroup('Parent',fig, ...
                'Units','normalized', ...
                'Position',[0.05 0.02 0.25 0.15], ...
                'Title','Event', ...
                'SelectionChangedFcn',@(src,evt)obj.onEventChanged(src,evt));
            obj.ConditionGroup = uibuttongroup('Parent',fig, ...
                'Units','normalized', ...
                'Position',[0.35 0.02 0.25 0.15], ...
                'Title','Condition', ...
                'SelectionChangedFcn',@(src,evt)obj.onConditionChanged(src,evt));
            obj.SideGroup = uibuttongroup('Parent',fig, ...
                'Units','normalized', ...
                'Position',[0.65 0.02 0.25 0.15], ...
                'Title','File channel (side)', ...
                'SelectionChangedFcn',@(src,evt)obj.onFileChannelChanged(src,evt));
            
            obj.buildRadios(obj.EventGroup, obj.EventFolders, obj.CurrentEvent);
            
            defaultCond = '';
            if ~isempty(obj.Conditions)
                defaultCond = obj.Conditions{1};
            end
            obj.buildRadios(obj.ConditionGroup, obj.Conditions, defaultCond);
            
            defaultSideInit = obj.pickDefaultSide(obj.SideOptions, '');
            obj.SelectedFileChannel = defaultSideInit;
            obj.buildRadios(obj.SideGroup, obj.SideOptions, defaultSideInit);
            
            % Status text
            obj.StatusText = uicontrol('Style','text', ...
                'Parent',fig, ...
                'Units','normalized', ...
                'Position',[0.05 0.18 0.9 0.03], ...
                'String',sprintf('Event: %s. Pick condition/side, then click an electrode.', ...
                                 obj.CurrentEvent), ...
                'BackgroundColor',fig.Color, ...
                'HorizontalAlignment','left', ...
                'FontWeight','bold');
        end
        
        %--- Callbacks ----------------------------------------------------
        function onElectrodeClick(obj, src, ~)
            obj.SelectedElectrode = src.UserData;
            obj.refreshImage();
        end
        
        function onConditionChanged(obj, ~, ~)
            obj.refreshImage();
        end
        
        function onFileChannelChanged(obj, ~, event)
            choice = strtrim(event.NewValue.String);
            obj.SelectedFileChannel = choice;
            obj.refreshImage();
        end
        
        function onEventChanged(obj, ~, event)
            newEvent = strtrim(event.NewValue.String);
            obj.reloadEvent(newEvent);
        end
        
        %--- Logic --------------------------------------------------------
        function refreshImage(obj)
            conds = obj.getRadioOptions(obj.ConditionGroup);
            if isempty(conds)
                set(obj.StatusText, 'String', 'No conditions loaded for this event.');
                cla(obj.AxImage);
                axis(obj.AxImage,'off');
                return;
            end
            
            cond = obj.getSelectedRadio(obj.ConditionGroup);
            candidates = obj.channelCandidates();
            if isempty(candidates)
                set(obj.StatusText, 'String', ...
                    'Select a condition, side (optional), then click an electrode.');
                cla(obj.AxImage);
                axis(obj.AxImage,'off');
                return;
            end
            
            found = false;
            for ci = 1:numel(candidates)
                key = obj.makeKey(candidates{ci}, cond);
                if ~isempty(obj.FileMap) && isKey(obj.FileMap, key)
                    imgPath = obj.FileMap(key);
                    img = imread(imgPath);
                    imshow(img, 'Parent', obj.AxImage);
                    title(obj.AxImage, sprintf('%s | %s', candidates{ci}, cond), ...
                        'Interpreter','none');
                    set(obj.StatusText, 'String', sprintf('Showing: %s', imgPath));
                    found = true;
                    break;
                end
            end
            
            if ~found
                cla(obj.AxImage);
                axis(obj.AxImage,'off');
                text(obj.AxImage, 0.5, 0.5, 'No image found', ...
                    'HorizontalAlignment','center', ...
                    'VerticalAlignment','middle', ...
                    'FontWeight','bold');
                title(obj.AxImage, '');
                set(obj.StatusText, 'String', ...
                    sprintf('No image for %s + %s', strjoin(candidates,' / '), cond));
            end
        end
        
        function c = channelCandidates(obj)
            % Use electrode, optionally combined with side (e.g., cp1_temp_left).
            c = {};
            elec = strtrim(obj.SelectedElectrode);
            side = strtrim(obj.SelectedFileChannel);
            if isempty(elec)
                return;
            end
            if ~isempty(side)
                c{end+1} = sprintf('%s_%s', elec, side); %#ok<AGROW>
            end
            c{end+1} = elec; %#ok<AGROW>
        end
        
        function reloadEvent(obj, evName)
            [fm, conds, sides] = obj.collectEventData(evName);
            if isempty(fm)
                errordlg(sprintf('No parsable JPG files found under %s.', evName), 'No Images');
                return;
            end
            prevElec = obj.SelectedElectrode;
            prevSide = obj.SelectedFileChannel;
            obj.FileMap = fm;
            obj.Conditions = conds;
            obj.SideOptions = sides;
            obj.CurrentEvent = evName;
            obj.SelectedElectrode = prevElec; % keep same electrode selection
            
            % Rebuild radio groups
            obj.buildRadios(obj.EventGroup, obj.EventFolders, evName);
            
            defaultCond = '';
            if ~isempty(conds)
                defaultCond = conds{1};
            end
            obj.buildRadios(obj.ConditionGroup, conds, defaultCond);
            
            defaultSide = obj.pickDefaultSide(sides, prevSide);
            obj.SelectedFileChannel = defaultSide;
            obj.buildRadios(obj.SideGroup, sides, defaultSide);
            
            set(obj.StatusText, 'String', ...
                sprintf('Event: %s. Pick condition/side, then click an electrode.', evName));
            obj.refreshImage();
        end
        
        %--- Data collection & helpers -----------------------------------
        function [fileMap, conditions, sideOptions] = collectEventData(obj, eventName)
            % Scan one event subfolder for JPGs and build lookup tables.
            jpgs = dir(fullfile(obj.ImgRoot, eventName, '*.jpg'));
            fileMap = containers.Map('KeyType','char','ValueType','char'); % key: "channel|condition"
            conditions = {};
            sideOptions = {};
            if isempty(jpgs)
                fileMap = [];
                return;
            end
            for k = 1:numel(jpgs)
                fname = jpgs(k).name;
                [~, base, ~] = fileparts(fname);
                [chanLabel, cond] = obj.parseFilenameParts(base);
                if isempty(chanLabel) || isempty(cond)
                    continue;
                end
                key = obj.makeKey(chanLabel, cond);
                fileMap(key) = fullfile(obj.ImgRoot, eventName, fname);
                conditions{end+1} = cond; %#ok<AGROW>
                sideCandidate = obj.extractSide(chanLabel);
                if ~isempty(sideCandidate)
                    sideOptions{end+1} = sideCandidate; %#ok<AGROW>
                end
            end
            if isempty(fileMap)
                fileMap = [];
            end
            conditions = unique(conditions);
            sideOptions = unique(sideOptions);
        end
        
        function buildRadios(obj, group, options, selected)
            % Replace children with a vertical stack of radio buttons.
            delete(group.Children);
            n = numel(options);
            if n == 0, return; end
            h = 1/n;
            for i = 1:n
                uicontrol('Parent',group,'Style','radiobutton','Units','normalized', ...
                    'Position',[0.05 1-i*h 0.9 h], ...
                    'String',options{i});
            end
            if nargin >=4 && ~isempty(selected)
                radios = findobj(group,'Style','radiobutton');
                labels = get(radios,'String');
                if ischar(labels)
                    labels = cellstr(labels);
                end
                match = find(strcmpi(labels, selected), 1);
                if isempty(match), match = 1; end
                group.SelectedObject = radios(match);
            end
        end
        
        function out = getSelectedRadio(obj, group) %#ok<INUSD>
            sel = group.SelectedObject;
            if isempty(sel) || ~isprop(sel,'String')
                out = '';
            else
                out = sel.String;
            end
        end
        
        function opts = getRadioOptions(obj, group) %#ok<INUSD>
            radios = findobj(group,'Style','radiobutton');
            if isempty(radios)
                opts = {};
                return;
            end
            labels = get(radios,'String');
            if ischar(labels)
                opts = cellstr(labels);
            else
                opts = labels;
            end
        end
        
        function defaultSide = pickDefaultSide(obj, sides, prevSide) %#ok<INUSD>
            % Choose previous side if still available, else prefer 'temp_left', else first.
            defaultSide = '';
            if nargin >=3 && ~isempty(prevSide) && any(strcmpi(sides, prevSide))
                defaultSide = prevSide;
                return;
            end
            idx = find(strcmpi(sides, 'temp_left'), 1);
            if ~isempty(idx)
                defaultSide = sides{idx};
            elseif ~isempty(sides)
                defaultSide = sides{1};
            end
        end
        
        function [chanLabel, condition] = parseFilenameParts(obj, baseName) %#ok<INUSD>
            % Split by underscores; last token is condition; middle tokens (2..end-1) are channelLabel.
            % Example: feedback_cp1_temp_left_neutral -> chanLabel="cp1_temp_left", condition="neutral"
            parts = strsplit(baseName, '_');
            if numel(parts) < 2
                chanLabel = '';
                condition = '';
                return;
            end
            condition = parts{end};
            if numel(parts) >= 3
                chanLabel = strjoin(parts(2:end-1), '_');
            else
                chanLabel = parts{end-1};
            end
        end
        
        function side = extractSide(obj, chanLabel) %#ok<INUSD>
            % Extract side/device part after the first token (e.g., cp1_temp_left -> temp_left).
            parts = strsplit(chanLabel, '_');
            if numel(parts) >= 2
                side = strjoin(parts(2:end), '_');
            else
                side = '';
            end
        end
        
        function key = makeKey(obj, chanLabel, condition) %#ok<INUSD>
            % Lowercase + trimmed key for map indexing.
            key = [lower(strtrim(chanLabel)) '|' lower(strtrim(condition))];
        end
    end
end
