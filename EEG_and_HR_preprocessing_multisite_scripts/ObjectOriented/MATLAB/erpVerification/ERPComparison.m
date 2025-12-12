classdef ERPComparison < handle
    properties (SetAccess = private)
        Biosemi ERPTable
        Muse    ERPTable
        biosemiGrouped
        museGrouped
    end
    
    methods
        function obj = ERPComparison(biosemiTable, museTable)
            if ~isa(biosemiTable, 'ERPTable') || ~isa(museTable, 'ERPTable')
                error('ERPComparison:InvalidInput', 'Both inputs must be ERPTable objects.');
            end
            obj.Biosemi = biosemiTable;
            obj.Muse    = museTable;

            if obj.Muse.HasSideColumn && ~obj.Muse.IsMuseChannel
                warning('ERPComparison:SideColumnNonMuse', ...
                    'Muse object has side information but channel is not marked as MUSE.');
            end
            if obj.Biosemi.HasSideColumn
                warning('ERPComparison:BiosemiHasSide', ...
                    'Biosemi object includes a side column; side filtering will be ignored.');
            end
            if obj.Biosemi.event ~= obj.Muse.event
                warning('ERPComparison:EventMismatch', ...
                    'Events differ between inputs (%s vs %s). Time alignment uses provided timeBin.', ...
                    obj.Biosemi.event, obj.Muse.event);
            end
            obj.biosemiGrouped = obj.getAverageByOutcome(obj.Biosemi.Data);
            obj.Biosemi.Data = [];
            obj.museGrouped = obj.getAverageByOutcome(obj.Muse.Data);
            obj.Muse.Data = [];

        end
        
        function plotComparison(obj, traces, sideLabel, museOnRightAxis, flipMuse, savePlot)
            if nargin < 2 || isempty(traces)
                traces = obj.defaultTraceList();
            end
            if nargin < 3
                sideLabel = string.empty;
            end
            if nargin < 4 || isempty(museOnRightAxis)
                museOnRightAxis = true;
            end
            if nargin < 5 || isempty(flipMuse)
                flipMuse = true;
            end
            if nargin < 6 || isempty(savePlot)
                savePlot = true;
            end
            
            traces = obj.normalizeTraces(traces);
            sideLabel = obj.normalizeMuseSide(sideLabel);
            
            sideValues = lower(string(obj.museGrouped.side));
            museData = obj.museGrouped(sideValues == sideLabel, :);

            if flipMuse
                museData.mean_signal = -museData.mean_signal;
            end
            
            bioWaves  = obj.computeWaveforms(obj.biosemiGrouped);
            museWaves = obj.computeWaveforms(museData);
            
            bioChanLabel = string(obj.Biosemi.channel);
            museChanLabel = string(obj.Muse.channel);
            if obj.Muse.HasSideColumn && ~isempty(sideLabel)
                museChanLabel = sprintf('%s (%s)', museChanLabel, sideLabel);
            end
            
            [colorMap, styleBio, styleMuse] = obj.buildStyles(traces);
            
            obj.renderComparison(bioWaves, museWaves, traces, museOnRightAxis, bioChanLabel, museChanLabel, colorMap, styleBio, styleMuse, savePlot, sideLabel);
        end
    end
    
    methods (Access = private)
        function traceList = defaultTraceList(~)
            traceList = ["punishment","neutral","reward","reward-neutral","punishment-neutral"];
        end
        
        function traces = normalizeTraces(obj, traces)
            valid = obj.defaultTraceList();
            if ischar(traces) || isstring(traces)
                traces = string(traces);
            elseif iscell(traces)
                traces = string(traces);
            end
            traces = lower(string(traces));
            if isempty(traces)
                traces = obj.defaultTraceList();
            end
            if ~all(ismember(traces, valid))
                bad = traces(~ismember(traces, valid));
                error('ERPComparison:InvalidTrace', ...
                    'Invalid trace selection: %s', strjoin(bad, ', '));
            end
        end
        
        function sideLabel = normalizeMuseSide(obj, sideLabel)
            if obj.Muse.HasSideColumn
                if nargin < 2 || isempty(sideLabel)
                    error('ERPComparison:SideRequired', ...
                        'Muse data requires side selection ("left" or "right").');
                end
                sideLabel = lower(string(sideLabel));
                if ~ismember(sideLabel, ["left","right"])
                    error('ERPComparison:InvalidSide', ...
                        'Invalid side "%s". Use "left" or "right".', sideLabel);
                end
            else
                sideLabel = string.empty;
            end
        end
        
        function groupedAverage = getAverageByOutcome(~,T)
            if ismember('side', T.Properties.VariableNames)
                groupedAverage = groupsummary(T, {'side','block','id','outcome','timeBin'}, "mean", "signal");
                groupedAverage.Properties.VariableNames{'mean_signal'} = 'signal';
    
                groupedAverage = groupsummary(groupedAverage, {'side','id','outcome','timeBin'}, "mean", "signal");
                groupedAverage.Properties.VariableNames{'mean_signal'} = 'signal';
                groupedAverage = groupsummary(groupedAverage, {'side','outcome','timeBin'}, "mean", "signal");
            else
                groupedAverage = groupsummary(T, {'block','id','outcome','timeBin'}, "mean", "signal");
                groupedAverage.Properties.VariableNames{'mean_signal'} = 'signal';
    
                groupedAverage = groupsummary(groupedAverage, {'id','outcome','timeBin'}, "mean", "signal");
                groupedAverage.Properties.VariableNames{'mean_signal'} = 'signal';
                groupedAverage = groupsummary(groupedAverage, {'outcome','timeBin'}, "mean", "signal");
            end
        end

        function waves = computeWaveforms(~, G)

            condCodes  = [-1 0 1];
            condLabels = {'Punishment','Neutral','Reward'};
            
            conditions = struct('code', {}, 'label', {}, 'time', {}, 'signal', {});
            for k = 1:3
                code = condCodes(k);
                Gi = G(G.outcome == code, :);
                if isempty(Gi)
                    conditions(k).code = code; %#ok<*AGROW>
                    conditions(k).label = condLabels{k};
                    conditions(k).time = [];
                    conditions(k).signal = [];
                    continue;
                end
                [tSorted, idx] = sort(Gi.timeBin);
                y = Gi.mean_signal(idx);
                conditions(k).code = code;
                conditions(k).label = condLabels{k};
                conditions(k).time = tSorted;
                conditions(k).signal = y;
            end
            
            rewardNeutral.time = [];
            rewardNeutral.signal = [];
            punishmentNeutral.time = [];
            punishmentNeutral.signal = [];
            
            if ~isempty(conditions(3).time) && ~isempty(conditions(2).time)
                [tCommon, ia, ib] = intersect(conditions(3).time, conditions(2).time);
                if ~isempty(tCommon)
                    rewardNeutral.time = tCommon;
                    rewardNeutral.signal = conditions(3).signal(ia) - conditions(2).signal(ib);
                end
            end
            if ~isempty(conditions(1).time) && ~isempty(conditions(2).time)
                [tCommon, ia, ib] = intersect(conditions(1).time, conditions(2).time);
                if ~isempty(tCommon)
                    punishmentNeutral.time = tCommon;
                    punishmentNeutral.signal = conditions(1).signal(ia) - conditions(2).signal(ib);
                end
            end
            
            waves.conditions = conditions;
            waves.rewardNeutral = rewardNeutral;
            waves.punishmentNeutral = punishmentNeutral;
        end
        
        function [colorMap, styleBio, styleMuse] = buildStyles(obj, traces)
            singleTrace = numel(traces) == 1;
            palette = lines(max(5, numel(traces) + 1));
            colorMap = containers.Map('KeyType','char','ValueType','any');
            
            if singleTrace
                key = char(traces(1));
                colorMap(key) = struct('bio', palette(1,:), 'muse', palette(2,:));
                styleBio = '-';
                styleMuse = '-';
            else
                for i = 1:numel(traces)
                    key = char(traces(i));
                    colorMap(key) = struct('bio', palette(i,:), 'muse', palette(i,:));
                end
                styleBio = '-';
                styleMuse = '--';
            end
        end
        
        function renderComparison(obj, bioWaves, museWaves, traces, museOnRightAxis, bioChanLabel, museChanLabel, colorMap, styleBio, styleMuse, savePlot, sideLabel)
            fName = sprintf('Momentum Validation - %s time', obj.Biosemi.event);
            singleTrace = numel(traces) == 1;
            fig = figure('Name', fName, 'Color', 'w');
            ax = gca;
            baseFont = 12;
            titleFont = 14;
            yyaxis left;
            hBio = obj.plotWaveSet(ax, bioWaves, traces, 'Biosemi', bioChanLabel, 1.5, colorMap, 'bio', styleBio, singleTrace);
            
            if museOnRightAxis
                yyaxis right;
            end
            hMuse = obj.plotWaveSet(ax, museWaves, traces, 'Muse', museChanLabel, 1.2, colorMap, 'muse', styleMuse, singleTrace);
            
            if museOnRightAxis
                yyaxis left;
                ylabel('Biosemi amplitude');
                yyaxis right;
                ylabel('Muse amplitude');
                yyaxis left;
            else
                ylabel('EEG amplitude');
            end
            xlabel(sprintf('Time peri %s (ms)', obj.Biosemi.event));
            title(fName, 'FontSize', titleFont, 'FontWeight', 'bold');
            subText = strjoin(string(traces), ' | ');
            subtitle(subText, 'FontSize', baseFont);
            grid on;
            ax.FontSize = baseFont;
            ax.XLabel.FontSize = baseFont;
            ax.YLabel.FontSize = baseFont;
            
            handles = [hBio(:); hMuse(:)];
            handles = handles(isgraphics(handles));
            if ~isempty(handles)
                names = get(handles, 'DisplayName');
                if ischar(names)
                    names = cellstr(names);
                end
                leg = legend(handles, names, 'Location', 'best');
                leg.FontSize = baseFont;
                leg.Box = 'off';
            end
            
            if savePlot
                outDir = fullfile(pwd, "generatedPlots", lower(string(obj.Biosemi.event)));
                if ~exist(outDir, 'dir')
                    mkdir(outDir);
                end
                fname = obj.buildFilename(traces, bioChanLabel, museChanLabel, sideLabel);
                outPath = fullfile(outDir, fname);
                try
                    exportgraphics(fig, outPath, 'Resolution', 150);
                catch
                    saveas(fig, outPath);
                end
            end
        end
        
        function handles = plotWaveSet(~,~, waves, traces, sourceLabel, channelLabel, lineWidthBase, colorMap, colorKey, lineStyle, singleTrace)
            handles = gobjects(0);
            
            function addHandle(h)
                if ~isempty(h) && all(isgraphics(h))
                    handles(end+1:end+numel(h)) = h;
                end
            end
            
            function h = plotCond(idx, traceKey)
                cond = waves.conditions(idx);
                if ismember(traceKey, traces) && ~isempty(cond.time)
                    c = colorMap(char(traceKey)).(colorKey);
                    if singleTrace
                        labelText = sprintf('%s (%s)', sourceLabel, channelLabel);
                    else
                        labelText = sprintf('%s (%s - %s)', cond.label, sourceLabel, channelLabel);
                    end
                    h = plot(cond.time, cond.signal, 'LineWidth', lineWidthBase, ...
                        'LineStyle', lineStyle, 'Color', c, ...
                        'DisplayName', labelText);
                else
                    h = gobjects(0);
                end
            end
            function h = plotDiff(diffData, labelText, traceKey)
                if ismember(traceKey, traces) && ~isempty(diffData.time)
                    c = colorMap(char(traceKey)).(colorKey);
                    if singleTrace
                        displayLabel = sprintf('%s (%s)', sourceLabel, channelLabel);
                    else
                        displayLabel = sprintf('%s (%s - %s)', labelText, sourceLabel, channelLabel);
                    end
                    h = plot(diffData.time, diffData.signal, lineStyle, 'LineWidth', lineWidthBase, ...
                        'Color', c, ...
                        'DisplayName', displayLabel);
                else
                    h = gobjects(0);
                end
            end
            
            hold on;
            addHandle(plotCond(1, "punishment"));
            addHandle(plotCond(2, "neutral"));
            addHandle(plotCond(3, "reward"));
            addHandle(plotDiff(waves.rewardNeutral, 'Reward - Neutral', "reward-neutral"));
            addHandle(plotDiff(waves.punishmentNeutral, 'Punishment - Neutral', "punishment-neutral"));
            handles = handles(isgraphics(handles));
        end
        
        function fname = buildFilename(obj, traces, bioChanLabel, museChanLabel, sideLabel)
            eventStr = lower(string(obj.Biosemi.event));
            bioStr = obj.sanitizeLabel(obj.Biosemi.channel);
            museStr = obj.sanitizeLabel(obj.Muse.channel);
            if obj.Muse.HasSideColumn && ~isempty(sideLabel)
                museStr = sprintf('%s_%s', museStr, obj.sanitizeLabel(sideLabel));
            end
            traceStr = obj.sanitizeLabel(strjoin(traces, '_'));
            fname = sprintf('%s_%s_%s_%s.jpg', eventStr, bioStr, museStr, traceStr);
        end
        
        function lbl = sanitizeLabel(~, lbl)
            lbl = lower(strrep(string(lbl), ' ', ''));
            lbl = regexprep(lbl, '[^A-Za-z0-9_-]', '');
        end
    end
end
