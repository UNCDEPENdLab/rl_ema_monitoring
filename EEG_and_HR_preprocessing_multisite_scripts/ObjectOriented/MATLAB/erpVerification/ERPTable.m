classdef ERPTable < handle
    properties (Access=public)
        Data
    end

    properties (SetAccess = private)
        
        FilePath
        event
        channel
        HasSideColumn logical = false
        IsMuseChannel logical = false
    end
    
    methods
        function obj = ERPTable(filePath)
            if nargin < 1 || isempty(filePath)
                error('ERPTable:FilePathRequired', ...
                    'You must provide a path to a parquet file.');
            end
            
            if ~isfile(filePath)
                error('ERPTable:FileNotFound', ...
                    'File not found: %s', filePath);
            end
            
            obj.FilePath = string(filePath); 
            [obj.event, obj.channel] = ERPTable.parseEegFilename(filePath);
            obj.IsMuseChannel = ERPTable.isMuseChannel(obj.channel);
            T = parquetread(obj.FilePath);
            
            requiredVars = ["block","trial","timeBin","signal","id","outcome"];
            haveVars = string(T.Properties.VariableNames);
            obj.HasSideColumn = ismember("side", haveVars);
            if obj.IsMuseChannel && ~obj.HasSideColumn
                error('ERPTable:MissingSide', ...
                    'MUSE channel "%s" requires a "side" column.', obj.channel);
            end
            if ~all(ismember(requiredVars, haveVars))
                error('ERPTable:MissingVariables', ...
                    'Input table must contain variables: %s', ...
                    strjoin(requiredVars, ', '));
            end
            
            if ~isa(T.id, "string")
                T.id = string(T.id);
            end
            if obj.HasSideColumn && ~isa(T.side, "string")
                T.side = string(T.side);
            end
            
            obj.Data = T;
        end
        
        function ids = listParticipants(obj)
            ids = unique(obj.Data.id);
        end
        
        function plotERPById(obj, participantId, byBlock, sideLabel, plotDiffOnRightAxis)
            if nargin < 3 || isempty(byBlock)
                byBlock = false;
            end
            if nargin < 4
                sideLabel = string.empty;
            end
            if nargin < 5 || isempty(plotDiffOnRightAxis)
                plotDiffOnRightAxis = true;
            end
            participantId = string(participantId);
            sideLabel = obj.normalizeSideLabel(sideLabel);
            
            Tsub = obj.getData(sideLabel);
            Tsub = Tsub(Tsub.id == participantId, :);
            if isempty(Tsub)
                warning('ERPTable:NoDataForId', ...
                    'No data found for participant "%s".', participantId);
                return;
            end
            
            figTitle = sprintf('ID %s - all blocks', participantId);
            if obj.HasSideColumn
                figTitle = sprintf('%s (%s)', figTitle, sideLabel);
            end
            
            if byBlock
                obj.localPlotByBlock(Tsub, participantId, sideLabel, plotDiffOnRightAxis);
            else
                obj.localPlotCollapsedBlocks(Tsub, figTitle, plotDiffOnRightAxis);
            end
        end
        
        function plotERPAllParticipants(obj, sideLabel, plotDiffOnRightAxis)
            if nargin < 2
                sideLabel = string.empty;
            end
            if nargin < 3 || isempty(plotDiffOnRightAxis)
                plotDiffOnRightAxis = true;
            end
            sideLabel = obj.normalizeSideLabel(sideLabel);
            Tdata = obj.getData(sideLabel);
            if isempty(Tdata)
                warning('ERPTable:NoData', 'No data available for the requested selection.');
                return;
            end
            
            figTitle = sprintf('All participants - all blocks channel: %s', obj.channel);
            if obj.HasSideColumn
                figTitle = sprintf('%s (%s)', figTitle, sideLabel);
            end
            obj.localPlotCollapsedBlocks(Tdata, figTitle, plotDiffOnRightAxis);
        end
        
        function G = getAveragedTable(obj, byBlock)
            if nargin < 2
                byBlock = false;
            end
            
            if byBlock
                grpVars = {'id','block','outcome','timeBin'};
            else
                grpVars = {'id','outcome','timeBin'};
            end
            
            G = groupsummary(obj.Data, grpVars, "mean", "signal");
        end

        function Tfiltered = getData(obj, sideLabel)
            if nargin < 2
                sideLabel = string.empty;
            end
            sideLabel = obj.normalizeSideLabel(sideLabel);
            Tfiltered = obj.applySideFilter(obj.Data, sideLabel);
        end

        function Tfiltered = applySideFilter(obj, T, sideLabel)
            if obj.HasSideColumn
                sideValues = lower(string(T.side));
                Tfiltered = T(sideValues == sideLabel, :);
            else
                Tfiltered = T;
            end
        end
    end
    
    methods (Access = private)
        function localPlotByBlock(obj, Tsub, participantId, sideLabel, plotDiffOnRightAxis)
            blocks  = unique(Tsub.block);
            nBlocks = numel(blocks);
            
            plotName = sprintf('ERP by block - ID %s', participantId);
            if obj.HasSideColumn
                plotName = sprintf('%s (%s)', plotName, sideLabel);
            end
            figure('Name', plotName, 'Color', 'w');
            
            for b = 1:nBlocks
                Tb = Tsub(Tsub.block == blocks(b), :);
                subplot(nBlocks, 1, b);
                obj.localPlotConditions(Tb, plotDiffOnRightAxis);
                blockTitle = sprintf('ID %s - Block %d', participantId, blocks(b));
                if obj.HasSideColumn
                    blockTitle = sprintf('%s (%s)', blockTitle, sideLabel);
                end
                title(blockTitle);
            end
        end
        
        function localPlotCollapsedBlocks(obj, T, figTitle, plotDiffOnRightAxis)
            figure('Name', figTitle, 'Color', 'w');
            obj.localPlotConditions(T, plotDiffOnRightAxis);
            title(figTitle);
        end
        
        function localPlotConditions(obj, T, plotDiffOnRightAxis)
            G = groupsummary(T, {'block','id','outcome','timeBin'}, "mean", "signal");
            G.Properties.VariableNames{'mean_signal'} = 'signal';

            G = groupsummary(G, {'id','outcome','timeBin'}, "mean", "signal");
            G.Properties.VariableNames{'mean_signal'} = 'signal';
            G = groupsummary(G, {'outcome','timeBin'}, "mean", "signal");
            condCodes  = [-1 0 1];
            condLabels = { ...
                'Punishment (-1)', ...
                'Neutral (0)',    ...
                'Reward (1)'};
            
            cols = lines(5);
            yyaxis left;
            hold on;
            
            times = cell(1,3);
            waves = cell(1,3);
            
            for k = 1:3
                code = condCodes(k);
                Gi = G(G.outcome == code, :);
                if isempty(Gi)
                    continue;
                end
                [tSorted, idx] = sort(Gi.timeBin);
                y = Gi.mean_signal(idx);
                times{k} = tSorted;
                waves{k} = y;
                
                plot(tSorted, y, 'LineWidth', 1.5, 'LineStyle', '-', ...
                    'Color', cols(k,:), ...
                    'DisplayName', condLabels{k});
            end
            
            plottedDiff = false;
            if ~isempty(waves{3}) && ~isempty(waves{2})
                if plotDiffOnRightAxis
                    yyaxis right;
                    hold on;
                end
                [tCommon, ia, ib] = intersect(times{3}, times{2});
                if ~isempty(tCommon)
                    diffWave = waves{3}(ia) - waves{2}(ib);
                    plot(tCommon, diffWave, '--', 'LineWidth', 1.5, ...
                        'Color', cols(4,:), ...
                        'DisplayName', 'Reward - Neutral');
                    plottedDiff = true;
                end
            end
            
            if ~isempty(waves{1}) && ~isempty(waves{2})
                if plotDiffOnRightAxis && ~plottedDiff
                    yyaxis right;
                    hold on;
                end
                [tCommon, ia, ib] = intersect(times{1}, times{2});
                if ~isempty(tCommon)
                    diffWave = waves{1}(ia) - waves{2}(ib);
                    plot(tCommon, diffWave, '--', 'LineWidth', 1.5, ...
                        'Color', cols(5,:), ...
                        'DisplayName', 'Punishment - Neutral');
                    plottedDiff = true;
                end
            end
            
            xlabel(sprintf('Time peri %s (ms)',obj.event));
            if plotDiffOnRightAxis && plottedDiff
                yyaxis left;
                ylabel('EEG amplitude');
                yyaxis right;
                ylabel('Difference amplitude');
                yyaxis left;
            else
                ylabel('EEG amplitude');
            end
            grid on;
            legend('show','Location','best');
        end
        
        function sideLabel = normalizeSideLabel(obj, sideLabel)
            if obj.HasSideColumn
                if nargin < 2 || isempty(sideLabel)
                    error('ERPTable:SideRequired', ...
                        'Side must be specified as "left" or "right" for channel "%s".', obj.channel);
                end
                sideLabel = lower(string(sideLabel));
                validSides = ["left","right"];
                if ~ismember(sideLabel, validSides)
                    error('ERPTable:InvalidSide', ...
                        'Invalid side "%s". Expected "left" or "right".', sideLabel);
                end
            else
                sideLabel = string.empty;
            end
        end

        
    end
    methods (Static)
        function T_filled = addOutcomeFromRef(T_noOut, T_ref)
            refKeys = unique(T_ref(:, {'id','block','trial','outcome'}));
            
            T_filled = outerjoin(T_noOut, refKeys, ...
                'Keys', {'id','block','trial'}, ...
                'Type', 'left', ...
                'MergeKeys', true, ...
                'RightVariables', 'outcome');
        end

        function [event, channel] = parseEegFilename(filePath)
            [~, name, ~] = fileparts(filePath);
        
            expr = '(stim|feedback|choice)_time_(.+)$';
            tokens = regexpi(name, expr, 'tokens', 'once');
        
            if isempty(tokens)
                error('Filename "%s" does not match pattern "<event>_time_<channel>.ext".', name);
            end
        
            event   = lower(tokens{1});
            channel = tokens{2};
        end
        
        function isMuse = isMuseChannel(channel)
            museChannels = ["temp","front"];
            isMuse = ismember(lower(string(channel)), museChannels);
        end

    end

end
