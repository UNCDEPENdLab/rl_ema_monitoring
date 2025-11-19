classdef TFPlotter < handle
    properties (Constant)
        srate = 256;
        WinsorizeThreshold = 7.5;

    end
    
    properties (Access=private)
        frequenciesOfInterest
    end

    properties
        directoryPath
        matPaths
        numberParticipants
        participantIDs
        individualParticipantsToPlot
        channelByFrequencyByTime
        epochByChannelByFrequency
        subsampleChannelByFrequencyByTime
    end

    methods
        function obj = TFPlotter(directoryPath)
            obj.frequenciesOfInterest = TimeFrequencyAnalyzer.frequenciesOfInterest;            
            obj.validateInputPath(directoryPath);
        end
        
        function runAnalyses(obj,typeOfPlots,individualParticipantsToPlot)
            obj.individualParticipantsToPlot = individualParticipantsToPlot;
            obj.collectData();
            obj.weightByFrequency();
            if ismember("individualByTime",typeOfPlots)
                obj.subsampleChannelByFrequencyByTime = obj.channelByFrequencyByTime(1:obj.individualParticipantsToPlot);
            end


            obj.winsorizeAcrossSubjects(false);

            %% Normalize frequency dimension
            obj.subsampleChannelByFrequencyByTime = cellfun(@(x) obj.zscoreFreq(x,2), obj.subsampleChannelByFrequencyByTime,'UniformOutput',false);
            obj.epochByChannelByFrequency = cellfun(@(x) obj.zscoreFreq(x,3), obj.epochByChannelByFrequency,'UniformOutput',false);

        end

    end
    
    methods (Access = private)
        function weightByFrequency(obj)
            %% Normalize data by frequency bin multiplying 1/freq times power
            obj.channelByFrequencyByTime = cellfun(@(singleParticipantData) obj.normalizeSingleParticipant(singleParticipantData,1), obj.channelByFrequencyByTime,'UniformOutput',false);
            obj.epochByChannelByFrequency = cellfun(@(x) obj.normalizeSingleParticipant(x,2), obj.epochByChannelByFrequency,'UniformOutput',false);

        end
        
        function normalizedParticipantData = normalizeSingleParticipant(obj,participantData,nChanDim)
            if nargin<3; nChanDim = 1;end
            % participantData: [nEpochs × nChan × nFreq] or [nChan x nFreq x nTime]
        
            nChan = size(participantData,nChanDim);
            if nChanDim==1
                normalizedParticipantData = participantData ./ repmat(obj.frequenciesOfInterest, nChan, 1);
            elseif nChanDim ==2
                F = reshape(obj.frequenciesOfInterest, [1, 1, numel(obj.frequenciesOfInterest)]);
                normalizedParticipantData = participantData ./ F;
            end
        end

        function collectData(obj)
            obj.participantIDs     = cell(obj.individualParticipantsToPlot,1);
            obj.epochByChannelByFrequency = cell(obj.individualParticipantsToPlot,1);

            for iSub = 1:obj.numberParticipants
                m = matfile(obj.matPaths{iSub});
                if ~isprop(m, 'TF') && ~isfield(m, 'TF')
                    error('File "%s" does not contain variable TF.', obj.matPaths{iSub});
                end
                TF = m.TF;  % loads full array: epochs × channels × freqs × time
                %% Collect Ids
                [~, name, ~] = fileparts(obj.matPaths{iSub});
                obj.participantIDs{iSub} = strtok(name, '_');

                %% Collect the subject average channels x frequency x time
                obj.channelByFrequencyByTime{iSub} = squeeze(nanmean(TF,1));   % → channels x frequency x time
        
                %% Collect subject average: epochs x channels x frequency
                subjAvg= squeeze(nanmean(TF,4));      % → epoch x channels  x frequency
                if iSub<=nExampleSubjects
                    obj.epochByChannelByFrequency{iSub} = subjAvg; 
                end
        
                fprintf("%i of %i completed \n",iSub,nSubs);
            end
        end

        function getMatFiles(obj)
        
            if ischar(obj.directoryPath) || isstring(obj.directoryPath)
                obj.directoryPath = char(obj.directoryPath);
            end
            files = dir(fullfile(obj.directoryPath, '*.mat'));
            obj.matPaths = fullfile({files.folder}, {files.name});
            
            if ischar(obj.matPaths) || isstring(obj.matPaths)
                obj.matPaths = cellstr(obj.matPaths);
            end
            obj.numberParticipants = numel(obj.matPaths);

        end

        function Xout = local_winsorize(obj,X)
            % X is [nTime×nSubs], treat all elements as one big vector
            x   = X(:);
            med = median(x, 'omitnan');
            mad = median(abs(x-med), 'omitnan')/0.6745;
            if mad==0, mad = std(x, 'omitnan'); end
        
            z = (x - med)./mad;
            z(z> obj.WinsorizeThreshold ) =  obj.WinsorizeThreshold;
            z(z<-obj.WinsorizeThreshold) = -obj.WinsorizeThreshold;
            xw = z.*mad + med;
        
            m = mean(xw, 'omitnan');
            s = std(xw, 'omitnan');
            if s==0, s=1; end
        
            xz = (xw - m)./s;
            Xout = reshape(xz, size(X));
        end

        function validateInputPath(obj, directoryPath)
            % Place holder to verify if receiving a single file or a path
            % to look for all dirs
            obj.directoryPath = directoryPath;
            obj.getMatFiles();
        end
        
        function winsorizeAcrossSubjects(obj,subsOnly)

            if nargin<2 || isempty(subsOnly), subsOnly = false; end
        
            if subsOnly
                %---- 3D case: each cell is [nChan×nFreq], cell array length = nSubs
                % stack into [nChan×nFreq×nSubs]
                % A = cat(3, dataCell{:});
                A=obj.channelByFrequencyByTime;
                [nChan,nFreq,nSubs] = size(A);
                B = nan(size(A));
        
                for ch = 1:nChan
                  for f = 1:nFreq
                    x = squeeze(A(ch,f,:));     % vector across subjects
                    med    = nanmedian(x);
                    madVal = nanmedian(abs(x-med))/0.6745;
                    if madVal==0, madVal = nanstd(x); end
        
                    z = (x - med)./madVal;
                    z(z> obj.WinsorizeThreshold) =  obj.WinsorizeThreshold;
                    z(z<-obj.WinsorizeThreshold) = -obj.WinsorizeThreshold;
                    xw = z.*madVal + med;       % back to raw scale
        
                    m = nanmean(xw);
                    s = nanstd(xw);
                    if s==0, s = 1; end
                    B(ch,f,:) = (xw - m)./s;
                  end
                end
        
                % split back into cell array
                obj.channelByFrequencyByTime = cell(nSubs,1);
                for k = 1:nSubs
                    obj.channelByFrequencyByTime{k} = B(:,:,k);
                end
        
            else
                %---- 4D case: each cell is [nChan×nFreq×nTime], length = nSubs
                % stack into 4-D: Chan × Freq × Time × Subj
                allData = cat(4, obj.channelByFrequencyByTime{:});
                [nChan,nFreq,nTime,nSubs] = size(allData);
        
                % flatten each (ch,f) into a cell of [nTime×nSubs]
                [Ch, Fr] = ndgrid(1:nChan, 1:nFreq);
                C = arrayfun(@(c,f) squeeze(allData(c,f,:,:)), ...
                             Ch, Fr, ...
                             'UniformOutput', false);
        
                % winsorize each block
                Cclean = cellfun(@(X) obj.local_winsorize(X), ...
                                 C, 'UniformOutput', false);
        
                % rebuild 4D array
                B = nan(nChan,nFreq,nTime,nSubs);
                for ch = 1:nChan
                  for f = 1:nFreq
                    B(ch,f,:,:) = Cclean{ch,f};
                  end
                end
        
                % split back into cell array [nChan×nFreq×nTime]
                obj.channelByFrequencyByTime = cell(nSubs,1);
                for k = 1:nSubs
                    obj.channelByFrequencyByTime{k} = squeeze(B(:,:,:,k));
                end
            end
        end


        function dataZ = zscoreFreq(data3d,dimensionToScale)

            if ~isnumeric(data3d) || ndims(data3d)~=3
                error('Input must be a 3-D numeric array [nChan×nFreq×nTime].');
            end
        
            % compute mean & std along dim-2 (frequency)
            mu    = mean(data3d, dimensionToScale, 'omitnan');   % size = [nChan×1×nTime]
            sigma = std( data3d, 0, dimensionToScale, 'omitnan'); % size = [nChan×1×nTime]
        
            % guard against sigma==0
            sigma(sigma==0) = 1;
        
            % implicit expansion (R2016b+)
            dataZ = (data3d - mu) ./ sigma;
        end

    end

end