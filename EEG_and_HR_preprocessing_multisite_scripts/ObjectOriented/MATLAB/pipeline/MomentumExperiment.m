classdef MomentumExperiment < handle
    properties
        
    end

    methods
        function obj = Utils()
        end
    end

    methods (Static)
        
        function preprocessedDirs = buildPreprocessedDirStruct(participantRawDir,preprocEEGDir)
            if ~isempty(preprocEEGDir)
                [~, participantId, ~] = fileparts(participantRawDir);
                preprocDir = fullfile(preprocEEGDir, 'Data_Processed', sprintf('subject_%s', participantId));
                preprocessedDirs = struct('EEG_muse', preprocDir);
            else
                preprocessedDirs = struct();
            end
        end

    end

end
