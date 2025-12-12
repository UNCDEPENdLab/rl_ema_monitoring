classdef ERPVerificationRunner < handle
    properties
        RootDirs
        BiosemiChannels
        MuseChannels
        Outcomes
        Sides
        ParseFcn
    end
    
    methods
        function obj = ERPVerificationRunner(mainRootOrDirs, varargin)
            if nargin < 1 || isempty(mainRootOrDirs)
                mainRootOrDirs = uigetdir(pwd, ...
                    'Select MAIN folder containing feedback/ choice/ stim/ subfolders');
                if isequal(mainRootOrDirs, 0)
                    error('ERPVerificationRunner:UserCancelled', ...
                          'User cancelled selection of MAIN root directory.');
                end
            end
            
            if iscell(mainRootOrDirs)
                obj.RootDirs = mainRootOrDirs;
            else
                mainRoot = char(mainRootOrDirs);
                obj.RootDirs = { ...
                    fullfile(mainRoot, 'feedback'), ...
                    fullfile(mainRoot, 'choice'), ...
                    fullfile(mainRoot, 'stim')    ...
                };
            end
            
            obj.BiosemiChannels = EEG_biosemi.biosemiChannels;
            obj.MuseChannels    = {'temp'};
            obj.Outcomes        = {'punishment-neutral','reward-neutral', ...
                                   'punishment','neutral','reward'};
            obj.Sides           = {'left','right'};
            obj.ParseFcn        = @ERPTable.parseEegFilename;
            
            if mod(numel(varargin),2) ~= 0
                error('ERPVerificationRunner:BadArgs', ...
                      'Name-value arguments must come in pairs.');
            end
            
            for k = 1:2:numel(varargin)
                name  = varargin{k};
                value = varargin{k+1};
                if ~ischar(name) && ~isstring(name)
                    error('ERPVerificationRunner:BadArgs', ...
                          'Parameter names must be char or string.');
                end
                switch lower(char(name))
                    case 'biosemichannels'
                        obj.BiosemiChannels = value;
                    case 'musechannels'
                        obj.MuseChannels = value;
                    case 'outcomes'
                        obj.Outcomes = value;
                    case 'sides'
                        obj.Sides = value;
                    case 'parsefcn'
                        obj.ParseFcn = value;
                    otherwise
                        error('ERPVerificationRunner:UnknownParam', ...
                              'Unknown parameter name: %s', name);
                end
            end
        end
        
        function run(obj)
            for dirIndex = 1:numel(obj.RootDirs)
                rootDir = obj.RootDirs{dirIndex};
                
                if ~isfolder(rootDir)
                    warning('ERPVerificationRunner:MissingDir', ...
                        'Root directory does not exist, skipping: %s', rootDir);
                    continue;
                end
                
                fprintf('ERPVerificationRunner: processing %s\n', rootDir);
                
                batchRunner = ERPBatchRunner(rootDir, ...
                    'BiosemiChannels', obj.BiosemiChannels, ...
                    'MuseChannels',    obj.MuseChannels, ...
                    'Outcomes',        obj.Outcomes, ...
                    'Sides',           obj.Sides, ...
                    'ParseFcn',        obj.ParseFcn);
                
                batchRunner.run();
            end
            
            fprintf('ERPVerificationRunner: done.\n');
        end
    end
end
