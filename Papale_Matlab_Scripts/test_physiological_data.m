%This script runs a few functions to help you test the quality of your EEG and HR data
%the path for the raw data is assumed to be: fullfile(pwd,'Data_Raw',['subject_' name])

function [EEG_percen,HR_percen,bad] = test_physiological_data(name,source,date_range,block,redoHR)
%delete_duplicates_and_merge_physio(name,'Drive'); %merge all physio to one file

assert(ischar(name),'name must be a string');
assert(ischar(source),'source must be a string');
assert(iscell(date_range),'date_range must be a cell array');
%assert(length(date_range)==1 | length(date_range)==2,'date_range must be length 1 or 2');

curD = pwd;
switch source
    case {'Drive'}
    case {'Bierka'}
        path = strcat(curD,'/Data_Raw/',name(1:6));
        if exist(path,'dir')>0
            cd(path);
            D = dir;
            nD = length(D);
            sf_found = 0;
            pf_found = 0;
            for iD=1:nD
                if strcmp(D(iD).name,'schedule')
                    sf_found = 1;
                end
                if strcmp(D(iD).name,'physio')
                    pf_found = 1;
                end
                if sf_found && pf_found
                    break;
                end
            end
                if ~sf_found
                    cd(curD);
                    error('path %s not found',strcat(path,'/schedule'));
                end
                if ~pf_found
                    cd(curD);
                    error('path %s not found',strcat(path,'/physio'));
                end
        else
            cd(curD);
            error('path %s not found',path);
        end
    case {'SharePoint'}
        subj_dir = '/Users/andypapale/OneDrive - University of Pittsburgh/Documents - Momentum/Momentum_App/data/Subjects';
        if exist(subj_dir,'dir')
            path = strcat(subj_dir,'/',name);
            cd(path);
            D = dir;
            nD = length(D);
            sf_found = 0;
            pf_found = 0;
            for iD=1:nD
                if strcmp(D(iD).name,'schedule')
                    sf_found = 1;
                end
                if strcmp(D(iD).name,'physio')
                    pf_found = 1;
                end
                if sf_found && pf_found
                    break;
                end
            end
                if ~sf_found
                    cd(curD);
                    error('path %s not found','/schedule');
                end
                if ~pf_found
                    cd(curD);
                    error('path %s not found','/physio');
                end
        else
            cd(curD);
            error('directory %s does not exist',strcat(subj_dir,'/',name));
        end
    otherwise
        cd(curD);
        error('unknown source %s',source);
end
cd(curD);
[Ntotal, Ngood, ~, bad]=EEGanalysis_test(name,path,curD,date_range,block); %test EEG data
[HRoutcome, stats, HR] = getHRperOutcome(name, path,redoHR,curD,date_range,block);%test HR data
% calculate the percentage of trials with good data (seperate for EEG and
% HR). Should be above 70%.
%bad = NaN;
EEG_percen=NaN;
EEG_percen=(Ngood/Ntotal)*100;
HR_percen=NaN;
HR_percen=((stats.Ntrials-stats.Ntrials_missing-stats.Ntrials_noisy)/stats.Ntrials)*100
cd(curD);
%close all;
end