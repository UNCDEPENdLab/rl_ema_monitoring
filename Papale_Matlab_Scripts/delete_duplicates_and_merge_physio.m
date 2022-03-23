% Run this function only once, to prepare a subject's physio files for further processing
% INPUT: name - character string indicating subejct (e.g., '101')
function delete_duplicates_and_merge_physio(name,source)

curD = pwd;
switch source
    case {'Drive'}
    case {'Bierka'}
        path = strcat(curD,'/Data_Raw/',name);
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
                path = strcat(path,'/physio');
        else
            cd(curD);
            error('path %s not found',path);
        end
    case {'SharePoint'}
        subj_dir = '/Users/andypapale/OneDrive - University of Pittsburgh/Documents - Momentum/Momentum_App/data/Subjects';
        if exist(subj_dir,'dir')
            path = strcat(subj_dir,'/',name,'/physio');
            cd(path);
        else
            cd(curD);
            error('directory %s does not exist',strcat(subj_dir,'/',name,'/physio'));
        end
    otherwise
        cd(curD);
        error('unknown source %s',source);
end

cd(curD);

    newpath = fullfile(curD,strcat('/Data_Processed/subject_',name));
    %Utilities.deleteDuplicates(path, name);
    %Utilities.replaceHebrewCharacters(path, name);
    Utilities.mergeDatabases(path, newpath, name, 'physio');
end