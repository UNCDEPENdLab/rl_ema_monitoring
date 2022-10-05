% Run this function only once, to prepare a subject's physio files for further processing
% INPUT: name - character string indicating subejct (e.g., '101')
% 2022-10-05 AndyP modified
function delete_duplicates_and_merge_physio(name,site)
    path = fullfile(pwd,'Data_Raw',['subject_' name]);
    if strcmp(site,'isr')
        Utilities.deleteDuplicates(path, name); 
        Utilities.replaceHebrewCharacters(path, name);
    end
    Utilities.mergeDatabases(path, name, 'physio');
end