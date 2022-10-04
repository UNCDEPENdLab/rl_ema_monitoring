% Run this function only once, to prepare a subject's physio files for further processing
% INPUT: name - character string indicating subejct (e.g., '101')
function delete_duplicates_and_merge_physio(name)
    path = fullfile(pwd,'Data_Raw',['subject_' name]);
    Utilities.deleteDuplicates(path, name);
    Utilities.replaceHebrewCharacters(path, name);
    Utilities.mergeDatabases(path, name, 'physio');
end