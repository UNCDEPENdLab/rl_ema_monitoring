function getAlignmentParameters(directoryOfParticipants)
    rootDir = '';
    
    subdirs = listSubdirectories(directoryOfParticipants);
    
    parfor i=1:numel(subdirs)
        tf = EEGanalysis_test_Validation(subdirs(i),rootDir,true);
    end
end

function subdirs = listSubdirectories(directoryPath)
    contents = dir(directoryPath);
    isSubdir = [contents.isdir] & ~ismember({contents.name},{'.','..'});
    subdirs = {contents(isSubdir).name};
    
end