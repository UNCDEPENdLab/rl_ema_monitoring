classdef Utilities < handle
    
    properties
        
    end
    
    methods(Static)
        function ddata = downsample(data, factor)
            for i = 1:size(data,1)
                ddata(i,:) = resample(data(i,:), 300, round(factor*300));
            end           
        end
        
        function deleteDuplicates(path, name)
            files = dir(fullfile(path, [name '_*.db']));
            filenames = strsplit([files.name],'.db')'; filenames(end) = [];
            filenames = regexprep(filenames,'\(.\)','');
            filenames = regexprep(filenames,'\(..\)','');
            sizes = [files.bytes]';
            todelete = [];
            for f = 1:length(filenames)
                if ~any(f==todelete)
                    others = setdiff(1:length(filenames), f);
                    others = setdiff(others, todelete);
                    same = find(strcmp(filenames{f},filenames(others)),1);
                    while ~isempty(same)
                        if sizes(others(same)) > sizes(f)
                            todelete(end+1) = f; 
                            same = [];
                        else
                            todelete(end+1) = others(same);
                            others = setdiff(others, others(same));
                            same = find(strcmp(filenames{f},filenames(others)),1);
                        end
                    end
                end
            end
            for f = 1:length(todelete); delete(fullfile(path, files(todelete(f)).name)); end
            files(todelete) = []; filenames(todelete) = [];
            for f = 1:length(files); if ~strcmp(files(f).name, [filenames{f} '.db']); movefile(fullfile(path, files(f).name), fullfile(path, [filenames{f} '.db'])); end; end
        end 
        
        function replaceHebrewCharacters(path, name)
            files = dir(fullfile(path, [name '_*.db']));
            filenames = strsplit([files.name],'.db')'; filenames(end) = [];
            filenames = regexprep(filenames,'\(.\)','');
            sizes = [files.bytes]';
            for f = 1:length(filenames)
                hebrew_chars = find(filenames{f}>=1488);
                if ~isempty(hebrew_chars)
                    new_filename = filenames{f};
                    new_filename(hebrew_chars) = char(new_filename(hebrew_chars) - 1423);
                    movefile(fullfile(path,[filenames{f} '.db']), fullfile(path,[new_filename '.db']));
                end
            end                
        end 
        
        function deleteDuplicateRows(db)
            tables = fetch(db,'SELECT name FROM sqlite_master WHERE type==''table''');
            tables(strcmp(tables, 'android_metadata')) = [];
            for t = 1:length(tables)
                tables{t}
                fetch(db, ['SELECT count(rowid) FROM ' tables{t}])
                str = fetch(db,['SELECT sql FROM sqlite_master WHERE tbl_name = ''' tables{t} ''' AND type = ''table''']);
                str = regexprep(str, 'CREATE TABLE', 'CREATE TABLE IF NOT EXISTS');
                columns = regexp(str{1},'\(.*\)', 'match');
                columns = regexp(columns{1},'[a-z_0-9]+', 'match','ignorecase');
                columns(cellfun(@(x)strcmp(x,'TEXT'),columns)) = [];
                columns(cellfun(@(x)strcmp(x,'INTEGER'),columns)) = [];
                columns(cellfun(@(x)strcmp(x,'FLOAT'),columns)) = [];
                db.exec(['DELETE FROM ' tables{t} ' WHERE rowid not in (SELECT MIN(rowid) from ' tables{t} ' GROUP BY ' strjoin(columns(1:end),',') ')']);
            end
        end
        
        function deleteDuplicateRowsForomEEGMuseAccelerometer(db)
            try
                db.exec(['DELETE FROM EEG_muse_accelerometer WHERE rowid not in (SELECT MIN(rowid) from EEG_muse_accelerometer GROUP BY recording_time)']);
            catch ex
                warning(ex.message)
            end
        end        
        
        function deleteDuplicateRowsFromFile(path, filename)
            db = sqlite(fullfile(path, filename), 'connect');
            Utilities.deleteDuplicateRows(db);
            db.close();
        end
        
        function db = getPhysio(name)
            path = fullfile(fileparts(fileparts(pwd)),'Data_Raw',['subject_' name]);
            datasbasename = fullfile(path, [name '_physio.db']);
            db = sqlite(datasbasename, 'connect');
        end
        
        function deleteDuplicateRowsFromEEGMuseAccelerometerFromFile(path, filename)
            db = sqlite(fullfile(path, filename), 'connect');
            Utilities.deleteDuplicateRowsForomEEGMuseAccelerometer(db);
            db.close();
        end
        
        function mergeDatabases(path, name, database)
            datasbasename = fullfile(path, [name '_' database '.db']);
            if ~exist(datasbasename, 'file') db = sqlite(datasbasename, 'create');
            else db = sqlite(datasbasename, 'connect');
            end
                
            files = dir(fullfile(path, [name '_' database '_*.db']));
            for f= 1:length(files)
                newdb = sqlite(fullfile(path, files(f).name), 'connect');
                tables = fetch(newdb,'SELECT name FROM sqlite_master WHERE type==''table''');
                tables(strcmp(tables, 'android_metadata')) = [];
                
                for t = 1:length(tables)
                    str = fetch(newdb,['SELECT sql FROM sqlite_master WHERE tbl_name = ''' tables{t} ''' AND type = ''table''']);
                    str = regexprep(str, 'CREATE TABLE', 'CREATE TABLE IF NOT EXISTS');
                    db.exec(str{1});
                    columns = regexp(str{1},'\(.*\)', 'match');
                    columns = regexp(columns{1},'[a-z_0-9]+', 'match','ignorecase');
                    columns(cellfun(@(x)strcmp(x,'TEXT'),columns)) = [];
                    columns(cellfun(@(x)strcmp(x,'INTEGER'),columns)) = [];
                    columns(cellfun(@(x)strcmp(x,'FLOAT'),columns)) = [];
                                        
                    for c=1:length(columns)
                        newdb.exec(['UPDATE ' tables{t} ' SET ' columns{c} '=-1 WHERE ' columns{c} ' IS NULL']); 
                    end
                    data = fetch(newdb, ['SELECT ' strjoin(columns(1:end),',') ' FROM ' tables{t}]);
                    if (~isempty(data)); insert(db, tables{t}, columns(1:end), data); end
                end
                newdb.close();
                delete(fullfile(path, files(f).name));
            end
            db.close();
        end 
        
        function edata = epoch(times, data, timings, pre, post, sampling_rate)
            step = 1000/sampling_rate;
            pre = round(pre / step);
            post = round(post / step);
            total_desired = 0; total_actual = 0;
            for i = 1:length(timings)
                ind = find(times>timings(i), 1, 'first');
                if ind > 1; inds = max(ind-pre,1):ind+post;
                else inds = []; 
                end
                desiredLength = round(pre+1+post);             
                actualLength = length(inds);
                
                if ~isempty(inds) && inds(end) > size(data,1)
                    addpost = inds(end) - size(data,1);
                    inds = inds(1):size(data,1);
                else
                    addpost = 0;
                end
                total_desired = total_desired + desiredLength;
                total_actual = total_actual + actualLength;
                edata(i,:,:) = cat(2, nan(1,desiredLength-actualLength,size(data,2)), permute(data(inds,:),[3 1 2]), nan(1,addpost,size(data,2)));
            end
            fprintf('Found a total of %d samples out of %d\n', total_actual,total_desired);
            
        end        
        
        function real_recording_time = createRealTime(arrive_time, sampling_rate)          
            real_recording_time = (arrive_time(1):1000/sampling_rate:(arrive_time(1)+(length(arrive_time)-1)*1000/sampling_rate))';
            maxDiff = max(real_recording_time-arrive_time);
            real_recording_time = real_recording_time - maxDiff;
        end
  
      
        function [real_time, break_indices] = correctRealTime(real_time, arrive_time, sampling_rate)
            diff_time = arrive_time-real_time;
            min_subsequent_diff = cummin(diff_time(end:-1:1));
            min_subsequent_diff = min_subsequent_diff(end:-1:1);
            index = find(min_subsequent_diff > 300, 1, 'first');
            break_indices = [];
            lastindex = 0;
            while ~isempty(index) 
                index = lastindex + index;
                break_indices = cat(1 ,break_indices, index);                
                real_time(index:end) = Utilities.createRealTime(arrive_time(index:end), sampling_rate);
                diff_time = arrive_time(index:end)-real_time(index:end);
                min_subsequent_diff = cummin(diff_time(end:-1:1));
                min_subsequent_diff = min_subsequent_diff(end:-1:1);
                lastindex = index - 1;
                index = find(min_subsequent_diff > 300, 1, 'first');
            end
        end
        
        function sampling_rate = adjustSamplingRate(real_time, arrive_time, sampling_rate)
            diff_time = arrive_time-real_time;
            ind_remove = diff_time>median(diff_time)+3*std(diff_time);
            indices = 1:length(diff_time);
            diff_time = diff_time(~ind_remove);
            indices = indices(~ind_remove);
            b = robustfit(indices,diff_time(1:end)-diff_time(1));
            sampling_rate = 1000/(1000/sampling_rate + b(2));            
        end
    end
        
end