function HR = readHR(output_folder,name, site, resetFlag)
if nargin<3 || isempty(resetFlag); resetFlag = 0; end
HRstep = 10;
if strcmp(site,'HUJI') 
    filename = fullfile(output_folder,'Data_Processed',['subject_' name],[name '_HR.mat']);
else
    filename = fullfile(pwd,'Data_Processed',['subject_' name],[name '_HR.mat']);
end
if ~resetFlag && exist(filename, 'file')
    load(filename, 'HR');
else
    %% get times and intervals
    if strcmp(site,'HUJI')
        filename = dir(strcat(fullfile(pwd,'Data_Raw',['subject_' name]),'/*physio.db'));
    else
        filename = dir(strcat(fullfile(output_folder,'Data_Processed',['subject_' name]),'/*physio.db'));
    end
    if length(filename) > 1
        error(sprintf('multiple physio files found for subject',name,'%s'));
    end
    if strcmp(site,'HUJI')
       dbname = fullfile(pwd,'Data_Raw',['subject_' name],[name '_physio.db']);
       db = sqlite(dbname);
    else
       db = sqlite(strcat(filename(1).folder,'/',filename(1).name));
    end
    try
        DATA = fetch(db, 'SELECT time_ms, rr_intervals, heartrate, contact FROM Polar_heartrate ORDER BY time_ms ASC');
        HR.contact = cellfun(@(x)strcmp(x,'true'),DATA(:,4));
    catch
        DATA = fetch(db, 'SELECT time_ms, rr_intervals, heartrate FROM Polar_heartrate ORDER BY time_ms ASC');
    end
    
    db.close();
    HR.times = cell2mat(DATA(:,1));
    HR.hr = cell2mat(DATA(:,3));
    HR.contact = cellfun(@(x)strcmp(x,'true'),DATA(:,4));
    intervals = cellfun(@(x)regexprep(x,'[\[\]]',''),DATA(:,2),'UniformOutput',false);
    intervals = cellfun(@(x)strsplit(x,','),intervals,'UniformOutput',false);
    
    %% change empty to 0
    empty = find(cellfun(@(x)strcmp(x{1},'empty') | strcmp(x{1},''),intervals));
    for i = 1:length(empty); intervals(empty(i)) = {{'0'}};  end
    
    %% flatten
    multi = find(cellfun(@(x)length(x)>1,intervals))';
    while ~isempty(multi)
        for i = length(multi):-1:1
            intervals = cat(1,intervals(1:multi(i)), intervals(multi(i)), intervals(multi(i)+1: end));
            HR.times = cat(1,HR.times(1:multi(i)), HR.times(multi(i)), HR.times(multi(i)+1: end));
            HR.hr = cat(1,HR.hr(1:multi(i)), HR.hr(multi(i)), HR.hr(multi(i)+1: end));
            HR.contact = cat(1,HR.contact(1:multi(i)), HR.contact(multi(i)), HR.contact(multi(i)+1: end));
            intervals{multi(i)}(end) = [];
            intervals{multi(i)+1}(1:end-1) = [];
        end
        multi = find(cellfun(@(x)length(x)>1,intervals))';
    end
    HR.intervals = cellfun(@(x)str2double(x{1}),intervals);
    
    %%  save
    if strcmp(site,'HUJI')
       if ~exist(strcat(output_folder, '\Data_Processed', ['\subject_', name],'\' ,[name, '_HR.mat']))  
           save(fullfile(output_folder, '\Data_Processed', ['subject_' name] , [name '_HR.mat']), 'HR', '-v7.3')
       end
    else
        if ~exist(strcat(filename(1).folder,'/',filename(1).name),'file')
            homedir = cd;
            cd(filename(1).folder)
            save(filename(1).name, 'HR','-v7.3'); % 2022-10-05 AndyP: support for data > 2GB
            cd(homedir);
        end
     end
    %end
end

%% find irregular times
difftimes = diff(HR.times);
ireg = find(mod(difftimes+50,1000)>100);
i = 1;
while i < length(ireg)-1
    i1 = ireg(i); i2= ireg(i+1);
    if abs(difftimes(i1)+difftimes(i2) - 2000) < 100 && difftimes(i1) > difftimes(i2) && all(difftimes(i1+1:i2-1))
        HR.times(i1+1:i2) = HR.times(i2+1);
    end
    i=i+1;
end

%% correct intervals by 1000/1024 (transform to ms)
HR.intervals = double(HR.intervals) * 1000/1024;

%% find discontinuities and split to sections
difftimes = diff(HR.times);
ilast = cat(1,find(difftimes~=0),length(HR.times));
csumint = cumsum(HR.intervals);
deviations = diff(double(HR.times(ilast)-HR.times(1))-csumint(ilast));
isplit = find(abs(deviations)>1700) + 1;
isplit = ilast(isplit);
minseg = 10;
while ~isempty(isplit) && isplit(1) < minseg
    HR.times(1:isplit(1)-1) = [];
    HR.hr(1:isplit(1)-1) = [];
    HR.intervals(1:isplit(1)-1) = [];
    try
        HR.contact(1:isplit(1)-1) = [];
    end
    isplit = isplit - isplit(1) + 1; isplit(1) = [];
end
todelete = [];
for i = 1:length(isplit)-1
    if isplit(i+1)-isplit(i) < minseg; todelete = cat(1, todelete, i+1); end
end

for i = 1:length(todelete)
    inds = isplit(todelete(i)-1):isplit(todelete(i))-1;
    HR.times(inds) = [];
    HR.intervals(inds) = [];
    HR.hr(inds) = [];
    try
        HR.contact(inds) = [];
    end
    isplit(todelete(i):end) = isplit(todelete(i):end) - length(inds);
    isplit(todelete(i)) = [];
    todelete = todelete-1;
end

if ~isempty(isplit)
    HRsplit.times = HR.times(1:isplit(1)-1);
    HRsplit.intervals = HR.intervals(1:isplit(1)-1);
    HRsplit.contact = HR.contact(1:isplit(1)-1);
    for i = 1:length(isplit)-1
        HRsplit(i+1).times = HR.times(isplit(i):isplit(i+1)-1); %#ok<*AGROW>
        HRsplit(i+1).intervals = HR.intervals(isplit(i):isplit(i+1)-1);
    end
    HRsplit(end+1).times = HR.times(isplit(end):end);
    HRsplit(end).intervals = HR.intervals(isplit(end):end);
else
    HRsplit = HR;
end

%% merge sections
ind = 1;
for i = 1:length(HRsplit)
    if sum(HRsplit(i).intervals>0)>=2
        [beattimes{ind}, wiggleroom(ind)] = correctTimings(HRsplit(i).times, HRsplit(i).intervals);
        wiggleintervals(ind) = length(HRsplit(i).intervals);
        data(ind).beattimes = beattimes{ind};
        [times, intervals, rate] = timings2samples(data(ind).beattimes, HRstep);
        data(ind).times = times;
        data(ind).intervals = intervals;
        data(ind).rate = rate;
        
        ind = ind + 1;
    end
end
data = mergeHRdata(data, HRstep);
data.wiggle = sum(wiggleroom.*wiggleintervals)/sum(wiggleintervals);
data.raw_times = HR.times;
data.raw_intervals = HR.intervals;
data.polar_computed_rate = HR.hr;
data.polar_contact = HR.contact;
HR= data;
end


function mdata = mergeHRdata(data, HRstep)
mdata = data(1);
for i = 2:length(data)
    if ~isempty(data(i).times)
        nanHRsteps = (data(i).times(1) - mdata.times(end)) / HRstep - 1;
        mdata.times(end+1:end+nanHRsteps) = mdata.times(end)+(1:1:nanHRsteps)*HRstep;
        mdata.intervals(end+1:length(mdata.times)) = nan;
        mdata.rate(end+1:length(mdata.times)) = nan;
        mdata.beattimes = cat(1,mdata.beattimes, data(i).beattimes);
        mdata.times = cat(1,mdata.times, data(i).times);
        mdata.intervals = cat(1,mdata.intervals, data(i).intervals);
        mdata.rate = cat(1,mdata.rate, data(i).rate);
    end
end
end

function [timings, wiggleroom] = correctTimings(times, intervals)
first_non_zero = find(intervals~=0,1,'first');
if isempty(first_non_zero)
    timings = [];
    wiggleroom = [];
else
    if first_non_zero>1
        times(1:first_non_zero-1) = [];
        intervals(1:first_non_zero-1) = [];
    end
    newintervals = intervals;
    timings(1) = times(1); for i = 2:length(newintervals); timings(i,1) = timings(i-1,1) + newintervals(i); end
    todelete = find(timings(1:end-1) == timings(2:end)) + 1;
    while ~isempty(todelete)
        timings(todelete) = []; times(todelete) = [];
        todelete = find(timings(1:end-1) == timings(2:end));
    end
    mismatch = @(x,y) sum(double(double(x-y) > double(1000)) .* double(x-y-1000) + double(double(x-y)<0) .* double(y - x));
    shft = fminbnd(@(x)mismatch(times,timings+x),-2000,0);
    while mismatch(times,timings+shft(end))==mismatch(times,timings+shft(end)+1); shft = cat(1,shft,shft(end)+1); end
    if length(shft)>1; shft = cat(1,shft(2:end),shft(1)); end
    while mismatch(times,timings+shft(end))==mismatch(times,timings+shft(end)-1); shft = cat(1,shft,shft(end)-1); end
    timings = cat(1,times(1)-intervals(1),timings);
    timings = timings + mean(shft);
    wiggleroom = length(shft);
end
end

function [times, intervals, rate] = timings2samples(timings, HRstep)
i = 0;
start = ceil(double(timings(1)) /  HRstep) * HRstep;
for t = start:HRstep:double(timings(end))
    i = i +1;
    times(i,1) = t;
    ind = find(timings > t, 1, 'first');
    if ~isempty(ind)
        intervals(i,1) = timings(ind) - timings(ind-1);
        rate(i,1) = 60000 / double(intervals(i));
    end
end
end
