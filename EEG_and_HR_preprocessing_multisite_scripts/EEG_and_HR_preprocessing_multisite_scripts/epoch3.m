function [edata,gap] = epoch3(times, data, timings, pre, post, sampling_rate,tol)
times=int64(times);
step = 1000/sampling_rate;
pre_samples = round(pre / step);
post_samples = round(post / step);
desiredLength = round(pre_samples+1+post_samples);
edata = nan(size(timings,1),desiredLength,size(data,2));
gap = nan(size(timings));
total_length = 0;
total_desired = 0;
for i = 1:length(timings)
    ind0 = find(times > timings(i), 1,'first'); %find the index of the first sample after the alignment event
    if  (~isempty(ind0))
        ind1 = ind0 - 1;
        gap0 = times(ind0)-timings(i); % gap between aligned physio timestamp and alignment event
        gap1 = times(ind1)-timings(i);
        gap_min = min(gap0,gap1);
        ix_gap = find(min(gap0,gap1));
        if ix_gap ==1 % use gap0 if gap0 <= gap1
        elseif ix_gap == 2 % use gap1
                ind0 = ind1;
        end
        if gap_min < tol % tolerance ~ 4ms
            ind = ind0 - pre_samples : ind0 + post_samples;
            keep = ones(size(ind));
            keep(times(ind) <= timings(i)-pre-tol) = 0;
            pre_pad = sum(keep==0);
            ind(times(ind) >= timings(i)+post+tol) = [];
            ind(times(ind) <= timings(i)-pre-tol) = [];
        else 
            ind = [];
        end
    else
        ind = [];
    end
    gap(i) = gap0;
    actualLength = length(ind);
    if  ~isempty(ind)
        if length(size(edata))>2 %EEG
            edata(i,pre_pad+1:length(ind),1) = data(ind, 1);
            edata(i,pre_pad+1:length(ind),2) = data(ind, 2);
            edata(i,pre_pad+1:length(ind),3) = data(ind, 3);
            edata(i,pre_pad+1:length(ind),4) = data(ind, 4);
        else %HR
            edata(i,all_epoch_ind) = data(ind);
        end
    end
    total_length = total_length + actualLength;
    total_desired = total_desired + desiredLength;
end
fprintf('Found a total of %d samples out of %d\n', total_length,total_desired);
end
