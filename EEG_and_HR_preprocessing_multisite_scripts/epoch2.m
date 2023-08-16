        function edata = epoch2(times, data, timings, pre, post, sampling_rate)
            times=int64(times);
            step = 1000/sampling_rate;
            pre_samples = round(pre / step);
            post_samples = round(post / step);
            desiredLength = round(pre_samples+1+post_samples);
            total_desired = 0; total_actual = 0;
            edata = nan(size(timings,1),desiredLength,size(data,2));
            for i = 1:length(timings)
                ind = find(times>=timings(i)-pre, 1, 'first'); %find the index of the first sample after the begining of the epoch
                gap = round((times(ind) - (timings(i)-pre))/step); %the gap between the first sample after the begining of the epoch to the time that was set to the beginig of the epoch (in units of samples)
                if  (~isempty(ind) && gap<desiredLength)
                    subsequent_samples_ind = ind+1:(ind+desiredLength-1-gap); %the possible indices of the rest of the samples (exluding the first one) until the end of the epoch
                    subsequent_times = times(subsequent_samples_ind);
                    subsequent_epoch_indices = round((subsequent_times-times(ind))/step)+gap+1;%the number of samples between each of the subsequent_times to the first time is used to define the indicies   
                    all_epoch_ind = vertcat(gap+1,subsequent_epoch_indices);
                    redundant_samples_ind = find(all_epoch_ind>desiredLength); %samples that exceed epoch length 
                    all_epoch_ind(redundant_samples_ind) = [];
                    subsequent_samples_ind(redundant_samples_ind) = [];
                    all_samples_ind = [ind,subsequent_samples_ind]';            
                else
                    all_samples_ind = []; all_epoch_ind = []; 
                end
                actualLength = length(all_epoch_ind);                
                total_desired = total_desired + desiredLength;
                total_actual = total_actual + actualLength;
                if  ~isempty(all_samples_ind)
                    if length(size(edata))>2 %EEG
                        edata(i,all_epoch_ind,1) = data(all_samples_ind, 1);
                        edata(i,all_epoch_ind,2) = data(all_samples_ind, 2);
                        edata(i,all_epoch_ind,3) = data(all_samples_ind, 3);
                        edata(i,all_epoch_ind,4) = data(all_samples_ind, 4);
                    else %HR
                        edata(i,all_epoch_ind) = data(all_samples_ind);
                    end
                end
            end
            fprintf('Found a total of %d samples out of %d\n', total_actual,total_desired);            
        end  
