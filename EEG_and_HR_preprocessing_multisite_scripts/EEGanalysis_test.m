function EEGanalysis_test(output_folder, name, site)
    if nargin<2
        output_folder = '/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA';      
    end
    %% read trial data
    if strcmp(site,'HUJI')
        filename = dir(strcat(fullfile(pwd,'Data_Raw',['subject_' name]),'/*schedule.db'));
    else
        filename = dir(strcat(fullfile(output_folder,'Data_Raw',[name],'schedule'),'/*schedule.db'));
    end
    if length(filename) > 1
        error(sprintf('multiple schedule files found for subject',name,'%s'));
    end
    db = sqlite(strcat(filename(1).folder,'/',filename(1).name));
    temp = cell2mat(fetch(db, 'SELECT feedback_time, feedback, block FROM trials WHERE choice_time IS NOT NULL AND stim1>=0 AND stim2>=0 ORDER BY choice_time ASC'));
    %temp = cell2mat(fetch(db, 'SELECT feedback_time, feedback FROM trials WHERE choice_time IS NOT NULL AND stim1>17 AND stim2>17 AND stim1<150 AND stim2<150'));
    Trial.feedback = temp(:,2);
    Trial.feedbackTimes = temp(:,1);
    db.close;
    
    %% read EEG and remove trials with NaN
    [EEG, sampling_rate] = readEEG(output_folder,name, site, 1);
    EEG.cleandata=EEG.data; EEG.cleandata(EEG.remove~=0)=nan; % 2022-10-05 AndyP: now remove noisy data
    epoch_data = Utilities.epoch(EEG.times, EEG.cleandata, Trial.feedbackTimes, 500, 1500, sampling_rate);
    ind_na_all = any(any(isnan(epoch_data),2),3);
    epoch_data_filtered_all = epoch_data(~ind_na_all,:,:);
    Ntotal = length(Trial.feedbackTimes);
    Ngood_all= size(epoch_data_filtered_all,1);
    EEG_percen_all=(Ngood_all/Ntotal)*100;
    
    %find the optimal combinatin of three electrodes:
    [best_single, best_two_config, best_three_config, EEG_percen_single, EEG_percen_best_two, EEG_percen_best_three, ind_na_best_single, ind_na_best_two, ind_na_best_three]=find_optimal_comb(epoch_data, Ntotal);

    
    save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_EEG.mat']), 'epoch_data', 'EEG_percen_all', 'ind_na_all', 'best_single', 'best_two_config', 'best_three_config', 'EEG_percen_single', 'EEG_percen_best_two', 'EEG_percen_best_three', 'ind_na_best_single', 'ind_na_best_two', 'ind_na_best_three', 'sampling_rate', 'EEG')
      
   
%% figures before cleaning data: figure 1 all data, figure 2-missing data

figure
for i=1:4
    subplot(4,1,i); imagesc(epoch_data(:,:,i));colorbar;
end
figure
for i=1:4
    subplot(4,1,i); imagesc(isnan(epoch_data(:,:,i)));colorbar;        
end


