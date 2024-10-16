function EEGanalysis_test(name)
    
	site = 'Pitt';
	if nargin<2
        output_folder = '/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA';      
    end

    filename = dir(strcat(fullfile(output_folder,'Data_Raw',name,'schedule'),'/*schedule.db'));
    if length(filename) > 1
        error(sprintf('multiple schedule files found for subject',name,'%s'));
    end
    db = sqlite(strcat(filename(1).folder,'/',filename(1).name));
    temp = cell2mat(fetch(db, 'SELECT feedback_time, stim_time, choice_time, feedback, block FROM trials WHERE choice_time IS NOT NULL AND stim1>=0 AND stim2>=-1000 ORDER BY choice_time ASC'));
    
    %temp = cell2mat(fetch(db, 'SELECT feedback_time, feedback FROM trials WHERE choice_time IS NOT NULL AND stim1>17 AND stim2>17 AND stim1<150 AND stim2<150'));
    Trial.feedbackTimes = temp(:,1);
    Trial.stimTimes = temp(:,2);
    Trial.choiceTimes = temp(:,3);
    Trial.feedback = temp(:,4);
    
    db.close;
    
	tol = 4; % 4ms tolerance of alignment
	pre_window = 1500; %1.5s
	post_window = 3000; 
    %% read EEG and remove trials with NaN
    [EEG, sampling_rate] = readEEG(output_folder,name, site, 0);
    % raw data AndyP 2023-08-16
    [epoch_data_feedback,gap_feedback] = epoch3(EEG.times, EEG.data, Trial.feedbackTimes, pre_window, post_window, sampling_rate,tol);
    [epoch_data_stim,gap_stim] = epoch3(EEG.times, EEG.data, Trial.stimTimes, pre_window, post_window, sampling_rate,tol);
    [epoch_data_choice,gap_choice] = epoch3(EEG.times, EEG.data, Trial.choiceTimes, pre_window, post_window, sampling_rate,tol);
    
    EEG.cleandata = EEG.data; EEG.cleandata(EEG.remove~=0)=NaN;
    [epoch_data_feedback_filtered,gap_feedback_filtered] = epoch3(EEG.times, EEG.cleandata, Trial.feedbackTimes, pre_window, post_window, sampling_rate,tol);
    [epoch_data_stim_filtered,gap_stim_filtered] = epoch3(EEG.times, EEG.cleandata, Trial.stimTimes, pre_window, post_window, sampling_rate,tol);
    [epoch_data_choice_filtered,gap_choice_filtered] = epoch3(EEG.times, EEG.cleandata, Trial.choiceTimes, pre_window, post_window, sampling_rate,tol);
    
    if pre_window==post_window && pre_window==1500
        ind_na_all_feedback = any(any(isnan(epoch_data_feedback_filtered(:,333:693,:)),2),3); %feedback appears at sample 385 and ends at sample 641, so trials with nan in the range of [-0.2, +0.2] from feedback will be considered as noisy
        ind_na_all_stim = any(any(isnan(epoch_data_stim_filtered(:,333:693,:)),2),3); %stim appears at sample 385 and ends at sample 641, so trials with nan in the range of [-0.2, +0.2] from feedback will be considered as noisy
        ind_na_all_choice = any(any(isnan(epoch_data_choice_filtered(:,333:693,:)),2),3); %choice occurs at sample 385 and ends at sample 641, so trials with nan in the range of [-0.2, +0.2] from feedback will be considered as noisy
    else
        ind_na_all_feedback = [];
        ind_na_all_stim = [];
        ind_na_all_choice = [];
    end
    Ntotal = length(Trial.feedbackTimes);
    Ngood_all= size(epoch_data_feedback_filtered,1);
    EEG_percen_all=(Ngood_all/Ntotal)*100;
    
    time_window = linspace(-pre_window,post_window,size(epoch_data_feedback,2));
    
    %find the optimal combinatin of three electrodes:
    [best_single, best_two_config, best_three_config, EEG_percen_single, EEG_percen_best_two, EEG_percen_best_three, ind_na_best_single, ind_na_best_two, ind_na_best_three]=find_optimal_comb(epoch_data_feedback, Ntotal);

    
    save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_EEG1.mat']), 'time_window','ind_na_all_feedback','ind_na_all_stim','ind_na_all_choice','epoch_data_feedback','epoch_data_feedback_filtered','epoch_data_stim','epoch_data_stim_filtered','epoch_data_choice','epoch_data_choice_filtered', 'EEG_percen_all', 'best_single', 'best_two_config', 'best_three_config', 'EEG_percen_single', 'EEG_percen_best_two', 'EEG_percen_best_three', 'ind_na_best_single', 'ind_na_best_two', 'ind_na_best_three', 'sampling_rate','gap_feedback','gap_stim','gap_choice','gap_feedback_filtered','gap_stim_filtered','gap_choice_filtered','tol')
      


