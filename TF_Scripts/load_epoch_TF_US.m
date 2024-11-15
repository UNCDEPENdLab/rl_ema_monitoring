function load_epoch_TF_US(name)

sampling_rate = 256.03;
data_folder = '/Volumes/bierka_root/datamesh/PROC/MMTM/Momentum_EMA/EEG_Data_Processed_Aug_24/Data_Processed/';
output_folder = '/Volumes/bierka_root/datamesh/PROC/MMTM/Momentum_EMA/';
%output_folder = '/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA';  

%filename_EEG = dir(strcat(fullfile(data_folder,['subject_' name]),['\' name '_EEG1.mat']));
filename_EEG = dir(strcat(fullfile(data_folder,['subject_' name]),['/' name '_EEG1.mat'])); % testing on Mac
if  ~isempty(filename_EEG)

    if ~exist(fullfile(output_folder, 'EEG_Data_Processed_Aug_24','TF_Analysis',['subject_' name]),'dir')
        mkdir(fullfile(output_folder, 'EEG_Data_Processed_Aug_24','TF_Analysis',['subject_' name]))
    end

    % feedback
    load(strcat(filename_EEG(1).folder,'/',filename_EEG(1).name), 'epoch_data_feedback','time_window')
    feedback_time = find(time_window==0);
    times = time_window;
    start = find(times < -1000,1,'last');
    stop = find(times > 2500,1,'first');
    times = times(start:stop);


    left_temp = epoch_data_feedback(:,:,1);
    [TF_left_temp,freqs] = EEGtimefreq_US(left_temp, sampling_rate);
    TF_left_temp = squeeze(TF_left_temp);
    TF_left_temp = TF_left_temp(:,:,start:stop);
    save(fullfile(output_folder, 'EEG_Data_Processed_Aug_24','TF_Analysis',['subject_' name] ,[name '_feedback_TF_left_temp']), 'TF_left_temp','sampling_rate','feedback_time','freqs','times', '-v7.3');
    clear TF_left_temp left_temp

    left_front = epoch_data_feedback(:,:,2);
    TF_left_front = EEGtimefreq_US(left_front, sampling_rate);
    TF_left_front = squeeze(TF_left_front);
    TF_left_front = TF_left_front(:,:,start:stop);
    save(fullfile(output_folder, 'EEG_Data_Processed_Aug_24','TF_Analysis',['subject_' name] ,[name '_feedback_TF_left_front']), 'TF_left_front','sampling_rate','feedback_time','freqs','times', '-v7.3')
    clear TF_left_front left_front

    right_front = epoch_data_feedback(:,:,3);
    TF_right_front = EEGtimefreq_US(right_front, sampling_rate);
    TF_right_front = squeeze(TF_right_front);
    TF_right_front = TF_right_front(:,:,start:stop);
    save(fullfile(output_folder, 'EEG_Data_Processed_Aug_24','TF_Analysis',['subject_' name] ,[name '_feedback_TF_right_front']), 'TF_right_front','sampling_rate','feedback_time','freqs','times', '-v7.3')
    clear TF_right_front right_front

    right_temp = epoch_data_feedback(:,:,4);
    TF_right_temp = EEGtimefreq_US(right_temp, sampling_rate);
    TF_right_temp = squeeze(TF_right_temp);
    TF_right_temp = TF_right_temp(:,:,start:stop);
    save(fullfile(output_folder, 'EEG_Data_Processed_Aug_24','TF_Analysis',['subject_' name] ,[name '_feedback_TF_right_temp']), 'TF_right_temp','sampling_rate','feedback_time','freqs','times', '-v7.3')
    clear TF_right_temp right_temp

    clear epoch_data_feedback

    % choice
    load(strcat(filename_EEG(1).folder,'/',filename_EEG(1).name), 'epoch_data_choice','time_window')
    choice_time = find(time_window==0);
    times = time_window;
    start = find(times < -1000,1,'last');
    stop = find(times > 2500,1,'first');
    times = times(start:stop);

    left_temp = epoch_data_choice(:,:,1);
    TF_left_temp = EEGtimefreq_US(left_temp, sampling_rate);
    TF_left_temp = squeeze(TF_left_temp);
    TF_left_temp = TF_left_temp(:,:,start:stop);
    save(fullfile(output_folder, 'EEG_Data_Processed_Aug_24','TF_Analysis',['subject_' name] ,[name '_choice_TF_left_temp']), 'TF_left_temp','sampling_rate','choice_time','freqs','times', '-v7.3');
    clear TF_left_temp left_temp

    left_front = epoch_data_choice(:,:,2);
    TF_left_front = EEGtimefreq_US(left_front, sampling_rate);
    TF_left_front = squeeze(TF_left_front);
    TF_left_front = TF_left_front(:,:,start:stop);
    save(fullfile(output_folder, 'EEG_Data_Processed_Aug_24','TF_Analysis',['subject_' name] ,[name '_choice_TF_left_front']), 'TF_left_front','sampling_rate','choice_time','freqs','times', '-v7.3')
    clear TF_left_front left_front


    right_front = epoch_data_choice(:,:,3);
    TF_right_front = EEGtimefreq_US(right_front, sampling_rate);
    TF_right_front = squeeze(TF_right_front);
    TF_right_front = TF_right_front(:,:,start:stop);
    save(fullfile(output_folder, 'EEG_Data_Processed_Aug_24','TF_Analysis',['subject_' name] ,[name '_choice_TF_right_front']), 'TF_right_front','sampling_rate','choice_time','freqs','times', '-v7.3')
    clear TF_right_front right_front

    right_temp = epoch_data_choice(:,:,4);
    TF_right_temp = EEGtimefreq_US(right_temp, sampling_rate);
    TF_right_temp = squeeze(TF_right_temp);
    TF_right_temp = TF_right_temp(:,:,start:stop);
    save(fullfile(output_folder, 'EEG_Data_Processed_Aug_24','TF_Analysis',['subject_' name] ,[name '_choice_TF_right_temp']), 'TF_right_temp','sampling_rate','choice_time','freqs','times', '-v7.3')
    clear TF_right_temp right_temp

    clear epoch_data_choice

    % stim
    load(strcat(filename_EEG(1).folder,'/',filename_EEG(1).name), 'epoch_data_stim','time_window')
    stim_time = find(time_window==0);
    times = time_window;
    start = find(times < -1000,1,'last');
    stop = find(times > 2500,1,'first');
    times = times(start:stop);

    left_temp = epoch_data_stim(:,:,1);
    TF_left_temp = EEGtimefreq_US(left_temp, sampling_rate);
    TF_left_temp = squeeze(TF_left_temp);
    TF_left_temp = TF_left_temp(:,:,start:stop);
    save(fullfile(output_folder, 'EEG_Data_Processed_Aug_24','TF_Analysis',['subject_' name] ,[name '_stim_TF_left_temp']), 'TF_left_temp','sampling_rate','choice_time','freqs','times', '-v7.3');
    clear TF_left_temp left_temp

    left_front = epoch_data_stim(:,:,2);
    TF_left_front = EEGtimefreq_US(left_front, sampling_rate);
    TF_left_front = squeeze(TF_left_front);
    TF_left_front = TF_left_front(:,:,start:stop);
    save(fullfile(output_folder, 'EEG_Data_Processed_Aug_24','TF_Analysis',['subject_' name] ,[name '_stim_TF_left_front']), 'TF_left_front','sampling_rate','stim_time','freqs','times', '-v7.3')
    clear TF_left_front left_front

    right_front = epoch_data_stim(:,:,3);
    TF_right_front = EEGtimefreq_US(right_front, sampling_rate);
    TF_right_front = squeeze(TF_right_front);
    TF_right_front = TF_right_front(:,:,start:stop);
    save(fullfile(output_folder, 'EEG_Data_Processed_Aug_24','TF_Analysis',['subject_' name] ,[name '_stim_TF_right_front']), 'TF_right_front','sampling_rate','stim_time','freqs','times', '-v7.3')
    clear TF_right_front right_front

    right_temp = epoch_data_stim(:,:,4);
    TF_right_temp = EEGtimefreq_US(right_temp, sampling_rate);
    TF_right_temp = squeeze(TF_right_temp);
    TF_right_temp = TF_right_temp(:,:,start:stop);
    save(fullfile(output_folder, 'EEG_Data_Processed_Aug_24','TF_Analysis',['subject_' name] ,[name '_stim_TF_right_temp']), 'TF_right_temp','sampling_rate','stim_time','freqs','times', '-v7.3')
    clear TF_right_temp right_temp

    clear epoch_data_stim


end
end