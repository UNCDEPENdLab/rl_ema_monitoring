function load_epoch_TF_US(name)

sampling_rate = 256.03;
data_folder = '/Users/andypapale/Momentum/';
output_folder = data_folder;
%output_folder = '/bgfs/adombrovski/DNPL_DataMesh/Data/Momentum_EMA';  

%filename_EEG = dir(strcat(fullfile(data_folder,['subject_' name]),['\' name '_EEG1.mat']));
filename_EEG = dir(strcat(fullfile('/Users/andypapale/Momentum/',['subject_' name]),['/' name '_EEG1.mat'])); % testing on Mac
if  ~isempty(filename_EEG)

    % feedback
    load(strcat(filename_EEG(1).folder,'/',filename_EEG(1).name), 'epoch_data_feedback','gap_feedback')
    feedback_time = (size(epoch_data_feedback,2)-1)/2; % assuming a centered window
    

    left_temp = epoch_data_feedback(:,:,1);
    [TF_left_temp,freqs] = EEGtimefreq_US(left_temp, sampling_rate);
    times = linspace(-1.5,1.5,size(left_temp,2)); %#ok<*NASGU>
    TF_left_temp = squeeze(TF_left_temp);
    save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_feedback_TF_left_temp']), 'TF_left_temp','sampling_rate','feedback_time','freqs','times', '-v7.3');
    clear TF_left_temp left_temp

%     left_front = epoch_data_feedback_filtered(:,:,2);
%     TF_left_front = EEGtimefreq_US(left_front, sampling_rate, feedback_time);
%     TF_left_front(1:size(TF_left_front,1),:,:) = TF_left_front ;
%     save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_feedback_TF_left_front']), 'TF_left_front','sampling_rate','feedback_time', '-v7.3')
%     clear TF_left_front left_front
% 
%     right_front = epoch_data_feedback_filtered(:,:,3);
%     TF_right_front = EEGtimefreq_US(right_front, sampling_rate, feedback_time);
%     TF_right_front(1:size(TF_right_front,1),:,:) = TF_right_front;
%     save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_feedback_TF_right_front']), 'TF_right_front','sampling_rate','feedback_time', '-v7.3')
%     clear TF_right_front right_front
% 
%     right_temp = epoch_data_feedback_filtered(:,:,4);
%     TF_right_temp = EEGtimefreq_US(right_temp, sampling_rate, feedback_time);
%     TF_right_temp(1:size(TF_right_temp,1),:,:) = TF_right_temp;
%     save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_feedback_TF_right_temp']), 'TF_right_temp','sampling_rate','feedback_time', '-v7.3')
%     clear TF_right_temp right_temp
%     
%     clear epoch_data_feedback_filtered
%     
%     % choice
%     load(strcat(filename_EEG(1).folder,'/',filename_EEG(1).name), 'epoch_data_choice_filtered','gap_choice_filtered')
%     choice_time = (size(epoch_data_choice_filtered,2)-1)/2; % assuming a centered window
%     
%     left_temp = epoch_data_choice_filtered(:,:,1);
%     TF_left_temp = EEGtimefreq_US(left_temp, sampling_rate, choice_time);
%     TF_left_temp(1:size(TF_left_temp,1),:,:) = TF_left_temp;
%     save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_choice_TF_left_temp']), 'TF_left_temp','sampling_rate','choice_time', '-v7.3');
%     clear TF_left_temp left_temp
%     
%     left_front = epoch_data_choice_filtered(:,:,2);
%     TF_left_front = EEGtimefreq_US(left_front, sampling_rate, choice_time);
%     TF_left_front(1:size(TF_left_front,1),:,:) = TF_left_front ;
%     save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_choice_TF_left_front']), 'TF_left_front','sampling_rate','choice_time', '-v7.3')
%     clear TF_left_front left_front
%     
%     
%     right_front = epoch_data_choice_filtered(:,:,3);
%     TF_right_front = EEGtimefreq_US(right_front, sampling_rate, choice_time);
%     TF_right_front(1:size(TF_right_front,1),:,:) = TF_right_front;
%     save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_choice_TF_right_front']), 'TF_right_front','sampling_rate','choice_time', '-v7.3')
%     clear TF_right_front right_front
% 
%     right_temp = epoch_data_choice_filtered(:,:,4);
%     TF_right_temp = EEGtimefreq_US(right_temp, sampling_rate, choice_time);
%     TF_right_temp(1:size(TF_right_temp,1),:,:) = TF_right_temp;
%     save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_choice_TF_right_temp']), 'TF_right_temp','sampling_rate','choice_time', '-v7.3')
%     clear TF_right_temp right_temp
%     
%     clear epoch_data_choice_filtered
%     
%     % stim
%     load(strcat(filename_EEG(1).folder,'/',filename_EEG(1).name), 'epoch_data_stim_filtered','gap_stim_filtered')
%     stim_time = (size(epoch_data_stim_filtered,2)-1)/2; % assuming a centered window
%     
%     left_temp = epoch_data_stim_filtered(:,:,1);
%     TF_left_temp = EEGtimefreq_US(left_temp, sampling_rate, stim_time);
%     TF_left_temp(1:size(TF_left_temp,1),:,:) = TF_left_temp;
%     save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_stim_TF_left_temp']), 'TF_left_temp','sampling_rate','choice_time', '-v7.3');
%     clear TF_left_temp left_temp
%     
%     left_front = epoch_data_stim_filtered(:,:,2);
%     TF_left_front = EEGtimefreq_US(left_front, sampling_rate, stim_time);
%     TF_left_front(1:size(TF_left_front,1),:,:) = TF_left_front ;
%     save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_stim_TF_left_front']), 'TF_left_front','sampling_rate','stim_time', '-v7.3')
%     clear TF_left_front left_front
%     
%     right_front = epoch_data_stim_filtered(:,:,3);
%     TF_right_front = EEGtimefreq_US(right_front, sampling_rate, stim_time);
%     TF_right_front(1:size(TF_right_front,1),:,:) = TF_right_front;
%     save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_stim_TF_right_front']), 'TF_right_front','sampling_rate','stim_time', '-v7.3')
%     clear TF_right_front right_front
%     
%     right_temp = epoch_data_stim_filtered(:,:,4);
%     TF_right_temp = EEGtimefreq_US(right_temp, sampling_rate, stim_time);
%     TF_right_temp(1:size(TF_right_temp,1),:,:) = TF_right_temp;
%     save(fullfile(output_folder, 'Data_Processed',['subject_' name] ,[name '_stim_TF_right_temp']), 'TF_right_temp','sampling_rate','stim_time', '-v7.3')
%     clear TF_right_temp right_temp
%     
%     clear epoch_data_stim_filtered
    
    
end
end





