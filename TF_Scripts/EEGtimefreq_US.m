function TF = EEGtimefreq_US(data, sampling_rate)
    Nsamples=size(data,2);
    N_electrodes=size(data,3);
    ft_defaults;    
    %freqs = 2:2:8;
    freqs = [2.1,2.5,2.9,3.5,4.2,5,5.9,7,8.4,10,11.8,14.1,16.8,20,23.7,28.2,33.6,40];
    %freqs = [2.1,2.5,2.9,3.5,4.2,5,5.9,7,8.4,10,11.8,14.1,16.8,20,23.7,28.2,33.6,40];
    %freqs = 1:4:30; 
    ftdata.label = {'channel'};
    ftdata.fsample = sampling_rate;
    ftdata.trial = cellfun(@(x)permute(x, [3 2 1]), mat2cell(data, ones(size(data,1),1),size(data,2), N_electrodes)','UniformOutput',false);
    ftdata.time = mat2cell(repmat(linspace(-1.5,1.5,size(data,2)),[length(ftdata.trial),1]),ones(size(data,1),1),Nsamples)';
    ftdata.sampleinfo = [];
    if N_electrodes==1
        ftdata.label = {'EEG1'};
    elseif N_electrodes==2
        ftdata.label = {'EEG1','EEG2'};
    elseif N_electrodes==4
        ftdata.label = {'EEG1','EEG2','EEG3','EEG4'};
    end   

    cfg              = [];
    cfg.output       = 'pow';
    cfg.method       = 'mtmconvol';
    cfg.taper        = 'hanning';
    cfg.foi          = freqs;
    cfg.t_ftimwin    = 4./cfg.foi;  
    cfg.toi          = 'all';            
    cfg.keeptrials = 'yes';

    pow = ft_freqanalysis(cfg,ftdata);
    TF = pow.powspctrm;
    %TF.data = permute(pow.powspctrm, [1 2 4 3]);
    %pow.powspctrm = pow.powspctrm - mean(pow.powspctrm(:,:,:,floor(feedback_time)-51:floor(feedback_time)-1),4, 'omitnan'); 
    %pow.powspctrm = pow.powspctrm(:,:,:,(floor(feedback_time)-51):ceil(feedback_time+sampling_rate+26));%keep only 200ms before feedback starts and 100 ms after feedback stops
    %if db==1
        %pow.powspctrm = 10*log10(pow.powspctrm);
    %end
    %TF.data = permute(pow.powspctrm, [1 2 4 3]);
    %TF.data = reshape(TF.data, [size(TF.data,1), size(TF.data,2)*size(TF.data,3)*size(TF.data,4)]);

    %TF.data = TF.data(:,mean(isnan(TF.data),1)<0.5);
    %TF.data = Utilities.downsample(TF.data,sampling_rate/30);    
end


