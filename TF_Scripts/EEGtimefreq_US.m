function [TF,freqs] = EEGtimefreq_US(data, sampling_rate)
    

    data = ft_preproc_dftfilter(data,sampling_rate,60);

    Nsamples=size(data,2);
    N_electrodes=size(data,3);
    ft_defaults;    
    %freqs = 2:2:8;
    %freqs = [0.44, 0.89];
    %freqs = sort(cat(2,linspace(0.88*(2/3),0.44*20*(2/3),15),linspace(0.44*(5/2),0.44*50*(5/2),15),linspace(0.44,0.44*250,20)));
    %freqs(freqs < 1) = [];
    freqs = unique(sort(round(cat(2,linspace(1,100,30),linspace(1,30,30)))));
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
    cfg.t_ftimwin    = 3./cfg.foi;  
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


