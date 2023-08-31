%this function finds alternative configurations in the case we choose to use
%only 3/2/1 electrodes:
function [best_single, best_two_config, best_three_config, EEG_percen_single, EEG_percen_best_two, EEG_percen_best_three, ind_na_best_single, ind_na_best_two, ind_na_best_three]=find_optimal_comb(epoch_data, Ntotal)
    
    % 2022-10-05 AndyP: Looking at the second dimension.  Number of trials
    % is in dim=1, Asking how many trials have 0 nans for each channel.
    trials_with_good_one = find(~any(isnan(epoch_data(:,:,1)),2));
    trials_with_good_two = find(~any(isnan(epoch_data(:,:,2)),2));
    trials_with_good_three = find(~any(isnan(epoch_data(:,:,3)),2));
    trials_with_good_four = find(~any(isnan(epoch_data(:,:,4)),2));
    
    %find best configuration of three electrodes:
    optional_cofig_three= ["1-2-3", "1-2-4", "1-3-4", "2-3-4"];
    good_one_two_three = intersect(intersect(trials_with_good_one,trials_with_good_two),trials_with_good_three);
    good_one_two_four = intersect(intersect(trials_with_good_one,trials_with_good_four),trials_with_good_two);      
    good_one_three_four = intersect(intersect(trials_with_good_one,trials_with_good_four),trials_with_good_three);
    good_two_three_four = intersect(intersect(trials_with_good_three,trials_with_good_two),trials_with_good_four);
    % find maximum of g123, g124, g134, g234, shouldn't we set this = TRUE
    % and have the default be FALSE? 2022-10-05 AndyP
    [max_three, ind_three] = max([length(good_one_two_three), length(good_one_two_four), length(good_one_three_four), length(good_two_three_four)]);
    best_three_config= optional_cofig_three(ind_three);
    EEG_percen_best_three= (max_three/Ntotal)*100;
    ind_na_best_three = true(Ntotal,1); 
    % 2022-10-05 AndyP: I think the default should be FALSE and we should conditionally set ind_na_best_three=TRUE based on index of max
    if ind_three==1
       ind_na_best_three(good_one_two_three)=false;
    elseif ind_three==2
        ind_na_best_three(good_one_two_four)=false;
    elseif ind_three==3
        ind_na_best_three(good_one_three_four)=false;
    else 
        ind_na_best_three(good_two_three_four)=false;
    end
    
    %find best configuration of two electrodes:
    optional_cofig_two= ["1-2", "1-3", "1-4", "2-3", "2-4", "3-4"];
    good_one_two = intersect(trials_with_good_one,trials_with_good_two);
    good_one_three = intersect(trials_with_good_one,trials_with_good_three);      
    good_one_four = intersect(trials_with_good_one,trials_with_good_four);
    good_two_three = intersect(trials_with_good_two,trials_with_good_three);
    good_two_four = intersect(trials_with_good_two,trials_with_good_four);
    good_three_four = intersect(trials_with_good_three,trials_with_good_four);
    [max_two, ind_two] = max([length(good_one_two), length(good_one_three), length(good_one_four), length(good_two_three), length(good_two_four), length(good_three_four)]);
    best_two_config= optional_cofig_two(ind_two);
    EEG_percen_best_two= (max_two/Ntotal)*100;
    ind_na_best_two = true(Ntotal,1);
    if ind_two==1
       ind_na_best_two(good_one_two)=false;
    elseif ind_two==2
       ind_na_best_two(good_one_three)=false;
    elseif ind_two==3
        ind_na_best_two(good_one_four)=false;
    elseif ind_two==4
        ind_na_best_two(good_two_three)=false;
    elseif ind_two==5
        ind_na_best_two(good_two_four)=false;
    else 
        ind_na_best_two(good_three_four)=false;
    end
    
    %find best single electrode:
    optional_single= ["1", "2", "3", "4"];
    [max_single, ind_single]=max([length(trials_with_good_one), length(trials_with_good_two), length(trials_with_good_three), length(trials_with_good_four)]);
    best_single= optional_single(ind_single);
    EEG_percen_single= (max_single/Ntotal)*100;
    ind_na_best_single = true(Ntotal,1);
    if ind_single==1
        ind_na_best_single(trials_with_good_one)=false;
    elseif ind_single==2
        ind_na_best_single(trials_with_good_two)=false;
    elseif ind_single==3
        ind_na_best_single(trials_with_good_three)=false;
    else 
        ind_na_best_single(trials_with_good_four)=false;
    end
    
end