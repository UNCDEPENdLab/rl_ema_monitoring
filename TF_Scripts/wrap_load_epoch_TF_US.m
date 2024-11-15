% 2023-12-29 AndyP
% Wrapper script for load_epoch_TF_US
% Runs TF analysis on Momentum subjects using FieldTrip toolbox

%IDs_new = readmatrix('momentum308.csv');
%load('MomentumIDs226.mat','IDs');

%IDs = setdiff(IDs_new(:,2), IDs(:,2));


%IDs = [440931, 440949, 540353, 540356, 540358, 540365, 540369, 540371, 540377,540384];
%ID1 = IDs(88,:);
%ID2 = IDs(61,:);

%IDs = [440779,540255,540254,540240, 540249,440731,540236,540258,540228,440745,540221,540238,440774];

IDs = [440663, 440529, 440667];
IDs = IDs';
%IDs = cat(1,ID1,ID2);
nD = length(IDs);


ft_defaults

parfor iD = 1:nD
    name = mat2str(IDs(iD));
    disp(name);
    load_epoch_TF_US(name);
end