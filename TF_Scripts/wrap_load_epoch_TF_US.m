% 2023-12-29 AndyP
% Wrapper script for load_epoch_TF_US
% Runs TF analysis on Momentum subjects using FieldTrip toolbox

IDs_new = readmatrix('momentum308.csv');
load('MomentumIDs226.mat','IDs');

IDs = setdiff(IDs_new(:,2), IDs(:,2));

%ID1 = IDs(88,:);
%ID2 = IDs(61,:);

%IDs = cat(1,ID1,ID2);
nD = length(IDs);


ft_defaults

parfor iD = 1:nD
    name = mat2str(IDs(iD));
    disp(name);
    load_epoch_TF_US(name);
end