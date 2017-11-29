%% Launcher script
% handles compatability across multiple installs on various machines.
% by matt.jaquiery@psy.ox.ac.uk

%% clear workspace and create directories
clear all;
close all;
clc;
experimentName = 'AdvisorChoice';
forceResolution = [1300 600];
feedbackEnabled = 0; 
debugMode = 1;
shortMode = 1;
pathRoot = 'MattWork';

%% set path 
switch pathRoot
    case 'MattHome'  
        my_path = 'G:\Documents\University\Programming\nofeedback_trust_matt\'; % working directory
        if ~feedbackEnabled
            results_path = 'G:\Documents\University\Oxford\Work\matlabResults\';
        else
            results_path = 'G:\Documents\University\Oxford\Work\matlabResults\Feedback\';
        end
        stims_path    = [my_path 'stims '];
        osSlash = '\'; 
        screenNumber = 2; % this tells psychtoolbox which screen to draw on
        
    case 'MattWork'
        my_path = 'C:\Users\mj221\Documents\Programming\MATLAB\nofeedback_trust\'; % working directory
        if ~feedbackEnabled
            results_path = 'C:\Users\mj221\Filr\My Files\Results\AdvisorChoice';
        else  
            results_path = 'C:\Users\mj221\Filr\My Files\Results\AdvisorChoice\';
        end
        stims_path    = [my_path 'stims']; 
        osSlash = '\';
        screenNumber = 1;
end

rmpath(genpath(my_path)); % remove the current path so we can add selectively
addpath(my_path); % add base folder
addpath(genpath([my_path experimentName])); % add experiment folder
addpath(genpath([my_path 'core'])); % add core files

% set some path extensions depending on the OS's slash direction
stim_folder = [osSlash 'sounds' osSlash 'Voice'];
          
cd(my_path)
screenTests
%main
