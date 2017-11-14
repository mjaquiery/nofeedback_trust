%% Launcher script
% handles compatability across multiple installs on various machines.
% by matt.jaquiery@psy.ox.ac.uk

%% clear workspace and create directories
clear all;
close all;
clc;
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
        stims_path    = [my_path 'stims'];
        addpath(my_path);
        addpath([my_path 'confidenceSlider']);
        slash = '\';
        
    case 'MattWork'
        my_path = 'C:\Users\mj221\Documents\Programming\MATLAB\nofeedback_trust\'; % working directory
        if ~feedbackEnabled
            results_path = 'C:\Users\mj221\Documents\Results\AdvisorChoice\';
        else
            results_path = 'C:\Users\mj221\Documents\Results\AdvisorChoice\';
        end
        stims_path    = [my_path 'stims'];
        addpath(my_path);
        addpath([my_path 'confidenceSlider']);
        slash = '\';
end

% set some path extensions depending on the OS's slash direction
stim_folder = [slash 'sounds' slash 'Voice'];
        
cd(my_path)
main