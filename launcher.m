  %% Launcher script
% handles compatability across multiple installs on various machines.
% by matt.jaquiery@psy.ox.ac.uk
%
% Invokes experiments by reference to experimentName which is the
% experiment folder. The experiment folder MUST contain main.m which is
% called by this script.

%% clear workspace and create directories
clear all;
close all;
clc;
%experimentName = 'HaloEffect';
%experimentName = 'AdvisorChoice';
experimentName = 'PoliticalDifferences';
forceResolution = [];
%forceResolution = [1300 600];
feedbackEnabled = 0; 
debugMode = 1;
shortMode = 1;
pathRoot = 'AlexLaptop';

%% set path   
switch pathRoot
    case 'MattHome'  
        my_path = 'G:\Documents\University\Programming\nofeedback_trust_matt\'; % working directory
        if ~feedbackEnabled
            results_path = ['D:\Users\MJ\Filr\My Files\Results\' experimentName];
        else
            results_path = ['D:\Users\MJ\Filr\My Files\Results\' experimentName '\Feedback\'];
        end
        stims_path    = [my_path 'stims'];
        osSlash = '\'; 
        screenNumber = 2; % this tells psychtoolbox which screen to draw on
        
    case 'MattWork'
        my_path = 'C:\Users\mj221\Documents\Programming\MATLAB\nofeedback_trust\'; % working directory
        if ~feedbackEnabled
            results_path = ['C:\Users\mj221\Filr\My Files\Results\' experimentName];
        else  
            results_path = ['C:\Users\mj221\Filr\My Files\Results\' experimentName '\Feedback\'];
        end
        stims_path    = [my_path 'stims']; 
        osSlash = '\';
        screenNumber = 1;
        
    case 'AlexLaptop'
        my_path = '/Users/Alex/Documents/MATLAB/Project/';
        results_path = ['/Users/Alex/Documents/MATLAB/Project/Results/' experimentName];
        stims_path = [my_path 'stims'];
        osSlash = '/';
        screenNumber = 0;
        Screen('Preference', 'SkipSyncTests', 1);
end

warning('off','MATLAB:rmpath:DirNotFound')
rmpath(genpath(my_path)); % remove the current path so we can add selectively
warning('on','MATLAB:rmpath:DirNotFound')
addpath(my_path); % add base folder
addpath(genpath([my_path experimentName])); % add experiment folder
addpath(genpath([my_path 'core'])); % add core files

% set some path extensions depending on the OS's slash direction
stim_folder = [osSlash 'sounds' osSlash 'Voice'];
          
cd(my_path)
%screenTests
main
