function [cfg, subject, trials] = prepR(cfg, subject, trials)
%% Prepare data for analysis in R
% Matt Jaquiery, Jan 2018
% 
% usage: [cfg, subject, trials] = prepR(cfg, subject, trials)
%
% inputs & outputs: 
% cfg: configuration object storing experiment configuration information
% subject: configuration object storing subject-specific details
% trials: list of trial objects
%
% This function will strip out the unwieldy data from the variables and
% return them.

cfg.advisor = rmfield(cfg.advisor, 'imdata');
cfg.nullAdvisor = rmfield(cfg.nullAdvisor, 'imdata');
cfg.noAdvisorChoice = rmfield(cfg.noAdvisorChoice, 'imdata');
cfg = rmfield(cfg, {...
    'xymatrix', ...
    'introSpeechFreq', ...
    'introSpeechData', ...
    'introSpeechChannels', ...
    'speechFreq', ...
    'speechData', ...
    'speechChannels', ...
    'currentTrial'});


