function [trials] = runTrial(trials, t)
%% Run an experimental trial and return the results
%
% usage: runTrial(trials, t)
%
% Inputs: 
% trials: trial list created by build_trials.m
% t: index of the current trial
%
% Outputs:
% trials: a version of the trial list with results recorded for trials(t)
%
global cfg; % configuration object
    
    switch trials(t).taskType 
        case cfg.taskType.dots
            trials = doDotTask(trials, t);
        case cfg.taskType.quiz
            % Quiz
            trials = doQuizTask(trials, t);
    end
    