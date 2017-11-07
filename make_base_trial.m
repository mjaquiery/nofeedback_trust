function [trial] = getNewTrial(trialID, block)
%% Returns a trial object with blanked entries for filling in later
% 
% Usage: 
%   block_trials1(end+1) = getNewTrial(block_trials1(end).id+1,block);
%
% Inputs:
%   trialID - the numerical identifier of the trial
%   block   - the block number
%
% Ouputs:
%   trial - a trial object with blank/default parameters
%
trial.id            = trialID; % numerical identifer (gives creation order even when shuffled)
trial.block         = b; % block number
trial.feedback      = false; % whether feedback is provided on the trial
trial.cj1           = []; % confidence judgement 1
trial.obsacc        = NaN; % accuracy of the advisor
trial.estim_obsacc  = []; % estimated accuracy of the advisor
trial.advisorID     = NaN; % the advisor id
trial.choice        = []; % choice of advisors presented to the judge
trial.choiceDecision = NaN; % judge's choice of advisor (1=left or 2=right)
trial.qanswers      = []; % answers to questionnaires
% should turn this into a function at some point
% trialid                                 = trialid+1;
% block_trials1(end+1).trialid            = trialid;
% block_trials1(end).estim_obsacc         = [];
% block_trials1(end).block                = b;
% block_trials1(end).feedback             = false;
% block_trials1(end).cj1                  = [];
% block_trials1(end).obsacc               = NaN; 
% block_trials1(end).obstype              = NaN;
% block_trials1(end).pic                  = NaN;
% block_trials1(end).voice                = NaN;
% block_trials1(end).choice               = [];
% block_trials1(end).choiceDecision       = -1; % NaN is used for selecting 'no advisor'
% block_trials1(end).choiceTime           = NaN;
% block_trials1(end).qanswers             = [];