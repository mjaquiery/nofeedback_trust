function [trial] = getNewTrial(trialId, block)
%% Returns a trial object with blanked entries for filling in later
% - by Matt Jaquiery
%
% Usage: 
%   block_trials = [block_trials getNewTrial(block_trials(end).id+1,block)];
%
% Inputs:
%   trialId - the numerical identifier of the trial
%   block   - the block number
%
% Ouputs:
%   trial - a trial object with blank/default parameters
%
%% Generic properties
trial.id            = trialId; % numerical identifer (gives creation order even when shuffled)
trial.block         = block; % block number
trial.break         = false; % whether to include a break before the trial
trial.instr         = false; % whether the trial is prepended with instructions
trial.questionnaire = false; % whether the trial includes a questionnaire presentation
trial.feedback      = false;

%% experiment-specific properties
trial.feedback          = false; % whether feedback is provided on the trial
trial.obsacc            = NaN; % accuracy of the advisor
trial.agree             = NaN; % whether advisor and judge agree
trial.step              = NaN; % see agreementf.m
trial.advisorId         = NaN; % the advisor id
trial.choice            = []; % choice of advisors presented to the judge
trial.advisorPolitics   = false; % whether the trial shows political info about the advisor
trial.taskType          = NaN; % for designs which use multiple tasks, note which kind this is

%% answer variables
trial.cor               = NaN; % whether the trial was correct (summarises .cor1 and .cor2)
trial.qanswers          = []; % answers to questionnaires
trial.cj1               = []; % confidence judgement 1
trial.cor1               = NaN; % whether trial was correct
trial.choiceDecision    = NaN; % judge's choice of advisor (1=left or 2=right)
trial.choiceTime        = NaN; % time the judge's choice of advisor was made
trial.estim_obsacc      = []; % estimated accuracy of the advisor
trial.resp2_time        = NaN;
trial.cj2               = NaN;
trial.cor2              = NaN;
trial.int2              = NaN;
trial.responded2        = NaN;
trial.rt2               = NaN;
trial.resp_advice_rt    = NaN;
trial.offset_speech     = NaN;