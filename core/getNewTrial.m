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
trial.feedback      = false; % whether the trial includes feedback on performance
trial.practice      = false; % whether the trial is a practice trial

%% experiment-specific properties
trial.feedback          = false; % whether feedback is provided on the trial
trial.obsacc            = NaN; % accuracy of the advisor
trial.agree             = NaN; % whether advisor and judge agree
trial.step              = NaN; % see agreementf.m
trial.advisorId         = NaN; % the advisor id
trial.choice            = []; % choice of advisors presented to the judge
trial.advisorPolitics   = false; % whether the trial shows political info about the advisor
trial.advisorPoliticsQ  = NaN; % the SECS question for which advice is shown
trial.taskType          = NaN; % for designs which use multiple tasks, note which kind this is
trial.overrideAdviceType = NaN; % advice type to use instead of advisor's for this trial

%% answer variables
trial.cj1               = []; % confidence judgement 1
trial.cj2               = NaN;
trial.cor               = NaN; % whether the trial was correct (summarises .cor1 and .cor2)
trial.cor1              = NaN; % whether trial 1 was correct
trial.cor2              = NaN;
trial.choiceDecision    = NaN; % judge's choice of advisor (1=left or 2=right)
trial.choiceTime        = NaN; % time the judge's choice of advisor was made
trial.estim_obsacc      = []; % estimated accuracy of the advisor
trial.int1              = NaN; % interval (which side) the 1st judgement was
trial.int2              = NaN;
trial.offset_speech     = NaN;
trial.offsetstim        = NaN; % (target) time the stimulus is removed
trial.onsetstim         = NaN; % (target) time the stimulus is first presented
trial.time_responseStart1    = NaN; % time responding is enabled
trial.time_responseStart2    = NaN; % time the second response is enabled
trial.time_response1        = NaN; % time of the first response
trial.time_response2        = NaN;
trial.time_startTrial   = NaN; % time the trial was started
trial.time_endTrial     = NaN; % time the trial ended
trial.tmissed_offset1   = NaN; % amount target offsetstim was missed by
trial.tmissed_onset1    = NaN;
trial.qanswers          = []; % answers to questionnaires