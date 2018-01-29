%% Tweaks to the set_cfg_shared setup specific to this experiment
% Define stimulus settings, advisor configuration, and experiment structure

%% initialize variables
% define stimulus related variables
cfg.stim.durstim     = .160; % stimulus duration
cfg.stim.SRI1        = 0.2; % stimulus-response interval (stimulus--cj1)
cfg.stim.RSI1        = 1; % response-stimulus interval (cj1 -- advice-prompt)
cfg.stim.RSI2        = 1; % response-stimulus interval (advice-prompt -- advice)
cfg.stim.initialDotDifference = 20;
% fixation cross flicker
cfg.stim.fixationFlicker.time.pre       = .5;   % time between trial start and flick off
cfg.stim.fixationFlicker.time.duration  = .1; % time cross is hidden during flicker
cfg.stim.fixationFlicker.time.post      = .05; % time between cross reappear and stimulus onset
if cfg.debug 
    %cfg.stim.initialDotDifference = 60; 
    %cfg.stim.durstim = 3;
end

%% Define advisors
cfg.advisors.count.real = 2; % number of adviceTypes (0:baseline, 1:same bias, 2:different bias)
cfg.advisors.count.practice = 2;
cfg.advisors.count.all = cfg.advisors.count.real + cfg.advisors.count.practice;
cfg.advisors.duration = 2; % duration of the sound files in s
pic = randperm(cfg.advisors.count.all);
voice = randperm(cfg.advisors.count.all);
% generate advisor objects
for i = 1:cfg.advisors.count.all
    cfg.advisor(i) = getNewAdvisor();
    cfg.advisor(i).id = i;
    cfg.advisor(i).adviceType = i; % skip baseline advisor
    cfg.advisor(i).pic = pic(i);
    cfg.advisor(i).voice = voice(i);
    cfg.advisor(i).name = getAdvisorName(cfg.advisor(i).voice);
    if i > cfg.advisors.count.real  
        cfg.advisor(i).adviceType = 0; % practice advisor is unbiased - always 70|30
    end
end
% don't forget the 'no advice' and 'no choice' advisors
cfg.nullAdvisor.id = NaN; cfg.nullAdvisor.name = getAdvisorName(NaN);
cfg.noAdvisorChoice.id = 0; cfg.noAdvisorChoice.name = getAdvisorName(0);
 
%% define trial and block structure variables
% Each block contains a number of trialsets.
% Each trialset contains x trials where 
% x = ac * (choice*(ac-1 + include_void_choice) + nochoice) + null
% ac = cfg.advisors.count.real
cfg.block_count                     = 12; % number of blocks
cfg.trialset.choice                 = 5; % choice trials with each advisor on the left (1/2 of all choice trials in a trialset)
cfg.trialset.include_void_choice    = 0; % include choices of an observer versus no feedback
cfg.trialset.nochoice               = 10; % no-choice trials for each advisor
cfg.trialset.null                   = 3; % number of null trials = number of observers x 2
cfg.block.trialset_count            = 1; % number of trial sets in each block
cfg.block.questionnaire_frequency   = 3; % include questionnaires after each how many blocks?

cfg.practice.block_count            = 2;
cfg.practice.trial_count            = 25;
cfg.practice.allowChoice            = 1;

if cfg.shortMode 
    if cfg.debug
        cfg.block_count = 1; 
        cfg.trialset.choice = 1; 
        cfg.trialset.nochoice = 1; 
        cfg.trialset.null = 1;%cfg.advisors.count.real * 0; 
        cfg.block.trialset_count = 1;
        cfg.block.questionnaire_frequency = 4;

        cfg.practice.block_count = 2;
        cfg.practice.trial_count = 2;    
    else
        cfg.block_count = 3; 
        cfg.trialset.choice = 5; 

        cfg.practice.block_count = 2;
        cfg.practice.trial_count = 6;            
    end
end

% text definitions and settings
set_cfg_text