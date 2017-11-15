cfg                         = [];
cfg.computer                = computer;
cfg.os                      = getenv('OS');
%cfg.scripts                 = getscripts(); % save current scripts in cfg;
%-- Input device
cfg.response.until_release  = 1;
cfg.response.escape         = 'ESCAPE';
cfg.response.pause          = 'p';

%% default
if ~isfield(cfg,'port'),            cfg.port = 'keyboard';end
if ~isfield(cfg,'experimenter'),    cfg.experimenter = 'space';end
if ~isfield(cfg,'escape'),          cfg.escape = 'escape';end
if ~isfield(cfg,'pause'),           cfg.pause = 'P';end
if ~isfield(cfg,'until_release'),   cfg.until_release = true;end
if ~isfield(cfg,'restarted'),       cfg.restarted = false;end
if ~isfield(cfg, 'debug'),          cfg.debug = debugMode;end
if ~isfield(cfg, 'shortMode')       cfg.shortMode = shortMode; end

%% initialize variables
% define stimulus related variables
cfg.stim.durstim     = .160; % stimulus duration
cfg.stim.SRI1        = 0.2; % stimulus-response interval (stimulus--cj1)
cfg.stim.RSI1        = 1; % response-stimulus interval (cj1 -- advice-prompt)
cfg.stim.RSI2        = 1; % response-stimulus interval (advice-prompt -- advice)
cfg.stim.initialDotDifference = 20;
if cfg.debug 
    %cfg.stim.initialDotDifference = 60; 
    %cfg.stim.durstim = .3;
end

%% Define advisors
cfg.advisors.count.real = 2; % number of adviceTypes (0:baseline, 1:same bias, 2:different bias)
cfg.advisors.count.practice = 1;
cfg.advisors.count.all = cfg.advisors.count.real + cfg.advisors.count.practice;
cfg.advisors.duration = 2; % duration of the sound files in s
pic = randperm(cfg.advisors.count.all);
voice = randperm(cfg.advisors.count.all);
% generate advisor objects
for i = 1:cfg.advisors.count.all
    cfg.advisor(i).id = i;
    cfg.advisor(i).adviceType = i; % skip baseline advisor
    cfg.advisor(i).pic = pic(i);
    cfg.advisor(i).voice = voice(i);
    cfg.advisor(i).name = getAdvisorName(cfg.advisor(i).voice);
    if i > cfg.advisors.count.real 
        cfg.advisors.practice = i; 
        cfg.advisor(i).adviceType = 0; % practice advisor is unbiased - always 70|30
    end
end
% don't forget the 'no advice' advisor
cfg.nullAdvisor.id = NaN; cfg.nullAdvisor.name = getAdvisorName(NaN);
 
%% the grid for the placement of the dots:
cfg.xymatrix = [repmat([-57,-51,-45,-39,-33,-27,-21,-15,-9,-3,3,9,15,21,27,33,39,45,51,57],1,20);...
    sort(repmat([-57,-51,-45,-39,-33,-27,-21,-15,-9,-3,3,9,15,21,27,33,39,45,51,57],1,20))];
% sort(repmat([-86,-67,-48,-29,-10,10,29,48,67,86],1,10))];

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

if cfg.shortMode==1
    cfg.block_count = 1; 
    cfg.trialset.choice = 1; 
    cfg.trialset.nochoice = 1; 
    cfg.trialset.null = 1;%cfg.advisors.count.real * 0; 
    cfg.block.trialset_count = 1;
    cfg.block.questionnaire_frequency = 4;

    cfg.practice.block_count = 2;
    cfg.practice.trial_count = 2;    
end

%% Set display variables
cfg.display.forceResolution = [1300 600];
cfg.display.fixationXsize = 20; % font size for the fixation + 
cfg.display.portrait.width = 258;
cfg.display.portrait.height = 325;
cfg.display.portrait.nameTextOffset = 20;
cfg.display.portrait.nameTextSize = 20;
cfg.display.choice.offset = 500;
cfg.display.choice.frame.width = 10;
cfg.display.choice.frame.gap = cfg.display.choice.frame.width + 5; % gap between the portrait and the frame
cfg.display.choice.frame.color = [.8 .8 .8];
cfg.display.choice.instructionTextOffset = cfg.display.portrait.height/2 + 50;

% Reseed the random-number generator for each expt.
cfg.resetrn = sum(100*clock);
rng(cfg.resetrn);

% save paths in cfg
cfg.path.base               = my_path;
cfg.path.results            = results_path;
cfg.path.stims              = stims_path;

set_cfg_text