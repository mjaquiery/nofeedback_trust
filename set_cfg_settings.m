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

%% initialize variables
% define stimulus related variables
cfg.stim.durstim     = .160; % stimulus duration
cfg.stim.RSI1        = 1; % response-stimulus interval (cj1 -- advice-prompt)
cfg.stim.RSI2        = 1; % response-stimulus interval (advice-prompt -- advice)
cfg.stim.initialDotDifference = 20;
if cfg.debug, cfg.stim.initialDotDifference = 60; end

% define observer-related variables
cfg.nobs                    = 3; % number of obstypes (0:baseline, 1:same bias, 2:different bias)
cfg.observer.pic            = randperm(4);
cfg.observer.voice          = randperm(4);
cfg.observer.duration       = 2; % duration in seconds
 
% the grid for the placement of the dots:
cfg.xymatrix = [repmat([-57,-51,-45,-39,-33,-27,-21,-15,-9,-3,3,9,15,21,27,33,39,45,51,57],1,20);...
    sort(repmat([-57,-51,-45,-39,-33,-27,-21,-15,-9,-3,3,9,15,21,27,33,39,45,51,57],1,20))];
% sort(repmat([-86,-67,-48,-29,-10,10,29,48,67,86],1,10))];

% define trial and block structure variables
% Each block contains a number of trialsets.
% Each trialset contains x trials where x = nobs * choice * nochoice + null
% + (nobs * include_void_choice)
cfg.block_count                     = 12; % number of blocks
cfg.trialset.choice                 = 2; % choice trials lead by each advisor (1/2 of all choice trials in a trialset)
cfg.trialset.include_void_choice    = 1; % include choices of an observer versus no feedback
cfg.trialset.nochoice               = 1; % no-choice trials for each advisor
cfg.trialset.null                   = cfg.nobs * 2; % number of null trials = number of observers x 2
cfg.block.trialset_count            = 2; % number of trial sets in each block
cfg.block.questionnaire_frequency   = 4; % include questionnaires after each how many blocks?

cfg.practice.block_count            = 2;
cfg.practice.trial_count            = 25;

if cfg.debug==1
    cfg.block_count = 1; 
    cfg.trialset.choice = 1; 
    cfg.trialset.nochoice = 0; 
    cfg.trialset.null = cfg.nobs * 0; 
    cfg.block.trialset_count = 1;
    cfg.block.questionnaire_frequency = 4;

    cfg.practice.block_count = 2;
    cfg.practice.trial_count = 2;    
end

%% Set display variables
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
rand('state',cfg.resetrn);

set_cfg_text

% save paths in cfg
cfg.path.base               = my_path;
cfg.path.results            = results_path;
cfg.path.stims              = stims_path;