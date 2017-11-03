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
% define paradigm related variables
cfg.ntrials = 36;       if cfg.debug cfg.ntrials=8; end % total trials in a block
cfg.nullt   = 4;        if cfg.debug cfg.nullt=1; end % null trials in a block
cfg.nblocks = 12;       if cfg.debug cfg.nblocks=2; end
cfg.ntrialsprac = 25;   if cfg.debug cfg.ntrialsprac=3; end % total trials in a practice block
cfg.nblocksprac = 2; % 1=task practice, 2=advice practice
cfg.ntrialsall = cfg.nblocksprac*cfg.ntrialsprac + cfg.ntrials*cfg.nblocks;
cfg.questionnaire_frequency = 4;

% define stimulus related variables
cfg.stim.durstim     = .160; % stimulus duration
cfg.stim.RSI1        = 1; % response-stimulus interval (cj1 -- advice-prompt)
cfg.stim.RSI2        = 1; % response-stimulus interval (advice-prompt -- advice)
cfg.stim.initialDotDifference = 20;

% define observer-related variables
cfg.nobs                    = 3; % number of obstypes (0:baseline, 1:same bias, 2:different bias)
cfg.observer.pic            = randperm(4);
cfg.observer.voice          = randperm(4);
cfg.observer.duration       = 2; % duration in seconds
 
% the grid for the placement of the dots:
cfg.xymatrix = [repmat([-57,-51,-45,-39,-33,-27,-21,-15,-9,-3,3,9,15,21,27,33,39,45,51,57],1,20);...
    sort(repmat([-57,-51,-45,-39,-33,-27,-21,-15,-9,-3,3,9,15,21,27,33,39,45,51,57],1,20))];
% sort(repmat([-86,-67,-48,-29,-10,10,29,48,67,86],1,10))];

% Reseed the random-number generator for each expt.
cfg.resetrn = sum(100*clock);
rand('state',cfg.resetrn);

set_cfg_text

% save paths in cfg
cfg.my_path                 = my_path;
cfg.results_path            = results_path;
cfg.stims_path              = stims_path;