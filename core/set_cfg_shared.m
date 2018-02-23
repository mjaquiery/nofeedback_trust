%% build the configuration object
% This file is executed first, which covers core environment settings,
% stimulus settings, advisor definitions, experiment structure, some
% display properties, the random generation seed, and path stuff
%
% Next, we execute the text definition script which defines text strings
% for instructions as well as some display properties of those text
% strings.
%
% Last, we execute a script which defines the bars we draw at various
% points such as the progress, confidence, and questionnaire bars.

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
 
%% the grid for the placement of the dots:
cfg.xymatrix = [repmat([-57,-51,-45,-39,-33,-27,-21,-15,-9,-3,3,9,15,21,27,33,39,45,51,57],1,20);...
    sort(repmat([-57,-51,-45,-39,-33,-27,-21,-15,-9,-3,3,9,15,21,27,33,39,45,51,57],1,20))];
% sort(repmat([-86,-67,-48,-29,-10,10,29,48,67,86],1,10))];

%% Set display variables
cfg.display.fixationX.size = 20; % font size for the fixation + 
cfg.display.fixationX.color = [0 0 0]';

cfg.display.portrait.width = 258;
cfg.display.portrait.height = 325; 
cfg.display.portrait.nameTextOffset = 20;
cfg.display.portrait.nameTextSize = 20;

cfg.display.choice.offset = 0.15;
cfg.display.choice.frame.width = 2;
cfg.display.choice.frame.gap = cfg.display.choice.frame.width + 2; % gap between the portrait and the frame
cfg.display.choice.frame.color = [.8 .8 .8]';
cfg.display.choice.instructionTextOffset = cfg.display.portrait.height/2 + 50;
cfg.display.choice.portraitSize = [cfg.display.portrait.width/2 cfg.display.portrait.height/2];
cfg.display.color.background = [.5 .5 .5]';

cfg.display.progress.color.frame    = [.9 .9 .9]'; % Progress bar container colour
cfg.display.progress.color.bar      = [.9 .9 .9]'; % Progress bar colour
cfg.display.progress.text.start     = 'start';
cfg.display.progress.text.end       = 'end';

% Reseed the random-number generator for each expt.
cfg.resetrn = sum(100*clock);
rng(cfg.resetrn);

% save paths in cfg
cfg.path.base               = my_path;
cfg.path.results            = results_path;
cfg.path.stims              = stims_path;
cfg.path.slash              = osSlash;

%% Audio files
cfg.audio.feedback.negative = [stims_path osSlash 'sounds' osSlash 'feedbackNegative.wav'];

%% Advice format flags
% these are currently only implemented for the quiz task
cfg.adviceFormat.none = 0;
cfg.adviceFormat.voice = 1;
cfg.adviceFormat.speechBubble = 2;
cfg.adviceFormat.arrow = 4; % not yet implemented

%% Task managers
cfg.taskManager.dotTask.lastTwoTrials = [];
cfg.taskManager.quiz.trialNumber = 0;

% bar and cursor settings
define_scale

% shared text strings
set_cfg_shared_text