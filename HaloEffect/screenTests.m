  %% STUDY 3 - agreement in confidence and uncertainty  #####################
%% script by Niccolo Pescetelli %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% niccolo.pescetelli@psy.ox.ac.uk   
%% Extensions by Matt Jaquiery
% matt.jaquiery@psy.ox.ac.uk
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global cfg;
global Sc;
%% PTB compatibility
% check for Opengl compatibility, abort otherwise
AssertOpenGL;        

          
%% Subjects' details and directory
create_subject_directory

tic
%%%%%%%%%%%%%%%%%%%%%%%  start PTB   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Sc = start_psychtb(subject.screen, forceResolution);
Screen('Preference','SuppressAllWarnings', 1);

%% Settings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_cfg_shared
set_cfg_settings

%% Read in audio files
import_audio_files

%% Build trials
build_trials

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%% start experiment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
starttrial=1;

Screen('TextSize',Sc.window,cfg.instr.textSize.medium);
Screen('TextColor',Sc.window,cfg.instr.textColor.default);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cfg.fps = Sc.fps;                                                               % refresh rate (Hz). IMPORTANT! Make sure this value equals the RR of the screen used AND the RR in the start_psychtb.m function
cfg.frame = (1/cfg.fps);                                                     % average duration (ms) of a frame
if ~isempty(subject.id)
    ListenChar(2);
    HideCursor();
end
% define rectangles
define_rectangles

% define confidence bar
define_scale

% define cursor possible positions along x-axis
cfg.bar.xshift = [linspace(cfg.bar.barrect(1)+cfg.bar.cursorwidth.*.5,...
        cfg.bar.gaprect(1)-cfg.bar.cursorwidth.*.5,cfg.bar.maxScale) ...
    linspace(cfg.bar.gaprect(3)+cfg.bar.cursorwidth.*.5, ...
        cfg.bar.barrect(3)-cfg.bar.cursorwidth.*.5,cfg.bar.maxScale)];

% load advisor portraits
load_advisor_portraits
 
% Get timestamp for experiment start
cfg.startexp = GetSecs;

% initialize dotdifference
trials(1).dotdifference = cfg.stim.initialDotDifference;
trials(2).dotdifference = cfg.stim.initialDotDifference;

%% Begin testing elements
cfg.currentTrialNumber = starttrial;
cfg.currentTrial = trials(starttrial);
t = 1;   

% add all static elements
%draw_static(Sc, cfg)
%Screen('Flip',Sc.window);

%introduce_observers

%questionnaire

%getAdvisorChoice(NaN, 2);

%estimated_obsacc();

%global SECSscore;
% [a, b, SECSscore] = SECS();

%showAdvisorPolitics(1);

for t = 1:2
    trials(t).advisorId = 3;
%     trials(t).taskType = cfg.taskType.quiz;
%     trials = doQuizTask(trials, t);
    trials(t).taskType = cfg.taskType.dots;
    trials = doDotTask(trials, t);
end

%quizFeedback(trials,1,5);

% defineQuiz();
% for t = 1:106
%     if t > length(trials)
%         trials = [trials trials(t-1)];
%     end
%     trials(t).taskType = cfg.taskType.quiz;
%     trials(t).question = cfg.quiz{cfg.quizOrder(t)};
%     trials(t).cor = rand() < .7;
% end

% quizFeedback(trials);

%% close PTB
Screen('CloseAll');
ListenChar(0);
DisableKeysForKbCheck([]);
ShowCursor();
Priority(0);
toc
    