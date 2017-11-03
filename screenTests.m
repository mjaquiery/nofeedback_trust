 %% STUDY 3 - agreement in confidence and uncertainty  #####################
%% script by Niccolo Pescetelli %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% niccolo.pescetelli@psy.ox.ac.uk
%% Extensions by Matt Jaquiery
% matt.jaquiery@psy.ox.ac.uk
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cd('G:\Documents\University\Programming\nofeedback_trust/')

%% clear workspace and create directories
clear all;
close all;
clc;
feedbackEnabled = 0;
debugMode = 1;
set_path

%% PTB compatibility
% check for Opengl compatibility, abort otherwise
AssertOpenGL;


%% Subjects' details and directory
create_subject_directory

tic
%% Settings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%  start PTB   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Sc = start_psychtb(subject.screen);
Screen('Preference','SuppressAllWarnings', 1);

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

% Get timestamp for experiment start
cfg.startexp = GetSecs;

% initialize dotdifference
trials(1).dotdifference = cfg.stim.initialDotDifference;
trials(2).dotdifference = cfg.stim.initialDotDifference;

%% Begin testing elements
    cfg.currentTrial = starttrial;

% add all static elements
draw_static(Sc, cfg)
Screen('Flip',Sc.window);

% collect response
if(cfg.debug)   disp('Collecting first response...'); end
[trials(t).cj1, trials(t).resp1_time, trials(t).int1] = drag_slider(Sc, cfg); % responded is 1 or 0; cj1 is the first confidence judgement
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Thanks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insimdata = imread(char('instructions/instr4.png'));
texins = Screen('MakeTexture', Sc.window, insimdata);
Screen('DrawTexture', Sc.window, texins);
Screen('Flip',Sc.window);
Beeper(261.63,.4,1);
WaitSecs(1.000);
KbWait;

%% close PTB
Screen('CloseAll');
ListenChar(0);
DisableKeysForKbCheck([]);
ShowCursor()
Priority(0);
toc
    