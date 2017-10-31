%% STUDY 3 - agreement in confidence and uncertainty  #####################
%% script by Niccolo Pescetelli %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% niccolo.pescetelli@psy.ox.ac.uk
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cd('C:/Users/acclab/Desktop/Niccolo/study3/')

%% clear workspace and create directories
clear all;
close all;
clc;
set_path

%% PTB compatibility
% check for Opengl compatibility, abort otherwise
AssertOpenGL;


%% Subjects' details and directory
create_subject_directory

tic
%% Settings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cfg                         = [];
%-- reseed random number generator
cfg.resetrn                 = rng('shuffle');
cfg.computer                = computer;
cfg.os                      = OS;
cfg.scripts                 = getscripts(); % save current scripts in cfg;
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

%% initialize variables
% define paradigm related variables
cfg.ntrials                     = 35;% total trials in a block
cfg.nullt                       = 5; % null trials in a block
cfg.nblocks                     = 12;
cfg.ntrialsprac                 = 25;% total trials in a practice block
cfg.nblocksprac                 = 2; % 1=task practice, 2=advice practice

% define stimulus related variables
cfg.stim.durstim                = .160; % stimulus duration
% response-stimulus interval (cj1 -- advice-prompt)
cfg.stim.RSI1                   = 1;    
% response-stimulus interval (advice-prompt -- advice)
cfg.stim.RSI2                   = 1;    
cfg.stim.beep                   = sin(2*pi*[0:1/44100:.1]*1000);% error tone
cfg.stim.beeprate               = 22254;                        % error tone rate
cfg.stim.beepvolume             = .5;                           % error tone volume
cfg.stim.initialDotDifference   = 20;

% define observer-related variables
cfg.nobs                    = 3; % number of obstypes (0:baseline, 1:same bias, 2:different bias)
cfg.observer.pic            = randperm(4);
cfg.observer.voice          = randperm(4);
cfg.observer.duration       = 2; % duration in seconds
 
% the grid for the placement of the dots:
cfg.xymatrix = [repmat(linspace(-57,57,20),1,20);...
    sort(repmat(linspace(-57,57,20),1,20))];

% landmarks on confidence scale
cfg.instr.cjtext        = {'maybe' 'certainly'};
cfg.instr.interval      = {'LEFT' 'RIGHT'};

% Text prompts
cfg.instr.prompt1           = {};
cfg.instr.finaldecision     = {'What'' s your final decision?'};
cfg.instr.instr             = {'Left click with your mouse to make a decision.' 'Press spacebar to confirm response.'};
cfg.instr.estimated_obsacc  = {'Your baseline accuracy (before any advice) was 71%' 'What do you think this person''s accuracy was?' 'In the next screen you will be prompted to enter a value' 'Press any button when you are ready' 'Enter a number between 0 and 100 and press Enter: '};

% save paths in cfg
cfg.my_path                 = my_path;
cfg.results_path            = results_path;
cfg.stims_path              = stims_path;

%% Read in audio files
import_audio_files

%% Build trials
build_trials

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%% start experiment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% in case experiment was restarted after crash
if (subject.restarted)
    [filename, pathname] = uigetfile('*.mat', 'Pick last saved file ');
    load([pathname filename]);
    starttrial = t;
    cfg.restarted = 1;
else starttrial=1;
end
%%%%%%%%%%%%%%%%%%%%%%%  start PTB   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Sc = start_psychtb(subject.screen);
% Screen('Preference','SuppressAllWarnings', 1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cfg.fps = Sc.fps;                                                               % refresh rate (Hz). IMPORTANT! Make sure this value equals the RR of the screen used AND the RR in the start_psychtb.m function
cfg.frame = (1/cfg.fps);                                                     % average duration (ms) of a frame
if ~isempty(subject.id)
    ListenChar(2);
    HideCursor();
end

%% define rectangles
define_rectangles

%% define confidence bar
cfg.bar.maxScale            = 50;
cfg.bar.minScale            = -50;
cfg.bar.nScale              = length([cfg.bar.minScale:cfg.bar.maxScale]);
cfg.bar.cursorwidth         = Sc.rect(3)/200;
cfg.bar.cursorheight        = 20;
cfg.bar.positiony           = .8;
cfg.bar.barrect             = CenterRectOnPoint([0 0 ...
    (cfg.bar.nScale*cfg.bar.cursorwidth) ...
    (cfg.bar.cursorheight)], ...
    Sc.center(1), ...
    Sc.rect(4)*cfg.bar.positiony);
cfg.bar.barlength           = cfg.bar.barrect(3)- cfg.bar.barrect(1);
cfg.bar.gap_size            = 10;
cfg.bar.gaprect             = CenterRectOnPoint([0,0, ...
    cfg.bar.cursorwidth * cfg.bar.gap_size, ...
    cfg.bar.cursorheight],...
    Sc.center(1), ...
    Sc.rect(4)*cfg.bar.positiony);
cfg.bar.gaplength=(cfg.bar.gaprect(3)-cfg.bar.gaprect(1)).*.5;

% define cursor possible positions along x-axis
cfg.bar.xshift = [linspace(cfg.bar.barrect(1)+cfg.bar.cursorwidth.*.5,...
        cfg.bar.gaprect(1)-cfg.bar.cursorwidth.*.5,cfg.bar.maxScale) ...
    linspace(cfg.bar.gaprect(3)+cfg.bar.cursorwidth.*.5, ...
        cfg.bar.barrect(3)-cfg.bar.cursorwidth.*.5,cfg.bar.maxScale)];


%% Get timestamp for experiment start
cfg.startexp = GetSecs;

%% initialize dotdifference
trials(1).dotdifference = cfg.stim.initialDotDifference;
trials(2).dotdifference = cfg.stim.initialDotDifference;

%% Experiment
for t = starttrial:length(trials)
    %% experimenter output
    if t > 1
        disp(['trial: ' num2str(t-1)])
        disp(['accuracy: ' num2str(trials(t-1).cor)])
        disp(['1st confidence: ' num2str(trials(t-1).cj1)])
        disp(['2nd confidence: ' num2str(trials(t-1).cj2)])
        disp(['obs type: ' num2str(trials(t-1).obstype)]);
        disp(['agree?: ' num2str(trials(t-1).agree)]);
        disp('------------------------------------------');
    end
    %% save and break
    if trials(t).break
        %% Save data every 20th trials
        save([results_path subject.dir '/behaviour/' subject.fileName '_' num2str(round(t/20))],'trials', 'cfg', 't')
        %% break
        Screen('TextSize',Sc.window,18);
        DrawFormattedText(Sc.window, 'Break. Press button to continue','center', 'center', [0 0 0]);
        Screen('Flip', Sc.window);
        collect_response(cfg.response, inf);
        feedback_interblock
    end
    %% instructions
    if trials(t).instr
        instructions
    end
    %% introduce observers
    if trials(t).block==3 && trials(t-1).block==2
        introduce_observers
    end
    %% questionnaire
    if trials(t).questionnaire
        questionnaire
    end
    
    %% start trial
    % add all static elements
    draw_static
    
    trials(t).time_starttrial = Screen('Flip',Sc.window);
    
    % define time: if first trial of block getTime; if normal trial, time =
    % end of previous trial
    if t == 1 || cfg.restarted == 1 || trials(t-1).break == 1, time = GetSecs; end
    
    % vectors are created that contain logical values to tell where
    % dots have to be set in the squares (randomized)
    if t > 2
        trials(t).dotdifference = staircase([trials(t-2).cor trials(t-1).cor],[trials(t-2).dotdifference trials(t-1).dotdifference]);
    end
    larger = 200 + trials(t).dotdifference;
    smaller = 200 - trials(t).dotdifference;
    trials(t).wheredots(trials(t).wherelarger,randsample(400,larger)) = 1;
    trials(t).wheredots(abs(trials(t).wherelarger-3),randsample(400,smaller)) = 1;
    trials(t).wheredots = logical(trials(t).wheredots);
    
    %% First-order stimulus and decision
    % draw stimulus rectangles
    Screen('DrawLines',Sc.window,innerrect1out,3,255);
    Screen('DrawLines',Sc.window,innerrect2out,3,255);
    Screen('DrawDots', Sc.window, cfg.xymatrix(:,squeeze(trials(t).wheredots(1,:))), 2, 255, center1, 2);
    Screen('DrawDots', Sc.window, cfg.xymatrix(:,squeeze(trials(t).wheredots(2,:))), 2, 255, center2, 2);
    
    % draw confidence scale
    draw_static
    
    % Show stimulus on screen at next possible display refresh cycle,
    % and record stimulus onset time in 'onsetstim':
    [VBLTimestamp trials(t).onsetstim Fts trials(t).tmissed_onset1] = Screen('Flip', Sc.window, time + cfg.stim.RSI2 - cfg.frame);
    
    % draw confidence scale
    draw_static
    
    % stimulus is shown for 160 ms and then disappears
    % no response collection before 160 ms
    [VBLts trials(t).offsetstim Fts trials(t).tmissed_offset1] = Screen('Flip',Sc.window,trials(t).onsetstim + cfg.stim.durstim - cfg.frame);
    
    % collect 1st response
    [trials(t).cj1, ...
        trials(t).resp1_time, ...
        trials(t).int1, ...
        trials(t).responded1] = drag_slider(Sc,cfg);
    
    
    % define new timestamp
    time = trials(t).resp1_time;
    
    % define correct response
    trials(t).cor = trials(t).int1 == trials(t).wherelarger;
    
    % define RT1
    trials(t).rt1 = trials(t).resp1_time - trials(t).offsetstim;
    
    %% define observer behaviour
    if ~isnan(trials(t).obstype)
        if trials(t).cor == 1
            clear toi
            toi = ...
                [trials.cor] == 1 & ... % use last 2 blocks for reference dsitribution
                ([trials(1:t).block] == trials(t).block-1 | ...
                    [trials(1:t).block] == trials(t).block-2); 
            [trials(t).agree, trials(t).step] = ...
                                            agreementf(trials(t).cj1, ...
                                                trials(t).obstype, ...
                                                abs([trials(toi).cj1]), ...
                                                'stepwise');
        else
            trials(t).agree = rand < .3; % flat agreement on incorrect trials
            trials(t).step  = NaN;
        end
        % define observer's accuracy
        if trials(t).agree == 1
            trials(t).obsacc = trials(t).cor;
        elseif trials(t).agree == 0
            trials(t).obsacc = 1 - trials(t).cor;
        else
            disp('Hesher was here')
        end
    else % null
        trials(t).agree  = NaN;
        trials(t).obsacc = NaN;
        trials(t).step   = NaN;
    end
    
    %% read in observer picture and voice
    if trials(t).block>1,
        % load textures for images
        images_texture
        PsychPortAudio('Close');
        load_observer,
    end
    
    %% Advice and final decision
    if trials(t).block > 1 
        if ~isnan(trials(t).obstype),
            present_advice;
            
            % prompt new confidence judgment
            [trials(t).cj2, ...
                trials(t).resp2_time, ...
                trials(t).int2, ...
                trials(t).responded2] = drag_slider(Sc, cfg, trials(t).cj1);

            
            % define new timestamp
            time = trials(t).resp2_time;
            
            % define correct response
            trials(t).cor2 = trials(t).int2 == trials(t).wherelarger;
            
            % define RT2
            trials(t).rt2 = trials(t).resp2_time - trials(t).resp_advice_rt;
            
        else % null
            present_delay;
            
            % assign variables on 2 judgement
            trials(t).resp2_time        = NaN;
            trials(t).cj2               = NaN;
            trials(t).cor2              = NaN;
            trials(t).int2              = NaN;
            trials(t).responded2        = NaN;
            trials(t).rt2               = NaN;
            trials(t).resp_advice_rt    = NaN;
            trials(t).offset_speech     = NaN;
            time                        = GetSecs;
        end
    else % 1st practice block
        trials(t).resp2_time        = NaN;
        trials(t).cj2               = NaN;
        trials(t).cor2              = NaN;
        trials(t).int2              = NaN;
        trials(t).responded2        = NaN;
        trials(t).rt2               = NaN;
        trials(t).resp_advice_rt    = NaN;
        trials(t).offset_speech     = NaN;
        time                        = GetSecs;
    end
    
    %% error feedback, but only on practice trials
    if trials(t).block == 1 || isnan(trials(t).obstype)
        if (trials(t).cor == 0 || isnan(trials(t).cor))
            pahandle = PsychPortAudio('Open', [], [], 0, [], 2);
            PsychPortAudio('FillBuffer', pahandle, ...
                repmat(cfg.stim.beep.*cfg.stim.beepvolume,2,1));
            PsychPortAudio('Start', pahandle, 1, 0, 1);
            PsychPortAudio('Close',pahandle);
        end
    elseif  trials(t).block > 1 && ~isnan(trials(t).obstype)
        if trials(t).cor2 == 0 || isnan(trials(t).cor2)
            pahandle = PsychPortAudio('Open', [], [], 0, [], 2);
            PsychPortAudio('FillBuffer', pahandle, ...
                repmat(cfg.stim.beep.*cfg.stim.beepvolume,2,1));
            PsychPortAudio('Start', pahandle, 1, 0, 1);
            PsychPortAudio('Close',pahandle);
        end
    end
    
    %% close screen buffers
    Screen('Close');
    
end
%% collect last questionnaire
questionnaire

%% save temporary final file
save([results_path subject.dir '/behaviour/' subject.fileName '_' num2str(round(t/20))],'trials', 'cfg', 't');

%% collect estimated observers accuracy
trials(t).estim_obsacc = estimated_obsacc(Sc,cfg);

%% save final file
save([results_path subject.dir '/behaviour/' subject.fileName '_final'],'subject','cfg','trials');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Thanks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insimdata = imread(char('instructions/instr4.png'));
texins = Screen('MakeTexture', Sc.window, insimdata);
Screen('DrawTexture', Sc.window, texins);
Screen('Flip',Sc.window);
Beeper(261.63,.4,1);
WaitSecs(.500);
KbWait;

%% close PTB
Screen('CloseAll');
ListenChar(0);
DisableKeysForKbCheck([]);
ShowCursor()
Priority(0);
toc
    