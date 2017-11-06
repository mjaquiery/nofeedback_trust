%% STUDY 3 - agreement in confidence and uncertainty  #####################
%% script by Niccolo Pescetelli %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% niccolo.pescetelli@psy.ox.ac.uk
%% Extensions by Matt Jaquiery
% matt.jaquiery@psy.ox.ac.uk
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cd('G:\Documents\University\Programming\nofeedback_trust_matt\')

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
% in case experiment was restarted after crash
if (subject.restarted)
    [filename, pathname] = uigetfile('*.mat', 'Pick last saved file ');
    load([pathname filename]);
    starttrial = t;
    cfg.restarted = 1;
else
    starttrial=1;
end
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

% initialize text on screen ?? - now done ad hoc (was previously
% duplicated)
%text_on_screen_vars

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
        %% Save dataon break trials
        save([cfg.path.results subject.dir '/behaviour/' subject.fileName '_' num2str(round(t/20))],'trials', 'cfg', 't')
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
    % update current trial number
    cfg.currentTrial = t;
    % add all static elements
    draw_static(Sc, cfg)
    
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
    draw_static(Sc, cfg)
    
    % Show stimulus on screen at next possible display refresh cycle,
    % and record stimulus onset time in 'onsetstim':
    [VBLTimestamp, trials(t).onsetstim, Fts, trials(t).tmissed_onset1] = Screen('Flip', Sc.window, time + cfg.stim.RSI2 - cfg.frame);
    
    % draw confidence scale
    draw_static(Sc, cfg)
    
    % stimulus is shown for 160 ms and then disappears
    % no response collection before 160 ms
    [VBLts, trials(t).offsetstim, Fts, trials(t).tmissed_offset1] = Screen('Flip',Sc.window,trials(t).onsetstim + cfg.stim.durstim - cfg.frame);
    
    % collect 1st response
    [trials(t).cj1, trials(t).resp1_time, trials(t).int1] = drag_slider(Sc, cfg); % responded is 1 or 0; cj1 is the first confidence judgement
    
    % define new timestamp
    time = trials(t).resp1_time;
    
    % define correct response
    trials(t).cor = ((trials(t).int1>0)+1) == trials(t).wherelarger;
    
    % define RT1
    trials(t).rt1 = trials(t).resp1_time - trials(t).offsetstim;
        
    %% Advisor stuff
    if trials(t).block>1
        PsychPortAudio('Close');
    end
    
    %% Choice of advisor
    if ~isempty(trials(t).choice)
        obsL = buildAdvisor(trials(t).choice(1), cfg);
        obsR = buildAdvisor(trials(t).choice(2), cfg);
        % get the judge's choice
        [choice, trials(t).choiceTime] = getAdvisorChoice(Sc, cfg, obsL, obsR);
        trials(t).choiceDecision = trials(t).choice(choice);
        % fill in the remaining trial details from the choice
        trials(t).obstype = trials(t).choiceDecision-1;
        if ~isnan(trials(t).obstype)
            trials(t).pic = cfg.observer.pic(trials(t).choiceDecision);
            trials(t).voice = cfg.observer.voice(trials(t).choiceDecision);
        end
    end
    
    %% define observer behaviour
    if ~isnan(trials(t).obstype)
        if trials(t).cor == 1
            clear toi
            toi = [trials.cor] == 1 & ... % use last 2 blocks for reference dsitribution
                ([trials(1:t).block] == trials(t).block-1 | [trials(1:t).block] == trials(t).block-2); 
            [trials(t).agree, trials(t).step] = ...
                agreementf(trials(t).cj1,trials(t).obstype,abs([trials(toi).cj1]),'stepwise');
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
            disp('Hesher was here') % should never show
        end
    else % null
        trials(t).agree  = NaN;
        trials(t).obsacc = NaN;
        trials(t).step   = NaN;
    end
    
    %% load the observer
    if trials(t).block > 1
        load_observer;
    end
        
    %% Advice and final decision
    if trials(t).block > 1 
        if ~isnan(trials(t).obstype)
            present_advice;
            
            % prompt new confidence judgment
            [trials(t).cj2, trials(t).resp2_time, trials(t).int2] = ...
                drag_slider(Sc, cfg, trials(t).cj1);
            
            % define new timestamp
            time = trials(t).resp2_time;
            
            % define correct response
            trials(t).cor2 = ((trials(t).int2>0)+1) == trials(t).wherelarger;
            
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
    
    %--close audio/screen buffers
    Screen('Close');
    resetPPA
    
    %% feedback
    if trials(t).block<3
        if ~trials(t).cor, Beeper; end
        colors=[.8 .2 .2;.2 .8 .2];
        PsychPortAudio('Close');
    end
end
% collect last questionnaire
questionnaire

% save temporary final file
save([cfg.path.results subject.dir '/behaviour/' subject.fileName '_' num2str(round(t/20))],'trials', 'cfg', 't');

% collect estimated observers accuracy
trials(t).estim_obsacc = estimated_obsacc(Sc,cfg);

%% save final file
save([cfg.path.results subject.dir '/behaviour/' subject.fileName '_final'],'subject','cfg','trials');

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
ShowCursor();
Priority(0);
toc
    