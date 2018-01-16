%% STUDY 3 - agreement in confidence and uncertainty  #####################
%% script by Niccolo Pescetelli %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% niccolo.pescetelli@psy.ox.ac.uk
%% Extensions by Matt Jaquiery
% matt.jaquiery@psy.ox.ac.uk
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% PTB compatibility
% check for Opengl compatibility, abort otherwise
AssertOpenGL;

%% Subjects' details and directory
create_subject_directory

if (subject.restarted)
    [filename, pathname] = uigetfile('*.mat', 'Pick last saved file '); 
    % loaded below to stop values being overwritten by defaults
end
tic
%%%%%%%%%%%%%%%%%%%%%%%  start PTB   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global Sc;
Sc = start_psychtb(subject.screen, forceResolution);
%% Settings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global cfg;
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
% in case experiment was restarted after crash
if (subject.restarted)
    load([pathname filename]);
    starttrial = cfg.currentTrialNumber;
    cfg.restarted = 1;
else
    starttrial=1;
end

Screen('Preference','SuppressAllWarnings', 1);
Screen('TextSize',Sc.window,cfg.instr.textSize.medium);
Screen('TextColor',Sc.window,cfg.instr.textColor.default);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cfg.fps = Sc.fps;                                                               % refresh rate (Hz). IMPORTANT! Make sure this value equals the RR of the screen used AND the RR in the start_psychtb.m function
cfg.frame = (1/cfg.fps); % average duration (ms) of a frame
if isinf(cfg.frame), cfg.frame = 1/59.9; end
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

% initialize text on screen ?? - now done ad hoc (was previously
% duplicated)
%text_on_screen_vars

%% Present the SECS questionnaire
global SECSscore;

for t = starttrial:length(trials)
    disp(['text color: ' int2str(sum(Screen('TextColor',Sc.window)))]);
    % update current trial number
    cfg.currentTrialNumber = t;
    cfg.currentTrial = trials(t);
    %% experimenter output
    if t > 1
        disp(['trial: ' num2str(t-1)])
        disp(['accuracy: ' num2str(trials(t-1).cor)])
        disp(['1st confidence: ' num2str(trials(t-1).cj1)])
        disp(['2nd confidence: ' num2str(trials(t-1).cj2)])
        disp(['Advisor: ' num2str(trials(t-1).advisorId)]);
        if ~isnan(trials(t-1).advisorId)
            disp(['advice type: ' num2str(cfg.advisor(trials(t-1).advisorId).adviceType)]);
        end
        disp(['agree?: ' num2str(trials(t-1).agree)]);
        disp('------------------------------------------');
    else
        [~, ~, SECSscore] = SECS(true); % get political leaning responses
    end
    %% save and break
    if trials(t).break
        %% Save dataon break trials
        save([cfg.path.results osSlash subject.dir osSlash subject.fileName '_' num2str(round(t/20))],'trials', 'cfg', 't', 'SECSscore')
        %% break
        Screen('TextSize',Sc.window,18);
        DrawFormattedText(Sc.window, 'Break. Press button to continue','center', 'center', [0 0 0]);
        Screen('Flip', Sc.window);
        collect_response(inf);
        feedback_interblock
    end
    %% instructions
    if trials(t).instr
        instructions(trials(t).block);
    end
    %% introduce observers
    if trials(t).block>1 && trials(t-1).block<5 && trials(t).block ~= trials(t-1).block
        if isnan(trials(t).advisorId) % first trial is a null trial
           for n = t:cfg.block.trialset_count*(cfg.trialset.real+cfg.trialset.null)
               if n>0 && trials(n).block == trials(t).block && ~isnan(trials(n).advisorId)
                   introduce_observers(trials(n).advisorId);
                   break;
               end
           end
        else
            introduce_observers(trials(t).advisorId);
        end
    end
    %% questionnaire
    if trials(t).questionnaire
        if isnan(trials(t).advisorId) % this trial is a null trial so search for the appropriate questionnaire
           for n = t-cfg.block.trialset_count*(cfg.trialset.real+cfg.trialset.null):t-1
               if n>0 && trials(n).block == trials(t-1).block && ~isnan(trials(n).advisorId)
                   questionnaire(trials(n).advisorId);
                   break;
               end
           end
        else
            questionnaire(trials(t).advisorId);
        end
        trials(t) = cfg.currentTrial; % update the trial with the results
    end
    %% advisor political information
    if trials(t).advisorPolitics
        if isnan(trials(t-1).advisorId) % last trial was a null trial so search for a trial with an advisorId
           for n = t-cfg.block.trialset_count*(cfg.trialset.real+cfg.trialset.null):t+cfg.block.trialset_count*(cfg.trialset.real+cfg.trialset.null)
               if n>0 && trials(n).block == trials(t-1).block && ~isnan(trials(n).advisorId)
                   trials(t).advisorPoliticsQ = showAdvisorPolitics(trials(n).advisorId);
                   break;
               end
           end
        else
            trials(t).advisorPoliticsQ = showAdvisorPolitics(trials(t-1).advisorId);
        end
        WaitSecs(3);
    end
    %% start trial
        
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
    % update trial description for debugging
    cfg.currentTrial = trials(t);
    
    % add progression bar and fixation cross
    draw_static([1 1 0 0 0])    
    trials(t).time_starttrial = Screen('Flip',Sc.window);
    
    WaitSecs(cfg.stim.fixationFlicker.time.pre); % brief delay
    fixationFlicker(); %  add a flicker so the fixation cross cues the stimulus
    WaitSecs(cfg.stim.fixationFlicker.time.post);
    
    % draw stimulus rectangles
    Screen('DrawLines',Sc.window,innerrect1out,3,255);
    Screen('DrawLines',Sc.window,innerrect2out,3,255);
    Screen('DrawDots', Sc.window, cfg.xymatrix(:,squeeze(trials(t).wheredots(1,:))), 2, 255, center1, 2);
    Screen('DrawDots', Sc.window, cfg.xymatrix(:,squeeze(trials(t).wheredots(2,:))), 2, 255, center2, 2);
    draw_static([1 1 0 0 0])
    
    % Show stimulus on screen at next possible display refresh cycle,
    % and record stimulus onset time in 'onsetstim':
    [VBLTimestamp, trials(t).onsetstim, Fts, trials(t).tmissed_onset1] = Screen('Flip', Sc.window, time + cfg.stim.RSI2 - cfg.frame);
    
    draw_static([1 1 0 0 0])
    
    % stimulus is shown for durstim (.160)s and then disappears
    [VBLts, trials(t).offsetstim, Fts, trials(t).tmissed_offset1] = Screen('Flip',Sc.window,trials(t).onsetstim + cfg.stim.durstim - cfg.frame);
    
    % after a SRI1 (.09)s delay the instructions appear and responding is enabled
    draw_static();
    [V, trials(t).responsestart] = Screen('Flip',Sc.window,trials(t).offsetstim + cfg.stim.SRI1 - cfg.frame);
    
    % collect 1st response
    [trials(t).cj1, trials(t).resp1_time, trials(t).int1] = drag_slider(); % responded is 1 or 0; cj1 is the first confidence judgement
    
    % define new timestamp
    time = trials(t).resp1_time;
    
    % define correct response
    %trials(t).cor = ((trials(t).int1>0)+1) == trials(t).wherelarger;
    trials(t).cor1 = trials(t).int1 == trials(t).wherelarger;
    trials(t).cor = trials(t).cor1;
    
    % define RT1
    trials(t).rt1 = trials(t).resp1_time - trials(t).offsetstim;
    
    % update trial description for debugging
    cfg.currentTrial = trials(t);
            
    %% Choice of advisor
    if ~isempty(trials(t).choice)
        % get the judge's choice
        [trials(t).choiceDecision, trials(t).choiceTime] = getAdvisorChoice(trials(t).choice(1), trials(t).choice(2));
        % fill in the remaining trial details from the choice
        trials(t).advisorId = trials(t).choice(trials(t).choiceDecision);
    end
    
    %% define advisor behaviour
    % NOTE: advisor behaviour depends on initial judgements, not
    % post-advice judgements
    if ~isnan(trials(t).advisorId)
        % advisors are always 80% accurate
        if rand <= .8
            trials(t).obsacc = 1;
            trials(t).agree = trials(t).cor1;
        else
            trials(t).obsacc = 0;
            trials(t).agree = ~trials(t).cor1;
        end
    else % null
        trials(t).agree  = NaN;
        trials(t).obsacc = NaN;
        trials(t).step   = NaN;
    end
    
    % update trial description for debugging
    cfg.currentTrial = trials(t);
        
    %% Advice and final decision
    if trials(t).block > 1 
        if ~isnan(trials(t).advisorId)
            load_observer_audio;
            present_advice;
            
            % prompt new confidence judgment
            [trials(t).cj2, trials(t).resp2_time, trials(t).int2] = ...
                drag_slider(trials(t).cj1);
            
            % define new timestamp
            time = trials(t).resp2_time;
            
            % define correct response
            %trials(t).cor2 = ((trials(t).int2>0)+1) == trials(t).wherelarger;
            trials(t).cor2 = trials(t).int2 == trials(t).wherelarger;
            trials(t).cor = trials(t).cor2;
            
            % define RT2
            trials(t).rt2 = trials(t).resp2_time - trials(t).resp_advice_rt;
            
        else % null
            present_delay;
            time                        = GetSecs;
        end
    else % 1st practice block
        time                        = GetSecs;
    end
    
    % update trial description for debugging
    cfg.currentTrial = trials(t);
    
    %--close audio/screen buffers
    Screen('Close');
    
    %% feedback
    if trials(t).block<3
        if ~trials(t).cor, playFeedback(); end
        %colors=[.8 .2 .2;.2 .8 .2];
    end
end

% save temporary final file
save([cfg.path.results osSlash subject.dir osSlash subject.fileName '_' num2str(round(t/20))],'trials', 'cfg', 't','SECSscore');

% collect estimated observers accuracy
trials(t).estim_obsacc = estimated_obsacc();

%% save final file
save([cfg.path.results osSlash subject.dir osSlash subject.fileName '_final'], 'subject', 'cfg', 'trials', 'SECSscore');
% prepare data for R
addpath([cfg.path.base experimentName osSlash 'analysis']);
[cfg, subject, trials] = prepR(cfg, subject, trials);
save([cfg.path.results osSlash subject.dir osSlash subject.fileName '_final_R'], 'subject', 'cfg', 'trials', 'SECSscore');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Thanks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insimdata = imread(cfg.intro{4}{1});
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
    