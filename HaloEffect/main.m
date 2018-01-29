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
    [filename, pathname] = uigetfile('*.mat', 'Pick last saved file ');
    load([pathname filename]);
    starttrial = t;
    cfg.restarted = 1;
else
    starttrial=1;
end

Screen('Preference','SuppressAllWarnings', 1);
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

% initialize text on screen ?? - now done ad hoc (was previously
% duplicated)
%text_on_screen_vars

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
    end
    %% save and break
    if trials(t).break
        %% Save dataon break trials
        save([cfg.path.results osSlash subject.dir osSlash subject.fileName '_' num2str(round(t/20))],'trials', 'cfg', 't', 'SECSscore')
        %% break
        Screen('TextSize',Sc.window,cfg.instr.textSize.large);
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
        if isnan(trials(t).advisorId) % this trial is a null trial
           for n = t+1:length(trials)
               if trials(n).block == trials(t).block && ~isnan(trials(n).advisorId)
                   trials(t).qanswers = questionnaire(trials(n).advisorId);
                   break;
               end
           end
        else
            trials(t).qanswers = questionnaire(trials(t).advisorId);
        end
    end
    %% start trial
        
    runTrial();
end

% collect final questionnaire
trials(t).qanswers = questionnaire();

% save temporary final file
save([cfg.path.results osSlash subject.dir osSlash subject.fileName '_' num2str(round(t/20))],'trials', 'cfg', 't');

% collect estimated observers accuracy
trials(t).estim_obsacc = estimated_obsacc();

%% save final file
save([cfg.path.results osSlash subject.dir osSlash subject.fileName '_final'],'subject','cfg','trials');

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
    