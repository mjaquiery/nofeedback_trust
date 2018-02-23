function [trials] = doDotTask(trials, t)
%% Run the Dot task and return the results
%
% usage: runTrial(trials, t)
%
% Inputs: 
% trials: trial list created by build_trials.m
% t: index of the current trial
%
% Outputs:
% trials: a version of the trial list with results recorded for trials(t)
%
global cfg; % configuration object
global Sc; % screen object

%% At some point move this stuff to a 'defineDotTask' function
if ~isfield(cfg, 'dotTask') || ~isfield(cfg.dotTask, 'adviceFormat')
    cfg.dotTask.adviceFormat = cfg.adviceFormat.voice;
end
if ~isfield(cfg.display, 'dotTask') || ~isfield(cfg.display.dotTask, 'adviceBubbleColor')
    cfg.display.dotTask.adviceBubbleColor = [0 0 0];
end

% define time: if first trial of block getTime; if normal trial, time =
% end of previous trial
if t == 1 || cfg.restarted == 1 || trials(t-1).break == 1 
    time = GetSecs; 
else
    time = trials(t-1).time_endTrial;
end

% vectors are created that contain logical values to tell where
% dots have to be set in the squares (randomized)
if length(cfg.taskManager.dotTask.lastTwoTrials) > 2
    trials(t).dotdifference = staircase([cfg.taskManager.dotTask.lastTwoTrials(2).cor ...
                                            cfg.taskManager.dotTask.lastTwoTrials(1).cor], ...
                                        [cfg.taskManger.dotTask.lastTwoTrials(2).dotdifference ...
                                            cfg.taskManager.dotTask.lastTwoTrials(1).dotdifference]);
else
    trials(t).dotdifference = cfg.stim.initialDotDifference;
end
larger = 200 + trials(t).dotdifference;
smaller = 200 - trials(t).dotdifference;
trials(t).wheredots(1:end) = 0; % set the dot array to blank (required because we allow loading)
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
Screen('DrawLines',Sc.window,cfg.rect.innerrect1out,3,255);
Screen('DrawLines',Sc.window,cfg.rect.innerrect2out,3,255);
Screen('DrawDots', Sc.window, cfg.xymatrix(:,squeeze(trials(t).wheredots(1,:))), 2, 255, cfg.rect.center1, 2);
Screen('DrawDots', Sc.window, cfg.xymatrix(:,squeeze(trials(t).wheredots(2,:))), 2, 255, cfg.rect.center2, 2);
draw_static([1 1 0 0 0])

% Show stimulus on screen at next possible display refresh cycle,
% and record stimulus onset time in 'onsetstim':
[VBLTimestamp, trials(t).onsetstim, Fts, trials(t).tmissed_onset1] = Screen('Flip', Sc.window, time + cfg.stim.RSI2 - cfg.frame);

draw_static([1 1 0 0 0])

% stimulus is shown for durstim (.160)s and then disappears
[VBLts, trials(t).offsetstim, Fts, trials(t).tmissed_offset1] = Screen('Flip',Sc.window,trials(t).onsetstim + cfg.stim.durstim - cfg.frame);

% after a SRI1 (.09)s delay the instructions appear and responding is enabled
draw_static();
[V, trials(t).time_responseStart1] = Screen('Flip',Sc.window,trials(t).offsetstim + cfg.stim.SRI1 - cfg.frame);

% collect 1st response
[trials(t).cj1, trials(t).time_response1, trials(t).int1] = drag_slider(); % responded is 1 or 0; cj1 is the first confidence judgement

% define new timestamp
time = trials(t).time_response1;

% define correct response
%trials(t).cor = ((trials(t).int1>0)+1) == trials(t).wherelarger;
trials(t).cor1 = trials(t).int1 == trials(t).wherelarger;
trials(t).cor = trials(t).cor1;

% update trial description for debugging
cfg.currentTrial = trials(t);

%% Advisor stuff
if trials(t).block>1
    PsychPortAudio('Close');
end

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
if hasAdvice(trials(t))
    % advisors are 60/70/80 or 80/70/60 agreement in correct (based on
    % confidence) and 30% agreement when participant is incorrect
    if trials(t).cor1 == 1
        toi = [trials(1:t).cor1] == 1 & ... % use last 2 blocks for reference dsitribution
            ([trials(1:t).block] == trials(t).block-1 | [trials(1:t).block] == trials(t).block-2); 
        if ~isnan(trials(t).overrideAdviceType)
            adviceType = trials(t).overrideAdviceType;
        else
            adviceType = cfg.advisor(trials(t).advisorId).adviceType;
        end
        [trials(t).agree, trials(t).step] = ...
            agreementf(trials(t).cj1,adviceType,abs([trials(toi).cj1]),'stepwise');
        clear toi
    else
        trials(t).agree = rand < .3; % flat agreement on incorrect trials
        trials(t).step  = NaN;
    end
    % define observer's accuracy
    if trials(t).agree == 1
        trials(t).obsacc = trials(t).cor;
    else
        trials(t).obsacc = 1 - trials(t).cor;
    end
else % null
    trials(t).agree  = NaN;
    trials(t).obsacc = NaN;
    trials(t).step   = NaN;
end

% update trial description for debugging
cfg.currentTrial = trials(t);

%% Advice and final decision
if hasAdvice(trials(t))
    if bitand(cfg.dotTask.adviceFormat, cfg.adviceFormat.voice)
        load_observer_audio;
        present_advice;
    end
    trials(t).time_responseStart2 = GetSecs;
    % prompt new confidence judgment
    if bitand(cfg.dotTask.adviceFormat, cfg.adviceFormat.speechBubble)
        [trials(t).cj2, trials(t).time_response2, trials(t).int2] = ...
            drag_slider(trials(t).cj1, trials(t));
    else
        [trials(t).cj2, trials(t).time_response2, trials(t).int2] = ...
            drag_slider(trials(t).cj1, trials(t));
    end
    % define new timestamp
    time = trials(t).time_response2;

    % define correct response
    %trials(t).cor2 = ((trials(t).int2>0)+1) == trials(t).wherelarger;
    trials(t).cor2 = trials(t).int2 == trials(t).wherelarger;
    trials(t).cor = trials(t).cor2;
else
    time                        = GetSecs;
    if isnan(trials(t).advisorId) % null
        present_delay;
    end
end

% update trial description for debugging
cfg.currentTrial = trials(t);

%% feedback
if trials(t).feedback
    if ~trials(t).cor, playFeedback(); end
    %colors=[.8 .2 .2;.2 .8 .2];
end

%--close audio/screen buffers
Screen('Close');
PsychPortAudio('Close');

trials(t).time_endTrial = time;

% update task manager
switch length(cfg.taskManager.dotTask.lastTwoTrials)
    case 0
        cfg.taskManager.dotTask.lastTwoTrials = t;
    case 1
        cfg.taskManager.dotTask.lastTwoTrials = [t cfg.taskManager.dotTask.lastTwoTrials];
    case 2
        cfg.taskManager.dotTask.lastTwoTrials = [t cfg.taskManager.dotTask.lastTwoTrials(1)];
end