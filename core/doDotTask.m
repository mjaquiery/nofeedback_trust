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

% define time: if first trial of block getTime; if normal trial, time =
% end of previous trial
if t == 1 || cfg.restarted == 1 || trials(t-1).break == 1 
    time = GetSecs; 
else
    time = trials(t-1).endTime;
end

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

%% load the observer
if trials(t).block > 1
    load_observer_audio;
end

% update trial description for debugging
cfg.currentTrial = trials(t);

%% Advice and final decision
if trials(t).block > 1 
    if ~isnan(trials(t).advisorId)
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

%% feedback
if trials(t).feedback
    if ~trials(t).cor, Beeper; end
    %colors=[.8 .2 .2;.2 .8 .2];
end

%--close audio/screen buffers
Screen('Close');
PsychPortAudio('Close');

trials(t).endTime = time;
