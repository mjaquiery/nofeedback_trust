if ~exist('mask1'), mask1 = [1 0 1 1 0]; end
if ~exist('mask2'), mask2 = [1 1 1 1 0]; end

% draw static elements
draw_static(mask1);

% draw advisor in the appropriate position
if ~isempty(trials(t).choice)
    if trials(t).choice(1) == trials(t).advisorId
        %chosen advisor is on top
        drawAdvisor(trials(t).advisorId, [Sc.center(1), Sc.center(2)-cfg.display.choice.offset*Sc.rect(4)], true);
    else
        %chosen advisor is on the bottom
        drawAdvisor(trials(t).advisorId, [Sc.center(1), Sc.center(2)+cfg.display.choice.offset*Sc.rect(4)], true, 2);
    end
else
    drawAdvisor(trials(t).advisorId);
end

% start voice play
trials(t).onsetobsspeech = PsychPortAudio('Start', trials(t).whichspeech, 1,GetSecs,1); %startTime = PsychPortAudio('Start', pahandle [, repetitions=1] [, when=0] [, waitForStart=0] [, stopTime=inf] [, resume=0]);

% flip image on screen
[VBLTimestamp2, trials(t).onsetobs, FlipTimestamp2, trials(t).tmissed_onset2] = Screen('Flip',Sc.window,time + cfg.stim.RSI1 - cfg.frame);

% To end audio playback as soon as it ends. Make sense only
% with precise durations
[startTime, trials(t).offset_speech, xruns, ~] = PsychPortAudio('Stop', trials(t).whichspeech,1); %[startTime endPositionSecs xruns estStopTime] = PsychPortAudio('Stop', pahandle [, waitForEndOfPlayback=0] [, blockUntilStopped=1] [, repetitions] [, stopTime]);

PsychPortAudio('Close');

% draw static elements
draw_static(mask2);

% offset image
[VBLts, trials(t).offsetobs, FTS, trials(t).tmissed_offset2] = Screen('Flip',Sc.window,trials(t).onsetobs + cfg.advisors.duration - cfg.frame);
time = trials(t).offsetobs;