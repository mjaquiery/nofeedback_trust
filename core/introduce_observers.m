function introduce_observers(advisorId, practiceObservers, forceReintroduction)
%% Introduce advisors to the participant
%
% usage: introduce_observers([advisorId = NaN[, practiceObservers = 0[, forceReintroduction = 0]]])
%
% Inputs: 
% advisorId - ID of the advisor to introduce. If blank, introduce all
% advisors
% practiceObservers - whether to introduce practice rather than real
% observers
% forceReintroduction - whether to introduce observers who are already
% introduced
global cfg;
global Sc;

if nargin < 3, forceReintroduction = 0; end
if nargin < 2, practiceObservers = 0; end
if nargin < 1, advisorId = NaN; end

cfg.expositionTime = [];
cfg.expositionTime=[];
oldTextSize = Screen('TextSize', Sc.window, cfg.instr.textSize.medium);
bounds = Screen('TextBounds',Sc.window,cfg.instr.intro.text{1});
if isnan(advisorId)
    if practiceObservers
        observers = cfg.advisors.count.real+1:cfg.advisors.count.all;
    else
        observers = randperm(cfg.advisors.count.real);
    end
    for obs = observers
        if ~isnan(cfg.advisor(obs).timeIntroduced) && ~forceReintroduction
            continue
        end
        drawAdvisor(obs);
        speech = PsychPortAudio('Open', [], [], 0, cfg.introSpeechFreq{cfg.advisor(obs).voice}, cfg.introSpeechChannels{cfg.advisor(obs).voice});
        PsychPortAudio('FillBuffer',speech , cfg.introSpeechData{cfg.advisor(obs).voice}); 
        ti=GetSecs;
        onset_pic(obs) = Screen('Flip',Sc.window,ti);
        onset_speech(obs)  = PsychPortAudio('Start', speech, 1);
        [startTime offset_speech(obs) xruns ~] = PsychPortAudio('Stop', speech,1);
        drawAdvisor(obs);
        Screen('DrawText', Sc.window, cfg.instr.intro.text{1}, Sc.center(1)-bounds(3)/2, cfg.instr.intro.position.y);
        Screen('Flip',Sc.window,ti+2);
        PsychPortAudio('Close', speech);
        [resp offset_pic(obs) kcode] = collect_response(inf);
        Screen('Flip',Sc.window);
        cfg.expositionTime(obs) = offset_pic(obs) - ti;
        % remember they were introduced
        if isnan(cfg.advisor(obs).timeFirstIntroduced)
            cfg.advisor(obs).timeIntroduced = GetSecs();
        end
    end
else
    if isnan(cfg.advisor(advisorId).timeIntroduced) || forceReintroduction
        drawAdvisor(advisorId);
        speech = PsychPortAudio('Open', [], [], 0, cfg.introSpeechFreq{cfg.advisor(advisorId).voice}, cfg.introSpeechChannels{cfg.advisor(advisorId).voice});
        PsychPortAudio('FillBuffer',speech , cfg.introSpeechData{cfg.advisor(advisorId).voice}); 
        ti=GetSecs;
        onset_pic(advisorId) = Screen('Flip',Sc.window,ti);
        onset_speech(advisorId)  = PsychPortAudio('Start', speech, 1);
        [startTime offset_speech(advisorId) xruns ~] = PsychPortAudio('Stop', speech,1);
        drawAdvisor(advisorId);
        Screen('DrawText', Sc.window, cfg.instr.intro.text{1}, Sc.center(1)-bounds(3)/2, cfg.instr.intro.position.y);
        Screen('Flip',Sc.window,ti+2);
        PsychPortAudio('Close', speech);
        [resp offset_pic(advisorId) kcode] = collect_response(inf);
        Screen('Flip',Sc.window);
        cfg.expositionTime(advisorId) = offset_pic(advisorId) - ti;
        if isnan(cfg.advisor(advisorId).timeFirstIntroduced)
            cfg.advisor(advisorId).timeFirstIntroduced = GetSecs();
        end
    end
end

cfg.obsIntro_times = [onset_pic; offset_pic; onset_speech; offset_speech];
clear startTime xruns resp kcode ti speech imagedata texture bounds pressKey

Screen('TextSize', Sc.window, oldTextSize);

function introduce(advisorId)
global cfg;
global Sc;
