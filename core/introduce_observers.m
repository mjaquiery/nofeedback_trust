function introduce_observers(advisorId)
%% Introduce advisors to the participant
%
% usage: introduce_observers([advisorId])
%
% Inputs: 
% advisorId - ID of the advisor to introduce. If blank, introduce all
% advisors
%
global cfg;
global Sc;

if nargin < 1, advisorId = NaN; end

cfg.expositionTime = [];
cfg.expositionTime=[];
observers = randperm(cfg.advisors.count.real);
oldTextSize = Screen('TextSize', Sc.window, cfg.instr.textSize.medium);
bounds = Screen('TextBounds',Sc.window,cfg.instr.intro.text{1});
if isnan(advisorId)
    for obs = observers
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
    end
else
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
end

cfg.obsIntro_times = [onset_pic; offset_pic; onset_speech; offset_speech];
clear startTime xruns resp kcode ti speech imagedata texture bounds pressKey

Screen('TextSize', Sc.window, oldTextSize);

function introduce(advisorId)
global cfg;
global Sc;
