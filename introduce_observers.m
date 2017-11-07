cfg.expositionTime = [];
cfg.expositionTime=[];
observers = randperm(3);
Screen('TextSize', Sc.window, cfg.instr.textSize.medium);
pressKey = 'Press any key';
bounds = Screen('TextBounds',Sc.window,pressKey);
for obs = observers
    drawAdvisor(Sc, cfg, obs);
    speech = PsychPortAudio('Open', [], [], 0, cfg.introSpeechFreq{cfg.advisor(obs).voice}, cfg.introSpeechChannels{cfg.advisor(obs).voice});
    PsychPortAudio('FillBuffer',speech , cfg.introSpeechData{cfg.advisor(obs).voice}); 
    ti=GetSecs;
    Screen('DrawText', Sc.window, 'Press any key', Sc.center(1)-bounds(3)/2, Sc.rect(4)*.8);
    onset_pic(obs) = Screen('Flip',Sc.window,ti);
    onset_speech(obs)  = PsychPortAudio('Start', speech, 1);
    [startTime offset_speech(obs) xruns ~] = PsychPortAudio('Stop', speech,1);
    PsychPortAudio('Close', speech);
    [resp offset_pic(obs) kcode] = collect_response(cfg,inf);
    Screen('Flip',Sc.window);
    cfg.expositionTime(obs) = offset_pic(obs) - ti;
end

cfg.obsIntro_times = [onset_pic; offset_pic; onset_speech; offset_speech];
clear startTime xruns resp kcode ti speech imagedata texture bounds pressKey