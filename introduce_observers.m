cfg.expositionTime = [];
cfg.expositionTime=[];
observers = randperm(3);
pressKey = 'Press any key';
bounds = Screen('TextBounds',Sc.window,pressKey);
for obs = observers
    imagedata = imread([stims_path '/observer',int2str(cfg.observer.pic(obs)),'.jpg']);
    Screen('DrawText', Sc.window, 'Press any key', Sc.center(1)-bounds(3)/2,Sc.center(2)+Sc.size(2)/3, 0);
    % make texture image out of image matrix 'imdata'
    texture = Screen('MakeTexture', Sc.window, imagedata);
    speech = PsychPortAudio('Open', [], [], 0, cfg.introSpeechFreq{cfg.observer.voice(obs)}, cfg.introSpeechChannels{cfg.observer.voice(obs)});
    PsychPortAudio('FillBuffer',speech , cfg.introSpeechData{cfg.observer.voice(obs)});
    
    ti=GetSecs;
    Screen('DrawTexture', Sc.window, texture, [], CenterRectOnPoint([0 0 258 325],(Sc.center(1)),(Sc.center(2))));
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