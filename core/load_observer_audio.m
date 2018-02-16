if hasAdvice(trials(t))
    
    freq = cfg.speechFreq(cfg.advisor(trials(t).advisorId).voice,:,:);
    chan = cfg.speechChannels(cfg.advisor(trials(t).advisorId).voice,:,:);
    data = cfg.speechData(cfg.advisor(trials(t).advisorId).voice,:,:);
    
    % with this if obsacc=1, i=wherelarger, but if obsacc=0, i=3-wherelarger
    i = abs((3* (1 - trials(t).obsacc) ) - trials(t).wherelarger); 
    
    % randomize order of the sentence ('I think it's on the right' vs 
    % 'It's on the right I think')
    trials(t).advice_order = randi(2); 
    
    % Fill audio buffer
    trials(t).whichspeech = PsychPortAudio('Open', [], [], 0, ...
                                freq{1,trials(t).advice_order,i}, ...
                                chan{1,trials(t).advice_order,i});
    PsychPortAudio('FillBuffer',trials(t).whichspeech , ...
                                data{1,trials(t).advice_order,i});
end



