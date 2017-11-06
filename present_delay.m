% draw observer picture
Screen('DrawTexture', Sc.window, sil_tex, [],...
    CenterRectOnPoint([0 0 cfg.display.portrait.width cfg.display.portrait.height],...
    Sc.center(1),Sc.center(2)));

% draw static elements
draw_static(Sc, cfg, true, true)

% flip image on screen
[VBLTimestamp2, trials(t).onsetobs, FlipTimestamp2, trials(t).tmissed_onset2] = Screen('Flip',Sc.window,time + cfg.stim.RSI1 - cfg.frame);

% draw static elements
draw_static(Sc, cfg)

% offset image
[VBLts, trials(t).offsetobs, FTS, trials(t).tmissed_offset2] = Screen('Flip',Sc.window,trials(t).onsetobs + cfg.observer.duration - cfg.frame);
time = trials(t).offsetobs;