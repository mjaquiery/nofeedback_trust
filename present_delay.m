drawAdvisor(Sc, cfg, NaN);

% draw static elements
draw_static(Sc, cfg, [1 0 1 1 0]);

% flip image on screen
[VBLTimestamp2, trials(t).onsetobs, FlipTimestamp2, trials(t).tmissed_onset2] = Screen('Flip',Sc.window,time + cfg.stim.RSI1 - cfg.frame);

% draw static elements
draw_static(Sc, cfg, [1 0 1 1 0]);

% offset image
[VBLts, trials(t).offsetobs, FTS, trials(t).tmissed_offset2] = Screen('Flip',Sc.window,trials(t).onsetobs + cfg.observer.duration - cfg.frame);
time = trials(t).offsetobs;