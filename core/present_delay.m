if ~exist('mask1'), mask1 = [1 0 1 1 0]; end
if ~exist('mask2'), mask2 = [1 1 1 1 0]; end

% draw static elements
draw_static(mask1);

drawAdvisor(NaN);

% flip image on screen
[VBLTimestamp2, trials(t).onsetobs, FlipTimestamp2, trials(t).tmissed_onset2] = Screen('Flip',Sc.window,time + cfg.stim.RSI1 - cfg.frame);

% draw static elements
draw_static(mask2);

% offset image
[VBLts, trials(t).offsetobs, FTS, trials(t).tmissed_offset2] = Screen('Flip',Sc.window,trials(t).onsetobs + cfg.advisors.duration - cfg.frame);
time = trials(t).offsetobs;