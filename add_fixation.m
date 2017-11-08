function add_fixation(Sc, cfg)
%% add fixation '+' the center of screen
oldTextSize = Screen('TextSize', Sc.window, cfg.display.fixationXsize);
DrawFormattedText(Sc.window, '+','center','center', [0 0 0]);
disp(['added fixation size ' int2str(Screen('TextSize',Sc.window))]);
Screen('TextSize', Sc.window, oldTextSize);
