function [] = add_blockReminder(Sc,cfg)
% Usage:
% [] = add_blockReminder(Sc,cfg,trials,t)
% 
% Niccolo Pescetelli

%% check required fields
if isfield(cfg,'conditionLabels')
    condition = cfg.currentBlock;
    %% add response istructions
    Screen('TextSize', Sc.window, 13);Screen('TextFont', Sc.window, 'Myriad Pro');
    txtline = ['This is a ' condition ' block.'];
    B0 = Screen('TextBounds',Sc.window,txtline);
    DrawFormattedText(Sc.window, txtline, ...
        Sc.rect(3)*.25 - B0(3)*.5,  Sc.rect(4)*.20, 255, [], [], [], 1.5);
    DrawFormattedText(Sc.window, txtline, ...
        Sc.rect(3)*.75 - B0(3)*.5,  Sc.rect(4)*.20, 255, [], [], [], 1.5);
end

return
