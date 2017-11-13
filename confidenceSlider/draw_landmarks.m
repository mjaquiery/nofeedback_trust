function [] = draw_landmarks(Sc,cfg)
% Usage: 
% [] = draw_landmarks(Sc,cfg)
% 
% Required fields are cfg.instr.cjtext, cfg.instr.interval and 
% cfg.bar.barlength.
% The first one refers to 2 confidence landmarks (eg. sure maybe)
% The second one refers to interval landmarks (eg. LEFT and RIGHT)
% The third one refers to the length of the confidence scale
% Defaults values are assigned only for cfg.instr fields.

% Niccolo Pescetelli

%% define font and font size
Screen('TextSize', Sc.window, 13);
Screen('TextFont', Sc.window, 'Myriad Pro');

%% check for required fields
if ~isfield(cfg,'instr') || ~isfield(cfg.instr,'cjtext') || ~isfield(cfg.instr,'instr')
    set_cfg_text
end
if ~isfield(cfg.instr,'xshift')
    define_scale
end

%% draw confidence landmarks
ypos = Sc.rect(4).*cfg.bar.positiony - 40;
for i=1:length(cfg.instr.xshift)
    % on even words we want to back up to align nicely on the scale
    xbump = 0;
    if mod(i,2)==0
        xbump = Screen('TextBounds', Sc.window, cfg.instr.cjtext{i});
        xbump = xbump(3);
    end
    Screen('DrawText', Sc.window, cfg.instr.cjtext{i}, ...
        cfg.instr.xshift(i) - xbump, ypos);
end

return