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
if ~isfield(cfg,'instr')
    cfg.instr.cjtext = {'Certainly' 'Maybe'};
    cfg.instr.interval = {'LEFT' 'RIGHT'};
end
if ~isfield(cfg.instr,'cjtext')
    cfg.instr.cjtext = {'Certainly' 'Maybe'};
end
if ~isfield(cfg.instr,'instr')
    cfg.instr.interval = {'LEFT' 'RIGHT'};
end
if ~isfield(cfg.instr, 'xshift')
    cfg.instr.xshift = [linspace(cfg.bar.gaprect(1)-cfg.bar.cursorwidth.*.5,...
            cfg.bar.barrect(1)+cfg.bar.cursorwidth.*.5,length(cfg.instr.cjtext)) ...
        linspace(cfg.bar.gaprect(3)+cfg.bar.cursorwidth.*.5, ...
            cfg.bar.barrect(3)-cfg.bar.cursorwidth.*.5,length(cfg.instr.cjtext))];
end

%% define instructions for confidence judgement
for i=1:length(cfg.instr.cjtext)
    bounds(i,:) = Screen('TextBounds',Sc.window,cfg.instr.cjtext{i});
end
LintBounds              = Screen('TextBounds',Sc.window,cfg.instr.interval{1});
RintBounds              = Screen('TextBounds',Sc.window,cfg.instr.interval{2});

%% draw confidence landmarks
for i=1:length(cfg.instr.xshift)
    Screen('DrawText', Sc.window, ...
        cfg.instr.cjtext{mod(i-1,length(cfg.instr.cjtext))+1}, ...
        cfg.instr.xshift(i) - bounds(mod(i-1,length(cfg.instr.cjtext))+1,3)/2, ...
        Sc.rect(4).*cfg.bar.positiony-40, 0);
end

%% draw interval landmarks
Screen('DrawText', Sc.window, cfg.instr.interval{1}, ...
    Sc.center(1)- (cfg.bar.barlength*.25 + cfg.bar.gaplength*.5) - LintBounds(3)*.5, ...
    (Sc.rect(4).*cfg.bar.positiony +40), 0);

Screen('DrawText', Sc.window, cfg.instr.interval{2}, ...
    Sc.center(1)+ (cfg.bar.barlength*.25 + cfg.bar.gaplength*.5) - RintBounds(3)*.5, ...
    (Sc.rect(4).*cfg.bar.positiony +40), 0);

return