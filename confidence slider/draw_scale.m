function [] = draw_scale(Sc,cfg)
% Usage:
% [] = draw_scale(Sc,cfg)
%
% cfg must have .bar fields in order to work. 
% cfg.bar must have gap_rect and barrect fields
% Default values are assigned otherwise.
%
% Niccolo Pescetelli

%% check for fields existence
if ~isfield(cfg,'bar')
    cfg.bar.maxScale            = 55;
    cfg.bar.minScale            = -55;
    cfg.bar.nScale              = length([cfg.bar.minScale:cfg.bar.maxScale]);
    cfg.bar.cursorwidth         = Sc.rect(3)/200;
    cfg.bar.cursorheight        = 20;
    cfg.bar.positiony           = .7;
    cfg.bar.barrect             = CenterRectOnPoint([0 0 (cfg.bar.nScale*cfg.bar.cursorwidth) (cfg.bar.cursorheight)], ...
        Sc.center(1)-cfg.bar.cursorwidth,Sc.rect(4)*cfg.bar.positiony);
    cfg.bar.barlength           = cfg.bar.barrect(3)- cfg.bar.barrect(1);
    cfg.bar.gap_size            = 11;
    cfg.bar.gaprect            = CenterRectOnPoint([0,0,cfg.bar.cursorwidth * cfg.bar.gap_size,cfg.bar.cursorheight],...
        Sc.center(1) -((cfg.bar.nScale*cfg.bar.cursorwidth/2)+ ...
        cfg.bar.cursorwidth) + (cfg.bar.maxScale * cfg.bar.cursorwidth  + ...
        cfg.bar.cursorwidth/2), Sc.size(2)*cfg.bar.positiony);
end
if ~isfield(cfg.bar,'gap_rect'),
    cfg.bar.gaprect            = CenterRectOnPoint([0,0,cfg.bar.cursorwidth * cfg.bar.gap_size,cfg.bar.cursorheight],...
        Sc.center(1) -((cfg.bar.nScale*cfg.bar.cursorwidth/2)+cfg.bar.cursorwidth) + ...
        (cfg.bar.maxScale * cfg.bar.cursorwidth  + cfg.bar.cursorwidth/2), ...
        Sc.rect(4)*cfg.bar.positiony);
end
if ~isfield(cfg.bar,'barrect'),
    cfg.bar.barrect             = CenterRectOnPoint([0 0 (cfg.bar.nScale*cfg.bar.cursorwidth) (cfg.bar.cursorheight)], ...
        Sc.center(1)-cfg.bar.cursorwidth,Sc.size(2)*cfg.bar.positiony);
end

%% draw barrect and gap
rect = [cfg.bar.barrect' cfg.bar.gaprect'];
Screen('FillRect', Sc.window, [[.3 .3 .3]' [.5 .5 .5]'],rect);
Screen('TextFont', Sc.window, 'Myriad Pro');

end