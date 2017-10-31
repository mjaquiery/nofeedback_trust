cfg.bar.maxScale            = 55;
cfg.bar.minScale            = -55;
cfg.bar.nScale              = 111;
cfg.bar.cursorwidth         = Sc.size(1)/200;
cfg.bar.cursorheight        = 20;
cfg.bar.positiony           = 1;
cfg.bar.barrect             = CenterRectOnPoint([0 0 (cfg.bar.nScale*cfg.bar.cursorwidth) (cfg.bar.cursorheight)],Sc.center(1)-cfg.bar.cursorwidth,Sc.rect(4)*cfg.bar.positiony);
cfg.bar.barlength           = cfg.bar.barrect(3)- cfg.bar.barrect(1);
cfg.bar.gap_size            = 11;
cfg.bar.gap_rect            = CenterRectOnPoint([0,0,cfg.bar.cursorwidth * cfg.bar.gap_size,cfg.bar.cursorheight],...
    Sc.center(1) -((cfg.bar.nScale*cfg.bar.cursorwidth/2)+cfg.bar.cursorwidth) + (cfg.bar.maxScale * cfg.bar.cursorwidth  + cfg.bar.cursorwidth/2), Sc.rect(4)*cfg.bar.positiony);
cfg.bar.gaplength           = 10;

% temporary short names
% maxScale                    = cfg.bar.maxScale;
% minScale                    = cfg.bar.minScale;
% nScale                      = cfg.bar.nScale;
% cursorwidth                 = cfg.bar.cursorwidth;
% cursorheight                = cfg.bar.cursorheight;
% barrect                     = cfg.bar.barrect;
% barlength                   = cfg.bar.barlength;
% gap_size                    = cfg.bar.gap_size;
% gap                         = cfg.bar.gap_rect;