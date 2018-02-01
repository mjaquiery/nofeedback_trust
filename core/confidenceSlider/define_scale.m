cfg.bar.maxScale            = 55;
cfg.bar.minScale            = -55;
cfg.bar.nScale              = length([cfg.bar.minScale:cfg.bar.maxScale]);
cfg.bar.cursorwidth         = 10;
cfg.bar.cursorheight        = 20;
cfg.bar.positiony           = .5;
cfg.bar.barrect             = CenterRectOnPoint([0 0 (cfg.bar.nScale*cfg.bar.cursorwidth) (cfg.bar.cursorheight)], ...
    Sc.center(1), Sc.rect(4)*cfg.bar.positiony);
cfg.bar.barlength           = cfg.bar.barrect(3)- cfg.bar.barrect(1);
cfg.bar.gap_size            = 11;
cfg.bar.gaprect            = CenterRectOnPoint([0,0,cfg.bar.cursorwidth * cfg.bar.gap_size,cfg.bar.cursorheight],...
    Sc.center(1), Sc.rect(4)*cfg.bar.positiony);
cfg.bar.gaplength           = 10;
cfg.bar.color.bar           = [.3 .3 .3]'; % bar colour
cfg.bar.color.cursor        = [.8 .8 .8]'; % cursor colour
cfg.bar.color.cj1           = [.99 .99 .0]'; % colour of the first confidence judgement indicator

% Questionnaire bar
cfg.bar.Q.y                 = Sc.size(2)*.6;

% define the text offsets here since they depend on the bar details
cfg.instr.xshift = [...
    cfg.bar.barrect(1) ...
    cfg.bar.gaprect(1) ...
    cfg.bar.gaprect(3) ...
    cfg.bar.barrect(3)...
    ];

    