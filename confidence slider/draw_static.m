function [] = draw_static(Sc,cfg)
% set font size
Screen('TextSize', Sc.window, 13);

%-- add progression bar. Helps motivation
Start='start';
End='end';
startbounds = Screen('TextBounds', Sc.window, Start);
endbounds = Screen('TextBounds', Sc.window, End);
viscursorwidth=1;
viscursorheight=20;
prbarrect=CenterRectOnPoint( ...
    [0 0 (sum(cfg.ntrialsall)* viscursorwidth) viscursorheight], ...
    Sc.center(1),Sc.rect(4).*.2);
donerect=[prbarrect(1) prbarrect(2) ...
    (prbarrect(1)+ cfg.currentTrial * viscursorwidth) prbarrect(4)];

% draw progression bar
Screen('FrameRect',Sc.window,[.9 .9 .9],prbarrect);
Screen('FillRect', Sc.window, [.9 .9 .9],donerect);
DrawFormattedText(Sc.window,Start, prbarrect(1)-startbounds(3)-10,prbarrect(2)-viscursorheight);
DrawFormattedText(Sc.window,End, prbarrect(3)+10,prbarrect(2)-viscursorheight);

% add fixation
add_fixation;

% draw scale
draw_scale_(Sc,cfg);

% draw confidence and interval landmarks
draw_landmarks(Sc,cfg);

% add response instructions
add_responseinstr(Sc,cfg);
return