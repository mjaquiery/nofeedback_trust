function draw_progression_bar(Sc, cfg)
% Draws the progression bar to help keep participants motivated
%
% Sc - screen object
% cfg - configuration struct

Start='start';
End='end';
startbounds = Screen('TextBounds', Sc.window, Start);
endbounds = Screen('TextBounds', Sc.window, End);
text_buffer = 10;
viscursorwidth=1;
viscursorheight=20;
prbarrect=CenterRectOnPoint( ...
    [0 0 (cfg.ntrialsall*viscursorwidth) viscursorheight], ...
    Sc.center(1),Sc.rect(4).*.1);
donerect=[prbarrect(1) prbarrect(2) ...
    (prbarrect(1)+ cfg.currentTrial * viscursorwidth) prbarrect(4)];

% draw progression bar
Screen('FrameRect',Sc.window,[.9 .9 .9],prbarrect);
Screen('FillRect', Sc.window, [.9 .9 .9],donerect);
DrawFormattedText(Sc.window,Start, prbarrect(1)-startbounds(3)-text_buffer,prbarrect(2)-viscursorheight);
DrawFormattedText(Sc.window,End, prbarrect(3)+text_buffer,prbarrect(2)-viscursorheight);
% 
% Start='start';
% End='end';
% startbounds = Screen('TextBounds', Sc.window, Start);
% endbounds = Screen('TextBounds', Sc.window, End);
% viscursorwidth=1;
% viscursorheight=20;
% subblock = t; 
% prbarrect=CenterRectOnPoint([0 0 (length(trials) * viscursorwidth) (viscursorheight)],Sc.center(1),Sc.center(2)-250);
% donerect=[prbarrect(1) prbarrect(2) (prbarrect(1)+ t * viscursorwidth) prbarrect(4)];
% 
% %add progression bar
% Screen('FrameRect',Sc.window,[.9 .9 .9],prbarrect);
% Screen('FillRect', Sc.window, [.9 .9 .9],donerect);
% DrawFormattedText(Sc.window,Start, prbarrect(1)-startbounds(3)-10,prbarrect(2)-viscursorheight);
% DrawFormattedText(Sc.window,End, prbarrect(3)+10,prbarrect(2)-viscursorheight);