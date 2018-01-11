function draw_progression_bar(drawCode)
%% Draws the progression bar to help keep participants motivated
%
% usage: internal only
% External usage should call via draw_static: draw_static([1 0 0 0 0]);
%
% Typically called from draw_static()
%
global cfg; % configuration object
global Sc; % Screen object

if nargin < 1, drawCode = 1; end
if drawCode == 0, return; end

startbounds = Screen('TextBounds', Sc.window, cfg.display.progress.text.start);
text_buffer = 10;
viscursorwidth=1;
viscursorheight=20;
prbarrect=CenterRectOnPoint( ...
    [0 0 (cfg.ntrialsall*viscursorwidth) viscursorheight], ...
    Sc.center(1),Sc.rect(4).*.1);
donerect=[prbarrect(1) prbarrect(2) ...
    (prbarrect(1)+ cfg.currentTrialNumber * viscursorwidth) prbarrect(4)];

% draw progression bar
Screen('FrameRect', Sc.window, cfg.display.progress.color.frame, prbarrect);
Screen('FillRect', Sc.window, cfg.display.progress.color.bar, donerect);
DrawFormattedText(Sc.window, cfg.display.progress.text.start, ...
    prbarrect(1)-startbounds(3)-text_buffer, prbarrect(2)-viscursorheight);
DrawFormattedText(Sc.window, cfg.display.progress.text.end, ...
    prbarrect(3)+text_buffer, prbarrect(2)-viscursorheight);
