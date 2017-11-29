function add_fixation()
%% add fixation '+' the center of screen
% - by Matt Jaquiery
%
% usage: add_fixation()
%
% Draws a fixation cross in the center of the screen
%

global cfg; % configuration object
global Sc; % Screen object

oldTextSize = Screen('TextSize', Sc.window, cfg.display.fixationX.size);
DrawFormattedText(Sc.window, '+','center','center', cfg.display.fixationX.color);
Screen('TextSize', Sc.window, oldTextSize);
