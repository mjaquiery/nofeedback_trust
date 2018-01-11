function add_fixation(drawCode)
%% add fixation '+' the center of screen
% - by Matt Jaquiery
%
% usage: add_fixation()
%
% Draws a fixation cross in the center of the screen
%

global cfg; % configuration object
global Sc; % Screen object

if nargin < 1, drawCode = 1; end
if drawCode == 0, return; end

oldTextSize = Screen('TextSize', Sc.window, cfg.display.fixationX.size);
DrawFormattedText(Sc.window, '+','center','center', cfg.display.fixationX.color);
Screen('TextSize', Sc.window, oldTextSize);
