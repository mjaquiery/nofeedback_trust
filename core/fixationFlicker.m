function fixationFlicker(t, drawMask)
%% flicker the fixation cross
% - by Matt Jaquiery
%
% usage: fixationFlicker([t[, drawMask]])
%
% Inputs:
%   t: time for which the fixation cross should disappear (defaults to
% cfg.stim.fixationFlicker.time.duration)
%   drawMask: the draw mask for draw_static (2nd position is ignored since
% this draws the cross and it handled by this function). Defaults to
% drawing only the cross and the progress bar
%
% Remove the fixation cross and then redraw it after t seconds. Used to
% alert the participant that a stimulus will appear soon.

global cfg; % configuration object
global Sc; % screen object

if nargin < 2, drawMask = [1 1 0 0 0]; end
if nargin < 1, t = cfg.stim.fixationFlicker.time.duration; end

drawMask(2) = 0;
    
draw_static(drawMask);
Screen('Flip', Sc.window);
WaitSecs(t);

drawMask(2) = 1;
draw_static(drawMask);
Screen('Flip', Sc.window);
