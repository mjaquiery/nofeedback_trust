function draw_static(drawMask)
%% draw the static screen elements
% 
% usage: 
%   draw_static([drawMask]);
%
% drawMask - logical 1-dimensional array indicating which elements to draw
%       1 - progression bar
%       2 - fixation cross
%       3 - scale
%       4 - landmarks
%       5 - response instructions
global cfg; % configuration object

if nargin < 1
    drawMask = ones(1,5);      
end

if cfg.debug
    debugTrialInfo();
end

%-- add progression bar. Helps motivation
if drawMask(1)
    draw_progression_bar();
end

% add fixation
if drawMask(2)
    add_fixation();
end

% draw scale
if drawMask(3)
    draw_scale_();
end

% draw confidence and interval landmarks
if drawMask(4)
    draw_landmarks();
end

% add response instructions
if drawMask(5)
    add_responseinstr();
end