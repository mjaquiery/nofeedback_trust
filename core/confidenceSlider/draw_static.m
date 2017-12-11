function draw_static(drawMask)
%% draw the static screen elements
% 
% usage: 
%   draw_static([drawMask]);
%
% drawMask - 1-dimensional array indicating which elements to draw
%       1 - progression bar
%       2 - fixation cross
%       3 - scale
%       4 - landmarks
%       5 - response instructions
%
% The value in the array is passed to its relevant drawing function for
% fine control where necessary. 0=don't draw; 1=draw normally; other values
% vary - see documentation for the individual draw functions
global cfg; % configuration object

if nargin < 1
    drawMask = ones(1,5);      
end

if cfg.debug
    debugTrialInfo();
end

%-- add progression bar. Helps motivation
if drawMask(1)
    draw_progression_bar(drawMask(1));
end

% add fixation
if drawMask(2)
    add_fixation(drawMask(2));
end

% draw scale
if drawMask(3)
    draw_scale_(drawMask(3));
end

% draw confidence and interval landmarks
if drawMask(4)
    draw_landmarks(drawMask(4));
end

% add response instructions
if drawMask(5)
    add_responseinstr(drawMask(5));
end