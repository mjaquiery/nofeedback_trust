function draw_static(Sc,cfg,drawMask)
%% draw the static screen elements
% 
% usage: 
%   draw_static(Sc, cfg)
%   draw_static(Sc, cfg, [1 0 1 1 1]);
%
% Sc - screen with an Sc.window to draw to
% cfg - configuration file
% drawMask - logical 1-dimensional array indicating which elements to draw
%       1 - progression bar
%       2 - fixation cross
%       3 - scale
%       4 - landmarks
%       5 - response instructions
if nargin < 3,    drawMask = ones(1,5);      end

if cfg.debug
    debugTrialInfo(Sc, cfg);
end

%-- add progression bar. Helps motivation
if drawMask(1)
    draw_progression_bar(Sc, cfg);
end

% add fixation
if drawMask(2)
    add_fixation(Sc, cfg);
end

% draw scale
if drawMask(3)
    draw_scale_(Sc,cfg);
end

% draw confidence and interval landmarks
if drawMask(4)
    draw_landmarks(Sc,cfg);
end

% add response instructions
if drawMask(5)
    add_responseinstr(Sc, cfg);
end