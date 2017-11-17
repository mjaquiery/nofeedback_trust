function [ft] = display_response_(temp,varargin)
% Usage:
% [ft] = display_response(temp[, cj1])
%
% Inputs:
% temp: vector containing haschanged boolean and current confidence 
%       judgement
% cj1: first confidece judgement if one is available
% 
% Outputs:
% ft - the time the screen flip occurred
%
global cfg; % configuration object
global Sc; % Screen object

if nargin < 2 
    show_cj1    = false; 
else
    show_cj1    = true;
    cj1         = varargin{1};
    int1        = sign(cj1);
end
gs = round(cfg.bar.gap_size/2);
[haschanged,cj] = deal(temp(1),temp(2));

%% display response
% draw static elements
draw_static([1 1 1 1 1]);

% display previous confidence
if show_cj1
    switch int1
        case -1 % 1
            positions = linspace(cfg.bar.gaprect(1)-cfg.bar.cursorwidth.*.5,...
                cfg.bar.barrect(1)+cfg.bar.cursorwidth.*.5,cfg.bar.maxScale);
        case 1
            positions = linspace(cfg.bar.gaprect(3)+cfg.bar.cursorwidth.*.5, ...
                cfg.bar.barrect(3)-cfg.bar.cursorwidth.*.5,cfg.bar.maxScale);
    end
    cj1rect = CenterRectOnPoint([0,0,cfg.bar.cursorwidth,cfg.bar.cursorheight],...
    positions(abs(cj1)), ...
    Sc.rect(4).*cfg.bar.positiony);
    Screen('FillRect', Sc.window, cfg.bar.color.cj1, cj1rect );
end

% define response cursor position
switch sign(cj)
    case -1
        positions = linspace(cfg.bar.gaprect(1)-cfg.bar.cursorwidth.*.5,...
            cfg.bar.barrect(1)+cfg.bar.cursorwidth.*.5,cfg.bar.maxScale);
    case 1
        positions = linspace(cfg.bar.gaprect(3)+cfg.bar.cursorwidth.*.5, ...
            cfg.bar.barrect(3)-cfg.bar.cursorwidth.*.5,cfg.bar.maxScale);
end

% draw cursor only after first click
if haschanged
    cursorrect = CenterRectOnPoint([0,0,cfg.bar.cursorwidth,cfg.bar.cursorheight],...
        positions(abs(cj)), ...
        Sc.rect(4) .* cfg.bar.positiony);
    Screen('FillRect', Sc.window, cfg.bar.color.cursor, cursorrect');
end

% Flip on screen
ft = Screen('Flip', Sc.window);
