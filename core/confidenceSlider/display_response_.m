function [ft] = display_response_(temp, cj1, trial)
% Usage:
% [ft] = display_response(temp[, cj1[, trial]])
%
% Inputs:
% temp: vector containing haschanged boolean and current confidence 
%       judgement
% cj1: first confidece judgement if one is available
% trial: if present, uses information in trial to draw advice overlay
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
    int1        = sign(cj1);
    if nargin < 3
        trial = []; 
    else
        % advice string        
        if trial.obsacc == 1 % advisor correct
            if trial.wherelarger == 1 % target on left
                adviceString = 'on the LEFT.';
            else
                adviceString = 'on the RIGHT.';
            end
        else % advisor incorrect
            if trial.wherelarger == 1 % target on left
                adviceString = 'on the RIGHT.';
            else
                adviceString = 'on the LEFT.';
            end
        end
        adviceString = ['I think there were more dots ' adviceString];
        % arguments for speech bubble drawing
        x = Sc.center(1);
        y = 300;
        bubbleArgs = struct('offset', [x y], 'bubbleFrameColor', cfg.display.dotTask.adviceBubbleColor);        
    end
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
    if ~isempty(trial)
        drawAdvisorBubble(trial.advisorId, adviceString, bubbleArgs);
    end
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
