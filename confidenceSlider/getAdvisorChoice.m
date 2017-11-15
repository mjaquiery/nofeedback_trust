function [choice,time] = getAdvisorChoice(Sc,cfg,advisorLID,advisorRID)
% Usage:
% [choice time] = getAdvisorChoice(Sc,cfg,advisorL,advisorR)
% Inputs:
% Sc: Sc structure
% cfg: cfg structure
% advisorL: advisor object for the left advisor - see images_texture.m
% advisorR: advisor object for the right advisor
%
% choice is 1 (left) or 2 (right)
% time is a GetSecs time
%
% function by matt.jaquiery@psy.ox.ac.uk

%% initalize variables
choice = 0; 
buttons = [];
tnow = GetSecs;
tmin = tnow + 0.25;

%% Draw the options
drawPortraitChoiceDisplay(Sc, cfg, advisorLID, advisorRID, choice);

%% Show mouse pointer
ShowCursor('Arrow');

%% collect response
while ~(any(buttons) && ~choice==0 && tnow>tmin)
    [resp_x, ~, buttons] = GetMouseWrapper(Sc);
        
    if resp_x<Sc.center(1) % if mouse on the left
        choice = -1;
    elseif resp_x>=Sc.center(1) % if mouse on the right
        choice = 1;
    end

    %--- display response
    drawPortraitChoiceDisplay(Sc, cfg, advisorLID, advisorRID, choice);
    tnow = GetSecs;
end
% we now have a click so we consider our choice confirmed

time = GetSecs;
choice = find([-1 1]==choice); % change choice to 1 or 2 rather than -1 or 1

%% hide back cursor
HideCursor;

return

function drawPortraitChoiceDisplay(Sc, cfg, obsL, obsR, choice)
%% draw static elements
draw_static(Sc, cfg, [1 1 0 0 0]);

drawAdvisor(Sc, cfg, obsL, [Sc.center(1)-cfg.display.choice.offset, Sc.center(2)]);
drawAdvisor(Sc, cfg, obsR, [Sc.center(1)+cfg.display.choice.offset, Sc.center(2)]);

%% draw dynamic elements
if choice~=0    
    rect = [0 0 ...
        cfg.display.portrait.width+cfg.display.choice.frame.gap*2 ...
        cfg.display.portrait.height+cfg.display.choice.frame.gap*2];

    switch choice
        case -1 % draw a box around the left portrait
            rect = CenterRectOnPoint(rect, Sc.center(1)-cfg.display.choice.offset, Sc.center(2));
        case 1 % draw a box around the right portrait
            rect = CenterRectOnPoint(rect, Sc.center(1)+cfg.display.choice.offset, Sc.center(2));
    end

    Screen('FrameRect', Sc.window, cfg.display.choice.frame.color, rect, cfg.display.choice.frame.width);
end

%% add some text instructions
Screen('TextSize', Sc.window, cfg.instr.textSize.medium);
txt = cfg.instr.chooseAdvisor{1};
textLength = Screen('TextBounds', Sc.window, txt, 0, 0);
DrawFormattedText(Sc.window, txt,...
    Sc.center(1)-textLength(3)/2,...
    Sc.center(2)-cfg.display.choice.instructionTextOffset);

%% execute drawings
Screen('Flip', Sc.window);