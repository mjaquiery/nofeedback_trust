function [choice,time] = getAdvisorChoice(Sc,cfg,advisorTID,advisorBID)
% Usage:
% [choice time] = getAdvisorChoice(Sc,cfg,advisorT,advisorB)
% Inputs:
% Sc: Sc structure
% cfg: cfg structure
% advisorTID: advisor object for the top advisor
% advisorBID: advisor object for the bottom advisor
%
% choice is 1 (left) or 2 (right)
% time is a GetSecs time
%
% function by matt.jaquiery@psy.ox.ac.uk

%% initalize variables
choice = 0; 
validChoice = false;
buttons = [];
tnow = GetSecs;
tmin = tnow + 0.25;

%% Draw the options
drawPortraitChoiceDisplay(Sc, cfg, advisorTID, advisorBID, choice);

%% Show mouse pointer
ShowCursor('Arrow');

%% collect response
while ~(any(buttons) && ~choice==0 && tnow>tmin && validChoice)
    validChoice = false;
    [~, resp_y, buttons] = GetMouseWrapper(Sc);
        
    if resp_y<Sc.center(2) % if mouse on the top
        choice = -1;
        if advisorTID~=0
            validChoice = true;
        end
    elseif resp_y>=Sc.center(2) % if mouse on the bottom
        choice = 1;
        if advisorBID ~= 0
            validChoice = true;
        end
    end

    %--- display response
    drawPortraitChoiceDisplay(Sc, cfg, advisorTID, advisorBID, choice);
    tnow = GetSecs;
end
% we now have a click so we consider our choice confirmed

time = GetSecs;
choice = find([-1 1]==choice); % change choice to 1 or 2 rather than -1 or 1

%% hide back cursor
HideCursor;

return

function drawPortraitChoiceDisplay(Sc, cfg, obsT, obsB, choice)
%% draw static elements
%draw_static(Sc, cfg, [1 0 0 0 0]);

drawAdvisor(Sc, cfg, obsT, [Sc.center(1), Sc.center(2)-cfg.display.choice.offset*Sc.rect(4)], true);
drawAdvisor(Sc, cfg, obsB, [Sc.center(1), Sc.center(2)+cfg.display.choice.offset*Sc.rect(4)], true, 2);

%% draw selection box
% only draw if we're considering a valid option
if choice~=0 && ~((choice==-1 && obsT==0) || (choice==1 && obsB==0))
    rect = [0 0 ...
        cfg.display.choice.portraitSize(1)+cfg.display.choice.frame.gap*2 ...
        cfg.display.choice.portraitSize(2)+cfg.display.choice.frame.gap*2];

    switch choice
        case -1 % draw a box around the top portrait
            rect = CenterRectOnPoint(rect, Sc.center(1), Sc.center(2)-cfg.display.choice.offset*Sc.rect(4));
        case 1 % draw a box around the bottom portrait
            rect = CenterRectOnPoint(rect, Sc.center(1), Sc.center(2)+cfg.display.choice.offset*Sc.rect(4));
    end

    Screen('FrameRect', Sc.window, cfg.display.choice.frame.color, rect, cfg.display.choice.frame.width);
end

%% add some text instructions
Screen('TextSize', Sc.window, cfg.instr.textSize.medium);
txt = cfg.instr.chooseAdvisor{1};
textLength = Screen('TextBounds', Sc.window, txt, 0, 0);
DrawFormattedText(Sc.window, txt,...
    Sc.center(1)-textLength(3)/2,...
    Sc.center(2));

%% execute drawings
Screen('Flip', Sc.window);