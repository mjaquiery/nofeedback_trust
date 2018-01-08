function [selection,time] = getAdvisorChoice(advisorTopId,advisorBottomId)
%% Presents advisor choice screen and returns selection
% - by Matt Jaquiery
%
% Usage:
% [choice time] = getAdvisorChoice(advisorTopId,advisorBottomId)
% Inputs:
% advisorTopId: advisor object for the top advisor
% advisorBottomId: advisor object for the bottom advisor
%
% Ouputs:
% selection: which advisor was chosen: 1 (top) or 2 (bottom)
% time: GetSecs time the choice was confirmed (clicked)
%
% Participants are allowed a choice of advisor in some trials. This
% function displays the portraits, one above and one below the center of
% the screen, and prompts the participant to choose. 
% Moving the mouse above or below the veritcal highlights a portrait by
% surrounding it with a selection box, and clicking confirms the selection.
% Click time is returned as time.
% The choice variable is presented in the interval [1,2] to allow
% derivation of the chosen advisor id by applying selection to the options
% encoded in the trial: advisorId = trials(t).choice(selection);

global cfg; % configuration object
global Sc; % screen object

%% initalize variables
selection = 0; 
validChoice = false;
buttons = [];
tnow = GetSecs;
tmin = tnow + 0.25;

%% Draw the options
drawPortraitChoiceDisplay(advisorTopId, advisorBottomId, selection);

%% Show mouse pointer
ShowCursorCenter('Arrow');

%% collect response
while ~(any(buttons) && ~selection==0 && tnow>tmin && validChoice)
    validChoice = false;
    [~, resp_y, buttons] = GetMouseWrapper;
        
    if resp_y<Sc.center(2) % if mouse on the top
        selection = -1;
        if advisorTopId~=0
            validChoice = true;
        end
    elseif resp_y>=Sc.center(2) % if mouse on the bottom
        selection = 1;
        if advisorBottomId ~= 0
            validChoice = true;
        end
    end

    %--- display response
    drawPortraitChoiceDisplay(advisorTopId, advisorBottomId, selection);
    tnow = GetSecs;
end
% we now have a click so we consider our choice confirmed

time = GetSecs;
selection = find([-1 1]==selection); % change choice to 1 or 2 rather than -1 or 1

%% hide back cursor
HideCursor;

return

function drawPortraitChoiceDisplay(obsT, obsB, choice)

global cfg; % configuration object
global Sc; % screen object

%% draw static elements
%draw_static([1 0 0 0 0]);

drawAdvisor(obsT, [Sc.center(1), Sc.center(2)-cfg.display.choice.offset*Sc.rect(4)], true);
drawAdvisor(obsB, [Sc.center(1), Sc.center(2)+cfg.display.choice.offset*Sc.rect(4)], true, 2);

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
txt = cfg.instr.chooseAdvisor.text{1};
textLength = Screen('TextBounds', Sc.window, txt, 0, 0);
DrawFormattedText(Sc.window, txt,...
    Sc.center(1)-textLength(3)/2,...
    Sc.center(2));

%% execute drawings
Screen('Flip', Sc.window);