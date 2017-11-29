function drawAdvisor(advisorId, centerOn, drawSmall, namePosition)
%% Shows the advisor portrait 
% - by Matt Jaquiery
%
% Usage: drawAdvisor(advisorId[, centerOn [, hideName]])
%
% Inputs:
% advisorId - advisor id
% centerOn - [x y] coordinates to centre the portrait (not
% portrait+name!). Default to Sc.center
% namePosition - whereabouts to draw the name: 0=don't draw;
%   1(default)=draw below; 2=draw above

global cfg; % configuration object
global Sc; % screen object

%% default argument handling
if nargin < 4,  namePosition = 1; end
if nargin < 3,  drawSmall = false; end
if nargin < 2,  centerOn = Sc.center; end

%% get advisor variables
if advisorId==0
    advisor = cfg.noAdvisorChoice;
elseif ~isnan(advisorId)
    advisor = cfg.advisor(advisorId);
else
    advisor = cfg.nullAdvisor;
end

oldTextSize = Screen('TextSize', Sc.window);

%% draw texture
% make texture image out of image matrix 'imdata'
if drawSmall
    rect = [0 0 cfg.display.choice.portraitSize(1) cfg.display.choice.portraitSize(2)];
else
    rect = [0 0 cfg.display.portrait.width cfg.display.portrait.height];
end
texture = Screen('MakeTexture', Sc.window, advisor.imdata);
Screen('DrawTexture', Sc.window, texture, [],...
    CenterRectOnPoint(rect,...
    centerOn(1),centerOn(2)));

%% write observer name
if namePosition ~= 0 && ~isempty(advisor.name)
    Screen('TextSize', Sc.window, cfg.display.portrait.nameTextSize);
    textLength = Screen('TextBounds', Sc.window, advisor.name);
    if drawSmall
        portraitEdge = cfg.display.choice.portraitSize(2)/2;
    else
        portraitEdge = cfg.display.portrait.height/2;
    end
    switch namePosition
        case 2 % draw above
            y = centerOn(2) - portraitEdge - cfg.display.portrait.nameTextOffset;
            ybase = 0;
        otherwise
            y = centerOn(2) + portraitEdge + cfg.display.portrait.nameTextOffset;
            ybase = 1;
    end
    Screen('DrawText', Sc.window, advisor.name,...
        floor(centerOn(1) - textLength(3)/2),...
        floor(y), cfg.instr.textColor.default, cfg.display.color.background, ybase);
end

%% cleanup
% set the text size back like a well behaved subroutine
Screen('TextSize', Sc.window, oldTextSize);
