function drawAdvisor(Sc, cfg, advisorID, centerOn, hideName)
%% Shows the advisor portrait - by Matt Jaquiery
%
% Usage: 
%   drawAdvisor(Sc, cfg, advisorID [, centerOn [, hideName]])
%
% Inputs:
%   Sc - screen object 
%   cfg - configuration object
%   advisorID - advisor id
%   centerOn - [x y] coordinates to centre the portrait (not
%   portrait+name!). Default to Sc.center
%   hideName - hide the name string, default to false

%% default argument handling
if nargin < 5,  hideName = false; end
if nargin <4,   centerOn = Sc.center; end

%% get advisor variables
if ~isnan(advisorID)
    advisor = cfg.advisor(advisorID);
else
    advisor = cfg.nullAdvisor;
end

oldTextSize = Screen('TextSize', Sc.window);

%% draw texture
% make texture image out of image matrix 'imdata'
texture = Screen('MakeTexture', Sc.window, advisor.imdata);
Screen('DrawTexture', Sc.window, texture, [],...
    CenterRectOnPoint([0 0 cfg.display.portrait.width cfg.display.portrait.height],...
    centerOn(1),centerOn(2)));

%% write observer name
if ~hideName
    Screen('TextSize', Sc.window, cfg.display.portrait.nameTextSize);
    textLength = Screen('TextBounds', Sc.window, advisor.name);
    Screen('DrawText', Sc.window, advisor.name,...
        floor(centerOn(1) - textLength(3)/2),...
        floor(centerOn(2) + cfg.display.portrait.height/2 + cfg.display.portrait.nameTextOffset));
end

%% cleanup
% set the text size back like a well behaved subroutine
Screen('TextSize', Sc.window, oldTextSize);