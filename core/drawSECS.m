function t = drawSECS(qnum, pscore, ascore)
%% Draw the SECS stuff
% - by Matt Jaquiery
%
% usage: drawSECS(qnum[, pscore[, ascore]])
%
% Inputs: 
% qnum - question number
% pscore - participant's score
% ascore - advisor's score
%
% Outputs:
% t - screen flip time
%
% Draw the sliding scale and labels for the SECS questionnaire

if nargin < 3, ascore = NaN; end
if nargin < 2, pscore = NaN; end
if nargin < 1, error('No question number specified'); end

global cfg;
global Sc;

qtxt = cfg.SECS.questions.text{qnum}; 

%% Calculate the text bounds    
% this is pretty ineffcient since we recalculate this every time
% probably unimportant, but we could save calculated values
Screen('TextSize', Sc.window, cfg.SECS.qSize);
Qbounds = Screen('TextBounds', Sc.window, qtxt);
Screen('TextSize', Sc.window, cfg.instr.textSize.small);
labelBounds = zeros(1,length(cfg.SECS.labels));
for j = 1:length(cfg.SECS.labels)
    sb = Screen('TextBounds', Sc.window, cfg.SECS.labels{j});
    labelBounds(j) = sb(3); % we only care about the width here    
end    
Screen('TextSize', Sc.window, cfg.instr.textSize.medium);
promptBounds = Screen('TextBounds', Sc.window, cfg.SECS.prompt);

% draw question text
Screen('TextSize', Sc.window, cfg.SECS.qSize);
Screen('DrawText', Sc.window, qtxt, Sc.center(1)- (Qbounds(3)/2), cfg.SECS.qy, 0);
% draw prompt
Screen('TextSize', Sc.window, cfg.instr.textSize.medium);
Screen('DrawText', Sc.window, cfg.SECS.prompt, Sc.center(1) - promptBounds(3)/2, ...
    cfg.SECS.by+cfg.instr.Q.position.labelOffsetY, 0);
%add labels
Screen('TextSize', Sc.window, cfg.instr.textSize.small);
Screen('DrawText', Sc.window, cfg.SECS.labels{1}, ...
    Sc.center(1) - cfg.SECS.bl/2 - labelBounds(1)/2, ... center - half bar length - offset
    cfg.SECS.by-cfg.instr.Q.position.labelOffsetY, 0);
Screen('DrawText', Sc.window, cfg.SECS.labels{2}, ...
    Sc.center(1) - labelBounds(2)/2, ... center - offset
    cfg.SECS.by-cfg.instr.Q.position.labelOffsetY, 0);
Screen('DrawText', Sc.window, cfg.SECS.labels{3}, ...
    Sc.center(1) + cfg.SECS.bl/2 - labelBounds(3)/2, ... center - half bar length - offset
    cfg.SECS.by-cfg.instr.Q.position.labelOffsetY, 0);
% draw response bar
if ~isnan(pscore)
    % add cursor position
    cursorrect = CenterRectOnPoint([0,0,cfg.SECS.cw,cfg.SECS.ch],...
        Sc.center(1) -((cfg.SECS.ns*cfg.SECS.cw/2)+cfg.SECS.cw) + (pscore * cfg.SECS.cw  + cfg.SECS.cw/2), cfg.SECS.by);
    if ~isnan(ascore)
        % add in the marker for the advisor
        advisorrect = CenterRectOnPoint([0,0,cfg.SECS.cw,cfg.SECS.ch],...
        Sc.center(1) -((cfg.SECS.ns*cfg.SECS.cw/2)+cfg.SECS.cw) + (ascore * cfg.SECS.cw  + cfg.SECS.cw/2), cfg.SECS.by);
        rect = [cfg.SECS.brct' cursorrect' advisorrect'];
        Screen('FillRect', Sc.window, [cfg.bar.color.bar cfg.bar.color.cursor cfg.SECS.advisorMarkerColor], rect);
    else
        rect = [cfg.SECS.brct' cursorrect'];
        Screen('FillRect', Sc.window, [cfg.bar.color.bar cfg.bar.color.cursor],rect);
    end
else
    Screen('FillRect', Sc.window, cfg.bar.color.bar, cfg.SECS.brct');
end

t = Screen('Flip', Sc.window);