function t = drawSECS(qnum, pscore, ascore, advisorId)
%% Draw the SECS stuff
% - by Matt Jaquiery
%
% usage: drawSECS(qnum[, pscore[, ascore]])
%
% Inputs: 
% qnum - question number
% pscore - participant's score
% ascore - advisor's score
% advisorId - advisor Id (for drawing portrait and name)
%
% Outputs:
% t - screen flip time
%
% Draw the sliding scale and labels for the SECS questionnaire
tic();
if nargin < 4 && nargin > 2, error('No advisorId specified for advisor score'); end
if nargin < 3, ascore = NaN; end
if nargin < 2, pscore = NaN; end
if nargin < 1, error('No question number specified'); end

global cfg;
global Sc;

qtxt = cfg.SECS.questions.text{qnum}; 

%% Calculate the text bounds    
% this is pretty ineffcient since we recalculate this every time
% probably unimportant, but we could save calculated values
oldTextSize = Screen('TextSize', Sc.window, cfg.SECS.qSize);
Screen('TextFont', Sc.window, 'Myriad Pro');
Qbounds = Screen('TextBounds', Sc.window, qtxt);
Screen('TextSize', Sc.window, cfg.instr.textSize.small);
labelBounds = zeros(1,length(cfg.SECS.labels));
for j = 1:length(cfg.SECS.labels)
    sb = Screen('TextBounds', Sc.window, cfg.SECS.labels{j});
    labelBounds(j) = floor(sb(3)); % we only care about the width here    
end    

% draw question text
Screen('TextSize', Sc.window, cfg.SECS.qSize);
Screen('DrawText', Sc.window, qtxt, Sc.center(1)- (Qbounds(3)/2), cfg.SECS.qy, 0);
if isnan(ascore)
    % only draw the prompt if inputs will be allowed (i.e. not showing an
    % advisor score)
    Screen('TextSize', Sc.window, cfg.instr.textSize.medium);
    promptBounds = Screen('TextBounds', Sc.window, cfg.SECS.prompt);
    Screen('TextSize', Sc.window, cfg.instr.textSize.medium);
    Screen('DrawText', Sc.window, cfg.SECS.prompt, Sc.center(1) - promptBounds(3)/2, ...
        cfg.SECS.by+cfg.instr.Q.position.labelOffsetY, 0);
    % progress bar and instructions
    draw_static([1 0 0 0 cfg.SECS.instruction.offsetY]);
else
    % progress bar only
    draw_static([1 0 0 0 0]);
end
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
        oldBGColor = Screen('TextBackgroundColor', Sc.window);
        % add a label for the participant's marker
        points = getTriangle([cursorrect(1)+(cursorrect(3)-cursorrect(1))/2, cursorrect(2)-cfg.SECS.cw], 0, [cfg.SECS.cw, cfg.SECS.cw]);
        Screen('FillPoly', Sc.window, cfg.SECS.markerLabel.you.bgColor, floor(points));
        labelBox = Screen('TextBounds', Sc.window, cfg.SECS.markerLabel.you.text, cfg.instr.textSize.small);
        labelBox([3 4]) = floor(labelBox([3 4])+cfg.SECS.markerLabel.padding*2); % pad out the box so it looks nice
        labelBox = CenterRectOnPoint(labelBox, points(1,1), points(3,2));
        labelBox([2 4]) = labelBox([2 4])+points(3,2)-points(1,2); % shift the box up to the correct height
        Screen('FillRect', Sc.window, cfg.SECS.markerLabel.you.bgColor, labelBox);  
        Screen('DrawText', Sc.window, cfg.SECS.markerLabel.you.text, ...
            labelBox(1)+cfg.SECS.markerLabel.padding, ...
            labelBox(2)+cfg.SECS.markerLabel.padding, [0 0 0]', cfg.SECS.markerLabel.you.bgColor);
        % add in the markers for the advisor and participant
        advisorrect = CenterRectOnPoint([0,0,cfg.SECS.cw,cfg.SECS.ch],...
        Sc.center(1) -((cfg.SECS.ns*cfg.SECS.cw/2)+cfg.SECS.cw) + (ascore * cfg.SECS.cw  + cfg.SECS.cw/2), cfg.SECS.by);
        rect = [cursorrect' advisorrect'];
        Screen('FillRect', Sc.window, cfg.bar.color.bar, cfg.SECS.brct');
        Screen('FrameRect', Sc.window, [cfg.SECS.markerColor.you cfg.SECS.markerColor.advisor], rect);
        % add a label for the advisor's marker
        points = getTriangle([advisorrect(1)+(advisorrect(3)-advisorrect(1))/2, advisorrect(4)+cfg.SECS.cw], ...
            180, [cfg.SECS.cw, cfg.SECS.cw]);
        Screen('FillPoly', Sc.window, cfg.SECS.markerLabel.advisor.bgColor, floor(points));
        labelBox = Screen('TextBounds', Sc.window, cfg.advisor(advisorId).name, cfg.instr.textSize.small);
        labelBox([3 4]) = floor(labelBox([3 4])+cfg.SECS.markerLabel.padding*2); % pad out the box so it looks nice
        labelBox = CenterRectOnPoint(labelBox, points(1,1), points(3,2));
        labelBox([2 4]) = labelBox([2 4])+points(3,2)-points(1,2); % shift the box down to the correct height
        % save the coordinates needed for text positioning
        txtx = labelBox(1)+cfg.SECS.markerLabel.padding;
        txty = labelBox(2)+cfg.SECS.markerLabel.padding;
        txth = labelBox(4)-labelBox(2);
        % expand the labelBox to accommmodate a portrait
        if labelBox(3)-labelBox(1) < cfg.SECS.portrait.width+cfg.SECS.markerLabel.padding*2
            labelBox(3) = labelBox(1)+cfg.SECS.portrait.width+cfg.SECS.markerLabel.padding*2;
        end
        labelBox(4) = labelBox(4)+cfg.SECS.portrait.height+cfg.SECS.markerLabel.padding;
        % recentre
        labelBox = CenterRectOnPoint(labelBox, points(1,1), points(3,2)+(labelBox(4)-labelBox(2))/2);
        % calculate the inner portrait box
        portraitBox = [labelBox(1)+cfg.SECS.markerLabel.padding, ...
            labelBox(2)+txth+cfg.SECS.markerLabel.padding, ...
            labelBox(3)-cfg.SECS.markerLabel.padding, ...
            labelBox(4)-cfg.SECS.markerLabel.padding];
        boxh = (portraitBox(4)-portraitBox(2));
        boxw = (portraitBox(3)-portraitBox(1));
        Screen('FillRect', Sc.window, cfg.SECS.markerLabel.advisor.bgColor, labelBox);
        Screen('FillRect', Sc.window, [0 0 0]', portraitBox);
        drawAdvisor(advisorId, [portraitBox(1)+boxw/2 portraitBox(2)+boxh/2], ...
            [cfg.SECS.portrait.width cfg.SECS.portrait.height], 0);
        Screen('DrawText', Sc.window, cfg.advisor(advisorId).name, txtx, txty, [0 0 0]', cfg.SECS.markerLabel.advisor.bgColor);
        Screen('TextBackgroundColor', Sc.window, oldBGColor);
    else
        rect = [cfg.SECS.brct' cursorrect'];
        Screen('FillRect', Sc.window, [cfg.bar.color.bar cfg.bar.color.cursor],rect);
    end
else
    Screen('FillRect', Sc.window, cfg.bar.color.bar, cfg.SECS.brct');
end

Screen('TextSize', Sc.window, oldTextSize);
t = Screen('Flip', Sc.window);
