function [] = draw_landmarks(drawCode)
% Usage: 
% 
% Required fields are cfg.instr.cjtext.text, cfg.instr.interval.text and 
% cfg.bar.barlength.
% The first one refers to 2 confidence landmarks (eg. sure maybe)
% The second one refers to interval landmarks (eg. LEFT and RIGHT)
% The third one refers to the length of the confidence scale
% Defaults values are assigned only for cfg.instr fields.

% Niccolo Pescetelli

global cfg; % configuration object
global Sc; % Screen object

if nargin < 1, drawCode = 1; end
if drawCode == 0, return; end

%% define font and font size
oldTextSize = Screen('TextSize', Sc.window, cfg.instr.textSize.small);
Screen('TextFont', Sc.window, 'Myriad Pro');

%% check for required fields
if ~isfield(cfg,'instr') || ~isfield(cfg.instr,'cjtext') || ~isfield(cfg.instr,'instr')
    error('Missing text fields in cfg.instr.cjtext or cfg.instr.instr.text\n');
end
if ~isfield(cfg.instr,'xshift')
    error('Missing field cfg.instr.xshift; this should be set in define_scale.m');
end

%% draw confidence landmarks
ypos = Sc.rect(4).*cfg.bar.positiony - cfg.instr.cjtext.position.offsetY;
for i=1:length(cfg.instr.xshift)
    % on even words we want to back up to align nicely on the scale
    xbump = 0;
    if mod(i,2)==0
        xbump = Screen('TextBounds', Sc.window, cfg.instr.cjtext.text{i});
        xbump = xbump(3);
    end
    Screen('DrawText', Sc.window, cfg.instr.cjtext.text{i}, ...
        cfg.instr.xshift(i) - xbump, ypos);
end

if drawCode ~= 2
    %% draw labels
    Screen('TextSize', Sc.window, cfg.instr.textSize.small);
    for i = 1:length(cfg.instr.interval.text)
        xshift = (cfg.bar.gaprect(1) - cfg.bar.barrect(1))/2; % half length of the left bar
        if mod(i,2)
            xbase = cfg.bar.barrect(1); % left edge of the CJ bar
        else
            xbase = cfg.bar.gaprect(3); % right edge of the gap
        end
        xbump = Screen('TextBounds', Sc.window, cfg.instr.interval.text{i});
        x = xshift + xbase - xbump(3)/2;
        ypos = Sc.rect(4).*cfg.bar.positiony + cfg.instr.cjtext.position.offsetY;
        Screen('DrawText', Sc.window, cfg.instr.interval.text{i}, x, ypos);
    end
end

Screen('TextSize', Sc.window, oldTextSize);
return