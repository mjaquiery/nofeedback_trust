function drawAdvisorBubble(advisorId, adviceString, args)
%% Draw an overlay of a speech bubble with the advisor's portrait
% Matt Jaquiery, Feb 2018
% 
% usage: drawAdvisorBubble(advisorId, adviceString[, args])
%
% Inputs:
% advisorId: id of the advisor giving the advice
% adviceString: text to draw in the speech bubble
% args: struct with fiels:
%   offset: [x=Sc.center(1) y=Sc.center(2)] coordinate of top left point at which to start drawing
%   portraitSize: [w=115 h=150] width and height of the portrait
%   bubblePadding: (default=10) padding between text and bubble frame
%   textSize: (default=18) size of the adviceString text
%   lineHeight: (default=20) height of each line of text
%   bubbleFrameColor: (default=[0 0 0]) color of the bubble frame
%
global cfg; % configuration object
global Sc;  % screen object

% parameter fixing
if nargin < 2, args = struct(); end
if ~isfield(args, 'lineHeight'), args.lineHeight = 20; end
if ~isfield(args, 'textSize'), args.textSize = 18; end
if ~isfield(args, 'bubblePadding'), args.bubblePadding = 10; end
if ~isfield(args, 'portraitSize'), args.portraitSize = [115 150]; end
if ~isfield(args, 'offset'), args.offset = [Sc.center(1) Sc.center(2)]; end
args.maxLineWidth = args.portraitSize(1) * 1.5;
args.triangleHeight = args.bubblePadding;
oldTextSize = Screen('TextSize', Sc.window, args.textSize);
% draw advisor
drawAdvisor(advisorId, ...
    [args.offset(1) args.offset(2)+args.portraitSize(2)/2], ...
    [args.portraitSize(1) args.portraitSize(2)], 0);
% write advice text
str = centerMultilineText(adviceString, [args.offset(1) 0], args.maxLineWidth, 1);
for i = 1:length(str.lines)
    cfg.inspect = str.lines{i};
    Screen('DrawText', Sc.window, str.lines{i}.text, ...
        str.lines{i}.x, ...
        args.offset(2)+args.portraitSize(2)+args.triangleHeight+args.bubblePadding+args.lineHeight*(i-1));
end
% arrow
points = floor(getTriangle([0 args.triangleHeight], 180, [args.triangleHeight args.triangleHeight]));
cfg.inspect = points;
Screen('FillPoly', Sc.window, args.bubbleFrameColor, points+[args.offset(1) args.offset(2)+args.portraitSize(2)]);
% box
Screen('FrameRect', Sc.window, args.bubbleFrameColor, ...
    [args.offset(1) - args.maxLineWidth/2 - args.bubblePadding ...
    args.offset(2) + args.portraitSize(2) + args.triangleHeight ...
    args.offset(1) + args.maxLineWidth/2 + args.bubblePadding*2 ...
    args.offset(2) + args.portraitSize(2) + args.triangleHeight + args.bubblePadding*2 + args.lineHeight*length(str.lines)]);
Screen('TextSize', Sc.window, oldTextSize);