function text = centerMultilineText(text, centerOn, maxLineWidth, lineHeight)
%% Return a text struct with drawing instructions for centering 'text'
% Matt Jaquiery, 2017
%
% usage: text = centerMultilineText(text, centerOn[, maxLineWidth[, lineHeight]])
%
% Inputs: 
% text: a text string to write
% centerOn: [x y] coordinates of the centre point
% maxLineWidth: maximum width of lines (default to screen width)
% lineHeight: multiple of Screen('TextSize') between lines (default 1.5)
%
% Outputs:
% text: a text string with text.lines{} containing the individual lines and
% their x y drawing coordinates in .lines{i}.text .x and .y. Also includes
% the bounding box in .bounds
%
% This will NOT handle splitting on non-whitespace, so if you're trying to
% write 'supercalifragilisticexpialidociously' or something then don't
% complain when it breaks.
% It also doesn't handle lines which flow too long vertically.
%
global cfg; % configuration object
global Sc;  % screen object

if nargin < 4, lineHeight = 1.5; end
if nargin < 3, maxLineWidth = Sc.size(1); end

txtbounds = Screen('TextBounds', Sc.window, text);
if txtbounds(3) <= maxLineWidth
    ['line "' text '" is short enough: ' int2str(txtbounds(3)) '/' int2str(maxLineWidth)];
    % text fits already
    line.text = text;
    line.x = floor(centerOn(1) - txtbounds(3)/2);
    line.y = centerOn(2);
    line.bounds = txtbounds;
    text = struct();
    text.lines{1} = line;
    return;
end

words = strsplit(text);
lastIndex = 1;
lines = {};
for i = 1:length(words)
    str = char(join(words(lastIndex:i)));
    ['trying ' str];
    txtbounds = Screen('TextBounds', Sc.window, str);
    if txtbounds(3) > maxLineWidth
        ['too long, splitting'];
        % this is too long, so save the previous line and start a new one
        lastStr = char(join(words(lastIndex:i-1)));
        line.bounds = prevBounds;
        line.text = lastStr;
        line.x = floor(centerOn(1) - line.bounds(3)/2);
        lines{length(lines)+1} = line;
        lastIndex = i;
    end
    prevBounds = txtbounds;
end

% add the last line
str = char(join(words(lastIndex:end)));
txtbounds = Screen('TextBounds', Sc.window, str);
line.bounds = txtbounds;
line.text = str;
line.x = floor(centerOn(1) - line.bounds(3)/2);
lines{length(lines)+1} = line;

% now sort out the line heights
ys = 0 : lines{1}.bounds(4)*lineHeight : lines{1}.bounds(4)*lineHeight*length(lines);
for i = 1:length(lines)
    lines{i}.y = ys(i) + centerOn(2) - median(ys);
end

out.text = text;
out.lines = lines;
text = out;
