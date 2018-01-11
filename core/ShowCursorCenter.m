function ShowCursorCenter(cursorType, centerOn)
%% Wrapper for ShowCursor which centers the cusor in the screen
% - Matt Jaquiery, 2018
%
% usage: ShowCursorCenter([cursorType[, centerOn]]);
%
% inputs:
% cursorType: type of cursor to show, defaults to ''
% centerOn: [x y] coordinates for the center. Defaults to Sc.center()

global Sc; % screen configuration object

if nargin < 2, centerOn = Sc.center; end
if nargin < 1, cursorType = ''; end

SetMouse(centerOn(1), centerOn(2));
if isempty(cursorType)
    ShowCursor();
else
    ShowCursor(cursorType);
end
