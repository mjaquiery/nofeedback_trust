function ShowCursorCenter(cursorType, centerOn)
%% Wrapper for ShowCursor which centers the cusor in the screen
% - Matt Jaquiery, 2018
%
% usage: ShowCursorCenter([cursorType[, centerOn]]);
%
% inputs:
% cursorType: type of cursor to show, defaults to ''
% centerOn: [x y] coordinates for the center. Defaults to Sc.center().
% Either x or y value can be set to NaN to use the Screen center for that
% axis.

global Sc; % screen configuration object

if nargin < 2 || length(centerOn) ~= 2, centerOn = Sc.center; end
if isnan(centerOn(1)), centerOn(1) = Sc.center(1); end
if isnan(centerOn(2)), centerOn(2) = Sc.center(2); end
if nargin < 1, cursorType = ''; end

SetMouse(centerOn(1)+Sc.onscreenRect(1), centerOn(2)+Sc.onscreenRect(2));
if isempty(cursorType)
    ShowCursor();
else
    ShowCursor(cursorType);
end
