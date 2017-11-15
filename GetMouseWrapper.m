function [x, y, buttons] = GetMouseWrapper(Sc)
%% Returns the mouse coordinates on the window of Sc rather than as raw coordinates
% - by Matt Jaquiery
%
% usage: [x, y, buttons] = GetMouseWrapper(Sc)
%
% inputs: 
%   Sc - screen object
%
% outputs: 
%   x - mouse x coordinate within Sc.rect
%   y - mouse y coordinate within Sc.rect
%   buttons - logical array where pressed buttons are represented by 1
[x, y, buttons] = GetMouse;
x = x - Sc.rect(1);
y = y - Sc.rect(2);