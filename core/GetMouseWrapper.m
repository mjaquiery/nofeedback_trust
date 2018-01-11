function [x, y, buttons] = GetMouseWrapper()
%% Returns the mouse coordinates on the window of Sc rather than as raw coordinates
% - by Matt Jaquiery
%
% usage: [x, y, buttons] = GetMouseWrapper
%
% outputs: 
%   x - mouse x coordinate within Sc.rect
%   y - mouse y coordinate within Sc.rect
%   buttons - logical array where pressed buttons are represented by 1
%
% This function handles mouse control for non-fullscreen psychtoolbox
% windows (used extensively in the development environment)

global Sc; % screen object

[x, y, buttons] = GetMouse;
x = x - Sc.onscreenRect(1);
y = y - Sc.onscreenRect(2);
