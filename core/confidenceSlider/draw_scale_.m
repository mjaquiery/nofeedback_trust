function [] = draw_scale_()
% Usage:
% draw_scale_
%
% cfg must have .bar fields in order to work. 
% cfg.bar must have gaprect and barrect fields
% Default values are assigned otherwise.
%
% Niccolo Pescetelli

global cfg; % configuration object
global Sc; % screen object

%% check for fields existence
if ~isfield(cfg,'bar') || ~isfield(cfg.bar,'gaprect') || ~isfield(cfg.bar,'barrect')
    error(['cfg.bar missing necessary field gaprect or barrect\n' 'Check configuration settings in confidenceSlider/define_scale.m']);
end

%% draw barrect and gap
rect = [cfg.bar.barrect' cfg.bar.gaprect'];
Screen('FillRect', Sc.window, [cfg.bar.color.bar cfg.display.color.background],rect);

end