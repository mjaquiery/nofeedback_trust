function [] = draw_scale_(Sc,cfg)
% Usage:
% [] = draw_scale(Sc,cfg)
%
% cfg must have .bar fields in order to work. 
% cfg.bar must have gaprect and barrect fields
% Default values are assigned otherwise.
%
% Niccolo Pescetelli

%% check for fields existence
if ~isfield(cfg,'bar') || ~isfield(cfg.bar,'gaprect') || ~isfield(cfg.bar,'barrect')
    define_scale
end

%% draw barrect and gap
rect = [cfg.bar.barrect' cfg.bar.gaprect'];
Screen('FillRect', Sc.window, [[.3 .3 .3]' [.5 .5 .5]'],rect);
Screen('TextFont', Sc.window, 'Myriad Pro');

end