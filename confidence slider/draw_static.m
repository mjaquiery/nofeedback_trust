function [] = draw_static(Sc,cfg)
% set font size
Screen('TextSize', Sc.window, 13);

%-- add progression bar. Helps motivation
draw_progression_bar(Sc, cfg);

% add fixation
add_fixation;

% draw scale
%if(cfg.debug)   disp('Drawing scale_'); end
draw_scale_(Sc,cfg);

% draw confidence and interval landmarks
%if(cfg.debug)   disp('Drawing landmarks'); end
draw_landmarks(Sc,cfg);

% add response instructions
%if(cfg.debug)   disp('Adding responseinstr'); end
add_responseinstr(Sc, cfg);

return