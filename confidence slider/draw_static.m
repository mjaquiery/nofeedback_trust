function [] = draw_static(Sc,cfg,suppressFixation,suppressResponseInstr)

if nargin < 4,    suppressResponseInstr = 0; end
if nargin < 3,    suppressFixation = 0;      end

% set font size
Screen('TextSize', Sc.window, 13);

%-- add progression bar. Helps motivation
draw_progression_bar(Sc, cfg);

% add fixation
if ~suppressFixation
    add_fixation;
end

% draw scale
%if(cfg.debug)   disp('Drawing scale_'); end
draw_scale_(Sc,cfg);

% draw confidence and interval landmarks
%if(cfg.debug)   disp('Drawing landmarks'); end
draw_landmarks(Sc,cfg);

% add response instructions
if ~suppressResponseInstr
    %if(cfg.debug)   disp('Adding responseinstr'); end
    add_responseinstr(Sc, cfg);
end

return