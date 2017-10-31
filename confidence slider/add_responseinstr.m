function [] = add_responseinstr(Sc,cfg)
% Usage:
% [] = add_responseinstr(Sc,cfg)
% 
% Required fields:
% cfg.instr.instr refers to the response mode instructions 
% at the bottom of the page.
% cfg.bar.positiony
% 
% Default values are assigned only to cfg.instr.instr
% 
% Niccolo Pescetelli

%% check required fields
if ~isfield(cfg,'instr')
    cfg.instr.instr = {'Left click with the mouse' ...
        'Press spacebar to confirm response'};
end
if ~isfield(cfg.instr,'instr')
    cfg.instr.instr = {'Left click with the mouse' ...
        'Press spacebar to confirm response'};
end

%% add response istructions
Screen('TextSize', Sc.window, 13);Screen('TextFont', Sc.window, 'Myriad Pro');
DrawFormattedText(Sc.window, cfg.instr.instr{1}, 'center', (Sc.rect(4)).*cfg.bar.positiony+80, 0);
DrawFormattedText(Sc.window, cfg.instr.instr{2}, 'center', (Sc.rect(4)).*cfg.bar.positiony+100, 0);

return
