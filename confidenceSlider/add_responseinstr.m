function [] = add_responseinstr()
% Usage:
% [] = add_responseinstr()
% 
% Required fields:
% cfg.instr.instr refers to the response mode instructions 
% at the bottom of the page.
% cfg.bar.positiony
% 
% Default values are assigned only to cfg.instr.instr
% 
% Niccolo Pescetelli

global cfg; % configuration object
global Sc; % screen object

%% check required fields
if ~isfield(cfg,'instr') || ~isfield(cfg.instr,'instr')
    error(['Missing instruction text cells cfg.instr\n' 'Check configuration in set_cfg_text.m']);
end

%% add response istructions
Screen('TextSize', Sc.window, cfg.instr.textSize.small);
Screen('TextFont', Sc.window, 'Myriad Pro');
DrawFormattedText(Sc.window, cfg.instr.instr{1}, 'center', (Sc.rect(4)).*cfg.bar.positiony+80, 0);
DrawFormattedText(Sc.window, cfg.instr.instr{2}, 'center', (Sc.rect(4)).*cfg.bar.positiony+100, 0);

return
