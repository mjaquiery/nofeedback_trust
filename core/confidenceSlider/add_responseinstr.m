function [] = add_responseinstr(drawCode)
% Usage:
% add_responseinstr(drawCode)
% 
% Inputs:
% drawCode: 0=don't draw; 1=use default offset; any other value=y offset (defaults to
% cfg.instr.instr.position.y)
%
% Required fields:
% cfg.instr.instr.text refers to the response mode instructions 
% at the bottom of the page.
% cfg.bar.positiony
%  
% Niccolo Pescetelli

global cfg; % configuration object
global Sc; % Screen object

if nargin < 1, drawCode = 1; end
if drawCode == 1, drawCode = cfg.instr.instr.position.y; end
if drawCode == 0, return; end

%% check required fields
if ~isfield(cfg,'instr') || ~isfield(cfg.instr,'instr')
    error(['Missing instruction text cell fields cfg.instr\n' 'Check configuration settings in set_cfg_text.m']);
end

%% add response istructions
oldTextSize = Screen('TextSize', Sc.window, cfg.instr.textSize.small);
Screen('TextFont', Sc.window, 'Myriad Pro');
DrawFormattedText(Sc.window, cfg.instr.instr.text{1}, 'center', ...
    (Sc.rect(4)*cfg.bar.positiony)+drawCode, 0);
DrawFormattedText(Sc.window, cfg.instr.instr.text{2}, 'center', ...
    (Sc.rect(4)*cfg.bar.positiony)+drawCode+cfg.instr.instr.position.lineHeight, 0);
Screen('TextSize', Sc.window, oldTextSize);

