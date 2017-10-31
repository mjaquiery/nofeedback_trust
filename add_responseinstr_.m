% This script was executed in study3 under the name of add_responseinstr.m
% I changed the name in order to run study3f so that the main script could
% run the add_responseintr.m script in /myfunctions
% 17.04.2015

%add response istructions
Screen('TextSize', Sc.window, 13);Screen('TextFont', Sc.window, 'Myriad Pro');
DrawFormattedText(Sc.window, instr{1}, 'center', (Sc.size(2)).*cfg.bar.positiony+80, 0);
DrawFormattedText(Sc.window, instr{2}, 'center', (Sc.size(2)).*cfg.bar.positiony+110, 0);
