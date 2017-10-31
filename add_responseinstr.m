%add response istructions
Screen('TextSize', Sc.window, 13);Screen('TextFont', Sc.window, 'Myriad Pro');
DrawFormattedText(Sc.window, instr{1}, 'center', (Sc.size(2)).*cfg.bar.positiony+80, 0);
DrawFormattedText(Sc.window, instr{2}, 'center', (Sc.size(2)).*cfg.bar.positiony+110, 0);
