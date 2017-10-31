%% display response

% draw static elements
draw_static

% display previous confidence
if show_cj1,
    cj1rect = CenterRectOnPoint([0,0,cursorwidth,cursorheight],...
        Sc.center(1) -((nScale*cursorwidth/2)+cursorwidth) + ...
        (((trials(t).cj1 - trials(t).int1)+(trials(t).int1*gs)+maxScale) * cursorwidth  + ...
        cursorwidth/2), Sc.center(2)+ Sc.size(2).*cfg.bar.positiony);
    Screen('FillRect', Sc.window, [0 0 0]',cj1rect );
    
    Screen('TextSize', Sc.window, 23); % change font size
    DrawFormattedText(Sc.window,fdTxt,'center',Sc.center(2) -100);
    Screen('TextSize', Sc.window, 13); % change back font size
end

% define response cursor position
cursorrect = CenterRectOnPoint([0,0,cursorwidth,cursorheight],...
    Sc.center(1) -((nScale*cursorwidth/2)+cursorwidth) + ((resp+(int*gs)+maxScale) * cursorwidth  + cursorwidth/2), Sc.size(2) .* cfg.bar.positiony);

% draw cursor only after first click
if haschanged, Screen('FillRect', Sc.window, [.8 .8 .8]',cursorrect'); end
Screen('TextFont', Sc.window, 'Myriad Pro');


% Flip on screen
ft = Screen('Flip', Sc.window);