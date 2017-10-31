function [ft] = display_response(Sc,cfg,temp,varargin)

if nargin < 4, 
    show_cj1    = false; 
else
    show_cj1    = true;
    cj1         = varargin{1};
    int1        = sign(cj1);
end
gs = round(cfg.bar.gap_size/2);
[haschanged,resp,int] = deal(temp(1),temp(2),temp(3));
fdTxt = cfg.instr.finaldecision{1};

%% display response
% draw static elements
draw_static

% display previous confidence
if show_cj1,
    cj1rect = CenterRectOnPoint([0,0,cfg.bar.cursorwidth,cfg.bar.cursorheight],...
    positions(abs(cj1)), ...
    Sc.rect(4).*cfg.bar.positiony);
    Screen('FillRect', Sc.window, [.4 .4 .4]',cj1rect );

    Screen('TextSize', Sc.window, 23); % change font size
    DrawFormattedText(Sc.window,fdTxt,'center',Sc.center(2) -100);
    Screen('TextSize', Sc.window, 13); % change back font size
end

% define response cursor position
cursorrect = CenterRectOnPoint([0,0,cfg.bar.cursorwidth,cfg.bar.cursorheight],...
    Sc.center(1) -((cfg.bar.nScale*cfg.bar.cursorwidth/2)+cfg.bar.cursorwidth) + ...
    ((resp+(int*gs)+cfg.bar.maxScale) * cfg.bar.cursorwidth  + cfg.bar.cursorwidth/2), ...
    Sc.rect(4) .* cfg.bar.positiony);

% draw cursor only after first click
if haschanged, Screen('FillRect', Sc.window, [.8 .8 .8]',cursorrect'); end
Screen('TextFont', Sc.window, 'Myriad Pro');


% Flip on screen
ft = Screen('Flip', Sc.window);

return