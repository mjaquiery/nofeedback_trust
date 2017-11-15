function instructions(Sc, cfg, block, stage)
%% provides instructions to the participant
% - by Matt Jaquiery
%
% usage: instructions(Sc, cfg, set[, stage])
%
% inputs:
%   Sc - a psychtoolbox screen object
%   cfg - configuration settings object
%   set - a cell containing filesystem paths of instruction images
%   stage - optional number indicating which cell entry to display
%

% input parameter screen
if nargin < 4, stage = 1; end

% make and draw the stage instructions
a = imread(cfg.intro{block}{stage});
stages = length(cfg.intro{block});
h = Screen('MakeTexture', Sc.window, a);
Screen('DrawTexture', Sc.window, h);
Screen('Flip', Sc.window);

[~, ~, keycode] = collect_response(cfg, inf);
Screen('Flip', Sc.window);

key = KbName(keycode);

switch keycode
    case 'ESCAPE'
        sca
    case 'LeftArrow'
        % go back if we can
        if stage > 1
            stage = stage - 1;
        end
    otherwise
        % onwards and upwards
        if stage < length(cfg.intro{block})
            stage = stage + 1;
        else
            WaitSecs(1.000);
            return
        end
end

% call the next instruction page
instructions(Sc, cfg, block, stage);
