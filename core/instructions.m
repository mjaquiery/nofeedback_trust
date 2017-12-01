function instructions(block, stage)
%% provides instructions to the participant
% - by Matt Jaquiery
%
% usage: instructions(Sc, cfg, set[, stage])
%
% inputs:
%   set - a cell containing filesystem paths of instruction images
%   stage - optional number indicating which cell entry to display
%

global cfg; % configuration object
global Sc; % Screen object

% input parameter screen
if nargin < 2, stage = 1; end
if class(block)~='double'
    inst = cfg.specialInstructions.(block);
else
    inst = cfg.intro{block};
end
% make and draw the stage instructions
a = imread(inst{stage});
stages = length(inst);
h = Screen('MakeTexture', Sc.window, a);
Screen('DrawTexture', Sc.window, h);
Screen('Flip', Sc.window);

[~, ~, keycode] = collect_response(inf);
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
        if stage < length(inst)
            stage = stage + 1;
        else
            WaitSecs(1.000);
            return
        end
end

% call the next instruction page
instructions(block, stage);
