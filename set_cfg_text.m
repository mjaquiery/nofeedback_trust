% landmarks on confidence scale
%cfg.instr.xshift = [-250,-50,50,250];
% now defined in define_scale.m

% Text prompts
cfg.instr.prompt1           = {};
cfg.instr.finaldecision     = {'What'' s your final decision?'};
cfg.instr.interval          = {'LEFT' 'RIGHT'};
cfg.instr.tooslow           = {'Too slow.' 'Please press any key to continue.'};
cfg.instr.wrongbutton       = {'Wrong button' 'Please press any key to continue.'};
cfg.instr.cjtext(:,:)       = {'certainly' 'maybe' 'maybe' 'certainly'};
cfg.instr.chooseAdvisor     = {'Click on one of the advisors to hear their advice'};
cfg.instr.instr             = {'Left click with your mouse to make a decision.' ...
    'Press spacebar to confirm response.'};
cfg.instr.estimated_obsacc  = {'Your baseline accuracy (before any advice) was 71%' ...
    'What do you think this person''s accuracy was?' ...
    'In the next screen you will be prompted to enter a value' ...
    'Press any button when you are ready' ...
    'Enter a number between 0 and 100 and press Enter: '};

cfg.instr.textSize.small    = 16;
cfg.instr.textSize.medium   = 24;
cfg.instr.textSize.large    = 32;
cfg.instr.textColor.default = [0 0 0];
cfg.instr.textBackground.default = [.5 .5 .5];

% Instruction Intro Images
x = [cfg.path.base 'instructions' slash 'instr'];
cfg.intro = {...
    {[x '1a.PNG'], [x '1b.PNG'], [x '1c.PNG']}, ...
    {[x '2a.PNG'], [x '2b.PNG']}, ...
    {[x '3.PNG']}, ...
    {[x '4.PNG']} ...
    };
clear x;