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

cfg.instr.textSize.small    = 20;
cfg.instr.textSize.medium   = 30;
cfg.instr.textSize.large    = 40;