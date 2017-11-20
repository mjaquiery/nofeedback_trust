% landmarks on confidence scale
%cfg.instr.xshift = [-250,-50,50,250];
% now defined in define_scale.m

%% Text prompts
cfg.instr.prompt1.text          = {};
cfg.instr.finaldecision.text    = {'What'' s your final decision?'};
cfg.instr.interval.text         = {'LEFT' 'RIGHT'};
cfg.instr.tooslow.text          = {'Too slow.' 'Please press any key to continue.'};
cfg.instr.wrongbutton.text      = {'Wrong button' 'Please press any key to continue.'};

cfg.instr.cjtext.text           = {'certainly' 'maybe' 'maybe' 'certainly'};
cfg.instr.cjtext.position.offsetY = 40;

cfg.instr.chooseAdvisor.text    = {'Click on one of the advisors to hear their advice'};

cfg.instr.instr.text            = {'Left click with your mouse to make a decision.' ...
    'Press spacebar to confirm response.'};
cfg.instr.instr.position.y          = 80;
cfg.instr.instr.position.lineHeight = 20;

cfg.instr.estimated_obsacc.text = {'Your baseline accuracy (before any advice) was 71%' ...
    'What do you think this person''s accuracy was?' ...
    'In the next screen you will be prompted to enter a value' ...
    'Press any button when you are ready' ...
    'Enter a number between 0 and 100 and press Enter: '};
cfg.instr.estimated_obsacc.position.y = [ ...% this works this way just because
    Sc.rect(4) * .1 ...
    Sc.rect(4) * .15 ...
    Sc.rect(4) * .85 ...
    Sc.rect(4) * .9 ...
    ];

cfg.instr.intro.text            = {'Press any key'};
cfg.instr.intro.position.y      = Sc.rect(4)*.9; 

cfg.instr.Q.q.retro.text        = {'How accurate do you think this person was when performing the task?'...
            'How much do you like this person?'...
            'How trustworthy are the opinions of this person?'...
            'How much are you influenced by the opinions of this person?'};
cfg.instr.Q.q.pro.text          = {'How accurate do you think this person will be performing the task?'...
            'How much are you going to like this person?'...
            'How trustworthy will be the opinions of this person?'...
            'How much will you be influenced by the opinions of this person?'};
cfg.instr.Q.a.text              = {'Extremely' 'Fairly' 'Not so much' 'Not at all'};
cfg.instr.Q.position.instr.y        = Sc.size(2)*.9;
cfg.instr.Q.position.instr.gap      = 30;
cfg.instr.Q.position.labelOffsetY   = 40; % distance between questionnaire bar and labels
cfg.instr.Q.position.advisorY       = Sc.size(2)*.05+cfg.display.portrait.height/2; % centre of the advisor portrait
cfg.instr.Q.position.q.y            = cfg.instr.Q.position.advisorY + cfg.display.portrait.height/2 + Sc.size(2)*.05;

%% Text properties
% size
cfg.instr.textSize.small    = 16;
cfg.instr.textSize.medium   = 24;
cfg.instr.textSize.large    = 32;
% colour
cfg.instr.textColor.default = [0 0 0]';

% Instruction Intro Images
x = [cfg.path.base 'instructions' osSlash 'instr'];
cfg.intro = {...
    {[x '1a.PNG'], [x '1b.PNG'], [x '1c.PNG'], [x '1d.PNG'], [x '1e.PNG']}, ...
    {[x '2a.PNG'], [x '2b.PNG'], [x '2c.PNG'], [x '2d.PNG'], [x '2e.PNG'], [x '2f.PNG']}, ...
    {[x '3.PNG']}, ...
    {[x '4.PNG']} ...
    };
clear x;