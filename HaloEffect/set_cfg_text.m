% landmarks on confidence scale
%cfg.instr.xshift = [-250,-50,50,250];
% now defined in define_scale.m

%% Text prompts
cfg.instr.prompt1.text          = {};
cfg.instr.finaldecision.text    = {'What'' s your final decision?'};
cfg.instr.interval.text         = {'LEFT' 'RIGHT'};
cfg.instr.tooslow.text          = {'Too slow.' 'Please press any key to continue.'};
cfg.instr.wrongbutton.text      = {'Wrong button' 'Please press any key to continue.'};

cfg.instr.cjtext.text           = {'sure' 'guessing' 'guessing' 'sure'};
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
cfg.intro = {{} {} {} {}};
slideCount = 16;
slideGroups = [8 6 1 1]; % length of the slide groups
x = [cfg.path.base 'instructions' osSlash 'Slide'];
j = 0;
for i = 1:slideCount
    if j == 0 || i > sum(slideGroups(1:j))
        j = j+1;
        cfg.intro{j} = {[x int2str(i) '.PNG']};
    else
       cfg.intro{j}{length(cfg.intro{j})+1} = [x int2str(i) '.PNG']; 
    end
end

%% special instructions can be defined in 
% SECS instruction path
cfg.specialInstructions.SECS = {[cfg.path.base 'instructions' osSlash 'SECS' osSlash 'Slide1.PNG']};

clear x slideCount slideGroups j i;