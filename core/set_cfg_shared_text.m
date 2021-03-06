%% Text prompts shared between trials
cfg.instr.prompt1.text          = {};
cfg.instr.finaldecision.text    = {'What'' s your final decision?'};
cfg.instr.interval.text         = {'LEFT' 'RIGHT'};
cfg.instr.tooslow.text          = {'Too slow.' 'Please press any key to continue.'};
cfg.instr.wrongbutton.text      = {'Wrong button' 'Please press any key to continue.'};

cfg.instr.cjtext.text           = {'sure' 'guessing' 'guessing' 'sure'};
cfg.instr.cjtext.position.offsetY = 40;

cfg.instr.chooseAdvisor.choice  = {'Click on one of the advisors to hear their advice'};
cfg.instr.chooseAdvisor.forced  = {'Click on the advisor to hear their advice'};
cfg.instr.chooseAdvisor.null    = {'Click on the silhouette to continue'};

cfg.instr.instr.text            = {'Left click with your mouse to make a decision.' ...
    'Press spacebar to confirm response.'};
cfg.instr.instr.position.y          = 80;
cfg.instr.instr.position.lineHeight = 20;

cfg.instr.estimated_obsacc.text = {'Your overall baseline accuracy (before any advice) was 71%' ...
    'What do you think this person''s accuracy was?' ...
    '[help_string]'...
    'Enter a number between 0 and 100 and press Enter: '};
cfg.instr.estimated_obsacc.position.y = [ ...% this works this way just because
    Sc.rect(4) * .3 ...
    Sc.rect(4) * .35 ...
    Sc.rect(4) * .65 ...
    Sc.rect(4) * .7 ...
    ];

cfg.instr.intro.text            = {'Press any key'};
cfg.instr.intro.position.y      = Sc.rect(4)*.6; 

cfg.instr.Q.q.retro.text        = {'How accurate do you think this person was when performing the task?'...
            'How much do you like this person?'...
            'How trustworthy are the opinions of this person?'...
            'How much are you influenced by the opinions of this person?'};
cfg.instr.Q.q.pro.text          = {'How accurate do you think this person will be performing the task?'...
            'How much are you going to like this person?'...
            'How trustworthy will be the opinions of this person?'...
            'How much will you be influenced by the opinions of this person?'};
cfg.instr.Q.a.text              = {'Extremely' 'Fairly' 'Not so much' 'Not at all'};
cfg.instr.Q.position.instr.y        = Sc.size(2)*.65;
cfg.instr.Q.position.instr.gap      = 30;
cfg.instr.Q.position.labelOffsetY   = 40; % distance between questionnaire bar and labels
cfg.instr.Q.position.q.y       = Sc.size(2)*.3; % centre of the advisor portrait
cfg.instr.Q.position.advisorY  = cfg.instr.Q.position.q.y + cfg.display.portrait.height/2;

%% Text properties
% size
cfg.instr.textSize.small    = 16;
cfg.instr.textSize.medium   = 24;
cfg.instr.textSize.large    = 32;
% colour
cfg.instr.textColor.default = [0 0 0]';

%% Instruction Intro Images
% Default instruction images are in ../instructions/Slide#.PNG
% These are defined in groups which are displayed together.
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

clear x slideCount slideGroups j i;