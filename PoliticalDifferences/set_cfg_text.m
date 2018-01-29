
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
cfg.specialInstructions.SECS = {...
    [cfg.path.base 'instructions' osSlash 'SECS' osSlash 'Slide1.PNG'] ...
    [cfg.path.base 'instructions' osSlash 'SECS' osSlash 'Slide2.PNG']};

%% ammend default instructions
cfg.intro{1}{1} = [cfg.path.base 'instructions' osSlash 'SECS' osSlash 'DotIntro.PNG'];
cfg.intro{2} = {...
    cfg.intro{2}{1} ...
    cfg.intro{2}{2} ...
    [cfg.path.base 'instructions' osSlash 'SECS' osSlash 'AdvisorIntro1.png'] ...
    [cfg.path.base 'instructions' osSlash 'SECS' osSlash 'AdvisorIntro2.png'] ...
    [cfg.path.base 'instructions' osSlash 'SECS' osSlash 'AdvisorPortraits.png']};
cfg.intro{3} = {...
    [cfg.path.base 'instructions' osSlash 'SECS' osSlash 'AdvisorPolitics.png'] ...
    cfg.intro{3}{1}};

clear x slideCount slideGroups j i;