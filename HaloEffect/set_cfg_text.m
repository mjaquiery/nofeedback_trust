% overwrite the estimated accuracy question to specify the dot task:
cfg.instr.estimated_obsacc.text = {'On the dot task, your overall accuracy (before any advice) was 71%' ...
    'What do you think this person''s accuracy was on the dot task?' ...
    '[help_string]'...
    'Enter a number between 0 and 100 and press Enter: '};

% Instruction Intro Images
cfg.intro = {{} {} {} {}};
slideCount = 16;
slideGroups = [8 6 1 1]; % length of the slide groups
x = [cfg.path.base 'instructions' osSlash 'HaloEffect' osSlash 'Slide'];
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