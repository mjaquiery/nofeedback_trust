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

clear x slideCount slideGroups j i;