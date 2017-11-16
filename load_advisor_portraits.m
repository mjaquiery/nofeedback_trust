% preloads and saves the image data for the advisors
for id = 1:cfg.advisors.count.all
    cfg.advisor(id).imdata = imread([cfg.path.stims '/observer' int2str(cfg.advisor(id).pic) '.jpg']);
end

cfg.nullAdvisor.imdata = imread([cfg.path.stims '/silouette.jpg']);
cfg.noAdvisorChoice.imdata = imread([cfg.path.stims '/x.jpg']);
