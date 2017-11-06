function [advisor] = buildAdvisor(advisorID, cfg)
%% Build an advisor object by ID
% usage:
% [advisorLeft] = buildAdvisor[trials(t).advisor, cfg];
% 
% Inputs:
% advisorID - the ID of the desired advisor
% cfg - configuration file (holds details of the id->pic/voice mappings
%
% Output:
% advisor:
%   .id - id (same as advisorID)
%   .data - image data for portrait
%   .voice - voice ID

advisor.id = advisorID;
if isnan(advisor.id) % no advice option
   advisor.data = imread([cfg.path.stims '/silouette.jpg']); 
   advisor.voice = NaN;
else
    advisor.data = imread([cfg.path.stims '/observer' num2str(cfg.observer.pic(advisor.id)) '.jpg']);
    advisor.voice = cfg.observer.voice(advisor.id);
end