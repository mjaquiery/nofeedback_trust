function [advisorID] = getRandomAdvisor(cfg, advisorType)
%% Returns a random advisor
% - by Matt Jaquiery
%
% usage: advisorID = getRandomAdvisor(cfg, [advisorType]);
%
% Inputs:
%   cfg: the configuration file
%   advisorType: 0=real only (default); 1=both; 2=practice only
%
% Outputs:
%
%   advisorID: the ID of the selected advisor

% fix optional inputs
if nargin < 2
    advisorType = 0;
end

switch advisorType
    case 1 % Both advisor types
        low = 1;
        high = cfg.advisors.count.all;
    case 2 % practice only
        low = cfg.advisors.count.real+1;
        high = cfg.advisors.count.all;
    otherwise % real only
        low = 1;
        high = cfg.advisors.count.real;
end
    
advisorID = floor(low+rand*(high+1-low));