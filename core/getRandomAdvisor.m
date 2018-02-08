function [advisorId] = getRandomAdvisor(advisorType)
%% Returns a random advisor
% - by Matt Jaquiery
%
% usage: advisorId = getRandomAdvisor(cfg, [advisorType]);
%
% Inputs:
%   cfg: the configuration file
%   advisorType: 0=real only (default); 1=both; 2=practice only
%
% Outputs:
%
%   advisorId: the Id of the selected advisor
%
% Advisor presentation is carefully counterbalanced in the main experiment,
% but this function enables practice blocks to quickly and dirtily build
% their trial types.

global cfg;

% fix optional inputs
if nargin < 1
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
    
advisorId = low+floor(rand*(high+1-low));