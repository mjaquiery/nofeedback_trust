function defineSECS(includeGuns)
%% define the SECS questionnaire
% Matt Jaquiery, 2017
%
% usage: defineSECS([includeGuns])
%
% Sets up the variables necessary for administering the SECS
%
% inputs:
% includeGuns: whether to include question about gun ownership
% (specifically relevant to US audiences). Defaults to false

global cfg;
global Sc;

if nargin < 1, includeGuns = false; end

%% define questionnaire items
cfg.SECS.questions.text = {...
    'Abortion' ...
    'Limited government' ...
    'Military and national security' ...
    'Religion' ...
    'Welfare benefits' ...
    'Traditional marriage' ...
    'Traditional values' ...
    'Fiscal responsibility' ...
    'Business' ...
    'The family unit' ...
    'Patriotism'};
cfg.SECS.questions.name = {...
    'abortion' ...
    'government' ...
    'security' ...
    'religion' ...
    'welfare' ...
    'marriage' ...
    'values' ...
    'money' ...
    'business' ...
    'family' ...
    'patriotism'};
if includeGuns
    cfg.SECS.questions.name{length(cfg.SECS.questions.name)} = 'guns';
    cfg.SECS.questions.text{length(cfg.SECS.questions.text)} = 'Gun ownership';
end

cfg.SECS.question_order = randperm(length(cfg.SECS.questions.text)); % randomize questions presentation
cfg.SECS.revMask = zeros(1,12); % reverse scoring mask
cfg.SECS.revMask([1 5]) = 1; % Abortion and Welfare are reverse-scored
cfg.SECS.socialMask = zeros(1,12); 
cfg.SECS.socialMask([1 3 4 7 8 11 12]) = 1; % identify the social subscale items
cfg.SECS.prompt = 'Please indicate the extent to which you feel positive or negative towards each issue';
cfg.SECS.labels = {'Negative', 'Neutral', 'Positive'};
cfg.SECS.markerLabel.you.text = 'You';
cfg.SECS.markerLabel.you.bgColor = [.99 .99 .0]';
cfg.SECS.markerLabel.advisor.bgColor = [.2 .6 .99]';
cfg.SECS.markerLabel.padding = 5;
cfg.SECS.markerColor.advisor = [.2 .6 .99]';
cfg.SECS.markerColor.you = [.99 .99 .0]';
cfg.SECS.portrait.width = floor(cfg.display.portrait.width*.35);
cfg.SECS.portrait.height = floor(cfg.display.portrait.height*.35); 

%% Define screen presentation details
cfg.SECS.maxS=100; % max score
cfg.SECS.minS=1; % min score
cfg.SECS.ns=100; % number of points
cfg.SECS.cw=cfg.bar.cursorwidth; % cursor [visual indicator] width
cfg.SECS.ch=cfg.bar.cursorheight; % cursor height 
cfg.SECS.by = Sc.size(2)*.7; % bar y position
cfg.SECS.qy = Sc.size(2)*.4; % question position y
cfg.SECS.qSize = 46; % question text size
cfg.SECS.brct=CenterRectOnPoint([0 0 (cfg.SECS.ns *cfg.SECS.cw) (cfg.SECS.ch)], Sc.center(1), cfg.SECS.by);
cfg.SECS.bl = cfg.SECS.brct(3)- cfg.SECS.brct(1); % length (width) of the scale rectangle
cfg.SECS.instruction.offsetY = -200;

